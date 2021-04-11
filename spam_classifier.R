#------------------------------------------------------------------------------#
'
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
'
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(svDialogs)
library(gmodels)
library(e1071)

#------------------------------------------------------------------------------#

###
# Load Data #
###
emails = read.csv("emails.csv")
#------------------------------------------------------------------------------#

###
# Data pre-processing #
###

"
  This dataset contains a lot of useless columns. As a matter of fact,
  every columns that goes past the second one has to be removed
"
clean_emails <- emails[, -c(3:ncol(emails))] # delete columns 5 through 7

"
  Now, even though we have gotten rid of the non-used columns, we can still
  see some noise in the labels.
  Labels have to be either 1 (spam) or 0(non spam); however, we can see some
  empty or unreadable text in some of the label column.
"

# Remove rows with empty emails or empty label
# Empty email
cat("Dataset size:", dim(clean_emails))

clean_emails <- clean_emails[!(clean_emails$text == ""), ]
cat("Dataset size after removing empty emails:", dim(clean_emails))

# Empty label
clean_emails <- clean_emails[!(clean_emails$spam == ""), ]
cat("Dataset size after removing empty labels:", dim(clean_emails))

# Noise label (only keep targets with values of 0 and 1)
clean_emails <- clean_emails[grepl("0|1", clean_emails$spam),]
cat("Dataset size after removing bad labels:", dim(clean_emails))

emails <- clean_emails


#------------------------------------------------------------------------------#

###
# Data insights #
###

### Compare spam vs non-spam occurences in the dataset

# We convert the second column into a numeric type
emails$spam <- sapply(emails$spam, as.numeric)

# Plot a histogram for better insight

emails.freq <- table(emails)
barplot(emails.freq,col=c("blue"),border=NA, main="Comparison between the 
  occurence of spam (1) and non-spam emails in the dataset (0)")


#------------------------------------------------------------------------------#
###
# Data mining (using the whole dataset, not the subsets)#
# The goal of this section to generate a frequency table in order to observer
# the most common words used in Spam emails.
###

# We divide the data into two separate sub-datasets, spam and clean.
spam <- emails[(emails$spam == 1), ]
nonSpam <- emails[(emails$spam == 0), ]

# We create a corpus for the spam dataset
corp <- Corpus(VectorSource(spam$text))
# We remove the numbers from the corpus
corp <- tm_map(corp, removeNumbers)
# We remove the the most common English stop words from the corpus
corp <- tm_map(corp, removeWords, stopwords("english"))

tmMatrix <- TermDocumentMatrix(corp)
m <- sort(rowSums(as.matrix(tmMatrix)),decreasing=TRUE)
df <- data.frame(word = names(v),freq=m)
# See the first 10 most frequent words in SPAM emails.
head(df, 10)

set.seed(1234)
# We generate a wordcloud through which we can observe the sparsity of the used
# words.
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#------------------------------------------------------------------------------#
###
# Statistical model using frequencies table #
# ~~~ We can use the frequency table generated above to ~~~
###

### Open a prompt window to enter an email.
### (we convert it into lowercase)
email <- tolower(dlgInput("Write an email", Sys.info()["email"])$res)

### we remove punctuation
email = gsub('[[:punct:] ]+',' ',email)
### we remove stop words
email<-removeWords(email,stopwords('en'))
  
### we break the email into an array of words
broken_email <- strsplit(email, " ")[[1]]
### we count the number of words that are found in spam emails
count_spam_words = 0
for(i in 1:length(broken_email)){
  current_word = broken_email[i]
  Exists = FALSE
  ### the less the number of spam words we use for counting, the more accurate
  ### the model => this means that the words that being used, are found to be 
  ### very common spam words.
  for (i in 1:1000){
    edit_distance <- adist(current_word,d$word[i])
    if(edit_distance<=0){
      Exists = TRUE
      cat(current_word, " exists as a common spam word.\n")
      count_spam_words = count_spam_words +  1
    }
  }
}
spam_score = count_spam_words / length(broken_email)
if(spam_score>0.5){
  msg_box("This email is a spam email.")
}else{
  msg_box("This email is not a spam email.")
}
cat("Spam score= ", spam_score*100,"%\n")
#------------------------------------------------------------------------------#


###
# Naive Bayes Classifier #
###
### Now we train a Naive Bayes Classifier to classify an input mail to see whether
### it is a spam or not.

# now we shuffle the dataset rows (spam vs non-spam)
set.seed(42)
rows <- sample(nrow(emails))
emails <- emails[rows, ]
rm(clean_emails)

# We take 4000 samples for the training set, and 1727 for the test set.
###
# x = email samples
# y = email labels
###
x_train <- emails$text[1:4000]
y_train <- emails$spam[1:4000]
x_test <- emails$text[4001:5727]
y_test <- emails$spam[4001:5727]
# We create the corpus of training and testing datasets
traincorpus <- Corpus(VectorSource( as.vector(x_train)))
testcorpus <- Corpus(VectorSource( as.vector(x_test)))

### We do some text transformations (as done above with the corpus) for both
### training and testing sets.
traincorpus <- tm_map(traincorpus, removeNumbers)
testcorpus <- tm_map(testcorpus, removeNumbers)
traincorpus <- tm_map(traincorpus, removeWords, stopwords("english"))
testcorpus <- tm_map(testcorpus, removeWords, stopwords("english"))

### We create the term document matrix for both training and test splits
corpus <- VCorpus(VectorSource(emails$text)) 
corpus <- tm_map(corpus, removeWords, stopwords("english"),lazy=TRUE)

emails_dtm <- DocumentTermMatrix(corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

train <- emails_dtm[1:4000,]
test <- emails_dtm[4001:5727,]

freqWords <- findFreqTerms(train,5)
freqTrain <- train[,freqWords]
freqTest <- test[,freqWords]

###
# Inspect the freq words of each email
###
'
inspect(freqTrain)
inspect(freqTest)
'
# If an item is frequent, it is set to Yes, No otherwise.
validate_freq <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

train <- apply(freqTrain, MARGIN = 2,
               validate_freq)
test <- apply(freqTest, MARGIN = 2,
              validate_freq)


classifier <- naiveBayes(train, y_train)
testPredict <- predict(classifier, test)
testPredict

CrossTable(testPredict, y_test,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

'
We can see that only 58 emails from 1727 emails 
from the test dataset have been wrongly classified
This gives an accuracy of 96.64%
*** We can do BETTER! 
We remove the english stopwords (i, me, my... etc)
We obtain 52 wrong classifications, resulting in an accuracy of ~97%
' 