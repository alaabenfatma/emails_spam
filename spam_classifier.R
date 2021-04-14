#------------------------------------------------------------------------------#
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

tmdMatrix <- TermDocumentMatrix(corp)
sorted_matrix <- sort(rowSums(as.matrix(tmdMatrix)),decreasing=TRUE)
df <- data.frame(word = names(sorted_matrix),freq=sorted_matrix)
# See the first 10 most frequent words in SPAM emails.
head(df, 10)

set.seed(14)
# We generate a wordcloud through which we can observe the sparsity of the used
# words.
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=.33, 
          colors=brewer.pal(8, "Dark2"))
#------------------------------------------------------------------------------#
###
# Statistical model using frequencies table #
# ~~~ We can use the frequency table generated above to ~~~
###

### Open a prompt window to enter an email.
email <- dlgInput("Write an email", Sys.info()["email"])$res


### we count the number of words that are found in spam emails
spam_check <- function(input_mail){
  ### (we convert it into lowercase)
  email <- tolower(input_mail)
  
  ### we remove punctuation
  email = gsub('[[:punct:] ]+',' ',email)
  
  ### we remove stop words
  email<-removeWords(email,stopwords('en'))
  
  ### we break the email into an array of words
  broken_email <- strsplit(email, " ")[[1]]
  
  count_spam_words = 0
  for(i in 1:length(broken_email)){
    current_word = broken_email[i]
    Exists = FALSE
    ### the less the number of spam words we use for counting, the more accurate
    ### the model => this means that the words that being used, are found to be 
    ### very common spam words.
    for (i in 1:1000){
      if(current_word==df$word[i]){
        Exists = TRUE
        count_spam_words = count_spam_words +  1
      }
    }
  }
  spam_score <- count_spam_words / length(broken_email)
  return(spam_score)
}
# Measure verification time
start.time <- Sys.time()
spam_score <- spam_check(email)
end.time <- Sys.time()
time.taken <- end.time - start.time
# Time taken to verify
time.taken

if(spam_score>0.5){
  msg_box("This email is a spam email.")
}else{
  msg_box("This email is not a spam email.")
}
cat("Spam score= ", spam_score*100,"%\n")

' #UNCOMMENT to start accuracy test (slow!!)
## Accuracy test over 100 samples 

checker_score = 0
for(i in 1:100) {
  row <- emails[i,]
  text <- row$text
  class <- row$spam
  spam_score <- spam_check(text)
  if(spam_score>0.5){
    if(class == 1){
      checker_score = checker_score +1
    }
  }else{
    if(class == 0){
      checker_score = checker_score +1
    }
  }
}
print(checker_score) # This score is the accuracy
'
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

config_for_dtm <- list(tolower = TRUE, removeNumbers = TRUE,
                       removePunctuation = TRUE,stemming = TRUE)
emails_dtm <- DocumentTermMatrix(corpus, control= config_for_dtm)

train <- emails_dtm[1:4000,]
test <- emails_dtm[4001:5727,]

freqWords <- findFreqTerms(train,5)
freqTrain <- train[,freqWords]
freqTest <- test[,freqWords]

###
# Inspect the freq words of each emails subsets
###
'
inspect(freqTrain)
inspect(freqTest)
'

# If an item is frequent, it is set to Yes, No otherwise.
validate_freq <- function(x) {x <- ifelse(x > 0, "Yes", "No")}

train <- apply(freqTrain, MARGIN = 2, validate_freq)
test <- apply(freqTest, MARGIN = 2, validate_freq)

# Measure training time
start.time <- Sys.time()
classifier <- naiveBayes(train, y_train)
end.time <- Sys.time()
time.taken <- end.time - start.time
# Time taken to train
time.taken
testPredict <- predict(classifier, test)
testPredict


CrossTable(testPredict, y_test, prop.chisq = FALSE, prop.t = FALSE, 
           dnn = c('predicted', 'actual'))

'
We can see that only 58 emails from 1727 emails 
from the test dataset have been wrongly classified
This gives an accuracy of 96.64%
*** We can do BETTER! 
We remove the english stopwords (i, me, my... etc)
We obtain 52 wrong classifications, resulting in an accuracy of ~97%

  Cell Contents
|-------------------------|
|                       N |
|           N / Row Total |
|           N / Col Total |
|-------------------------|

 
Total Observations in Table:  1727 

 
             | actual 
   predicted |         0 |         1 | Row Total | 
-------------|-----------|-----------|-----------|
           0 |      1291 |         1 |      1292 | 
             |     0.999 |     0.001 |     0.748 | 
             |     0.962 |     0.003 |           | 
-------------|-----------|-----------|-----------|
           1 |        51 |       384 |       435 | 
             |     0.117 |     0.883 |     0.252 | 
             |     0.038 |     0.997 |           | 
-------------|-----------|-----------|-----------|
Column Total |      1342 |       385 |      1727 | 
             |     0.777 |     0.223 |           | 
-------------|-----------|-----------|-----------|
' 