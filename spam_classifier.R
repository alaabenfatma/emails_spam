#------------------------------------------------------------------------------#
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
library(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(svDialogs)
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
rm(clean_emails)
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
  occurence of spam (0) and non-spam emails in the dataset (1)")



#------------------------------------------------------------------------------#
###
# Data mining #
###
'
# We divide the data into two separate sub-datasets, spam and clean.
spam <- emails[(clean_emails$spam == 1), ]
nonSpam <- emails[(clean_emails$spam == 0), ]

# We create a corpus for the spam dataset
documents <- Corpus(VectorSource(spam$text))
## We can inspect our Corpus as well
#inspect(documents)

# We remove the numbers from the corpus
#documents <- tm_map(documents, removeNumbers)
# We remove the the most common English stop words from the corpus
documents <- tm_map(documents, removeWords, stopwords("english"))
# Remove common words
documents <- tm_map(documents, removeWords, c("will")) 
documents <- tm_map(documents, removeWords, c("subject")) 

tmMatrix <- TermDocumentMatrix(documents)
m <- as.matrix(tmMatrix)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))'
#------------------------------------------------------------------------------#

###
# Statistical model using frequencies table #
###

### Open a prompt window to enter an email.
### i.e Hello, please send me money with western union, please sir, I am in Africa.
### (we convert it into lowercase)
email <- tolower(dlgInput("Write an email", Sys.info()["email"])$res)

### we remove punctuation
email = gsub('[[:punct:] ]+',' ',email)

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
  for (i in 1:3000){
    if(current_word==d$word[i]){
      Exists = TRUE
      cat(current_word, " exists as a common spam word.\n")
      count_spam_words = count_spam_words +  1
    }
  }
}
spam_score = count_spam_words / length(broken_email)
cat("Spam score= ", spam_score*100,"%\n")
### using the example given above, we get 64.28571 %
### another example: Dear Juliette, today the president of France, E. Macron, 
### has given a speech. (spam score: 30.76923%)