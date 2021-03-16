library(ggplot2)
## Read the emails dataset

###
# Load Data #
###
emails = read.csv("emails.csv")

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

emails = clean_emails
###
# Data insights #
###

### Compare spam vs non-spam occurences in the dataset

# We convert the second column into a numeric type
emails$spam <- sapply(emails$spam, as.numeric)

# Plot a histogram for better insight

emails.freq <- table(emails)
barplot(emails.freq,col=c("blue"),border=NA, main="Comparison between the occurence
        of spam (0) and non-spam emails in the dataset (1)")
