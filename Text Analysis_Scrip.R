 
#######################################################
###                 Text Analysis                   ###
#######################################################



# Define a project called "Text Analysis" and store it in a file called "Modelling Techniques"

# Download the script "Text Analysis_Script" into the file "Modelling Techniques"



# Packages
install.packages("stringr")
install.packages("stringi")
install.packages("XML")
install.packages("RCurl")
install.packages("httr")

library("stringr")
library("stringi")
library("XML")
library("RCurl")
library("httr")

# Download the html document 

html <- getURL("https://www.consumeraffairs.com/computers/apple_imac.html", followlocation = TRUE)



# Parse html (Read the html document into an R object) 

doc = htmlParse(html, asText=TRUE)



# Extract only the text with the following structure <font color=red> "\<p>  Text text text \</p>" 
# The output is a vector of texts

plain.text <- xpathSApply(doc, "//p", xmlValue)

# xmlValue() extraction function

# Comments are stored into a vector 

plain.text<-as.vector(plain.text)[7:83]


# Packages 

install.packages("tm")  # A text mining package
oui
install.packages("SnowballC") # Text stemming (words are reduced to their root forms)
install.packages("wordcloud") # generate a word-cloud 
install.packages("RColorBrewer") # colors



library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


#  Corpus: a collection of text documents 
# Each element in plain.text is treated as a document

docs <- Corpus(VectorSource(plain.text))

# Detailed information of the corpus

inspect(docs)

# -------------------
# Document cleaning 
# -------------------

# Create a transformation function which modifies the content of the corpus

toSpace <- content_transformer(function (x, pattern ) gsub(pattern, " ", x))


# Special characters are replaced by white spaces

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")



# Document cleaning 

# Convert text to lowercase

docs <- tm_map(docs, content_transformer(tolower))


# Delete numbers

docs <- tm_map(docs, removeNumbers)


# Delete English stop words

docs <- tm_map(docs, removeWords, stopwords("english"))


# Delete your own list of unwanted words

docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 


# Delete punctuation

docs <- tm_map(docs, removePunctuation)


# Remove additional empty spaces

docs <- tm_map(docs, stripWhitespace)


#  Most frequent terms 

# For each document (in columns, the number of time a terms appears (in rows))

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
m[1:30,1:20]


# Most frequent terms in the corpus

v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)



# <font color=blue> wordcloud </font>

wordcloud(words = d$word, freq = d$freq, min.freq = 5, max.words=200, random.order=FALSE, colors=brewer.pal(8, "Dark2"))



# Frequent words 

findFreqTerms(dtm, lowfreq = 4)


# Word Frequency 

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,col =rainbow(10), main ="Most frequent words",ylab = "Word frequencies")


#----------------------
#  Sentiment Analysis 
#----------------------

# packages

install.packages("tidytext")

install.packages("dplyr")

install.packages("sentimentr")



library("tidytext")
library("dplyr")
library("sentimentr")


# Sentiment Analysis
# Texts in plain.text are merged to form a unique text

plain.text2<-paste(plain.text, collapse = "\n")

# Compute the general sentiment of each sentence of the resulting text

sentiment(plain.text2)

# Extract terms with a sentiment

extract_sentiment_terms(plain.text2)


# Compute the general sentiment of the text

sentiment_by(plain.text2)



