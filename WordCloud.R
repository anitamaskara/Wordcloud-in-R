install.packages('tm')
install.packages('twitteR')
install.packages('wordcloud')
install.packages('RColorBrewer')
install.packages('e1017')
install.packages('class')

library(tm)
library(wordcloud)
library(twitteR)
library(RColorBrewer)

ckey <- 'API key'
skey <- 'aPI secret key'
token <- 'Access token'
sectoken <- 'Access token secret'

# Connect to Twitter
setup_twitter_oauth(ckey, skey, access_token = token, access_secret = sectoken)

# Searching for most recent 1000 tweets in english related to soccer
soccer.tweets <- searchTwitter('soccer', n=1000, lang = 'en')

# Grabbing the text from the tweets
soccer.texts <- sapply(soccer.tweets, function(x) x$getText())

# clean the text to remove emoticons and characters that are not in UTF-8 
soccer.texts <- iconv(soccer.texts, 'UTF-8', 'ASCII') #iconv() is a encoding function

# create a corpus
soccer.corpus <- Corpus(VectorSource(soccer.texts))

# Create a Document Term Matrix
# We'll apply some transformations using the TermDocumentMatrix Function

# the list has a list of all the functions that will be performed on each text. These functions are in tm library
# we will convert everything to lowercase first
# then we will remove punctuation
# we will remove "soccer" in addition to all the stopwords in english language
# Lastly, we will remove numbers as well
term.doc.matrix <- TermDocumentMatrix(soccer.corpus, 
                                      control = list(tolower=TRUE,
                                                     removePunctuation = TRUE,
                                                     stopwords=c("soccer", "http", stopwords('english')),
                                                     removeNumbers=TRUE
                                                     ))


class(term.doc.matrix) 
#the output shows that it is "TermDocumentMatrix"    "simple_triplet_matrix"

# so we will convert term.doc.matrix to matrix
term.doc.matrix <- as.matrix(term.doc.matrix)

# Get word count
word.frequency <- sort(rowSums(term.doc.matrix), decreasing = T)

# looking at the head of word.frequency
head(word.frequency)
str(word.frequency) # shows words are listed under column "names"


# Creating a dataframe with all the words and their counts
dm <- data.frame(word = names(word.frequency), freq = word.frequency)

head(dm)

# create the wordcloud
wordcloud(dm$word, dm$freq, random.order = FALSE, colors = brewer.pal(8, "Dark2"))


