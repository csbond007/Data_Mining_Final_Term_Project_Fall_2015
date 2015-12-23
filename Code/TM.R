library(XML)
library(tm)

# read some news data from an XML file and transform it into a corpus
news = as.data.frame(xmlToDataFrame("Data/news_documents.xml", stringsAsFactors = FALSE)[,"c"])
news = VCorpus(DataframeSource(news))

# inspect the first two documents (sub-corpus)
inspect(news[1:2])

# view content of first document
news[[1]]$content

# compute TF-IDF matrix
news.tfidf = DocumentTermMatrix(news, control = list(weighting = weightTfIdf))

# clean up the corpus
news.clean = tm_map(news, stripWhitespace)                          # remove extra whitespace
news.clean = tm_map(news.clean, removeNumbers)                      # remove numbers
news.clean = tm_map(news.clean, removePunctuation)                  # remove punctuation
news.clean = tm_map(news.clean, content_transformer(tolower))       # ignore case
news.clean = tm_map(news.clean, removeWords, stopwords("english"))  # remove stop words
news.clean = tm_map(news.clean, stemDocument)                       # stem all words

# reinspect the first two documents
inspect(news.clean[1:2])

# recompute TF-IDF matrix
news.clean.tfidf = DocumentTermMatrix(news.clean, control = list(weighting = weightTfIdf))

# look at the non-zero term weights for the first document
first.doc = inspect(news.clean.tfidf[1,])
first.doc[,first.doc > 0]
