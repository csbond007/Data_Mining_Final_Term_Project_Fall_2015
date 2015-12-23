library(XML)
library(tm)
library(topicmodels)

setwd('C:/Users/Kaustav.Kaustav-Dell/Desktop/Data-Mining/Homework/Homework5')

# read random sample of 100 news documents
set.seed(1)
news = as.data.frame(xmlToDataFrame("Data/news_documents.xml", stringsAsFactors = FALSE)[,"c"])
news = VCorpus(DataframeSource(news))
news = news[sample(1:length(news), 100)]

# clean and compute tfidf
news.clean = tm_map(news, stripWhitespace)                          # remove extra whitespace
news.clean = tm_map(news.clean, removeNumbers)                      # remove numbers
news.clean = tm_map(news.clean, removePunctuation)                  # remove punctuation
news.clean = tm_map(news.clean, content_transformer(tolower))       # ignore case
news.clean = tm_map(news.clean, removeWords, stopwords("english"))  # remove stop words
news.clean = tm_map(news.clean, stemDocument)                       # stem all words
news.clean.tf = DocumentTermMatrix(news.clean, control = list(weighting = weightTf))

# remove empty documents
row.sums = apply(news.clean.tf, 1, sum)
news = news[row.sums > 0]
news.clean.tf = news.clean.tf[row.sums > 0,]

# train topic model with 50 topics
topic.model = LDA(news.clean.tf, 50)

# look at the top 10 words within the first 5 topics
terms(topic.model, 10)[,1:5]

# look at the top 10 topics within the first 5 documents
topics(topic.model, 10)[,1:5]

# group documents by most likely topic and look at one of the document groups
document.most.likely.topic = topics(topic.model, 1)
document.topic.clusters = split(news, document.most.likely.topic)
document.topic.clusters[[3]][[1]]$content
document.topic.clusters[[3]][[2]]$content
document.topic.clusters[[3]][[3]]$content

# gamma contains the document-topic matrix
topic.model@gamma[1:5,]

# cluster documents in topic space
document.topic.probabilities = topic.model@gamma  # topic distribution for each document
topic.space.kmeans.clusters = kmeans(document.topic.probabilities, 10)
topic.space.clustered.news = split(news, topic.space.kmeans.clusters$cluster)
topic.space.clustered.news[[1]][[1]]$content
topic.space.clustered.news[[1]][[2]]$content
topic.space.clustered.news[[1]][[3]]$content
