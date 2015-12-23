# Name - Kaustav Saha
# Computing ID - ks3hw
# Data-Mining - Case Study 5

rm(list = ls())

setwd('C:/Users/Kaustav.Kaustav-Dell/Desktop/Data-Mining/Homework/Homework5')

library(tm)
library(topicmodels)
library(SnowballC)
library(data.table)
library(reshape)
library(ggplot2)
library(ks)
library(RColorBrewer)

source("CrimeUtil.R")

KDE_Visualization <- function(tweets)
{
  kde.resolution.meters = 200
  
  kde.sample.points = tweets[,c("x","y")]
  
  # get estimation points for KDE -- a grid of evenly spaced points
  kde.est.points = get.grid.points(city.boundary, kde.resolution.meters, FALSE)
  
  # run and plot KDE, using 500 points from the sample
  kde.est = run.spatial.kde(kde.sample.points, kde.est.points, 500) 
  
  plot.spatial.kde(kde.est, kde.est.points)
  
  # add city boundary to KDE plot for interpretability
  plot(city.boundary, add=TRUE)
}

## Non-KDE based visualization
Non_KDE_Visualization <- function(tweets)
{
  plot(city.boundary)
  points(tweets[,c("x", "y")], pch = "o" , col="red" , cex = .1)
}

# read chicago boundary
city.boundary <- read.shapefile("City_20Boundary/City_Boundary.shp", "poly", "+init=epsg:3435", "+init=epsg:26971")

# read a small sample of tweets
tweets = read.csv("tweets_large_sample.csv", header = TRUE, stringsAsFactors = FALSE)


# reproject from degrees to meters
tweets.locations.lonlat = cbind(tweets$longitude, tweets$latitude)
meters.locations = project(tweets.locations.lonlat, proj="+init=epsg:26971")
tweets$x = meters.locations[,1]
tweets$y = meters.locations[,2]
tweets = tweets[is.finite(tweets$x) & is.finite(tweets$y),]  # some tweets are posted from outside the bounds of the chicago projection and their reprojections are infinite. remove such tweets.

# filter all tweets to be in the city
points.in.boundary = points.within.boundary(tweets[,c("x", "y")], "+init=epsg:26971", city.boundary)
tweets = tweets[points.in.boundary,]

tweets$day.of.week = wday(tweets$timestamp)
tweets$month = month(tweets$timestamp)
tweets$year = year(tweets$timestamp)

KDE_Visualization(tweets)
Non_KDE_Visualization(tweets)

weekend_tweets = tweets[tweets$day.of.week == 1 | tweets$day.of.week == 7,]
weekday_tweets = tweets[setdiff(rownames(tweets),rownames(weekend_tweets)),]

KDE_Visualization(weekend_tweets)
Non_KDE_Visualization(weekend_tweets)

KDE_Visualization(weekday_tweets)
Non_KDE_Visualization(weekday_tweets)

#################################################################################

LDA_Analysis <- function(tweet_set,topic_size,size)
{
  ## Analysis
  tweet_analysis = as.data.frame(tweet_set[,"text"])
  tweet_analysis = VCorpus(DataframeSource(tweet_analysis))
  tweet_analysis = tweet_analysis[sample(1:length(tweet_analysis), size)]
  
  # clean and compute tfidf
  tweet_analysis.clean = tm_map(tweet_analysis, stripWhitespace)                          # remove extra whitespace
  tweet_analysis.clean = tm_map(tweet_analysis.clean, removeNumbers)                      # remove numbers
  tweet_analysis.clean = tm_map(tweet_analysis.clean, removePunctuation)                  # remove punctuation
  tweet_analysis.clean = tm_map(tweet_analysis.clean, content_transformer(tolower))       # ignore case
  tweet_analysis.clean = tm_map(tweet_analysis.clean, removeWords, stopwords("english"))  # remove stop words
  tweet_analysis.clean = tm_map(tweet_analysis.clean, stemDocument)                       # stem all words
  tweet_analysis.clean.tf = DocumentTermMatrix(tweet_analysis.clean, control = list(weighting = weightTf))
  
  # remove empty documents
  row.sums = apply(tweet_analysis.clean.tf, 1, sum)
  tweet_analysis = tweet_analysis[row.sums > 0]
  tweet_analysis.clean.tf = tweet_analysis.clean.tf[row.sums > 0,]
  
  best.model <- lapply(seq(2,topic_size, by=1), function(k){LDA(tweet_analysis.clean.tf, k)})
  
  best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
  
  best.model.logLik.df <- data.frame(topics=c(2:topic_size), LL=as.numeric(as.matrix(best.model.logLik)))
  
  num_topics <- as.data.frame(best.model.logLik.df[which.max(best.model.logLik.df$LL),])
  
  k <- num_topics$topics # Optimized number of topics
  
  # train topic model with k topics
  topic.model = LDA(tweet_analysis.clean.tf, k)
  
  gammaDF <- as.data.frame(topic.model@gamma) # gamma contains the document-topic matrix
  names(gammaDF) <- c(1:k)
  
  # Now for each doc, find just the top-ranked topic   
  toptopics <- as.data.frame(cbind(document = row.names(gammaDF), 
                                   topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
  # inspect...
  print(head(toptopics,10))
  
  # calculate perplexity
  print(perplexity(topic.model,use_theta = TRUE))
  
  # look at the top 10 words within the topics
  print(terms(topic.model, 10))
  
}

weekend_sample_size <- as.integer(nrow(weekend_tweets)/20)
LDA_Analysis(weekend_tweets,2,weekend_sample_size)
LDA_Analysis(weekend_tweets,4,weekend_sample_size)
LDA_Analysis(weekend_tweets,6,weekend_sample_size)
LDA_Analysis(weekend_tweets,8,weekend_sample_size)
LDA_Analysis(weekend_tweets,10,weekend_sample_size)
LDA_Analysis(weekend_tweets,12,weekend_sample_size)
LDA_Analysis(weekend_tweets,14,weekend_sample_size)
LDA_Analysis(weekend_tweets,16,weekend_sample_size)
LDA_Analysis(weekend_tweets,18,weekend_sample_size)
LDA_Analysis(weekend_tweets,20,weekend_sample_size)

weekday_sample_size <- as.integer(nrow(weekday_tweets)/50)
LDA_Analysis(weekday_tweets,2,weekday_sample_size)
LDA_Analysis(weekday_tweets,4,weekday_sample_size)
LDA_Analysis(weekday_tweets,6,weekday_sample_size)
LDA_Analysis(weekday_tweets,8,weekday_sample_size)
LDA_Analysis(weekday_tweets,10,weekday_sample_size)
LDA_Analysis(weekday_tweets,12,weekday_sample_size)
LDA_Analysis(weekday_tweets,14,weekday_sample_size)
LDA_Analysis(weekday_tweets,16,weekday_sample_size)
LDA_Analysis(weekday_tweets,18,weekday_sample_size)
LDA_Analysis(weekday_tweets,20,weekday_sample_size)


####################################################################
convert_polygon_to_points <- function(type.points)
{
  poly_specs <- slot(type.points,"polygons")
  points <- as.data.frame(matrix(0,nrow = length(poly_specs),ncol = 2))
  names(points) <- c('x', 'y')
  for (i in 1:length(poly_specs)) {
    point_x_y <- slot(slot(poly_specs[[i]],"Polygons")[[1]],"coords")
    
    points[i,"x"] <- mean(point_x_y[,1])
    points[i,"y"] <- mean(point_x_y[,2])
  }
  return(points)
}

## School
School.points = read.shapefile("Sets/Schools/CPS_School_Locations_SY1415.shp", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
School.points = as.data.frame(School.points)
setnames(School.points, "coords.x1", "x")
setnames(School.points, "coords.x2", "y")

## Neighborhoods
Neighborhoods.points = read.shapefile("Sets/Boundaries_-_Neighborhoods/Neighborhoods.shp", "poly", "+init=epsg:3435", "+init=epsg:26971")
Neighborhoods.points = convert_polygon_to_points(Neighborhoods.points)

## Parks
Parks.points = read.shapefile("Sets/Parks/Parks.shp", "poly", "+init=epsg:3435", "+init=epsg:26971")
Parks.points = convert_polygon_to_points(Parks.points)

## Landmarks
Landmarks.points = read.shapefile("Sets/Landmarks/LandmarksNationalRegister.shp", "poly", "+init=epsg:3435", "+init=epsg:26971")
Landmarks.points <- convert_polygon_to_points(Landmarks.points)

## Rail_Stations
Rail_Stations.points = read.shapefile("Sets/CTA_Stations/CTA_Stations.shp", "points", "+init=epsg:3435", "+init=epsg:26971")@coords
Rail_Stations.points = as.data.frame(Rail_Stations.points)
setnames(Rail_Stations.points, "coords.x1", "x")
setnames(Rail_Stations.points, "coords.x2", "y")

r <- 50 ## modify values to 50,40,30,20,10,5

twitter_set <- data.frame(text=character(), stringsAsFactors = FALSE)

tweet_split = function(sub_tweet)
{
  sub_tweet = as.data.frame(sub_tweet)
  twitter_set = rbind(twitter_set,sub_tweet)
}

radius_function = function(point)
{
  x_value <- point["x"]
  y_value <- point["y"]
  sub.tweets <- tweets[(sqrt((x_value - tweets$x)^2 + (y_value - tweets$y)^2) < r), ]
  
  apply(sub.tweets, 1, tweet_split)
}

#######
school_twitter_set = apply(School.points, 1, radius_function)

school_twitter_set = as.data.frame(school_twitter_set)
school_twitter_set = t(school_twitter_set)
school_twitter_set = as.data.frame(school_twitter_set)
setnames(school_twitter_set,"1","timestamp")
setnames(school_twitter_set,"2","text")
setnames(school_twitter_set,"3","longitude")
setnames(school_twitter_set,"4","latitude")
setnames(school_twitter_set,"5","x")
setnames(school_twitter_set,"6","y")
setnames(school_twitter_set,"7","day.of.week")
setnames(school_twitter_set,"8","month")
setnames(school_twitter_set,"9","year")

nrow(school_twitter_set) 
school_twitter_set <- unique(school_twitter_set)
nrow(school_twitter_set) 
####

#######
Neighborhoods_twitter_set = apply(Neighborhoods.points, 1, radius_function)

Neighborhoods_twitter_set = as.data.frame(Neighborhoods_twitter_set)
Neighborhoods_twitter_set = t(Neighborhoods_twitter_set)
Neighborhoods_twitter_set = as.data.frame(Neighborhoods_twitter_set)
setnames(Neighborhoods_twitter_set,"1","timestamp")
setnames(Neighborhoods_twitter_set,"2","text")
setnames(Neighborhoods_twitter_set,"3","longitude")
setnames(Neighborhoods_twitter_set,"4","latitude")
setnames(Neighborhoods_twitter_set,"5","x")
setnames(Neighborhoods_twitter_set,"6","y")
setnames(Neighborhoods_twitter_set,"7","day.of.week")
setnames(Neighborhoods_twitter_set,"8","month")
setnames(Neighborhoods_twitter_set,"9","year")

nrow(Neighborhoods_twitter_set) 
Neighborhoods_twitter_set <- unique(Neighborhoods_twitter_set)
nrow(Neighborhoods_twitter_set) 
####

#######
Parks_twitter_set = apply(Parks.points, 1, radius_function)

Parks_twitter_set = as.data.frame(Parks_twitter_set)
Parks_twitter_set = t(Parks_twitter_set)
Parks_twitter_set = as.data.frame(Parks_twitter_set)
setnames(Parks_twitter_set,"1","timestamp")
setnames(Parks_twitter_set,"2","text")
setnames(Parks_twitter_set,"3","longitude")
setnames(Parks_twitter_set,"4","latitude")
setnames(Parks_twitter_set,"5","x")
setnames(Parks_twitter_set,"6","y")
setnames(Parks_twitter_set,"7","day.of.week")
setnames(Parks_twitter_set,"8","month")
setnames(Parks_twitter_set,"9","year")

nrow(Parks_twitter_set) 
Parks_twitter_set <- unique(Parks_twitter_set)
nrow(Parks_twitter_set) 
####

#######
Landmarks_twitter_set = apply(Landmarks.points, 1, radius_function)

Landmarks_twitter_set = as.data.frame(Landmarks_twitter_set)
Landmarks_twitter_set = t(Landmarks_twitter_set)
Landmarks_twitter_set = as.data.frame(Landmarks_twitter_set)
setnames(Landmarks_twitter_set,"1","timestamp")
setnames(Landmarks_twitter_set,"2","text")
setnames(Landmarks_twitter_set,"3","longitude")
setnames(Landmarks_twitter_set,"4","latitude")
setnames(Landmarks_twitter_set,"5","x")
setnames(Landmarks_twitter_set,"6","y")
setnames(Landmarks_twitter_set,"7","day.of.week")
setnames(Landmarks_twitter_set,"8","month")
setnames(Landmarks_twitter_set,"9","year")

nrow(Landmarks_twitter_set) 
Landmarks_twitter_set <- unique(Landmarks_twitter_set)
nrow(Landmarks_twitter_set) 
####

#######
Rail_Stations_twitter_set = apply(Rail_Stations.points, 1, radius_function)

Rail_Stations_twitter_set = as.data.frame(Rail_Stations_twitter_set)
Rail_Stations_twitter_set = t(Rail_Stations_twitter_set)
Rail_Stations_twitter_set = as.data.frame(Rail_Stations_twitter_set)
setnames(Rail_Stations_twitter_set,"1","timestamp")
setnames(Rail_Stations_twitter_set,"2","text")
setnames(Rail_Stations_twitter_set,"3","longitude")
setnames(Rail_Stations_twitter_set,"4","latitude")
setnames(Rail_Stations_twitter_set,"5","x")
setnames(Rail_Stations_twitter_set,"6","y")
setnames(Rail_Stations_twitter_set,"7","day.of.week")
setnames(Rail_Stations_twitter_set,"8","month")
setnames(Rail_Stations_twitter_set,"9","year")

nrow(Rail_Stations_twitter_set) 
Rail_Stations_twitter_set <- unique(Rail_Stations_twitter_set)
nrow(Rail_Stations_twitter_set) 
####

### r = 50
LDA_Analysis(school_twitter_set,10,nrow(school_twitter_set)) 
LDA_Analysis(Neighborhoods_twitter_set,10,nrow(Neighborhoods_twitter_set))
LDA_Analysis(Parks_twitter_set,10,nrow(Parks_twitter_set))
LDA_Analysis(Landmarks_twitter_set,10,nrow(Landmarks_twitter_set))
LDA_Analysis(Rail_Stations_twitter_set,10,nrow(Rail_Stations_twitter_set))

## r = 40
LDA_Analysis(school_twitter_set,10,nrow(school_twitter_set))
LDA_Analysis(Neighborhoods_twitter_set,10,nrow(Neighborhoods_twitter_set))
LDA_Analysis(Parks_twitter_set,10,nrow(Parks_twitter_set))
LDA_Analysis(Landmarks_twitter_set,10,nrow(Landmarks_twitter_set))
LDA_Analysis(Rail_Stations_twitter_set,10,nrow(Rail_Stations_twitter_set))

## r = 30
LDA_Analysis(school_twitter_set,10,nrow(school_twitter_set)) 
LDA_Analysis(Neighborhoods_twitter_set,10,nrow(Neighborhoods_twitter_set))
LDA_Analysis(Parks_twitter_set,10,nrow(Parks_twitter_set))
LDA_Analysis(Landmarks_twitter_set,10,nrow(Landmarks_twitter_set))
LDA_Analysis(Rail_Stations_twitter_set,10,nrow(Rail_Stations_twitter_set))

## r = 20
LDA_Analysis(school_twitter_set,10,nrow(school_twitter_set)) 
LDA_Analysis(Neighborhoods_twitter_set,10,nrow(Neighborhoods_twitter_set))
LDA_Analysis(Parks_twitter_set,10,nrow(Parks_twitter_set))
LDA_Analysis(Landmarks_twitter_set,10,nrow(Landmarks_twitter_set))
LDA_Analysis(Rail_Stations_twitter_set,10,nrow(Rail_Stations_twitter_set))

## r = 10
LDA_Analysis(school_twitter_set,10,nrow(school_twitter_set)) 
LDA_Analysis(Neighborhoods_twitter_set,10,nrow(Neighborhoods_twitter_set))
LDA_Analysis(Parks_twitter_set,10,nrow(Parks_twitter_set))
LDA_Analysis(Landmarks_twitter_set,10,nrow(Landmarks_twitter_set))
LDA_Analysis(Rail_Stations_twitter_set,10,nrow(Rail_Stations_twitter_set))

## r = 5
LDA_Analysis(school_twitter_set,10,nrow(school_twitter_set)) 
LDA_Analysis(Neighborhoods_twitter_set,10,nrow(Neighborhoods_twitter_set))
LDA_Analysis(Parks_twitter_set,10,nrow(Parks_twitter_set))
LDA_Analysis(Landmarks_twitter_set,10,nrow(Landmarks_twitter_set))
LDA_Analysis(Rail_Stations_twitter_set,10,nrow(Rail_Stations_twitter_set))

#########################################################################

############################################################ Bonus Problem  #######################################

Build_data.frame <- function(data,flag,tweet_t)
{
  twitter_set <- NULL
  r <- 50
  
  for(i in 1:nrow(data))
  {
    x_value <- data[i,"x"]
    y_value <- data[i,"y"]
    sub.tweets <- tweet_t[(sqrt((x_value - tweet_t$x)^2 + (y_value - tweet_t$y)^2) < r), ]
    if(nrow(sub.tweets)>0)
    {
      sub.tweets <- as.data.frame(sub.tweets)
      if(flag == 1)
      {
        sub.tweets$response <- data[i,"response"]
      }
      else
      {
        sub.tweets$x <- x_value 
        sub.tweets$y <- y_value 
      }
      twitter_set = rbind(twitter_set,sub.tweets)
    }
  }
  
  twitter_set <- unique(twitter_set)
  twitter_set <- twitter_set[nchar(twitter_set$text)>100,]
  
  ## Analysis
  tweet_analysis = as.data.frame(twitter_set[,"text"])
  tweet_analysis = VCorpus(DataframeSource(tweet_analysis))
  
  # clean and compute tfidf
  tweet_analysis.clean = tm_map(tweet_analysis, stripWhitespace)                          # remove extra whitespace
  tweet_analysis.clean = tm_map(tweet_analysis.clean, removeNumbers)                      # remove numbers
  tweet_analysis.clean = tm_map(tweet_analysis.clean, removePunctuation)                  # remove punctuation
  tweet_analysis.clean = tm_map(tweet_analysis.clean, content_transformer(tolower))       # ignore case
  tweet_analysis.clean = tm_map(tweet_analysis.clean, removeWords, stopwords("english"))  # remove stop words
  tweet_analysis.clean = tm_map(tweet_analysis.clean, stemDocument)                       # stem all words
  tweet_analysis.clean.tf = DocumentTermMatrix(tweet_analysis.clean, control = list(weighting = weightTf))
  
  # remove empty documents
  row.sums = apply(tweet_analysis.clean.tf, 1, sum)
  tweet_analysis = tweet_analysis[row.sums > 0]
  tweet_analysis.clean.tf = tweet_analysis.clean.tf[row.sums > 0,]
  
  # train topic model with 10 topics
  topic.model = LDA(tweet_analysis.clean.tf, 10)
  
  gammaDF <- as.data.frame(topic.model@gamma) 
  names(gammaDF) <- c(1:10)
  
  twitter_set <- cbind(twitter_set,gammaDF)
  if(flag == 1)
  {
    twitter_set <- twitter_set[,c("response","1","2","3","4","5","6","7","8","9","10")]
  }
  else
  {
    twitter_set <- twitter_set[,c("x","y","1","2","3","4","5","6","7","8","9","10")]
  }
  
  print(terms(topic.model, 10))
  
  return(twitter_set)
}


Model_Analysis <- function(training,testing,eval,t1,t2,t3)
{
  prediction.resolution.meters = 200
  
  # get negative observations within chicago
  non.crime.points = cbind(0, get.grid.points(city.boundary, prediction.resolution.meters, TRUE))
  names(non.crime.points)[1] = "response"
  
  # get positive observations from training dataset within chicago
  training.crime.points = cbind(1, training[,c("x","y")])
  names(training.crime.points)[1] = "response"
  
  # combine positive and negative points
  training.data = rbind(non.crime.points, training.crime.points)
  
  tweet_t <- tweets[tweets$month==t1 & tweets$year==2014,]
  set <- Build_data.frame(training.data,1,tweet_t)
  nrow(set[set$response>0,])
  
  glm.fit = glm(response ~ ., data = set, family=binomial)
  print(summary(glm.fit))
  
  # build dataframe to predict
  prediction.points = testing[,c("x","y")]
  prediction.data = as.data.frame(prediction.points)
  
  tweet_t <- tweets[tweets$month==t2 & tweets$year==2014,]
  prediction.data <- Build_data.frame(prediction.data,0,tweet_t)
  prediction.data <- as.data.frame(prediction.data)
  
  prediction.threat <- as.data.frame(prediction.data[,-(1:2)])
  ####################################################
  # run prediction
  threats = predict(glm.fit, prediction.threat, type="response")
  threats = as.data.frame(threats)
  # build prediction dataframe
  temp1 = prediction.data[,c("x","y")]
  assault.prediction = cbind(temp1,threats)
  
  # build dataframe to evaluate
  eval.points = eval[,c("x","y")]
  eval.data = as.data.frame(eval.points)
  
  tweet_t <- tweets[tweets$month==t3 & tweets$year==2014,]
  eval.data <- Build_data.frame(eval.data,0,tweet_t)
  eval.data <- eval.data[,c("x","y")]
  
  # evaluate prediction 
  plot.surveillance.curve(assault.prediction, eval.data, prediction.resolution.meters, city.boundary)
  
}

# read Assault data for January - December 2014
assault = sample.crime("2014_ASSAULT.csv", -1, 1, 12)

assault_jan = assault[assault$month == 1,]
assault_feb = assault[assault$month == 2,]
assault_mar = assault[assault$month == 3,]

###############################
Model_Analysis(assault_jan,assault_feb,assault_mar,1,2,3)
###############################

