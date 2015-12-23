library(rgdal)
library(maptools)
library(lubridate)

setwd('C:/Users/Kaustav.Kaustav-Dell/Desktop/Data-Mining/Homework/Homework5')

source("CrimeUtil.R")

# read chicago boundary
city.boundary <- read.shapefile("City_20Boundary/City_Boundary.shp", "poly", "+init=epsg:3435", "+init=epsg:26971")

# read a small sample of tweets
#tweets = read.csv("tweets_small_sample.csv", header = TRUE, stringsAsFactors = FALSE)
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

# convert timestamp string to date representation, and pull out hour, day of week, and month
tweets$timestamp = as.POSIXct(tweets$timestamp)
tweets$hour = hour(tweets$timestamp)
tweets$day.of.week = wday(tweets$timestamp)
tweets$month = month(tweets$timestamp)

# plot
plot(city.boundary)
points(tweets[,c("x", "y")], pch = "o" , col="red" , cex = .1)

###################################################
mytext =  subset(tweets, select=c("text"))


# train topic model with 50 topics
topic.model = LDA(mytext, 1)

# look at the top 10 words within the first 5 topics
terms(topic.model, 10)[,1:5]

# look at the top 10 topics within the first 5 documents
topics(topic.model, 10)[,1:5]
