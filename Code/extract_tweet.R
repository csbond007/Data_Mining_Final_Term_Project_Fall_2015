library("twitteR")
library("ROAuth")
library("plyr")

rm(list = ls())

setwd('C:/Users/Kaustav.Kaustav-Dell/Desktop/Data-Mining/Project')

tweet_extraction <- function(location)
{
  tweet_set <- NULL
  
  tweets_geolocated <- searchTwitter("Terrorist", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Terrorist")
  
  tweets_geolocated <- searchTwitter("ISIS", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("ISIS")
  
  tweets_geolocated <- searchTwitter("Syria", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Syria")
  
  tweets_geolocated <- searchTwitter("Iraq", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Iraq")
  
  tweets_geolocated <- searchTwitter("ISIL", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("ISIL")
  
  tweets_geolocated <- searchTwitter("AK-47", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("AK-47")
  
  tweets_geolocated <- searchTwitter("ParisAttacks", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("ParisAttacks")
  
  tweets_geolocated <- searchTwitter("Raqqa", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Raqqa")
  
  tweets_geolocated <- searchTwitter("France", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("France")
  
  tweets_geolocated <- searchTwitter("Taliban", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Taliban")
  
  tweets_geolocated <- searchTwitter("IslamicState", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("IslamicState")
  
  tweets_geolocated <- searchTwitter("Islam", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Islam")
  
  tweets_geolocated <- searchTwitter("Muslim", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Muslim")
  
  tweets_geolocated <- searchTwitter("Jihadists", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Jihadists")
  
  tweets_geolocated <- searchTwitter("Obama", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Obama")
  
  tweets_geolocated <- searchTwitter("Turkey", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Turkey")
  
  tweets_geolocated <- searchTwitter("Iran", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Iran")
  
  tweets_geolocated <- searchTwitter("USA", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("USA")
  
  tweets_geolocated <- searchTwitter("JeSuisParis", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("JeSuisParis")
  
  tweets_geolocated <- searchTwitter("NotAfraid", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("NotAfraid")
  
  tweets_geolocated <- searchTwitter("CharlieHebdo", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("CharlieHebdo")
  
  tweets_geolocated <- searchTwitter("ParisShooting", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("ParisShooting")
  
  tweets_geolocated <- searchTwitter("ViveLaFrance", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("ViveLaFrance")
  
  tweets_geolocated <- searchTwitter("refugees", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("refugees")
  
  tweets_geolocated <- searchTwitter("Suicidebombing", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Suicidebombing")
  
  tweets_geolocated <- searchTwitter("Terrorism", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Terrorism")
  
  tweets_geolocated <- searchTwitter("AlQaeda", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("AlQaeda")
  
  tweets_geolocated <- searchTwitter("Pakistan", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Pakistan")
  
  tweets_geolocated <- searchTwitter("Hollande", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Hollande")
  
  tweets_geolocated <- searchTwitter("Putin", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Putin")
  
  tweets_geolocated <- searchTwitter("Russia", n=1500, lang="en", geocode=location)
  if(length(tweets_geolocated) > 0)
  {
    tweets_geolocated.df <- twListToDF(tweets_geolocated)
    tweet_set <- as.data.frame(rbind(tweet_set,tweets_geolocated.df))
  }
  print("Russia")
  
  return(tweet_set)
}
 #############################################################################

setwd('C:/Users/Kaustav.Kaustav-Dell/Desktop/Data-Mining/Project/Major_Cities')

tweet_aggregation_city <- function(city,location,i){
  
  if(i %% 2 == 0) 
  {
    consumer_key = "w9P3lWw9axx28WjZc9CVVunGG"
    consumer_secret = "AAMjSYJnOJJO4icWeDu13ebJJLfjMKNciHavXihD0o3sRB22bm"
    access_token = "4320576620-1vi7F5B6THvA918FD35z0hdBC072oJCc6lEPWNs"
    access_secret = "I75xQC9PnoHiPA9vRSaRubiwAYLvcfW9HkfvP8yGIrIRA"
    
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  }
  else
  {
    consumer_key = "AYg9Nkya7R7Nh58qBsF1Iz12R" 
    consumer_secret = "WSBCKOo8nMhYZkqQpG1iIGhhoics3Jpe2LKvqUl0amIirbIch7"
    access_token = "2205366252-3vfw87PoZ8aCJimUgzWlb8MDtUrSyY0zLsJCIwT"
    access_secret = "1OPVhZEXCEasRInY5HRjokH4INz29nGdnJ3wqGHhzoe9r"
    
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    
  }
  
  tweet_set <- as.data.frame(tweet_extraction(location))
  
  if(length(tweet_set) > 0)
  {
    filename <- paste0(city,".csv")
    write.csv(tweet_set, file = filename)
  }
  print(i)
  print(city)
}

name_city =NULL
name_city = data.frame(city=character(0),location=character(0))

#Chennai/Coordinates
Chennai = "13.0827,80.2707,500mi"
sub_name_city = data.frame(city="Chennai",location=Chennai)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Los Angeles/Coordinates
Los_Angeles = "34.0500,118.2500,500mi"
sub_name_city = data.frame(city="Los_Angeles",location=Los_Angeles)
name_city <- rbind.data.frame(name_city, sub_name_city)

London = "51.5072,0.1275,500mi"
sub_name_city = data.frame(city="London",location=London)
name_city <- rbind.data.frame(name_city, sub_name_city)

Paris = "48.8567,2.3508,500mi"
sub_name_city = data.frame(city="Paris",location=Paris)
name_city <- rbind.data.frame(name_city, sub_name_city)

Berlin = "52.5167,13.3833,500mi"
sub_name_city = data.frame(city="Berlin",location=Berlin)
name_city <- rbind.data.frame(name_city, sub_name_city)

Rome = "41.9000,12.5000,500mi"
sub_name_city = data.frame(city="Rome",location=Rome)
name_city <- rbind.data.frame(name_city, sub_name_city)

Madrid = "40.4000,3.7167,500mi"
sub_name_city = data.frame(city="Madrid",location=Madrid)
name_city <- rbind.data.frame(name_city, sub_name_city)

Raqqa = "35.9500, 39.0167,500mi"
sub_name_city = data.frame(city="Raqqa",location=Raqqa)
name_city <- rbind.data.frame(name_city, sub_name_city)

Bagdad = "33.3333, 44.4333,500mi"
sub_name_city = data.frame(city="Bagdad",location=Bagdad)
name_city <- rbind.data.frame(name_city, sub_name_city)

Mumbai = "18.9750, 72.8258,500mi"
sub_name_city = data.frame(city="Mumbai",location=Mumbai)
name_city <- rbind.data.frame(name_city, sub_name_city)

Delhi = "28.6139, 77.2090,500mi"
sub_name_city = data.frame(city="Delhi",location=Delhi)
name_city <- rbind.data.frame(name_city, sub_name_city)

Kolkata = "22.5667, 88.3667,500mi"
sub_name_city = data.frame(city="Kolkata",location=Kolkata)
name_city <- rbind.data.frame(name_city, sub_name_city)

#San Diego/Coordinates
San_Diego = "32.7150, 117.1625,500mi"
sub_name_city = data.frame(city="San_Diego",location=San_Diego)
name_city <- rbind.data.frame(name_city, sub_name_city)

#New_York_City/Coordinates
New_York_City = "40.7127, 74.0059,500mi"
sub_name_city = data.frame(city="New_York_City",location=New_York_City)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Boston/Coordinates
Boston = "42.3601, 71.0589,500mi"
sub_name_city = data.frame(city="Boston",location=Boston)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Chicago/Coordinates
Chicago = "41.8369, 87.6847,500mi"
sub_name_city = data.frame(city="Chicago",location=Chicago)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Washington_D_C/Coordinates
Washington_D_C = "38.9047, 77.0164,500mi"
sub_name_city = data.frame(city="Washington_D_C",location=Washington_D_C)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Rio_de_Janeiro/Coordinates
Rio_de_Janeiro = "22.9068, 43.1729,500mi"
sub_name_city = data.frame(city="Rio_de_Janeiro",location=Rio_de_Janeiro)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Sao_Paulo/Coordinates
Sao_Paulo = "23.5500, 46.6333,500mi"
sub_name_city = data.frame(city="Sao_Paulo",location=Sao_Paulo)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Buenos_Aires/Coordinates
Buenos_Aires = "34.6033, 58.3817,500mi"
sub_name_city = data.frame(city="Buenos_Aires",location=Buenos_Aires)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Beijing/Coordinates
Beijing = "39.9167, 116.3833,500mi"
sub_name_city = data.frame(city="Beijing",location=Beijing)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Shanghai/Coordinates
Shanghai = "31.2000, 121.5000,500mi"
sub_name_city = data.frame(city="Shanghai",location=Shanghai)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Saint_Petersburg/Coordinates
Saint_Petersburg = "59.9500, 30.3000,500mi"
sub_name_city = data.frame(city="Saint_Petersburg",location=Saint_Petersburg)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Moscow/Coordinates
Moscow = "55.7500, 37.6167,500mi"
sub_name_city = data.frame(city="Moscow",location=Moscow)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Istanbul/Coordinates
Istanbul = "41.0136, 28.9550,500mi"
sub_name_city = data.frame(city="Istanbul",location=Istanbul)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Perth/Coordinates
Perth = "31.9522, 115.8589,500mi"
sub_name_city = data.frame(city="Perth",location=Perth)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Adelaide/Coordinates
Adelaide = "34.9290, 138.6010,500mi"
sub_name_city = data.frame(city="Adelaide",location=Adelaide)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Sydney/Coordinates
Sydney = "33.8650, 151.2094,500mi"
sub_name_city = data.frame(city="Sydney",location=Sydney)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Melbourne/Coordinates
Melbourne = "37.8136, 144.9631,500mi"
sub_name_city = data.frame(city="Melbourne",location=Melbourne)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Hobart/Coordinates
Hobart = "42.8806, 147.3250,500mi"
sub_name_city = data.frame(city="Hobart",location=Hobart)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Tokyo/Coordinates
Tokyo = "35.6833, 139.6833,500mi"
sub_name_city = data.frame(city="Tokyo",location=Tokyo)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Seoul/Coordinates
Seoul = "37.5667, 126.9667,500mi"
sub_name_city = data.frame(city="Seoul",location=Seoul)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Auckland/Coordinates
Auckland = "36.8406, 174.7400,500mi"
sub_name_city = data.frame(city="Auckland",location=Auckland)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Johannesburg/Coordinates
Johannesburg = "26.2044, 28.0456,500mi"
sub_name_city = data.frame(city="Johannesburg",location=Johannesburg)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Nairobi/Coordinates
Nairobi = "1.2833, 36.8167,500mi"
sub_name_city = data.frame(city="Nairobi",location=Nairobi)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Kampala/Coordinates
Kampala = "0.3136, 32.5811,500mi"
sub_name_city = data.frame(city="Kampala",location=Kampala)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Cairo/Coordinates
Cairo = "30.0500, 31.2333,500mi"
sub_name_city = data.frame(city="Cairo",location=Cairo)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Kabul/Coordinates
Kabul = "34.5333, 69.1667,500mi"
sub_name_city = data.frame(city="Kabul",location=Kabul)
name_city <- rbind.data.frame(name_city, sub_name_city)

#Islamabad/Coordinates
Islamabad = "33.7167, 73.0667,500mi"
sub_name_city = data.frame(city="Islamabad",location=Islamabad)
name_city <- rbind.data.frame(name_city, sub_name_city)

sapply(1:nrow(name_city), function(x) tweet_aggregation_city(as.character(name_city$city[x]),as.character(name_city$location[x]),x))


######################################################################################
prepare_df <- function(path){
  
  setwd(path)
  
  temp = list.files(pattern="*.csv")
  temp_list <- as.data.frame(temp)
  names(temp_list) <- "file_name"
  
  data1 <- NULL
  
  for(i in 1:nrow(temp_list))
  {
    file_name = as.character(temp_list$file_name[i])
    print(i)
    print(file_name)
    sample_df <- as.data.frame(read.csv(file_name, header = T),stringsAsFactors=FALSE)
    sample_df <- sample_df[,c("text","longitude","latitude")]
    data1 <- as.data.frame(rbind(data1,sample_df))
  }
  
  df <- data1
  
  # repeat entries
  repeat_entries <- df[duplicated(df),]
  repeat_rows <- as.numeric(rownames(repeat_entries))
  
  df <- df[-repeat_rows,]
  df <- df[complete.cases(df),]
  
  return(df)
}

#################################################################################
major_city_df <- as.data.frame(prepare_df('C:/Users/Kaustav.Kaustav-Dell/Desktop/Data-Mining/Project/Major_Cities'),stringsAsFactors=FALSE)
setwd('C:/Users/Kaustav.Kaustav-Dell/Desktop/Data-Mining/Project/collected_tweets/')
write.csv(major_city_df, file ="major_city.csv",row.names=FALSE)
#############################################################################

###############################################################################
setwd('C:/Users/Kaustav.Kaustav-Dell/Desktop/Data-Mining/Project/')

data1 <- NULL
data2 <- NULL
data3 <- NULL
data4 <- NULL

data1 <- as.data.frame(read.csv('clean_city1.csv', header = T),stringsAsFactors=FALSE)
data2 <- as.data.frame(read.csv('clean_city2.csv', header = T),stringsAsFactors=FALSE)
data3 <- as.data.frame(read.csv('clean_city3.csv', header = T),stringsAsFactors=FALSE)
data4 <- as.data.frame(read.csv('clean_city4.csv', header = T),stringsAsFactors=FALSE)

names(data1) <- c("Latitude","Longitude","Radius")
names(data2) <- c("Latitude","Longitude","Radius")
names(data3) <- c("Latitude","Longitude","Radius")
names(data4) <- c("Latitude","Longitude","Radius")

total_loc_data <- NULL
total_loc_data <- as.data.frame(rbind(data1,data2,data3,data4),stringsAsFactors=FALSE)
total_loc_data <- total_loc_data[!duplicated(total_loc_data[, c("Latitude","Longitude")]),]

setwd('C:/Users/Kaustav.Kaustav-Dell/Desktop/Data-Mining/Project/Sample1/')

tweet_aggregation <- function(Latitude,Longitude,i){
  
  if(i %% 2 == 0) 
  {
    consumer_key = "w9P3lWw9axx28WjZc9CVVunGG"
    consumer_secret = "AAMjSYJnOJJO4icWeDu13ebJJLfjMKNciHavXihD0o3sRB22bm"
    access_token = "4320576620-1vi7F5B6THvA918FD35z0hdBC072oJCc6lEPWNs"
    access_secret = "I75xQC9PnoHiPA9vRSaRubiwAYLvcfW9HkfvP8yGIrIRA"
    
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  }
  else
  {
    consumer_key = "AYg9Nkya7R7Nh58qBsF1Iz12R" 
    consumer_secret = "WSBCKOo8nMhYZkqQpG1iIGhhoics3Jpe2LKvqUl0amIirbIch7"
    access_token = "2205366252-3vfw87PoZ8aCJimUgzWlb8MDtUrSyY0zLsJCIwT"
    access_secret = "1OPVhZEXCEasRInY5HRjokH4INz29nGdnJ3wqGHhzoe9r"
    
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    
  }
  
  Radius <- ",500mi"
  location <- paste0(Latitude,",",Longitude,Radius)
  print(i)
  print("------------------------------------------------------------------------")
  print(location)
  
  tweet_set <- NULL
  tweet_set <- as.data.frame(tweet_extraction(location))
  
  filename <- paste0(i,".csv")
  write.csv(tweet_set, file = filename)
}

total_loc_data <- as.data.frame(total_loc_data,stringsAsFactors=FALSE)
total_loc_data <- total_loc_data[complete.cases(total_loc_data),]

set.seed(121)
sampler <- as.data.frame(total_loc_data[sample(nrow(total_loc_data), 1500), ],stringsAsFactors=FALSE)
write.csv(sampler, file ="fusion.csv",row.names=FALSE)

##########################################################################################################
setwd('C:/Users/Kaustav.Kaustav-Dell/Desktop/Data-Mining/Project/Sample1')
sapply(1001:nrow(sampler), function(x) tweet_aggregation(sampler$Latitude[x],sampler$Longitude[x],x))
########################################################################################################

###########################################################################################
total_city_df <- as.data.frame(prepare_df('C:/Users/Kaustav.Kaustav-Dell/Desktop/Data-Mining/Project/Sample1'),stringsAsFactors=FALSE)
setwd('C:/Users/Kaustav.Kaustav-Dell/Desktop/Data-Mining/Project/collected_tweets/')
write.csv(total_city_df, file ="sample1.csv",row.names=FALSE)
setwd('C:/Users/Kaustav.Kaustav-Dell/Desktop/Data-Mining/Project/Sample1')
########################################################################################
