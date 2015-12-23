library(plyr)
library(dplyr)
library(lazyeval)

# read in the document-topic matrix
data.doc_topic <- read.table("sample1_doc-topics.txt")
sample1 <- read.csv("/home/juan/workspace/Fall2015/DM_Final_Project/sample1.csv", stringsAsFactors = FALSE)

# adjust the names 
paste0(rep("Topic-", 200), 0:399)
names(data.doc_topic) <- c("V1","Doc_Key",paste0(rep("Topic_", 200), 0:399))

# Add Doc_Key to sample1
sample1$Doc_Key <- data.doc_topic$Doc_Key

# the rows of the topic model variables sum to one, so they can be considered the proprotion
# of that topic for the corresponing document
rowSums(data.doc_topic[, -c(1,2)])

# tweets by ordered by topic 5
Topic5_TweetKeys <- data.doc_topic %>% arrange(desc(Topic_5)) %>% select(Doc_Key)
head(Topic5_TweetKeys)

#### TWEETS IN THIS KEY ARE INDEXED AT ZERO. The key is merely the row number when loaded into a 
#### dataframe in R from sample1.csv

# Now, we can generalize the above approach (NOTE standard eval version of arrange is used here)

sort.by.topic <- function(topic){
  data.doc_topic %>% arrange_(interp(~desc(v), v=as.name(topic))) %>% select(Doc_Key) %>% .[[1]]
}

# check it out:
test <- sort.by.topic("Topic_5")
head(test)

# get topic names
TOPICS <- names(data.doc_topic[1,-c(1,2)])

refugee_migration <- TOPICS[c(5,8,62,317,394) + 1]
terrorist_attack <- TOPICS[c(10,21,35,46,86,107,126,291,356,380) + 1]
fear <-  TOPICS[c(15,80,85,111,136,241,332,363,373,393) + 1]
religious_extremism <- TOPICS[c(19,70,130,158,215,227,358,383,388,396) + 1]
support_refugee <- TOPICS[ c(31,71,145,204,257,331)+ 1]
threat <- TOPICS[c(41,106,164,195,210,223,254,294,314,346,384) + 1]
response <- TOPICS[c(94,113,155,157,166,192,200,209,213,229,236,267,280,369,376,386) + 1]
political_conflict <- TOPICS[c(23,125,143,208,253,290,297,339) + 1]

df.refugee_migration <- vapply(refugee_migration, sort.by.topic, integer(56325))
df.terrorist_attack <- vapply(terrorist_attack, sort.by.topic, integer(56325))
df.fear <- vapply(fear, sort.by.topic, integer(56325))
df.religious_extremism <- vapply(religious_extremism, sort.by.topic, integer(56325))
df.support_refugee <- vapply(support_refugee, sort.by.topic, integer(56325))
df.threat <- vapply(threat, sort.by.topic, integer(56325))
df.response <- vapply(response, sort.by.topic, integer(56325))
df.political_conflict <- vapply(political_conflict, sort.by.topic, integer(56325))

# use SE transmute to collapse the columns by our groupings
data.doc_topic_collapse = transmute_(data.doc_topic, 
                                     REFUGEE_MIGRATION = paste(refugee_migration, collapse = " + "),
                                     TERRORIST_ATTACK = paste(terrorist_attack, collapse = " + "),
                                     FEAR = paste(fear, collapse = " + "),
                                     RELIGIOUS_EXTREMISM = paste(religious_extremism, collapse = " + "),
                                     SUPPORT_REFUGEE = paste(support_refugee, collapse = " + "),
                                     THREAT = paste(threat, collapse = " + "),
                                     RESPONSE = paste(response, collapse = " + "),
                                     POLITICAL_CONFLICT = paste(political_conflict, collapse = " + "))

# add doc_key
data.doc_topic_collapse$Doc_Key = sample1$Doc_Key

final.data = inner_join(data.doc_topic_collapse, sample1[,-c(1)], by = "Doc_Key")

