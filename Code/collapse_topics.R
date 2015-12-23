library(plyr)
library(dplyr)
library(lazyeval)

# read in the document-topic matrix
data.doc_topic <- read.table("majorcity_doc-topics.txt")
major.city <- read.csv("/home/juan/workspace/Fall2015/DM_Final_Project/major_city.csv", stringsAsFactors = FALSE)

# adjust the names 
paste0(rep("Topic-", 200), 0:399)
names(data.doc_topic) <- c("V1","Doc_Key",paste0(rep("Topic_", 200), 0:399))

# Add Doc_Key to major.city
major.city$Doc_Key <- data.doc_topic$Doc_Key

# Helper function that will be applied over major.city if we want to sort by topic
sort.by.topic <- function(topic){
  data.doc_topic %>% arrange_(interp(~desc(v), v=as.name(topic))) %>% select(Doc_Key) %>% .[[1]]
}

# get topic names
TOPICS <- names(data.doc_topic[1,-c(1,2)])

# from manual annotation:
refugee_migration <- TOPICS[c(6,93,106,141,300,303,352,366) + 1]
terrorist_attack <- TOPICS[c(25,44,63,73,142,145,207,214,280,286,379,397) + 1]
fear <-  TOPICS[c(50,95,116,119,135,167,168,179,202,234,238,263,305,364,386) + 1]
religious_extremism <- TOPICS[c(0,9,11,24,169,197,212,275,292,298,314,320,328,334,345,357,388) + 1]
support_refugee <- TOPICS[ c(26,60,121,134,165,245,295,342)+ 1]
threat <- TOPICS[c(31,70,104,125,127,153,158,163,201,226,240,308,336,343,355,369,394) + 1]
response <- TOPICS[c(42,52,58,72,75,94,101,120,128,160,171,190,211,221,241,250,269,293,317,337,353,358,375,380,382,384,390) + 1]
political_conflict <- TOPICS[c(14,15,59,83,87,146,161,173,185,191,228,236,239,247,257,316,332,389,391,393) + 1]


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
data.doc_topic_collapse$Doc_Key = major.city$Doc_Key

final.data = inner_join(data.doc_topic_collapse, major.city[,-c(1)], by = "Doc_Key")

