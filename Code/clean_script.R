# use like this in terminal:
# Rscript clean_script.R [file-to-clean] [new-file]
# example:
# Rscript clean_script.R rawData.csv cleanData.csv
 
args<-commandArgs(TRUE)

from <- args[1]
target <- args[2]

# create data frame
df <- read.csv(from, stringsAsFactors = FALSE)

# remove urls from text column
df$text  <- gsub("https?:\\/\\/[\\da-zA-Z\\.-\\/\\?=]+", "", df$text)
# remove newline characters:
df$text <- gsub("\\n"," ", df$text)
# rearrange columns and add dummy variable column filled with 'x'
df <- data.frame(x = 1:length(df$text), dummy = rep('x', length(df$text)), text = df$text)
write.table(df, file = target, row.names = FALSE, col.names = FALSE)
