###########################################################
#### The scripts below shows how to import text data and ## 
#### pre-processthe data for general text mining.##########
###########################################################
library(wordcloud)
library(tm)
library(foreign)
library(MASS)
library(readxl)
library(dplyr)
library(topicmodels)
library(ggplot2)
library(koRpus)
library(wordnet)
library(stm)
library(data.table)


# set the working directory
setwd("C:/My Data/NHIS/Injury Narratives/INJVERB.dat")


# Creat an empty list
data.l = list() 


# Create a list of file names.
file_list=list.files(path="C:/My Data/NHIS/Injury Narratives/INJVERB.dat", pattern=".dat")


# Read the ".dat" files into a single list
data.l = sapply(file_list, readLines)


# Take a look at a small piece of the data
str(data.l[[1]])
head(data.l[[1]])


# Create the "iid" variable for each elemetn of the list. 
# The input "x" is a vector of characters
create_iid <- function(x){  
  
  dt = data.frame(year = substr(x,3,6),hhx = substr(x,7,12),fmx = substr(x,13,14),
             fpx = substr(x,15,16), ipepno = substr(x,17,18),iphow = substr(x,19,318),
             stringsAsFactors = FALSE)  
  
  dt$iid = paste(dt$year,dt$hhx, dt$fmx, dt$fpx, dt$ipepno, sep = "")
  dt$hhx = NULL
  dt$fmx = NULL
  dt$fpx = NULL
  dt$ipepno = NULL
  
  return(dt)
}


# Does the function work as it supposed to be?
test = create_iid(data.l[[1]])
View(test)
# Good. So remove "test"
rm(test)


# Might also consider the "rbindlist" function
data_temp.d = do.call(rbind, (lapply(data.l, create_iid)))
class(data_temp.d)


# Merge cause of injuries, created by IHIS.
library(foreign)
ihis.d= read.dta("C:/My Data/NHIS/linking/2016.0112.linking.dta")
data.d = merge(data_temp.d, ihis.d, 'iid')


# Check the variable names
names(data.d)


# Remove all except data.d
rm(list=setdiff(ls(), "data.d")) 


# Save the current image for future uses
save.image("C:/My Data/NHIS/2004-10/RData/2016.0125.data before cleaning.RData")


# merge with e-code and ICD-9 code. Already have ircausnew as the 
# grouped e-code
# e-code -- to 4-digit level


# Limit the analysis to injuries at residential units
homeinj.d = data.d[which((data.d$irwhere1 == "Inside home" | 
                          data.d$irwhere1 == "Outside home"|
                          data.d$irwhere2 == "Inside home" | 
                          data.d$irwhere2 == "Outside home")),]



text.v = data.d$iphow

# count number of words of each response
temp = sapply(gregexpr("\\S+", text.v), length)
sum(temp)/length(temp) # on average 11.87 words in each response
rm(temp)


metadata = data.d[,c("iid", "sex", "age", "irecausnew","iphow", "irwhere1","irwhere2")]


#######################################################
########### Pre-processing the text data, except for ##
########### spelling check and lemmatization ##########
#######################################################


# Convert text to lower case
metadata$iphow <- tolower(metadata$iphow)


# correct these before you replace punctuations with space
metadata$iphow = gsub("w/", " with ", metadata$iphow, fixed=TRUE)


# w/ -> "with ". Sometimes people don't but a space after w/
metadata$iphow = gsub("97", "NA", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("911", "nine-one-one", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("1", " one", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("2", " two", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("3", " three", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("4", " four", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("5", " five", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("6", " six", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("7", " seven", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("8", " eight", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("9", " nine", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("bldg", "building", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("x-ray", "xray", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("E.R.", " Emergency room", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub(" rt ", " right ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("thru", "through", metadata$iphow, fixed=TRUE)#  -> play
metadata$iphow = gsub("couldn't", "could not", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("didn't", "did not", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("doesn't", "does not", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("don't", "do not", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("p.e.", "physical education", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("p;lay", "play", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub(" fall ", " fell ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub(" get ", " got ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("ininjured", "injure", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("injued", "injure", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("injuired", "injure", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("injuiry", "injure", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("injured", "injure", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("injures", "injure", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("injuried", "injure", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("injuries", "injure", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("injuring", "injure", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("injury", "injure", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("injuried", "injure", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("injuyry", "injure", metadata$iphow, fixed=TRUE)


# Replaced these with a space: ;, /, \, ", <, >, ?, -, (, ), $. 
# Don't just remove them
metadata$iphow = gsub(".", " ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub(".", " ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub(",", " ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub(";", " ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("(", " ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub(")", " ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("-", " ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("<", " ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub(">", " ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("?", " ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("/", " ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("\\", " ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("\"", " ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("\'", " ", metadata$iphow, fixed=TRUE)
metadata$iphow = gsub("&", "and", metadata$iphow, fixed=TRUE)


# Replace multiple spaces with a single space
metadata$iphow <- gsub(pattern = " +", replacement = " ", 
                       x = metadata$iphow)


# Save the current image for future uses
save.image("C:/My Data/NHIS/2004-10/RData/2016.0125.data after cleaning.RData")


# Limit the analysis to injuries at home
homeinj.d = metadata[which((metadata$irwhere1 == "Inside home" | 
                            metadata$irwhere1 == "Outside home"|
                            metadata$irwhere2 == "Inside home" | 
                            metadata$irwhere2 == "Outside home")),]


# Examine the distribution of home injuries by injury mechanism
library(data.table)
dt = as.data.table(homeinj.d)
dt[, .N ,by = irecausnew]
rm(dt)
# So, "fall", "struck by", and "overexertion" are the big three


# Randomly select 100 cases for "struck by" and "overexertion", and 
# read its narratives
struckby.d = homeinj.d[which(homeinj.d$irecausnew == "Struck by object or person"),]
overexer.d = homeinj.d[which(homeinj.d$irecausnew == "Overexertion/strenuous movements"),]
sample.struckby <- struckby.d[sample(1:nrow(struckby.d), 100,replace=FALSE),]
sample.overexer <- overexer.d[sample(1:nrow(overexer.d), 100,replace=FALSE),]


# Export as xlsx files
library(xlsx)
write.xlsx(sample.struckby, "sample.struckby.xlsx")
write.xlsx(sample.overexer, "sample.overexer.xlsx")

