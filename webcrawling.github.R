###########################################################
#### The scripts below shows how to scrape weather data ### 
#### from wunderground.com ################################
###########################################################

library(XML)
library(stringr)
library(xlsx)
library(plyr)
library(dplyr)

setwd("C:/My Data/webcrawling/data")

# The CSV file provides the web addresses ("url") from which we want to extract the weather data
urllist.d <- read.csv("C:/My Data/webcrawling/data/monthly_url.csv")


outdata.t1.temp.d = NULL

# Looping over the URLS in the CSV file
for (n in 1:84) {
  
  # This is the url from which you will extract the tables
  url <- paste(urllist.d[n,2])
  
  # The readHTMLTable will try to indenfity the tables on the webpage
  tables.l <- readHTMLTable(url)
  
  # The number of tables in tables.l
  y<-length(tables.l)
  
  # 
  if (y[1] < 4){    
    next 
  } 
  
  # This is one of the two tables that we want
  my.table.d <- tables.l[[1]]
  
  # Create an id variable 
  my.table.d$id<-urllist.d[n,1]
  
  # Forcing all the ements in my.table.d to be come standard characters
  my.table.d[] <- lapply(my.table.d, as.character)
  
  # Combine the two dataframes by row (dplyr package)
  outdata.t1.temp.d <-bind_rows(outdata.t1.temp.d,my.table.d)
  
  rm(my.table.d)
  save(outdata.t1.temp.d, file = "outdata.t1.temp.d.RData")

}


outdata.t2.temp.d = NULL


# The same procedure, but the other table on each webpage (urllist.d[n,2])
for (n in 1:84) {
  
  url <- paste(urllist.d[n,2])
  tables.l <- readHTMLTable(url)
  y<-length(tables.l)
  
  if (y[1] < 4){    
    next 
  }  
  my.table.d <- tables.l[[2]]
  my.table.d$id<-urllist.d[n,1]
  my.table.d[] <- lapply(my.table.d, as.character)
  outdata.t2.temp.d <-bind_rows(outdata.t2.temp.d,my.table.d)
  rm(my.table.d)
  save(outdata.t2.temp.d, file = "outdata.t2.temp.d.RData")
}


# Reformat to stop errors in exporting to csv
outdata.t1.d <- data.frame(lapply(outdata.t1.temp.d, as.character), stringsAsFactors=FALSE)
write.csv(outdata.t1.d,"outdata.t1.csv")

outdata.t2.d <- data.frame(lapply(outdata.t2.temp.d, as.character), stringsAsFactors=FALSE)
write.csv(outdata.t2.d,"outdata.t2.csv")

# remove all in memory
rm(list=ls())
