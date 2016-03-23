############################################################
### The scripts below shows how to merge two datasets and ##
#### conduct basic desriptive analysis. ####################
############################################################
library(foreign)
library(MASS)
library(readxl)
library(dplyr)
library(ggplot2)
library(stm)
library(data.table)
#install.packages("gmodels", dependencies = TRUE)
library(gmodels)
# to be published to github; merging ...etc.


# set the working directory
setwd("C:/My Data/NHIS/2016.0308/")


load("C:/My Data/NHIS/Injury Narratives/RData/2016.0129.data before cleaning.RData")

names(data.d)

# Remove metadata because it is simply a subset of data.d
rm(metadata) 

# Read in data in Stata format, downlaod from IHIS
data0308.d = read.dta("2016.0308.dta")

# Check the variable names 
names(data0308.d)

# Check the format of the two id variables
head(data.d$iid)
head(data0308.d$nhisiid)

# The two id variables have slightly different format. The data.d$iid
# was constructed using BLS text file, in such a manner that
# dt$iid = paste(dt$year,dt$hhx, dt$fmx, dt$fpx, dt$ipepno, sep = "")
# So, I consrtuct another id variable and put it to the data0308.d


year = substr(data0308.d$nhisiid,3,6)
hhx = substr(data0308.d$nhisiid,7,12)
fmx = substr(data0308.d$nhisiid,13,14)
fpx = substr(data0308.d$nhisiid,15,16)
ipepno = substr(data0308.d$nhisiid,18,19)

data0308.d$iid = paste(year,hhx,fmx,fpx,ipepno,sep = "")

# Now see if the id variables look alike
head(data0308.d$iid)
head(data.d$iid)

# merge the two data frames
datanew.d= merge(data.d, data0308.d, by = "iid")

# Remove all objects except for the new merged dataset
rm(list=setdiff(ls(), "datanew.d"))

# rename a variable
# names(datanew.d)[names(datanew.d) == 'iid'] = "id"

# Take out posoining cases, as it is out of the scope of the study
names(datanew.d)
table(datanew.d$irpoisyn)

poisoning.d = datanew.d[which(datanew.d$irpoisyn == "Yes, poisoning episode"),]
nrow(poisoning.d)

nrow(datanew.d)
datanew.d = datanew.d[which(datanew.d$irpoisyn == "No, not a poisoning episode"),]
nrow(datanew.d)
# Number of rows decreases by exactly 250. Good.

# Cross tabulate age against sex, using the CrossTable function from the gmodels package
CrossTable(datanew.d$age,datanew.d$sex)
# Table is too long. So consider recode the age variable into groups

# First, look at the distriubtion of age
hist(datanew.d$age)
# Oops....it appears that "age" is a factor variable

plot(factor(table(datanew.d$age)))
# Something went wrong...

table(datanew.d$age)

barplot(table(datanew.d$age), main = "Age of Subjets Reporing Injuries, NHIS 2004-2010")

attach(datanew.d)

levels(age)
table(age)

# Check if there is missing in the age variable
sum(is.na(age))
table(is.na(age))
# No missing in the age variable. Good. 

datanew.d$temp = as.character(age)
table(temp)
str(temp)

datanew.d$agenew = as.numeric(temp)

# "85+" will be converted to "NA". So put the value back and 
# remeber it represents age 85+, not just exactly age 85.
datanew.d$agenew[which(is.na(datanew.d$agenew) == 1)] = 85

# New record respondent's age into the "ageg" variable.
datanew.d$ageg[datanew.d$agenew <= 5] = "0-5"
datanew.d$ageg[datanew.d$agenew >=6 & datanew.d$agenew <= 10] = "6-10"
datanew.d$ageg[datanew.d$agenew >=11 & datanew.d$agenew <= 15] = "11-15"
datanew.d$ageg[datanew.d$agenew >=16 & datanew.d$agenew <= 20] = "16-20"
datanew.d$ageg[datanew.d$agenew >=21 & datanew.d$agenew <= 30] = "21-30"
datanew.d$ageg[datanew.d$agenew >=31 & datanew.d$agenew <= 40] = "31-40"
datanew.d$ageg[datanew.d$agenew >=41 & datanew.d$agenew <= 50] = "41-50"
datanew.d$ageg[datanew.d$agenew >=51 & datanew.d$agenew <= 60] = "51-60"
datanew.d$ageg[datanew.d$agenew >=61 & datanew.d$agenew <= 70] = "61-70"
datanew.d$ageg[datanew.d$agenew >=71 & datanew.d$agenew <= 80] = "71-80"
datanew.d$ageg[datanew.d$agenew >=81 | datanew.d$agenew == 85] = "81+"

detach(datanew.d)

# Now tabulate age group by sex. Look much nicer.
CrossTable(datanew.d$ageg,datanew.d$sex)

save.image("C:/My Data/NHIS/Injury Narratives/RData/2016.0322.Rdata")

