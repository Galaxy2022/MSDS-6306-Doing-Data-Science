library(XML)
library(dplyr)
library(tidyr)
library(stringi)
library(rvest)
library(ggplot2)
library(RCurl)


#Live Session 4


install.packages("WDI")
## Install and load package
library(WDI)

## Search for fertilizer consumption data
WDIsearch("Data")

## Use indicator number to gather data
FertConsumpData <- WDI(indicator="AG.CON.FERT.ZS")

MaleOFSD <- WDI(country = "US", indicator="UIS.ROFST.H.2.Q3.M", start = 2017, end = 2018)



 #Basics

data <-getURL("https://www.w3schools.com/xml/simple.xml")
doc <- xmlParse(data)
names <- xpathSApply(doc,"//name",xmlValue)
price <- xpathSApply(doc,"//price",xmlValue)
description <- xpathSApply(doc,"//description",xmlValue)
bfasts = data.frame(names,price,description)
bfasts
bfasts$description
which(grepl("toast",bfasts$description))
grep("covered",bfasts$description)


hp<-read_html("https://www.w3schools.com/xml/simple.xml")
hp_nameR <- html_nodes(hp,"name")
hp_priceR <- html_nodes(hp,"price")
hp_descR <- html_nodes(hp,"description")
hp_nameR
hp_name = stri_sub(hp_nameR,7,-8)
hp_name
hp_price = stri_sub(hp_priceR,8,-9)
hp_price
hp_desc = stri_sub(hp_descR,14,-15)
hp_desc
grep("toast", hp_desc)
grepl("toast",hp_desc)


#Breakout 1
data <-getURL("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml")
doc <- xmlParse(data)
names <- xpathSApply(doc,"//name",xmlValue)
zipcodes <- xpathSApply(doc,"//zipcode",xmlValue)
councildistrict <- xpathSApply(doc,"//councildistrict",xmlValue)
rests = data.frame(names,zipcodes,councildistrict)
dim(rests)
restsDTown = rests[which(rests$councildistrict == "11"),]
grep("Sushi",rests$names,ignore.case = T)

hp<-read_html("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml")
hp_name2 <- html_nodes(hp,"name")
hp_zipcode2 <- html_nodes(hp,"zipcode")
hp_councildistrict2 <- html_nodes(hp,"councildistrict")

#How many restaurants total 
restByDist = hist(as.numeric(councildistrict))
barplot(height = restByDist$counts, names = (as.character(seq(1,13,1))),xlab = "Council District",ylab = "Number of Restaurants")
barplot(height = restByDist$counts, names = (as.character(seq(1,13,1))),xlab = "Council District",ylab = "Number of Restaurants", horiz = TRUE)




#Harry Potter

#1A / 1B
hp<-read_html("http://www.imdb.com/title/tt1201607/fullcredits?ref_=tt_ql_1")
hp_table<-html_nodes(hp,"table")
derp<-html_table(hp_table)

# Find the right table
derp[1]

#1C - Cleaning
a<-data.frame(derp[3])
names(a) <- c("Blank", "Actor", "Blank2","Character")
df<-a[2:length(a$Actor),c("Actor", "Character")]
df$Character[10] <- "Griphook / Professor Filius Flitwick"

# 1D -Edit The Cast List
b<-df %>%
  slice(-92) %>% # Removes the row that is just noting the rest is alphabetical
  separate(Actor, into=c("FirstNames", "Surname"), sep="[ ](?=[^ ]+$)") # Separates the Last Name

#1E 
head(b, 10)







#Stars
stars<-read_html("http://www.espn.com/nhl/team/roster/_/name/dal/dallas-stars")
stars_table<-html_nodes(stars, "table")
stars_dfs<-html_table(stars_table, fill = TRUE)

Rost1 = stars_dfs[[3]]
Rost2 = stars_dfs[[6]]
Rost3 = stars_dfs[[9]]
Rost4 = stars_dfs[[12]]
Rost5 = stars_dfs[[15]]

Roster = rbind(Rost1,Rost2)
Roster = rbind(Roster,Rost3)
Roster = rbind(Roster, Rost4)
Roster = rbind(Roster, Rost5)

# Convert columns to numeric - 2C
df1[,3:3]<-sapply(df1[,3], as.numeric)
df1









