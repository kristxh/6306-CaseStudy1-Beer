library(tidyverse)
library(tidyr)
library(plyr)
library(dplyr)

#read data files beers.csv and breweries.csv
dfbeers = read.csv(file.choose(), header = TRUE)
dfbreweries = read.csv(file.choose(), header = TRUE)

#count number of breweries present in each state
breweriesnumbers= as.data.frame(table(dfbreweries$State))

#change name for dfbeers's column "Brewery_id" into "Brew_ID"
colnames(dfbeers)[5]= "Brew_ID"

#merge beer data and brewery data by "Brew_ID" 
df = merge(dfbeers,dfbreweries,"Brew_ID")

#change column names for df's columns "Name.x" and "Name.y"
colnames(df)[which(colnames(df) %in% c("Name.x","Name.y"))] <- c("Beer_name","Brewery_name")
head(df)
tail(df)

# count missing values in each column
library(naniar)
s = sapply(df, function(x) sum(is.na(x)))
gg_miss_var(df)

#Compute the median alcohol content and international bitterness unit for each state 
#(*only return 1 value, idk why :( )
df %>% filter(!is.na(ABV)) %>% group_by(State) %>% summarize(m = median(ABV))

#tapply() works but contain missing values
as.data.frame(tapply(df$ABV, df$State, median))

df %>% filter(!is.na(IBU)) %>% group_by(State) %>% summarize(m = median(IBU))
