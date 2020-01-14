library(tidyverse)
library(tidyr)
library(dplyr) #do not load "plyr" after this

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
head(df,6)
tail(df,6)

# count missing values in each column
library(naniar)
s = sapply(df, function(x) sum(is.na(x)))
s

gg_miss_var(df)

#remove missing value in df

df_clean = df%>% filter(!is.na(ABV) & !is.na(IBU))

#question 4
#Compute the median alcohol content and international bitterness unit for each state
#Draw bar chart

df_median= df_clean %>% group_by(State) %>% summarize(median_ABV = median(ABV), median_IBU = median(IBU))
df_median %>% ggplot(aes(x= State, y = median_ABV, fill = State))+ geom_bar(stat = 'Identity')+ coord_flip()
df_median %>% ggplot(aes(x= State, y = median_ABV, fill = State))+ geom_bar(stat = 'Identity')+ coord_flip()


#question 5
#Which state has the maximum alcoholic (ABV) beer? Which state has the most bitter (IBU) beer?
df_max= df_clean%>% group_by(State) %>% summarise(max_ABV= max(ABV), max_IBU = max(IBU))

maxABV <- max(df_clean[,"ABV"])
df_clean[df_clean$ABV == maxABV, "State"]

maxIBU<- max(df_clean[,"IBU"])
df_clean[df_clean$IBU == maxIBU, "State"]

#question 6
#summary statistics and distribution of the ABV variable
hist(df_clean$ABV)
summary(df_clean$ABV)

#comment: 


#question 7
#relationship between the bitterness of the beer and its alcoholic content? Draw a scatter plot.  
df_clean %>% ggplot(aes(x=ABV, y=IBU))+ geom_point()

library(plotly)
df_clean %>%select(ABV,IBU) %>%ggpairs()

#comment: 

#question 8
library(class)
library(caret)
library(e1071)

df_IPA=df_clean[grep("IPA",df_clean$Beer_name),]
df_Ale=df_clean[grep("Ale",df_clean$Beer_name),]

#create a new column "beer_type" to classify beer as "IPA" or "other" based on beers' name
df_new= df_clean %>% mutate ( beer_type= ifelse(grepl("IPA",df_clean$Beer_name)=="TRUE", "IPA" ,
                                        ifelse(grepl("Ale",df_clean$Beer_name)=="TRUE","other","na")))

#classify budsweiser beers
dfTest = data.frame(ABV = 5, IBU = 11)

knn(df_new[,c("ABV","IBU")], dfTest, df_new$beer_type, k = 5, prob = TRUE)

#IPA vs other types of Ale 
set.seed(6)
splitPerc = .75
df_buds = df_new %>% filter(beer_type == "IPA" | beer_type == "other")

trainIndices = sample(1:dim(df_buds)[1],round(splitPerc * dim(df_buds)[1]))
train = df_buds[trainIndices,]
test = df_buds[-trainIndices,]

# k = 3
classifications = knn(train[,c("ABV","IBU")],test[,c("ABV","IBU")],train$beer_type, prob = TRUE, k = 3)
table(classifications,test$beer_type)
confusionMatrix(table(classifications,test$beer_type))


#k=1 to 30 / one training
accs = data.frame(accuracy = numeric(30), k = numeric(30))

for(i in 1:30)
{
  classifications = knn(train[,c("ABV","IBU")],test[,c("ABV","IBU")],train$beer_type, prob = TRUE, k = i)
  table(test$beer_type,classifications)
  CM = confusionMatrix(table(test$beer_type,classifications))
  accs$accuracy[i] = CM$overall[1]
  accs$k[i] = i
}

plot(accs$k,accs$accuracy, type = "l", xlab = "k")


