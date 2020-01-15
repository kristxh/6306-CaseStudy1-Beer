# Import dependencies
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(ggthemes)
library(usmap)

# Import Beer and Brewery data
beer_df <-  read_csv("Beers.csv")
brew_df <-  read_csv("Breweries.csv")

# Check to make sure data imported
dim(beer_df)
dim(brew_df)
View(brew_df)

#####################################################
# 1.  How many breweries are present in each state?

#  Group breweries by state
brew_by_state <- brew_df %>% group_by(State) %>% summarize(count=n())

# Display # of breweries by state
View(brew_by_state)

# Plot # of breweries by state in a barchart
brew_by_state %>%
  ggplot(aes(x = State, y = count)) +
  geom_bar(stat = "identity", fill = "blue", alpha = .5) +
  ggtitle("Number of Breweries by State") + xlab("State") + ylab("Number of Breweries")

# Need to fix this plot of breweries by state in a map
plot_usmap(data = brew_by_state$State, values = brew_by_state$count, color = "red") + 
  scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
  theme(legend.position = "right")


#####################################################
# 2.  Merge the Beer Data with the Brewery Data.  Print first and last six observations.

# Change the column names in the Beer df
setnames(beer_df, old=c("Name","Brewery_id"), new=c("Beer_name", "Brew_ID"))

# Change column name in the Brewery df
setnames(brew_df, old=c("Name"), new=c("Brewery_name"))

# Make the state column a factor
brew_df$State <-  factor(brew_df$State)
str(brew_df)

# Merge the brewery data with the beer data (left join)
(all_df <- merge(beer_df, brew_df, by="Brew_ID"))
dim(all_df)

# Print the first & last 6 observations
ht <- function(df, m=6, n=m){
  # print the head and tail together
  list(HEAD = head(df,m), TAIL = tail(df,n))
}

ht(all_df)

first6= head(all_df,6)
last6= tail(all_df,6)

#####################################################
# 3.  Address the missing values in each column

# count missing values in each column
library(naniar)
s = sapply(df, function(x) sum(is.na(x)))
s

gg_miss_var(all_df)

# Remove missing data for beer & rename the Name and Brewery_id Columns
(clean_df <- all_df %>% filter(!is.na(ABV) & !is.na(IBU) & !is.na(Style)))
dim(clean_df)

#####################################################
# 4.  Compute the median alcohol content and international bitterness unit for each state.  Plot a bar chart to compare.

# Dataframe with median values for ABV and IBU
(med_abv_ibu <- clean_df %>% 
   group_by(State) %>%
   summarize(med_abv = median(ABV), med_ibu = median(IBU), count = n()))

# Plot
ggplot(med_abv_ibu, aes(x = State)) +
  geom_col(aes( y = med_ibu, fill="redfill")) +
  geom_text(aes(y = med_ibu, label = med_ibu), fontface = "bold", vjust = 2.4, color = "white", size = 3) +
  geom_line(aes(y = med_abv * 1000, group = 1, color = 'blackline')) +
  geom_text(aes(y = med_abv * 1000, label = round(med_abv, 2)), vjust = -.4, color = "black", size = 3) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 500)) +
  scale_fill_manual('', labels = 'Median Bitterness (IBU)', values = "#C00000") +
  scale_color_manual('', labels = 'Median Alcohol By Volumen (ABV)', values = 'black') +
  ggtitle("Median Alcohol Content & International Bitterness by State") +
  theme_minimal()

#####################################################
# 5.  Which state has the maxiumum alcoholic (ABV) beer?  Which state has the most bitter (IBU) beer?
max_abv_ibu <- clean_df %>% group_by(State) %>% summarize(max_abv = max(ABV), max_ibu = max(IBU), count = n())

# State with max ABV
(highest_abv <- max_abv_ibu %>% filter(rank(desc(max_abv))<=1))

# State with max IBU
(highest_ibu <- max_abv_ibu %>% filter(rank(desc(max_ibu))<=1))


#####################################################
# 6.  Comment on the summary statistics and distribution of the ABV variable.
summary(clean_df$ABV)

#Extract rows of Min and Max ABV

clean_df[which.max(clean_df$ABV),]
clean_df[which.min(clean_df$ABV),]

# Plot
clean_df %>%
  ggplot(aes(y = ABV)) +
  geom_boxplot()

clean_df %>%
  ggplot(aes(x = ABV)) +
  geom_histogram(colour="black",fill="navy")


#Comment:
#Kentucky has the beer with the max ABV of 0.125 while Oregon has the one with the min ABV of 0.027.
#The distribution of ABV variable is slightly right-skewed.



#####################################################
# 7.  Is there an apparant relationship between the bitterness of the beer and its alcoholic content?  Draw a scatter plot.  Make your best judgment of a relationship and EXPLAIN your answer.

# Plot ABV and IBU
ggplot(data = clean_df) +
  geom_point(mapping = aes(x = ABV, y = IBU, color=ABV), position ="jitter") +
  ggtitle("Alcohol Content & Bitterness of Beers") + xlab("Alcohol by Volume (ABV)") + ylab("International Bitterness Units (IBU)")

#correlation
cor.test(clean_df$ABV,clean_df$IBU)

#ANSWER:
#With p-value < 2.2e-16, there is sufficient evidence at alpha= .05 level of significance
#to suggest that the data is linearly correlated. Correlation estimate =0.67 suggests that
#the relationship between IBU and ABV is positive and strong.

#####################################################
# 8.  Investigate the difference in IBU and ABV between IPAs and other Ales

# Check to see what types of beer contain the word "Ale"
unique(clean_df$Style)

# Create a dataframe with only IPAs & Ales
(ipa_ale_df <- clean_df %>% filter(str_detect(Style, 'IPA') |str_detect(Style, 'Ale')))

# Add a column for "IPA" or "Ale"
ipa_ale_df$beer_type = ifelse(grepl("IPA", ipa_ale_df$Style), "IPA", "Ale")

# Check to make sure there are no NAs for IPA or Ale
ipa_ale_df %>% filter(is.na(beer_type))
str(ipa_ale_df)

# Split data into train and test data
# Train/Test with 70/30
split_pct = .7
set.seed(4)

trainIndices = sample(1:dim(ipa_ale_df)[1],round(splitPerc * dim(ipa_ale_df)[1]))
train = ipa_ale_df[trainIndices,]
test = ipa_ale_df[-trainIndices,]

dim(train)
head(train)
dim(test)
head(test)



# Run the KNN model on the train and test data with k = 5
classifications = knn(train[,c("ABV","IBU")],test[,c("ABV","IBU")],train$beer_type, prob = TRUE, k = 5)


# Create a confusion matrix of the results with k = 5
table(classifications,test$beer_type)
CM_KNN_IPA = confusionMatrix(table(classifications,test$beer_type))
CM_KNN_IPA

# Predict what Budweiser would be classified as
bud_test = data.frame(ABV = .05, IBU = 12)

classify_bud = knn(train[,c("ABV","IBU")], bud_test, train$beer_type, prob = TRUE, k = 5)
classify_bud

#IPA vs Ale
#Loop for many k and one training
accs = data.frame(accuracy = numeric(100), k = numeric(100))

for(i in 1:100)
{
  classifications = knn(train[,c("ABV","IBU")],test[,c("ABV","IBU")],train$beer_type, prob = TRUE, k = i)
  table(test$beer_type,classifications)
  CM = confusionMatrix(table(test$beer_type,classifications))
  accs$accuracy[i] = CM$overall[1]
  accs$k[i] = i
}

plot(accs$k,accs$accuracy, type = "l", xlab = "k")


# Scatter plot
ggplot(data = ipa_ale_df) +
  geom_point(mapping = aes(x = ABV, y = IBU, color=beer_type), position ="jitter") +
  ggtitle("IPA and Ales") + xlab("Alcohol by Volume (ABV)") + ylab("International Bitterness Units (IBU)")



#####################################################
# 9.  What states are closest to the Bud ABV/IBU profile?

# Run the KNN model on the train and test data and add the state parameter (Use all data to train/test)
classify_w_state = knn(train[, 4:5],test[, 4:5],train$State, prob = TRUE, k = 5)
classify_bud_st = knn(train[, 4:5], bud_test, train$State, prob = TRUE, k = 5)

CM_KNN_State = confusionMatrix(table(classify_w_state,test$State))
CM_KNN_State


# Are any states missing?
ipa_ale_df %>% filter(is.na(State))

#####################################################
# Are any of the beers in the Craft Brew Alliance represented in this dataset

bud_crafts <- list("Kona", "Omission", "Widmer", "Red Hook", "Cisco", "AMB", "Appalachian", "Wynwood", "Square Mile", "pH")

for (i in 1:length(bud_crafts)){
  temp_df <- clean_df %>% filter(str_detect(Beer_name, bud_crafts[[i]]))
  print(i)
  print(temp_df)
}

# Try breweries
for (i in 1:length(bud_crafts)){
  temp_df <- clean_df %>% filter(str_detect(Brewery_name, bud_crafts[[i]]))
  print(i)
  print(temp_df)
}

#####################################################
# Two y axes test

statepop
