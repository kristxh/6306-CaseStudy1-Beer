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

# Plot # of breweries by state in a map (doesn't work right now)
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

#####################################################
# 3.  Address the missing values in each column

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
# TBD
n <- ggplot(med_abv_ibu) + 
  geom_bar(mapping = aes(x = State, y = med_abv), stat = "identity", colour = "blue", fill = "navy", position = "dodge") + 
  geom_bar(mapping = aes(x = State, y = med_ibu), stat = "identity", colour = "red", fill = "red", position = "dodge")  +
  scale_y_continuous(sec.axis = sec_axis(~ . * .001)) 

n


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

# Plot
clean_df %>%
  ggplot(aes(y = ABV)) +
  geom_boxplot()


#####################################################
# 7.  Is there an apparant relationship between the bitterness of the beer and its alcoholic content?  Draw a scatter plot.  Make your best judgment of a relationship and EXPLAIN your answer.

# Plot ABV and IBU
ggplot(data = clean_df) +
  geom_point(mapping = aes(x = ABV, y = IBU, color=ABV), position ="jitter") +
  ggtitle("Alcohol Content & Bitterness of Beers") + xlab("Alcohol by Volume (ABV)") + ylab("International Bitterness Units (IBU)")


#####################################################
# 8.  Investigate the difference in IBU and ABV between IPAs and other Ales

# Check to see what types of beer contain the word "Ale"
unique(clean_df$Style)

# Create a dataframe with only IPAs & Ales
(ipa_ale_df <- clean_df %>% filter(str_detect(Style, 'IPA') |str_detect(Style, 'Ale')))

# Add a column for "IPA" or "Ale"
ipa_ale_df$ipa_or_ale = ifelse(grepl("IPA", ipa_ale_df$Style), "IPA", "Ale")

# Check to make sure there are no NAs for IPA or Ale
ipa_ale_df %>% filter(is.na(ipa_or_ale))
str(ipa_ale_df)

# Split data into train and test data
# Train/Test with 70/30
split_pct = .7
set.seed(4)
train_idx = sample(seq(1:length(ipa_ale_df$Style)),round(split_pct*length(ipa_ale_df$Style)))
train_ipa = ipa_ale_df[train_idx,]
test_ipa = ipa_ale_df[-train_idx,]
dim(train_ipa)
head(train_ipa)
dim(test_ipa)
head(test_ipa)

# Run the KNN model on the train and test data
classifications = knn(train_ipa[, 4:5],test_ipa[, 4:5],train_ipa$ipa_or_ale, prob = TRUE, k = 5)
str(train_ipa)
# Create a confusion matrix of the results
CM_KNN_IPA = confusionMatrix(table(classifications,test_ipa$ipa_or_ale))
CM_KNN_IPA

# Predict what Budweiser would be classified as
bud_test = data.frame(ABV = .05, IBU = 12)

# Run k-NN model for each class
classify_bud = knn(train_ipa[, 4:5], bud_test, train_ipa$ipa_or_ale, prob = TRUE, k = 5)

# Scatter plot
ggplot(data = ipa_ale_df) +
  geom_point(mapping = aes(x = ABV, y = IBU, color=ipa_or_ale), position ="jitter") +
  ggtitle("IPA and Ales") + xlab("Alcohol by Volume (ABV)") + ylab("International Bitterness Units (IBU)")



#####################################################
# 9.  What states are closest to the Bud ABV/IBU profile?

# Run the KNN model on the train and test data and add the state parameter
classify_w_state = knn(train_ipa[, 4:5],test_ipa[, 4:5],train_ipa$State, prob = TRUE, k = 5)

CM_KNN_State = confusionMatrix(table(classify_w_state,test_ipa$State))
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

