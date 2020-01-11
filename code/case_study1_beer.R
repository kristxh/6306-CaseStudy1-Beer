# Import dependencies
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(ggthemes)

# Import Beer and Brewery data
beer_df <-  read_csv("Beers.csv")
brew_df <-  read_csv("Breweries.csv")

# Check to make sure data imported
dim(beer_df)
dim(brew_df)


#####################################################
# 1.  How many breweries are present in each state?

#  Group breweries by state
brew_by_state <- brew_df %>% group_by(State) %>% summarize(count=n())
  
# Display # of breweries by state
View(brew_by_state)

# Plot # of breweries by state
brew_by_state %>%
  ggplot(aes(x = State, y = count)) +
  geom_bar(stat = "identity", fill = "blue", alpha = .5) +
  ggtitle("Number of Breweries by State") + xlab("State") + ylab("Number of Breweries")
  

#####################################################
# 2.  Merge the Beer Data with the Brewery Data.  Print first and last six observations.

# Change the column names in the Beer df
setnames(beer_df, old=c("Name","Brewery_id"), new=c("Beer_name", "Brew_ID"))

# Change column name in the Brewery df
setnames(brew_df, old=c("Name"), new=c("Brewery_name"))

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

#####################################################
# 5.  Which state has the maxiumum alcoholic (ABV) beer?  Which state has the most bitter (IBU) beer?
max_abv_ibu <- all_data_df %>% group_by(State) %>% summarize(max_abv = max(ABV), max_ibu = max(IBU), count = n())

# State with max ABV
(highest_abv <- max_abv_ibu %>% filter(rank(desc(max_abv))<=1))

# State with max IBU
(highest_ibu <- max_abv_ibu %>% filter(rank(desc(max_ibu))<=1))


#####################################################
# 6.  Comment on the summary statistics and distribution of the ABV variable.
summary(all_data_df$ABV)


#####################################################
# 7.  Is there an apparant relationship between the bitterness of the beer and its alcoholic content?  Draw a scatter plot.  Make your best judgment of a relationship and EXPLAIN your answer.

# Plot ABV and IBU
ggplot(data = all_data_df) +
  geom_point(mapping = aes(x = ABV, y = IBU, color = State), position ="jitter") +
  ggtitle("Alcohol Content & Bitterness of Beers") + xlab("Alcohol by Volume (ABV)") + ylab("International Bitterness Units (IBU)")


#####################################################
# 8.  Investigate the difference in IBU and ABV between IPAs and other Ales

# Check to see what types of beer contain the word "Ale"
unique(all_data_df$Style)

# Filter by IPAs
(ipa_df <- all_data_df %>% filter(str_detect(Style, 'IPA')))

# Filter by Ales
(ale_df <- all_data_df %>% filter(str_detect(Style, 'Ale')))


# Scatter plot
(plot1 <- ggplot(ipa_df, aes(ABV, IBU)) + 
    geom_point(color="blue") +
    geom_point(data = ale_df, color = "red")
)


