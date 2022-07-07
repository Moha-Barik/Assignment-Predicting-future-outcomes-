#############################################################################


## Determine the most popular and the most expensive products purchased by a particular group of customers.

   # which are the most popular (i.e. with the most number of reviews) Lego sets purchased by customers who are at the most 25 years old (<25 years)? 
   # What is the most expensive Lego set purchased by customers who are at least 25 years old (>25 years)?

#############################################################################

## Install the tidyverse package
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(purrr)
library(DataExplorer)

## Import the data set
lego <- read.csv("lego.csv", header = TRUE)

## Explore the data set
head(lego)

# create_report(lego)
## Convert data frame to a tibble
as.tibble(lego)

## Use the glimpse() function
glimpse(lego)

## Use the summary() function
summary(lego)

view(lego)

str(lego)

# check for NA values 
sum(is.na(lego))

# round list_prices to 2 decimals 

lego2 <- round (lego, digits = 2)
view(lego2)

################  Which age group submits the most reviews?

#create plot 
e <- ggplot(lego2, aes(ages, num_reviews)) + 
  geom_point(color = 'grey') +
  scale_x_continuous(breaks = seq(0, 30, 3)) +
  scale_y_continuous(breaks = seq(0, 400, 50))

# add text labels 
e2 <- e + geom_text(aes(label = num_reviews), nudge_x = 1, nudge_y = 5, check_overlap = TRUE, color='blue')

# add labels 
e2 + labs(title = "Number of Reviews by age group",
          x = 'ages', y = 'number of reviews')  

############### What is the most expensive Lego set purchased by customers who are at least 25 years old (>25 years)?

# create a new dataframe for ages 25+ 
data_age25 <- lego[lego$age>=25,] 

# round values of dataframe to two decimal points
data_age25_rounded <- round (data_age25, digits = 2)

# view data frame filtered and rounded
view(data_age25_rounded)

# create scatterplot to show price by piece count
p <- ggplot(data_age25_rounded, aes(piece_count, list_price)) + geom_point(color='grey') +
  scale_x_continuous(breaks = seq(0, 35000, 200)) + 
  scale_y_continuous(breaks = seq(0, 30000, 50))

# add a layer to show piece count for the most expensive set (1413)
p2 <- p + geom_text( 
  data = data_age25_rounded %>% filter(list_price>250), # Filter data first
  aes(label=piece_count), nudge_x = 3, nudge_y = 5, color = 'blue', 
  check_overlap = TRUE)

# add labels 
p2 + labs(title = "Most Popular Lego Set", 
          subtitle = "customers older than 25 years old",
          x = 'lego piece count', y = 'price')  

############### What is the most expensive Lego set purchased by customers who are at at the most 25 years old?

# create a new dataframe for ages 25+ 
data_age25_less <- lego[lego$age<=25, ] 

# round values of dataframe to two decimal points
data_age25_less_rounded <- round (data_age25_less, digits = 2)

# view data frame filtered and rounded
view(data_age25_less_rounded)

# create scatterplot to show price by piece count
p <- ggplot(data_age25_less_rounded, aes(piece_count, list_price)) + geom_point(color='grey') +
  scale_x_continuous(breaks = seq(0, 8000, 1000)) + 
  scale_y_continuous(breaks = seq(0, 1200, 200))

# add a layer to show piece count for the most expensive set (1413)
p2 <- p + geom_text( 
  data = data_age25_less_rounded %>% filter(list_price>1000), # Filter data first
  aes(label=piece_count), nudge_x = 3, nudge_y = 5, color = 'blue', 
  check_overlap = TRUE)

# add labels 
p2 + labs(title = "Most Popular Lego Set", 
          subtitle = "customers younger than 25 years old",
          x = 'lego piece count', y = 'price')  
