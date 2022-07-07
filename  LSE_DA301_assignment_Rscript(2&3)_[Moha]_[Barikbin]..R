
########################################################

   ## What are the predicted sales (in millions) for the next financial year 
   ## for each of the video games for stores based in Europe and North America?


## Every variable has its own pattern of variation, which can reveal interesting information. 
########################################################

## Install the tidyverse package
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(purrr)
library(DataExplorer)

## Import the data set
sales <- read.csv("games_sales.csv", header = TRUE)

# view 
view(sales)

#create_report(sales)

## Convert data frame to a tibble
as.tibble(sales)

## Use the glimpse() function
glimpse(sales)
dim(sales)
## Use the summary() function
summary(sales)
# check for NA values 
sum(is.na(sales))
apply(sales, 2, anyNA)


## manipulating the strings by converting all the values under Genre to lowercase for consistency. 

sales$Genre <- str_to_lower (sales$Genre)

## merging the values for the variables Genre and Platform. 

sales$genre_platform <- str_c(sales$Genre, ' ', sales$Platform) 

head(sales)
view(sales)

# count number of observations for unique genre and platform combinations 
sales %>% 
  count(genre_platform)


# count observations for our continuous variables NA, EU and global sales 

sales %>% 
  count(cut_width(Global_Sales, 1))

sales %>% 
  count(cut_width(EU_Sales, 0.5))

sales %>% 
  count(cut_width(NA_Sales, 0.5))

# explore variation
genre_agg <- sales %>%
  group_by(Genre) %>%
  summarise(sales_total = sum(Global_Sales))

as_tibble(genre_agg)

# plot 
ggplot(genre_agg, aes(x= Genre, y = sales_total)) + geom_col() + scale_x_discrete(guide = guide_axis(n.dodge=2))

# perform alternative aggregate 

sales_agg <- select(sales, c("genre_platform", "EU_Sales", "NA_Sales", "Global_Sales"))
as_tibble(sales_agg)

agg1 = sales_agg %>% group_by(genre_platform) %>% summarize(cnt_rows = n(),
                                                      min_sales_EU = min(EU_Sales),
                                                      max_sales_EU = max(EU_Sales), 
                                                      min_sales_NA = min(NA_Sales),
                                                      max_sales_NA = max(NA_Sales),
                                                      Avg_sales_EU = mean(EU_Sales),
                                                      Avg_sales_NA = mean(NA_Sales),
                                                      )

agg1
########################################################

# draw a simple scatter plot to view outliers 
ggplot(sales_agg,
       mapping = aes(x = Global_Sales, y = Global_Sales)) +
  geom_point() + labs(title = "Global Sales outliers")

view(sales_agg)

# remove outlier 
new_sales1 <- filter(sales_agg, Global_Sales < 40.00)

# view new dataframe 
head(new_sales)
summary(new_sales)

view(new_sales)
######################################################## 

# initial plot EU_sales 
ggplot(data = new_sales1, mapping = aes(x = EU_Sales)) + 
  geom_histogram(binwidth = 0.25) +
  coord_cartesian(ylim = c(0, 50), xlim = c(0, 15)) + labs(title = "EU Sales outliers")

# 3 values are larger than 10 

new_sales2 <- filter(new_sales1, EU_Sales < 10.00)

view(new_sales2)
# removing these 3 values, since they might impact the significance of my statistical analysis. 

# initial plot NA_sales 
ggplot(data = new_sales2, mapping = aes(x = NA_Sales)) + 
  geom_histogram(binwidth = 0.25) +
  coord_cartesian(ylim = c(0, 50), xlim = c(0, 30)) +
  labs(title = "North America Sales outliers")

# remobing 2 values larger than 20.00 
new_sales <- filter(new_sales2, NA_Sales < 20.00)

view(new_sales)

## How will you evaluate the skewness of the data?
  # Which plot will help you study the skewness of the data?
  # If the data is skewed, is it skewed towards right or left?

#install.packages("moments") 
# install.packages('psych')
library(moments)
library(psych)

# describe data frame 

describe(new_sales)

# Positive skewness means  the mean is larger than the median and data leans to the right.
skewness(new_sales$NA_Sales) 

skewness(new_sales$EU_Sales) 

# normal distribution, has a coefficient of kurtosis equal to 3. 
kurtosis(new_sales$NA_Sales) 

kurtosis(new_sales$EU_Sales) 


## Shapiro-Wilk test can be performed as follow:
  # Null hypothesis: the data are normally distributed
# select random values 
shapiro_sales <- new_sales[sample(nrow(new_sales), 5000), ]

view(shapiro_sales)

shapiro.test (shapiro_sales$NA_Sales)
shapiro.test (shapiro_sales$EU_Sales)
# result: W = 0.31609, p-value < 2.2e-16 
# Since this value is less than .05, we have sufficient evidence to say that the sample data does not come from a population that is normally distributed.

############## 
## What is the correlation between the two variables that will help you predict global sales?
  # Which variables will you use for studying the correlation?
  # What type of plot will help you visualize the correlation? 

# Measure normality for EU sales 

# plot specfically for outlier detection. 
qqnorm(new_sales$EU_Sales, col="red")
# Add a reference line
qqline(new_sales$EU_Sales, col="blue", lwd=2) 
# Measure normality for NA sales
# With a qqplot.
qqnorm(new_sales$NA_Sales, col="blue")
# Add a reference line
qqline(new_sales$NA_Sales, col="red", lwd=2)                                        


############# 

## Pearson's correlation does NOT assume normality. 
## It is an estimate of the correlation between any two continuous random variables and is a consistent estimator under relatively general conditions. 
## Even tests based on Pearson's correlation do not require normality if the samples are large enough because of the CLT

############
# find correlation 
cor(new_sales$EU_Sales, new_sales$NA_Sales)

# Correlation test between EU and NA sales variables:
res <- cor.test(new_sales$EU_Sales, new_sales$NA_Sales, 
                method = "pearson")
res 

# Extract the correlation coefficient
res$estimate

############################ Assignment activity 6 


# What are the predicted sales (in millions) for the next financial year for each of the video games for stores based in Europe and North America?


# Plot the relationship with base R graphics
ggplot(new_sales, aes(EU_Sales, NA_Sales)) + 
  geom_point() + 
  geom_smooth() +
  labs(title = "EU - NA sales reationship ")

## Fit the simple linear regression model
sales_model <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = new_sales)

## View the model
sales_model

## View more outputs for the model - the full regression table
summary(sales_model)

# The distribution of model residuals should be approximately normal.
hist(residuals(sales_model), col = "steelblue")


## View residuals on a plot
plot(sales_model$residuals)
#create fitted value vs residual plot
plot(residuals(sales_model)) 
#add horizontal line at 0
abline(h = 0, lty = 2)


# A multiple R-squared of 1 indicates a perfect linear relationship 
# here the R-squared is 0.914701. meaning 91% of the data can be explained by the predictors in the model.
# Residual standard error: 0.2891 -> meaning  the observed values fall an average of 0.2891 units from the regression line.

# we can see p value 2e-16 is very small so the variables are highly significant 

# create a new dataframe 
NA_EU_sub <- new_sales [, c ('NA_Sales', 'EU_Sales', 'Global_Sales')]

# predict global sales using model based on NA and EU sales 
NA_EU_sub$Predicted_Global_Sales <- predict(sales_model, newdata = NA_EU_sub)

# view results 
NA_EU_sub

# using the above model, we can predict global sales values by inputing EU and NA sales. 


# predictTest = predict(sales_model, newdata = NA_EU_sub, interval = 'confidence')
# predictTest 


### As we only have data for one year, the predicted value can be compared with
## the observed value so make sure that you indicate clearly what you predict
## (i.e. the the first rank, or the full range).




















