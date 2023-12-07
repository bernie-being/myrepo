
#Clear
cat("\014")  
rm(list=ls())

#Set Directory
rm(list=ls())
setwd("C:/Users/usuario/Downloads/EC349")

#Load Libraries
install.packages(c("tidytext", "dplyr", "tm", "stringr"))
install.packages("randomForest")
install.packages(c("knitr", "kableExtra", "webshot"))
library(knitr)
library(kableExtra)
library(webshot)
library(randomForest)
library(tidytext)
library(dplyr)
library(tm)
library(stringr)
library(glmnet)
library(ggplot2)
library(tidyverse)
library(tree)
library(rpart)
library(rpart.plot)
library(jsonlite)
library(tidyr)
library(caret)
library(purrr)
library(tidytext)
library(glue)






#Load the Data. In this work, it was decided that the smaller versions of the datasets will be used as the original versions were all too big for the computer to work with efficiently. 
business_data <- stream_in(file("yelp_academic_dataset_business.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
user_data <- load("yelp_user_small.Rda") #This is one of the two datasets that are the small versions - this will have an impact in the accuracy and predictability of the model, but it is a necessary step to be able to efficiently work with the data. 
tip_data  <- stream_in(file("yelp_academic_dataset_tip.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
review_data  <- load("yelp_review_small.Rda") #This is one of the two datasets that are the small versions - this will have an impact in the accuracy and predictability of the model, but it is a necessary step to be able to efficiently work with the data.
checkin_data  <- stream_in(file("yelp_academic_dataset_checkin.json")) #note that stream_in reads the json lines (as the files are json lines, not json)

#==================
  
#Data Exploration: 
#In this section, the data is explored and looked at to manage it better in later sections

# The "head()" and "summary()" commands are used to examine the first few rows of each dataset and their statistics.
  #As this is a huge amount of data, it is possible some errors or missing values are present so we need to "clean up" the data. Using these we see were are these missing values and how many there are. 
  
  
##Business Data
summary(business_data)
head(business_data)
sapply(business_data, function(x) sum(is.na(x)))
#Here, we are shown that there are a lot of missing values in the "attributes" section. This could possibly influence the model a lot, as there is missing information. However, in this model we decided to leave it as missing information due to the following reasons:
# The data records that the review website "(yelp)" has no data on these attributes, meaning that they were not significant enough to be mentioned in the restaurant reviews and information. If these features were not significant enough to mention their presence or lack there-off, then they serve as information to the model nonetheless as they most likely did not influence the reviews. 
#However, we have to keep in mind this limitation of the data and how it could affect the project, so to get to a middle point we will remove all attribute columns that have more than 70% of missing or N/A values, treating them as outliers. 
# We set the 70% threshold as a variable to use later.
threshold <- 0.7
# We use that variable to see what columns have more than 70% values missing.
missing_values <- sapply(business_data$attributes, function(x) mean(is.na(x)) > threshold)
print(missing_values)    
# We now use that to get the names of the columns that we need to remove
columns_to_remove <- names(missing_values)[missing_values]
# Remove these columns from the dataset
business_data$attributes <- business_data$attributes[, !colnames(business_data$attributes) %in% columns_to_remove]
#Change the name of the "stars" column as other datasets have the same named column to express different things.Eliminate the previous one and place this column first in the dataset.  
business_data$`Business_Average_Stars` <- business_data$stars
business_data[["stars"]] <- NULL



#There are also some missing values in the Hours section. In the end, we have decided to remove these values from the dataset as there are many reasons which make them not useful for the model. First, there are many missing values. Second, they are written in a character type that only details what time range the restaurant is open each day, which cannot be analysed.
sapply(business_data$hours, function(x) table(x, useNA = "ifany"))
    #A preliminary look through this tells me that theres a wide variety of data here, some of it either false or not useful (as there are, for example, more than 7 thousand instances were these places are open for 0 minutes (0:00-0:00), for 15 minutes only, (0:0-0:15) etc. This tells me that much of this data is simply not useful and needs to either be eliminated or simplified in some way. 
      #In the end, it was decided to eliminate all the data of "hours", as there is far too many inconsistent and not useful data we can extract from this for the model, not to mention way too many data points that would need to be simplified instead.  
      #However, in order to simplify all of that data instead of completely ignore it, a new column will replace them that states if the places are open on weekends.
business_data$open_on_weekends <- apply(business_data$hours[, c("Saturday", "Sunday")], 1, function(x) any(!is.na(x) & x != ""))
business_data$hours <- NULL
business_data <- business_data %>%
  mutate(open_on_weekends = as.numeric(open_on_weekends))

#To better use the variable, we need to turn the Price Range variable into a numeric one
business_data$attributes$RestaurantsPriceRange2 <- factor(business_data$attributes$RestaurantsPriceRange2, levels = c("None", "1", "2", "3", "4"))
business_data$attributes$RestaurantsPriceRange2 <- as.numeric(business_data$attributes$RestaurantsPriceRange2) - 1

#I have noticed that theres a lot of different responses in the Wifi attribute category. We should change it this way. 
unique(business_data$attributes$WiFi)
    #Based on the unique responses, create a new numeric column. For No wifi, we put a 0. For wifi that exists but needs payment, a 1, and for free wifi a 2
business_data$attributes$WiFi_numeric <- ifelse(is.na(business_data$attributes$WiFi), NA,
                                                ifelse(business_data$attributes$WiFi %in% c("u'no'", "'no'", "None"), 0,
                                                       ifelse(business_data$attributes$WiFi %in% c("u'free'", "'free'"), 2, 1)))
business_data$attributes <- subset(business_data$attributes, select = -WiFi)
#Finally, turning the rest of the remaining attributes into numeric 0/1 columns. 
AttributeColumns <- c("BusinessAcceptsCreditCards", "BikeParking", "RestaurantsTakeOut", 
                        "RestaurantsDelivery", "OutdoorSeating", "RestaurantsReservations", 
                        "GoodForKids")
business_data$attributes <- business_data$attributes %>%
  mutate(across(all_of(AttributeColumns), ~ifelse(. == "True", 1, ifelse(. == "False", 0, NA))))
#Eliminate redundant information from the data set: latitude and longitude columns as they are just inflating the data, and the information on location is already provided by the city, state, and postal code.
business_data$city <- tolower(business_data$city)
business_data$state <- tolower(business_data$state)
business_data <- business_data[, !(names(business_data) %in% c("latitude", "longitude", "city", "state", "address"))]





##Review Data
summary(review_data_small)
head(review_data_small)
sapply(review_data_small, function(x) sum(is.na(x)))
#There are no missing values so we don't need to delve deeper into that.



##User Data
summary(user_data_small)
head(user_data_small)
  #There is a big section called "compliments" in this dataset, detailing the number of compliments that this user has received- a way in which yelp lets other users send some, as yelp says: "good vibes" to other users. This is unnecessary, as the model wants to know how to predict reviews, while the compliments are something specific to how other people feel about the User's profile and not about how reviews are done. Whis could in the end affect the model, but its something unnecessary for us (based on our previous work in the business understanding and analytical approach sections)
sapply(user_data_small, function(x) sum(is.na(x)))
  #No missing data, no further action needed.
#This section is to remove unnecessary data from the dataset
user_data_small <- user_data_small[, -c(12:22)]
user_data_small <- user_data_small[, -c(9:10)]
user_data_small <- user_data_small[, !colnames(user_data_small) %in% c("name", "yelping_since")]
user_data_small$elite_years <- sapply(strsplit(user_data_small$elite, ","), function(x) length(x))
user_data_small$elite <- NULL  # We remove the "elite" column, instead replacing it iwht a simple column that counts how many years this user was considered elite, which will be easier to analyse later. 
#Elite users are, according to Eater (2009) Elite users are recognized by Yelp for “well-written reviews, high quality tips, a detailed personal profile, an active voting and complimenting record, and a history of playing well with others.”
  #It is expected that Elite users will be more likely to post high-veracity reviews, so its possible that this will be useful eventually so we will save the information for now. 
#we also see that there are some columns that may need renaming to avoid confusion considering that other datasets have the same names for other variables that do not mean the same thing.
user_data_small <- user_data_small %>%
  rename(
    user_review_count = review_count,
    user_profile_usefulness = useful,
    user_profile_funny = funny,
    user_profile_coolness = cool,
    user_average_stars_given = average_stars
  )


##Tips Data
summary(tip_data)
head(tip_data)
sapply(tip_data, function(x) sum(is.na(x)))
#There is no need for the date and the compliment count sections. there's at most 6 compliments on a tip, which is not only not significant but also has no direct effect on how a user is to review an establishment. 
tip_data <- tip_data[, !colnames(tip_data) %in% c("date", "compliment_count")]

##Check-in Data
summary(checkin_data)
head(checkin_data)
sapply(checkin_data, function(x) sum(is.na(x)))
#looking at the data provided by this dataset, it is a struggle to see if it would be useful for the model, as it only presents the dates for user log-ins which are not relevant to the reviews.

#We separate the review data into a test and training data sets, which will serve as the base for the merging of other dataset's variables into them.
set.seed(1)
a <- createDataPartition(review_data_small$stars, p = 0.4, list = FALSE)
training_data <- review_data_small[-sample(a, size = 10000), ]
test_data <- review_data_small[sample(a, size = 10000), ]

nrow(training_data)
nrow(test_data)

#we decided to make the data set smaller as the large size before did not allow for some analysis later. 
training_data <- training_data[sample(1:(nrow(training_data)), size = 80000, replace = FALSE), ]




#the next section is where, after multiple attempts at developing a linear model with the variables in the dataset until now, we attempt to examine the text of the reviews itself. The best way to examine this was determined to be sentiment analysis (which was raised as an option when reading previous literature in the subject.)
#Sentiment analysis is a way to identify how a person feels when writing a text, by assigning values to certain words. For example, words used for excitedly showing happiness like "ecstatic" will have a high POSITIVE score, while something like "disappointing" will have a negative score. based on this, we sum up a score. This score could potentially be used to predict if a user will assign the business a high or lower score rating. 
#Using three sentiment lexicons (all three were options from some websites teaching how to do sentiment analysis)

install.packages("textdata", repos = 'http://cran.us.r-project.org')

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


afinn_lexicon <- get_sentiments("afinn")
bing_lexicon <- get_sentiments("bing")
bing_lexicon <- bing_lexicon %>%
  mutate(
    bing_numeric = case_when(
      sentiment == "positive" ~ 1,
      sentiment == "negative" ~ -1,
      TRUE ~ 0
    )
  )
nrc_lexicon <- get_sentiments("nrc")
unique(nrc_lexicon$sentiment)
sentiment_scores <- c("anger" = -2, "anticipation" = 1, "disgust" = -2, "fear" = -1, 
                      "joy" = 2, "sadness" = -1, "surprise" = 0, "trust" = 2, 
                      "positive" = 1, "negative" = -1)
nrc_lexicon <- nrc_lexicon %>%
  mutate(nrc_numeric = case_when(sentiment %in% names(sentiment_scores) ~ sentiment_scores[sentiment], TRUE ~ 0))


#here, we add columns to both the train and test datasets that detail the score they get from each lexicon analysis. 

training_data <- training_data %>%
  rowwise() %>%
  mutate(afinn_score = sum(afinn_lexicon$value[afinn_lexicon$word %in% unlist(strsplit(text, " "))]))
training_data <- training_data %>%
  rowwise() %>%
  mutate(bing_score = sum(bing_lexicon$bing_numeric[bing_lexicon$word %in% unlist(strsplit(text, " "))]))
training_data <- training_data %>%
  rowwise() %>%
  mutate(ncr_score = sum(nrc_lexicon$nrc_numeric[nrc_lexicon$word %in% unlist(strsplit(text, " "))]))


test_data <- test_data %>%
  rowwise() %>%
  mutate(afinn_score = sum(afinn_lexicon$value[afinn_lexicon$word %in% unlist(strsplit(text, " "))]))
test_data <- test_data %>%
  rowwise() %>%
  mutate(bing_score = sum(bing_lexicon$bing_numeric[bing_lexicon$word %in% unlist(strsplit(text, " "))]))
test_data <- test_data %>%
  rowwise() %>%
  mutate(ncr_score = sum(nrc_lexicon$nrc_numeric[nrc_lexicon$word %in% unlist(strsplit(text, " "))]))


#Finally, we will create a simple linear model using these newly created variables, to examine their relationship 
LinearModelv1 <- lm(stars ~ afinn_score + bing_score + ncr_score, data = training_data)
summary(LinearModelv1)

#use the test data we have to see if the model is working correctly and will predict well. 

predictions <- predict(LinearModelv1, newdata = test_data)
data.frame( R2 = R2(predictions, test_data$stars),
            RMSE = RMSE(predictions, test_data$stars),
            MAE = MAE(predictions, test_data$stars))
#Thile this is a good start, a high error and a low explanatory power hold this model back.
#create a lasso model with the same variables in order to confirm values and errors. 

X <- model.matrix(stars ~ afinn_score + bing_score + ncr_score, data = training_data)
Y <- training_data$stars

lasso_modelv1 <- cv.glmnet(X, Y, alpha = 1)
best_lambda <- lasso_modelv1$lambda.min
plot(lasso_modelv1)
X_test <- model.matrix(stars ~ afinn_score + bing_score + ncr_score, data = test_data)
predictions <- predict(lasso_modelv1, newx = X_test, s = best_lambda)
mean(abs(predictions - test_data$stars))



#create a data set with the training data as a base, adding other data and values that might be useful and found in other sets. 

merged_data <- merge(training_data, business_data, by = "business_id", all.x = TRUE, all.y = FALSE)
merged_data <- merge(merged_data, user_data_small, by = "user_id", all.x = TRUE, all.y = FALSE)

hist(merged_data$stars, main = "Star Score Frequency Distribution", xlab = "Stars", ylab = "Frequency")

#Data set is incredibly large and must be trimmed down for other analysis later on (this step came after noticing that sentiment analysis would take a long time with such a large data set)
#Decrease size by only using data for restaurants, to trim the set down but also focus this analysis in one category to avoid problems (variables that affect the rating of a hair salon might not be the same of a restaurant, for example.)
categories_list <- strsplit(as.character(merged_data$categories), ", ")
all_categories <- unlist(strsplit(as.character(merged_data$categories), ", "))
all_categories <- all_categories[all_categories != ""]
top_category <- names(sort(table(all_categories), decreasing = TRUE)[1])

# Keep only the rows with the top "category". We cannot analyse the reviews left on restaurants the same way we analyse beauty salons or hotels as there are different parameters, works used, and people going to these. using the largedt pool of data (the one from reviews from restaurants) will bring a better and more accurate result. 
merged_data <- merged_data[sapply(strsplit(as.character(merged_data$categories), ", "), function(lst) top_category %in% lst), ]


merged_data$numeric_postal_code <- as.numeric(gsub("[^0-9]", "", merged_data$postal_code))
#The data set is just too large, and has many columns that are not useful now. After checking that all else is correct, we decided to eliminate the character/non-numeric columns. 
head(merged_data)
merged_data_text <- merged_data[c("stars", "text", "user_id", "business_id")]

eliminate_columns <- c("user_id", "business_id", "name", "postal_code", "categories", "review_id")
merged_data <- merged_data %>%
  select(-one_of(eliminate_columns))
merged_data$attributes$BusinessParking <- NULL
merged_data$date <- NULL

merged_data <- merged_data[complete.cases(merged_data[, c("Business_Average_Stars", "user_average_stars_given", "review_count")]), ]


#There are a lot of missing values in the data that prevent the models from being propperly done. 


merged_data$attributes$BusinessAcceptsCreditCards <- ifelse(
  is.na(merged_data$attributes$BusinessAcceptsCreditCards),
  0,
  merged_data$attributes$BusinessAcceptsCreditCards
)
merged_data$attributes$BikeParking <- ifelse(
  is.na(merged_data$attributes$BikeParking),
  0,
  merged_data$attributes$BikeParking
)
merged_data$attributes$RestaurantsPriceRange2 <- ifelse(
  is.na(merged_data$attributes$RestaurantsPriceRange2),
  0,
  merged_data$attributes$RestaurantsPriceRange2
)
merged_data$attributes$RestaurantsTakeOut <- ifelse(
  is.na(merged_data$attributes$RestaurantsTakeOut),
  0,
  merged_data$attributes$RestaurantsTakeOut
)
merged_data$attributes$RestaurantsDelivery <- ifelse(
  is.na(merged_data$attributes$RestaurantsDelivery),
  0,
  merged_data$attributes$RestaurantsDelivery
)
merged_data$attributes$OutdoorSeating <- ifelse(
  is.na(merged_data$attributes$OutdoorSeating),
  0,
  merged_data$attributes$OutdoorSeating
)
merged_data$attributes$RestaurantsReservations <- ifelse(
  is.na(merged_data$attributes$RestaurantsReservations),
  0,
  merged_data$attributes$RestaurantsReservations
)
merged_data$attributes$GoodForKids <- ifelse(
  is.na(merged_data$attributes$GoodForKids),
  0,
  merged_data$attributes$GoodForKids
)
merged_data$attributes$WiFi_numeric <- ifelse(
  is.na(merged_data$attributes$WiFi_numeric),
  0,
  merged_data$attributes$WiFi_numeric
)
merged_data$user_review_count <- ifelse(
  is.na(merged_data$user_review_count),
  0,
  merged_data$user_review_count
)
merged_data$user_profile_usefulness <- ifelse(
  is.na(merged_data$user_profile_usefulness),
  0,
  merged_data$user_profile_usefulness
)
merged_data$user_profile_funny <- ifelse(
  is.na(merged_data$user_profile_funny),
  0,
  merged_data$user_profile_funny
)
merged_data$user_profile_coolness <- ifelse(
  is.na(merged_data$user_profile_coolness),
  0,
  merged_data$user_profile_coolness
)
merged_data$user_average_stars_given <- ifelse(
  is.na(merged_data$user_average_stars_given),
  0,
  merged_data$user_average_stars_given
)
merged_data$elite_years <- ifelse(
  is.na(merged_data$elite_years),
  0,
  merged_data$elite_years
)
merged_data$numeric_postal_code <- ifelse(
  is.na(merged_data$numeric_postal_code),
  0,
  merged_data$numeric_postal_code
)

merged_data <- merged_data %>%
  rowwise() %>%
  mutate(afinn_score = sum(afinn_lexicon$value[afinn_lexicon$word %in% unlist(strsplit(text, " "))]))
merged_data <- merged_data %>%
  rowwise() %>%
  mutate(bing_score = sum(bing_lexicon$bing_numeric[bing_lexicon$word %in% unlist(strsplit(text, " "))]))
merged_data <- merged_data %>%
  rowwise() %>%
  mutate(ncr_score = sum(nrc_lexicon$nrc_numeric[nrc_lexicon$word %in% unlist(strsplit(text, " "))]))
merged_data$text <- NULL







#Merged_data will be used as the training dataset, as it is based on the values located in "training_data". Due to this, we use "testing_data" and add the same merged data to this as we did with the other dataset, to create a testing dataset that can be used. 

merged_test_data <- merge(test_data, business_data, by = "business_id", all.x = TRUE, all.y = FALSE)
merged_test_data <- merge(merged_test_data, user_data_small, by = "user_id", all.x = TRUE, all.y = FALSE)

categories_list <- strsplit(as.character(merged_test_data$categories), ", ")
all_categories <- unlist(strsplit(as.character(merged_test_data$categories), ", "))
all_categories <- all_categories[all_categories != ""]
top_category <- names(sort(table(all_categories), decreasing = TRUE)[1])

# Keep only the rows with the top "category". We cannot analyse the reviews left on restaurants the same way we analyse beauty salons or hotels as there are different parameters, works used, and people going to these. using the largedt pool of data (the one from reviews from restaurants) will bring a better and more accurate result. 
merged_test_data <- merged_test_data[sapply(strsplit(as.character(merged_test_data$categories), ", "), function(lst) top_category %in% lst), ]
#To use the postal code as a numeric variable which can be used, we transform it.
merged_test_data$numeric_postal_code <- as.numeric(gsub("[^0-9]", "", merged_test_data$postal_code))
#The data set is just too large, and has many columns that are not useful now. After checking that all else is correct, we decided to eliminate the character/non-numeric columns. 
head(merged_test_data)
merged_test_data_text <- merged_test_data[c("stars", "text", "user_id", "business_id")]
eliminate_columns <- c("user_id", "business_id", "name", "postal_code", "categories", "review_id")
merged_test_data <- merged_test_data %>%
  select(-one_of(eliminate_columns))
merged_test_data$attributes$BusinessParking <- NULL
merged_test_data$date <- NULL


merged_test_data <- merged_test_data[complete.cases(merged_test_data[, c("Business_Average_Stars", "user_average_stars_given", "review_count")]), ]


#There are a lot of missing values in the data that prevent the models from being propperly done. 


merged_test_data$attributes$BusinessAcceptsCreditCards <- ifelse(
  is.na(merged_test_data$attributes$BusinessAcceptsCreditCards),
  0,
  merged_test_data$attributes$BusinessAcceptsCreditCards
)
merged_test_data$attributes$BikeParking <- ifelse(
  is.na(merged_test_data$attributes$BikeParking),
  0,
  merged_test_data$attributes$BikeParking
)
merged_test_data$attributes$RestaurantsPriceRange2 <- ifelse(
  is.na(merged_test_data$attributes$RestaurantsPriceRange2),
  0,
  merged_test_data$attributes$RestaurantsPriceRange2
)
merged_test_data$attributes$RestaurantsTakeOut <- ifelse(
  is.na(merged_test_data$attributes$RestaurantsTakeOut),
  0,
  merged_test_data$attributes$RestaurantsTakeOut
)
merged_test_data$attributes$RestaurantsDelivery <- ifelse(
  is.na(merged_test_data$attributes$RestaurantsDelivery),
  0,
  merged_test_data$attributes$RestaurantsDelivery
)
merged_test_data$attributes$OutdoorSeating <- ifelse(
  is.na(merged_test_data$attributes$OutdoorSeating),
  0,
  merged_test_data$attributes$OutdoorSeating
)
merged_test_data$attributes$RestaurantsReservations <- ifelse(
  is.na(merged_test_data$attributes$RestaurantsReservations),
  0,
  merged_test_data$attributes$RestaurantsReservations
)
merged_test_data$attributes$GoodForKids <- ifelse(
  is.na(merged_test_data$attributes$GoodForKids),
  0,
  merged_test_data$attributes$GoodForKids
)
merged_test_data$attributes$WiFi_numeric <- ifelse(
  is.na(merged_test_data$attributes$WiFi_numeric),
  0,
  merged_test_data$attributes$WiFi_numeric
)
merged_test_data$user_review_count <- ifelse(
  is.na(merged_test_data$user_review_count),
  0,
  merged_test_data$user_review_count
)
merged_test_data$user_profile_usefulness <- ifelse(
  is.na(merged_test_data$user_profile_usefulness),
  0,
  merged_test_data$user_profile_usefulness
)
merged_test_data$user_profile_funny <- ifelse(
  is.na(merged_test_data$user_profile_funny),
  0,
  merged_test_data$user_profile_funny
)
merged_test_data$user_profile_coolness <- ifelse(
  is.na(merged_test_data$user_profile_coolness),
  0,
  merged_test_data$user_profile_coolness
)
merged_test_data$user_average_stars_given <- ifelse(
  is.na(merged_test_data$user_average_stars_given),
  0,
  merged_test_data$user_average_stars_given
)
merged_test_data$elite_years <- ifelse(
  is.na(merged_test_data$elite_years),
  0,
  merged_test_data$elite_years
)
merged_test_data$numeric_postal_code <- ifelse(
  is.na(merged_test_data$numeric_postal_code),
  0,
  merged_test_data$numeric_postal_code
)

merged_test_data <- merged_test_data %>%
  rowwise() %>%
  mutate(afinn_score = sum(afinn_lexicon$value[afinn_lexicon$word %in% unlist(strsplit(text, " "))]))
merged_test_data <- merged_test_data %>%
  rowwise() %>%
  mutate(bing_score = sum(bing_lexicon$bing_numeric[bing_lexicon$word %in% unlist(strsplit(text, " "))]))
merged_test_data <- merged_test_data %>%
  rowwise() %>%
  mutate(ncr_score = sum(nrc_lexicon$nrc_numeric[nrc_lexicon$word %in% unlist(strsplit(text, " "))]))
merged_test_data$text <- NULL






LinearModelv2 <- lm(stars ~ useful + funny + cool + is_open + user_average_stars_given +
                      bing_score + afinn_score + ncr_score + user_review_count + Business_Average_Stars,
                    data = merged_data)

summary(LinearModelv2)
residuals(LinearModelv2)
plot(residuals(LinearModelv2), main = "Residuals Plot", ylab = "Residuals")

#Testing the model

predictions <- predict(LinearModelv2, newdata = merged_test_data)
data.frame( R2 = R2(predictions, merged_test_data$stars),
            RMSE = RMSE(predictions, merged_test_data$stars),
            MAE = MAE(predictions, merged_test_data$stars))

#Lasso model to verify

X <- model.matrix(stars ~ useful + funny + cool + is_open + user_average_stars_given + bing_score + afinn_score + ncr_score + user_review_count + Business_Average_Stars, data = merged_data)
Y <- merged_data$stars

lasso_modelv2 <- cv.glmnet(X, Y, alpha = 1)
best_lambda <- lasso_modelv2$lambda.min
plot(lasso_modelv2)
X_test <- model.matrix(stars ~ useful + funny + cool + is_open + user_average_stars_given + bing_score + afinn_score + ncr_score + user_review_count + Business_Average_Stars, data = merged_test_data)
predictions <- predict(lasso_modelv2, newx = X_test, s = best_lambda)
mean(abs(predictions - merged_test_data$stars))







###Now that we have all the data prepared again, we can create a model. However, there are a lot of variables that could hold different amounts of explanatory power in star prediction. Getting the ones with the highest explanatory power is the aim of this project, and as such we shall create a random forest model to plot and view the importance of each variable. 
#this section, we need to unnest the attributes section momentarily as it was, in a way, "a dataset in another dataset". 
#To get the importance of each variable, we can use the random forest model. 

merged_data <- tidyr::unnest(merged_data, cols = (names(merged_data)))
merged_test_data <- tidyr::unnest(merged_test_data, cols = (names(merged_test_data)))
Target_Variable <- "stars"

#Create the random forest model
rf_model <- randomForest(as.factor(get(Target_Variable)) ~ . , data = merged_data, ntree = 100)

#View and plot the importance of each variable 
importance(rf_model)
varImpPlot(rf_model)


#Using the 5 most important variables, we create a final linear regression model. We see that there is an r-squared value of 0.43, which means this model explains 43% of the variation. 
LinearModelv3 <- lm(stars ~ user_average_stars_given + bing_score + afinn_score + user_review_count +Business_Average_Stars , data = merged_data)
summary(LinearModelv3)

predictions <- predict(LinearModelv3, newdata = merged_test_data)
data.frame( R2 = R2(predictions, merged_test_data$stars),
            RMSE = RMSE(predictions, merged_test_data$stars),
            MAE = MAE(predictions, merged_test_data$stars))


#Lasso model to verify

X <- model.matrix(stars ~ useful + funny + cool + afinn_score + bing_score + ncr_score + review_count + is_open + BusinessAcceptsCreditCards + BikeParking + RestaurantsPriceRange2 + RestaurantsTakeOut + RestaurantsDelivery + OutdoorSeating + RestaurantsReservations + GoodForKids + WiFi_numeric + Business_Average_Stars + open_on_weekends + user_review_count + user_profile_usefulness + user_profile_funny + user_profile_coolness + user_average_stars_given + elite_years + numeric_postal_code, data = merged_data)
Y <- merged_data$stars

lasso_modelv3 <- cv.glmnet(X, Y, alpha = 1)
best_lambda <- lasso_modelv3$lambda.min
plot(lasso_modelv3)
X_test <- model.matrix(stars ~ useful + funny + cool + afinn_score + bing_score + ncr_score + review_count + is_open + BusinessAcceptsCreditCards + BikeParking + RestaurantsPriceRange2 + RestaurantsTakeOut + RestaurantsDelivery + OutdoorSeating + RestaurantsReservations + GoodForKids + WiFi_numeric + Business_Average_Stars + open_on_weekends + user_review_count + user_profile_usefulness + user_profile_funny + user_profile_coolness + user_average_stars_given + elite_years + numeric_postal_code, data = merged_test_data)
predictions <- predict(lasso_modelv3, newx = X_test, s = best_lambda)
mean(abs(predictions - merged_test_data$stars))


#the mean absolute error in this model goes down to a 0.82, which is lower than previous iterations of the model. It says there the model's predictions are off by 0.8 stars generally. 


#For the purposes of the final report and easy visualization on the results of each model, we will create a table that shouws the summary data from each model. 
titles <- c("Version 1", "Version 2", "Version 3")

residual_standard_error <- c(1.326, 1.029, 1.055)
multiple_r_squared <- c(0.1988, 0.4605, 0.4336)
adjusted_r_squared <- c(0.1988, 0.46, 0.4334)
p_value <- c("< 2.2e-16", "< 2.2e-16", "< 2.2e-16")
r2 <- c(0.2141986, 0.441894, 0.4383494)
rmse <- c(1.31752, 1.054521, 1.056934)
mae <- c(1.114106, 0.8243429, 0.8269731)

SummaryDataSet <- data.frame(
  Version = titles,
  Residual_Standard_Error = c(1.326, 1.029, 1.055),
  Multiple_R_Squared = c(0.1988, 0.4605, 0.4336),
  Adjusted_R_Squared = c(0.1988, 0.46, 0.4334),
  P_Value = c("< 2.2e-16", "< 2.2e-16", "< 2.2e-16"),
  R2 = c(0.2141986, 0.441894, 0.4383494),
  RMSE = c(1.31752, 1.054521, 1.056934),
  MAE = c(1.114106, 0.8243429, 0.8269731))

SummaryTable <- kable(SummaryDataSet, "html") %>% kable_styling("striped", full_width = FALSE)
print(SummaryTable)
