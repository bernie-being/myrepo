
#Clear
cat("\014")  
rm(list=ls())

#Set Directory
rm(list=ls())
setwd("C:/Users/usuario/Downloads/EC349")

#Load Libraries
install.packages(c("tidytext", "dplyr", "tm", "stringr"))
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
  
  
## Business Data
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





## Review Data
summary(review_data_small)
head(review_data_small)
sapply(review_data_small, function(x) sum(is.na(x)))
#There are no missing values so we don't need to delve deeper into that.



## User Data
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


## Tips Data
summary(tip_data)
head(tip_data)
sapply(tip_data, function(x) sum(is.na(x)))
#There is no need for the date and the compliment count sections. there's at most 6 compliments on a tip, which is not only not significant but also has no direct effect on how a user is to review an establishment. 
tip_data <- tip_data[, !colnames(tip_data) %in% c("date", "compliment_count")]

## Check-in Data
summary(checkin_data)
head(checkin_data)
sapply(checkin_data, function(x) sum(is.na(x)))
#looking at the data provided by this dataset, it is a struggle to see if it would be useful for the model, as it only presents the dates for user log-ins which are not relevant to the reviews.




set.seed(1)
a <- createDataPartition(review_data_small$stars, p = 0.4, list = FALSE)
training_data <- review_data_small[-sample(a, size = 10000), ]
test_data <- review_data_small[sample(a, size = 10000), ]

nrow(training_data)
nrow(test_data)

training_data <- training_data[sample(1:(nrow(training_data)), size = 80000, replace = FALSE), ]






install.packages("textdata", repos = 'http://cran.us.r-project.org')

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


merged_data_text$afinn<- merged_data_text %>% 
  cross_join(get_sentiments("afinn")) %>% 
  group_by(text) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")
afinn_score <- merged_data_text %>%
  unnest_tokens(word, text) %>%
  inner_join("AFINN", by = "word") %>%
  group_by(index = row_number()) %>%
  summarise(afinn_score = sum(score, na.rm = TRUE))


merged_data_text <- merged_data_text %>%
  left_join("affin", by = c(word = "word")) %>%
  group_by(text) %>%
  summarise(afinn_score = sum(score, na.rm = TRUE))


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



model <- lm(stars ~ afinn_score + bing_score + ncr_score, data = training_data)
summary(model)















X <- model.matrix(stars ~ afinn_score + bing_score + ncr_score, data = training_data)
Y <- training_data$stars

lasso_model <- cv.glmnet(X, Y, alpha = 1)
best_lambda <- lasso_model$lambda.min
print(paste("Optimal Lambda:", best_lambda))
plot(lasso_model)
X_test <- model.matrix(stars ~ afinn_score + bing_score + ncr_score, data = test_data)
predictions <- predict(lasso_model, newx = X_test, s = best_lambda)
mae <- mean(abs(predictions - test_data$stars))
print(paste("Mean Absolute Error:", mae))

























merged_data <- merge(training_data, business_data, by = "business_id", all.x = TRUE, all.y = FALSE)
merged_data <- merge(merged_data, user_data_small, by = "user_id", all.x = TRUE, all.y = FALSE)



categories_list <- strsplit(as.character(merged_data$categories), ", ")
all_categories <- unlist(strsplit(as.character(merged_data$categories), ", "))
all_categories <- all_categories[all_categories != ""]
top_category <- names(sort(table(all_categories), decreasing = TRUE)[1])

# Keep only the rows with the top "category". We cannotanalyse the reviews left on restaurants the same way we analyse beauty salons or hotels as there are different parameters, works used, and people going to these. using the largedt pool of data (the one from reviews from restaurants) will bring a better and more accurate result. 
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










merged_test_data <- merge(test_data, business_data, by = "business_id", all.x = TRUE, all.y = FALSE)
merged_test_data <- merge(merged_test_data, user_data_small, by = "user_id", all.x = TRUE, all.y = FALSE)

categories_list <- strsplit(as.character(merged_test_data$categories), ", ")
all_categories <- unlist(strsplit(as.character(merged_test_data$categories), ", "))
all_categories <- all_categories[all_categories != ""]
top_category <- names(sort(table(all_categories), decreasing = TRUE)[1])

# Keep only the rows with the top "category". We cannotanalyse the reviews left on restaurants the same way we analyse beauty salons or hotels as there are different parameters, works used, and people going to these. using the largedt pool of data (the one from reviews from restaurants) will bring a better and more accurate result. 
merged_test_data <- merged_test_data[sapply(strsplit(as.character(merged_test_data$categories), ", "), function(lst) top_category %in% lst), ]



merged_test_data$numeric_postal_code <- as.numeric(gsub("[^0-9]", "", merged_test_data$postal_code))
#The data set is just too large, and has many columns that are not useful now. After checking that all else is correct, we decided to eliminate the character/non-numeric columns. 
head(merged_test_data)
merged_test_data_text <- merged_test_data[c("stars", "text", "user_id", "business_id")]

eliminate_columns <- c("user_id", "business_id", "name", "postal_code", "categories", "review_id")
merged_test_data <- merged_test_data %>%
  select(-one_of(eliminate_columns))
merged_test_data$attributes$BusinessParking <- NULL
merged_test_data$date <- NULL
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

#checking for Multicollinearity
cor(merged_data, method = "pearson")
#In general, we can say that there is no multicolinearity in this dataset, as there is no coefficient above 0.7

Modelv0 <- lm(stars ~ Business_Average_Stars + user_average_stars_given + review_count, data = merged_data)
summary(Modelv0)


Modelv1 <- lm(stars ~ Business_Average_Stars + user_average_stars_given + review_count + afinn_score + ncr_score + bing_score, data = merged_data)
summary(Modelv1)
#Looking at the summary, we determine that the p-value of this model is: p-value < 2.2e-16. This is a very small p-value, which tells us that at least one of our predictor variables is significantly related to the "Stars" variable. 
  #However, the r-squared value is extremely low, at 0.2417. Meaning that only 24% of the variation can be explained by our model. 
#We now use the merged dataset to create a linear model, in the first attempt to predict reviews. 

#Fitting the Lasso model
set.seed(1)
X <- model.matrix(stars ~ Business_Average_Stars + user_average_stars_given + review_count, data = merged_data)
Y <- merged_data$stars

# Fit Lasso model
lasso_model1 <- cv.glmnet(X, Y, alpha = 1)  # alpha = 1 corresponds to Lasso
control = trainControl(method = "cv", number = 5)
lasso_model1 = train(X, Y, method = "glmnet", trControl = control)
lasso_model1






X <- model.matrix(stars ~ Business_Average_Stars + user_average_stars_given + review_count + afinn_score + bing_score + ncr_score, data = merged_data)
Y <- merged_data$stars

lasso_model2 <- cv.glmnet(X, Y, alpha = 1)
best_lambda <- lasso_model2$lambda.min
print(paste("Optimal Lambda:", best_lambda))
plot(lasso_model2)
X_test <- model.matrix(stars ~ Business_Average_Stars + user_average_stars_given + review_count + afinn_score + bing_score + ncr_score, data = merged_test_data)
predictions <- predict(lasso_model2, newx = X_test, s = best_lambda)
mae <- mean(abs(predictions - merged_test_data$stars))
print(paste("Mean Absolute Error:", mae))














set.seed(1)
X <- model.matrix(stars ~ Business_Average_Stars + user_average_stars_given + review_count + afinn_score + ncr_score + bing_score, data = merged_data)
Y <- merged_data$stars

# Fit Lasso model
lasso_model2 <- cv.glmnet(X, Y, alpha = 1)  # alpha = 1 corresponds to Lasso
control = trainControl(method = "cv", number = 5)
lasso_model2 = train(X, Y, method = "glmnet", trControl = control)
lasso_model2


merged_data_text <- merged_data_text %>%
  sample_n(size = 90000, replace = FALSE, seed = 1)
















































head(merged_data)
Model_Attributes <- lm(stars ~ attributes$BikeParking + attributes$RestaurantsPriceRange2 + attributes$RestaurantsTakeOut + attributes$RestaurantsDelivery + attributes$WiFi_numeric + open_on_weekends, data = merged_data)
summary(Model_Attributes)
#The attributes of a restaurant have nothing to do with the rating (0.01068 r-squared value)


Model_Users <- lm(stars ~ user_profile_usefulness  + user_average_stars_given + elite_years, data = merged_data)
summary(Model_Users)
#profile usefulness and elite status is not a relevant variable at all, implying that being an active, engaged and highly rated reviewer does not influence the score one would give to an establishment. 

merged_data



#Until now, it seems clear that the best model so far has been "Model_v0". IN oredr to continue exploring the data in it, we will be exploring and eliminating any outliers. 
merged_data_2 <- merged_data
Q1 <- quantile(merged_data$Business_Average_Stars, 0.25)
Q3 <- quantile(merged_data$Business_Average_Stars, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
outliers <- merged_data$Business_Average_Stars < lower_bound | merged_data$Business_Average_Stars > upper_bound
merged_data_2 <- merged_data_2[!outliers, ]






subset_data <- merged_data[sample(nrow(merged_data), 2000), ]








#Now, we will try another approach: Using the text in the reviews and comparing it to the "tips" that were left on the business.
#the logic behind this is, if people follow the tips (what is best to order or avoid for customers), will that be an indication that their reviews are going to increase in value?

set.seed(1)  
sample_size <- 10000


sample_training_data <- training_data[sample(nrow(training_data), size = sample_size, replace = TRUE), ]
sample_tip_data <- tip_data[sample(nrow(tip_data), size = sample_size, replace = TRUE), ]
merged_text_data <- merge(sample_training_data, sample_tip_data, by = "business_id", all.x = TRUE)
summary(is.na(merged_text_data))
selected_columns <- c("stars", "text.x", "text.y")
merged_text_data <- merged_text_data[selected_columns]

merged_text_data$common_words_count <- sapply(1:nrow(merged_text_data), function(i) {
  words_x <- strsplit(as.character(merged_text_data$text.x[i]), "\\s")[[1]]
  words_y <- strsplit(as.character(merged_text_data$text.y[i]), "\\s")[[1]]
  common_words <- intersect(words_x, words_y)
  length(common_words)
})

cor(merged_text_data$stars, merged_text_data$common_words_count)
