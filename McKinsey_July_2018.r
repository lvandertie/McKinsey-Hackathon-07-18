# Luke Vandertie
# McKinsey Hackathon July 20-22, 2018

library(dummies)
library(plyr)


#----Logistic Regression to create benchmark probability model----
setwd("C:/Users/lvandertie001/Documents/GitHub/McKinsey-Hackathon-07-18")
# Read data
train.data <- read.csv(file = "train_ZoGVYWq.csv")
  # Try to read in straight from online
  #train.online <- read.csv("https://datahack.analyticsvidhya.com/contest/mckinsey-analytics-online-hackathon-4/download/test-file")

# Inspect data, look for if there is any pattern in missing data (MCAR vs MAR, etc)
count(train.data, "renewal")
colnames(train.data)[colSums(is.na(train.data)) > 0]
View(train.data[is.na(train.data$Count_3.6_months_late), ])

# Function to format datset

drop_redundant <- function(data.frame.var) {
  # Convert to dummies
  data.frame.var <- dummy.data.frame(data.frame.var)
  # Drop redundant
  data.frame.var$sourcing_channelA <- NULL
  data.frame.var$residence_area_typeRural <- NULL
  data.frame.var$id <- NULL
  # Impute missing values (Numerical)
  #   Is there a better way for the Counts columns?
  #   Should inspect type of missingness before just assigning mean
  data.frame.var$application_underwriting_score[is.na(data.frame.var$application_underwriting_score)] <- mean(data.frame.var$application_underwriting_score, na.rm = TRUE)
  data.frame.var$Count_3.6_months_late[is.na(data.frame.var$Count_3.6_months_late)] <- mean(data.frame.var$Count_3.6_months_late, na.rm = TRUE)
  data.frame.var$Count_6.12_months_late[is.na(data.frame.var$Count_6.12_months_late)] <- mean(data.frame.var$Count_6.12_months_late, na.rm = TRUE)
  data.frame.var$Count_more_than_12_months_late[is.na(data.frame.var$Count_more_than_12_months_late)] <- mean(data.frame.var$Count_more_than_12_months_late, na.rm = TRUE)
  # Account for this being a rare event
  
  
  # Potentially combine the count of months (% of claims in each)
  return(data.frame.var)
}

train.data.converted <- drop_redundant(train.data)

colnames(train.data.converted)[colSums(is.na(train.data.converted)) > 0]

# Create glm
logistic.model <- glm(renewal ~ . , train.data.converted, family = "binomial")

#----Predict benchmark probability using glm----
test.data <- read.csv(file = "test_66516Ee.csv")

# inspect test data
colnames(test.data)[colSums(is.na(test.data)) > 0]

test.data.converted <- drop_redundant(test.data)

test.data.converted$predicted_prob <- predict(logistic.model, test.data.converted, type = "response")
test.data.converted <- cbind(test.data$id, test.data.converted)

#----Plug Premium into optimization equation to get ideal Incentive

test.data.converted$incentive <- -400 * log( log(200/2.706706 * (100/test.data.converted$premium))/3  ) 
# Set NA's to zero
test.data.converted$incentive[is.na(test.data.converted$incentive)] <- 0

colnames(test.data.converted)[colSums(is.na(test.data.converted)) > 0]

#---- Prepare for output format ----



#---- Test work for incentive calc. ----
#Incentive <- 1500
#Hours <- 10*(1 - exp(-Incentive/400))
#Hours <- 1
#delta_p <- 20*(1 - exp(- Hours/5))

curve(log(log(200/2.706706 * (100/x))/3), from = 0, to = 10000, xlab = "Premium", ylab = "Incentive")

