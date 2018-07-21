# Luke Vandertie
# McKinsey Hackathon July 20-22, 2018

library(dummies)

#----Logistic Regression to create benchmark probability model----
setwd("C:/Users/lvandertie001/Documents/GitHub/McKinsey-Hackathon-07-18")
# Read data
train.data <- read.csv(file = "train_ZoGVYWq.csv")
  # Try to read in straight from online
  #train.online <- read.csv("https://datahack.analyticsvidhya.com/contest/mckinsey-analytics-online-hackathon-4/download/test-file")

# Convert to Categorical and Drop Redundant

drop_redundant <- function(data.frame.var) {
  data.frame.var <- dummy.data.frame(data.frame.var)
  data.frame.var$sourcing_channelA <- NULL
  data.frame.var$residence_area_typeRural <- NULL
  data.frame.var$id <- NULL
  # Potentially combine the count of months (% of claims in each)
  return(data.frame.var)
}

train.data.converted <- drop_redundant(train.data)

# Create glm
logistic.model <- glm(renewal ~ . , train.data.converted, family = "binomial")

#----Predict benchmark probability using glm----
test.data <- read.csv(file = "test_66516Ee.csv")

test.data.converted <- drop_redundant(test.data)

test.predictions <- predict(logistic.model, test.data.converted, type = "response")

# Look at a histogram of revalues
# Inspect NA's


#----Plug Premium into optimization equation to get ideal Incentive
Premium <- 3000

#Incentive <- 1500
#Hours <- 10*(1 - exp(-Incentive/400))
#Hours <- 1
#delta_p <- 20*(1 - exp(- Hours/5))

incentive <- -400 * log( log(200/2.706706 * (100/Premium))/3  ) 

curve(log(log(200/2.706706 * (100/x))/3), from = 0, to = 10000, xlab = "Premium", ylab = "Incentive")

