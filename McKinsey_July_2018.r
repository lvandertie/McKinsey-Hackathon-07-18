# Luke Vandertie
# McKinsey Hackathon July 20-22, 2018

#----Logistic Regression to create benchmark probability model----


#----Plug Premium into optimization equation to get ideal Incentive
Premium <- 3000

#Incentive <- 1500
#Hours <- 10*(1 - exp(-Incentive/400))
#Hours <- 1
#delta_p <- 20*(1 - exp(- Hours/5))

incentive <- -400 * log( log(200/2.706706 * (100/Premium))/3  ) 

curve(log(log(200/2.706706 * (100/x))/3), from = 0, to = 10000, xlab = "Premium", ylab = "Incentive")
