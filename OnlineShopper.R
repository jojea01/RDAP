setwd("/Users/jose/Desktop/All_R/RDAP")
shopper <- read.csv("/Users/jose/Desktop/All_R/DataFiles/online_shoppers_intention.csv")


shopper_num <- shopper[,-c(11,16,17,18)]
shopper_char <- shopper[,c(11,16,17,18)]

str(shopper_num)
str(shopper_char)

pairs(shopper_num)

cov(shopper_num)

shopper_target <- data.frame(shopper[,18])
summary(shopper_target)

Test 
