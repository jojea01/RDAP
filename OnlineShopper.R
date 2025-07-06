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

#--- Changing nominal variables to numerical-------------
shopper_clean <- shopper

shopper_clean$month_num <- match(shopper_clean$Month, month.abb)
shopper_clean <- subset(shopper_clean, select = -Month)

shopper_clean$Revenue <- ifelse(shopper_clean$Revenue == "TRUE", 1,
                     ifelse(shopper_clean$Revenue == "FALSE", 2,NA))

shopper_clean$VisitorType <- ifelse(shopper_clean$VisitorType == "Returning_Visitor", 1,
                                ifelse(shopper_clean$VisitorType == "New_Visitor", 2,NA))


shopper_clean$Weekend <- ifelse(shopper_clean$Weekend == "TRUE", 1,
                                ifelse(shopper_clean$Weekend == "FALSE", 2,NA))

shopper_clean$Revenue <- as.numeric(shopper_clean$Revenue)
shopper_clean$Weekend <- as.numeric(shopper_clean$Weekend)
shopper_clean$VisitorType <- as.numeric(shopper_clean$VisitorType)


install.packages("corrplot")
library(corrplot)

cor <-cor(shopper_clean)
print(cor)
quartz(width=10, height = 10)
corrplot(cor, method = "color", addCoef.col = "black")

plot(cor)


#-- Droping variables Exit Rate ProductRelated_Duration Administrative_Duration Infromational_Duration  ----------

shop_drop <- shopper_clean[, !(names(shopper_clean) %in% c("ProductRelated_Duration", "Administrative_Duration", "Informational_Duration"))]
shop_drop

#-- T-test


#-- Cross-Validation
set.seed(1115)
df_n <- nrow(shopper)
n_V_set <- floor(df_n * 0.3)         
n_T_set <- df_n - n_V_set

V_set <- sample(x=df_n, size=n_V_set, replace=FALSE)
T_set <- c(1:df_n)[-V_set]
T_set

# Principal Component

#---SVM---- Clean

library(e1071) 
library(rpart) 
set.seed(623) 
TUNE.kyposis <- tune(svm, Revenue ~., data=shop_drop[T_set,],  
                     kernel="linear",  
                     scale=FALSE,  
                     ranges=list(cost=c(0.01, 1, 10))) 

#---- tree -----
tree_Oz_devi <- tree(V4 ~ ., split="deviance", data=Ozone.TRN, na.action=na.omit)
head(tree_Oz_devi$frame)

whole_prediction_devi <- predict(tree_Oz_devi, newdata = Ozone.VLD, type="vector")

# Cross-validation
pred_tree_Oz <- predict(tree_Oz_devi, newdata=Ozone.VLD, type="vector")
no_na_tree <- na.omit(round(pred_tree_Oz)- Ozone.VLD$V4)
mean((no_na_tree)^2)

# tree plot
plot(tree_Oz_devi)
title(main="Classification Tree by deviance")
text(tree_Oz_devi, cex=0.75, pretty=0)

#--- random forest

set.seed(441)
BG_Oz <- randomForest(V4 ~ ., data=Ozone.TRN, ntree=500, importance=TRUE, na.action=na.omit)
# Cross-Validation MSE
pred_BG_Oz <- predict(BG_Oz, newdata=Ozone.VLD)
no_na_BG <- na.omit(round(pred_BG_Oz)- Ozone.VLD$V4)
mean((no_na_BG)^2)

plot(pred_BG_Oz, Ozone.VLD$V4, main="Bagging, using a test set", xlab="y-predicted", ylab="y-observed")
abline(a=0, b=1, col="blue")

varImpPlot(BG_Oz, sort=TRUE, scale=TRUE) ## Default: sort=TRUE & scale=TRUE

# sorted importance
apply(importance(BG_Oz, type=1, scale=TRUE), 2, sort, decreasing=TRUE)

apply(importance(BG_Oz, type=2, scale=TRUE), 2, sort, decreasing=TRUE)
