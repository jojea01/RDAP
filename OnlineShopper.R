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

# target bar graph
shopper_rev <- sum(shopper_clean[shopper_clean$Revenue == 1,]$Revenue)
shopper_norev <- sum(shopper_clean[!shopper_clean$Revenue == 1,]$Revenue)

shopper_rev <- c(shopper_rev)
shopper_norev <- c(shopper_norev)

counts_matrix <- matrix(c(shopper_rev, shopper_norev), 
                        nrow = 2, 
                        byrow = TRUE)

rownames(counts_matrix) <- c("Revenue", "No Revenue")
colnames(counts_matrix) <- c("Count")

barplot(counts_matrix,
        main = "Shopper Revenue Status",
        ylab = "Count",
        col = c("blue", "red"),
        beside = TRUE)
legend("topleft",
       legend = c("Revenue", "No Revenue"),
       fill = c("blue", "red"))

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
shop_drop <- shopper_clean[, !(names(shopper_clean) %in% c("ProductRelated_Duration", "Administrative_Duration", "Informational_Duration","BounceRates"))]

shop_drop
########## I dropped theses varraibles becasue either they were correlated with other varraibles or had a relation of less than 5% to the target######

#-- anova test

shop_fit <- lm(Revenue ~ ., data=shopper_clean)
summary(shop_fit)
anova(shop_fit)

#-- Droping variables from anova ----------

shop_drop <- shopper_clean[, !(names(shopper_clean) %in% c("Browser", "Region", "TrafficType", "Weekend"))]

#-- Cross-Validation
set.seed(1115)

df_n <- nrow(shop_drop)
n_V_set_30 <- floor(df_n * 0.3)

# Sample row indices for validation set
val_indices_30 <- sample(x = df_n, size = n_V_set_30, replace = FALSE)

V_set_30 <- shop_drop[val_indices_30, ]
T_set_30 <- shop_drop[-val_indices_30, ]


# Principal Component

pca_result <- prcomp(x=shop_drop, scale. = TRUE) 
biplot(pca_result, scale = 0, main = "PCA Biplot")


pca_result$x
#---SVM---- Clean

library(e1071) 
library(rpart) 
set.seed(623) 
TUNE.kyposis <- tune(svm, Revenue ~., data=shop_drop[T_set,],  
                     kernel="linear",  
                     scale=FALSE,  
                     ranges=list(cost=c(0.01, 1, 10))) 

#-- Partitioning
set.seed(1115)

df_n_30 <- nrow(shop_drop)
n_V_set_30 <- floor(df_n_30 * 0.3)

# Sample row indices for validation set
val_indices_30 <- sample(x = df_n_30, size = n_V_set_30, replace = FALSE)

V_set_30 <- shop_drop[val_indices_30, ]
T_set_30 <- shop_drop[-val_indices_30, ]

#---- tree -----
library(tree)

tree_devi <- tree(Revenue ~ ., split="deviance", data=T_set_30, na.action=na.omit)
head(tree_devi$frame)

whole_prediction_devi <- predict(tree_devi, newdata = T_set_30, type="vector")

# Cross-validation
pred_tree_shop_30 <- predict(tree_devi, newdata=V_set_30, type="vector")
no_na_tree_30 <- na.omit(round(pred_tree_shop)- V_set_30$Revenue)
mse_30_tree <- mean((no_na_tree_30)^2)

# tree plot
plot(tree_devi)
title(main="Classification Tree of Shopper by deviance")
text(tree_devi, cex=0.75, pretty=0)

# stability test
set.seed(1115)

df_n_20 <- nrow(shop_drop)
n_V_set_20 <- floor(df_n_20 * 0.20)
n_T_set_20 <- df_n_20 - n_V_set_20

val_indices_20 <- sample(x = df_n_20, size = n_V_set_20, replace = FALSE)

V_set_20 <- shop_drop[val_indices_20, ]
T_set_20 <- shop_drop[-val_indices_20, ]

pred_tree_shop_20 <- predict(tree_devi, newdata=V_set_20, type="vector")
no_na_tree_20 <- na.omit(round(pred_tree_shop_20)- V_set_20$Revenue)
mse_20_tree <- mean((no_na_tree_20)^2)

set.seed(1115)

df_n_10 <- nrow(shop_drop)
n_V_set_10 <- floor(df_n_10 * 0.3)

# Sample row indices for validation set
val_indices_10 <- sample(x = df_n_10, size = n_V_set_10, replace = FALSE)

V_set_10 <- shop_drop[val_indices_10, ]
T_set_10 <- shop_drop[-val_indices_10, ]

pred_tree_shop_10 <- predict(tree_devi, newdata=V_set_10, type="vector")
no_na_tree_10 <- na.omit(round(pred_tree_shop_10)- V_set_10$Revenue)
mse_10_tree <- mean((no_na_tree_10)^2)

mse_10_tree
mse_20_tree
mse_30_tree


#--- random forest
library(randomForest)

set.seed(1115)
BG_shop <- randomForest(Revenue ~ ., data=T_set, ntree=500, importance=TRUE, na.action=na.omit)

# Cross-Validation MSE
pred_BG_shop_30_RF <- predict(BG_shop, newdata=V_set_30)
no_na_BG_30_RF <- na.omit(round(pred_BG_shop_30_RF) - V_set_30$Revenue)
mse_30_RF <- mean((no_na_BG_30_RF)^2)

plot(pred_BG_shop, V_set$Revenue, main="Bagging, using a validation set", xlab="y-predicted", ylab="y-observed")
  
varImpPlot(BG_shop, sort=TRUE, scale=TRUE)

# sorted importance
apply(importance(BG_shop, type=1, scale=TRUE), 2, sort, decreasing=TRUE)

apply(importance(BG_shop, type=2, scale=TRUE), 2, sort, decreasing=TRUE)

# stability test

pred_BG_shop_20_RF <- predict(BG_shop, newdata=V_set_20)
no_na_BG_20_RF <- na.omit(round(pred_BG_shop_20_RF) - V_set_20$Revenue)
mse_20_RF <- mean((no_na_BG_20_RF)^2)

pred_BG_shop_10_RF <- predict(BG_shop, newdata=V_set_10)
no_na_BG_10_RF <- na.omit(round(pred_BG_shop_10_RF) - V_set_10$Revenue)
mse_10_RF <- mean((no_na_BG_10_RF)^2)

mse_10_RF
mse_20_RF
mse_30_RF


#----K-Means-----


shop_scaled <- scale(shop_drop[T_set,]) 
WSS <- NULL 
PVE <- NULL 

for (k in 1:10) { 
  KMC.tmp <- kmeans(shop_scaled, centers = k, nstart = 25) 
  WSS <- c(WSS, KMC.tmp$tot.withinss) 
  PVE <- c(PVE, KMC.tmp$betweenss / KMC.tmp$totss) 
} 

plot(PVE, ylim=c(0, 1),  
     col="blue", pch=16, cex=1.5, type="b", lwd=1.5, 
     xlab="Number of clusters", ylab="Percent variability explained", 
     main="Coleman Data", 
     col.axis="blue", col.lab="blue") 
points(WSS / max(WSS), col="red", pch=16, cex=1.5, type="b", lwd=1.5) 
axis(side=4, at = c(0, 0.25, 0.5, 0.75, 1), 
     labels = round(seq(0, max(WSS), length.out = 5)), 
     col.axis="red", col.ticks="red") 
mtext(side=4, text="Within-cluster sum of squares", line=3, col="red") 
legend("topright", legend=c("PVE", "WSS (scaled)"), 
       col=c("blue", "red"), pch=16, lty=1) 

set.seed(7673) 
KMC <- kmeans(scale(coleman), centers=8) 
KMC

