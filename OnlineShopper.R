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

# summary statistics
summary(shopper_clean)

#----- bar graph target------
shopper_rev <- sum(shopper_clean$Revenue == 1, na.rm = TRUE)
shopper_rev
shopper_norev <- sum(shopper_clean$Revenue == 2, na.rm = TRUE)
shopper_norev

counts_matrix <- matrix(c(shopper_rev, shopper_norev), 
                        nrow = 2, 
                        byrow = TRUE)

rownames(counts_matrix) <- c("Revenue", "No Revenue")
colnames(counts_matrix) <- c("Revenue Status")

barplot(counts_matrix,
        main = "Shopper Revenue Status",
        ylab = "Count",
        col = c("blue", "red"),
        beside = TRUE)
legend("topleft",
       legend = c("Revenue", "No Revenue"),
       fill = c("blue", "red"),
       xpd = TRUE,
       bty = "n")

print(counts_matrix)

# correlation plot
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

#-- Dropping variables from anova ----------

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

#-- Partitioning2.0
set.seed(1115)

df_n_30 <- nrow(shop_drop)
n_V_set_30 <- floor(df_n_30 * 0.30)

# Sample row indices for validation set
val_indices_30 <- sample(x = df_n_30, size = n_V_set_30, replace = FALSE)

V_set_30 <- shop_drop[val_indices_30, ]
T_set_30 <- shop_drop[-val_indices_30, ]

#---- tree -----
library(tree)

tree_devi <- tree(Revenue ~ ., split="deviance", data=T_set_30, na.action=na.omit)
tree_devi$frame
summary(tree_devi)

# Cross-validation
pred_tree_shop_30 <- predict(tree_devi, newdata=V_set_30, type="vector")
no_na_tree_30 <- na.omit(round(pred_tree_shop_30) - V_set_30$Revenue)
mse_30_tree <- mean((no_na_tree_30)^2)

#--- r2
mean_rev <- mean(V_set_30$Revenue)
tss <- sum((V_set_30$Revenue - mean_rev)^2)
rss <- sum((V_set_30$Revenue - pred_tree_shop_30)^2)
# Explained Variance
r2_tree <- 1 - (rss / tss)
r2_tree

# confusion matrix
conf_mat_tree <- table(Predicted = round(pred_tree_shop_30), Actual = V_set_30$Revenue)
print(conf_mat_tree)

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
n_V_set_10 <- floor(df_n_10 * 0.10)

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
RF_shop <- randomForest(Revenue ~ ., data=T_set_30, ntree=500, importance=TRUE, na.action=na.omit)
RF_shop

# Cross-Validation MSE
pred_shop_30_RF <- predict(RF_shop, newdata=V_set_30)
no_na_30_RF <- na.omit(round(pred_shop_30_RF) - V_set_30$Revenue)
mse_30_RF <- mean((no_na_30_RF)^2)

# -- r2

# Total sum of squares
tss <- sum((V_set_30$Revenue - mean_rev)^2)
rss_rf <- sum(na.omit(V_set_30$Revenue - pred_shop_30_RF)^2)
# explained variance
r2_rf <- 1 - (rss_rf / tss)
r2_rf

# confusion matrix
conf_mat_RF <- table(Predicted = round(pred_shop_30_RF), Actual = V_set_30$Revenue)
print(conf_mat_RF)

plot(pred_shop_30_RF, V_set_30$Revenue, main="Random Forest, using a validation set", xlab="y-predicted", ylab="y-observed")
  
varImpPlot(RF_shop, sort=TRUE, scale=TRUE)

# sorted importance
apply(importance(RF_shop, type=1, scale=TRUE), 2, sort, decreasing=TRUE)

apply(importance(RF_shop, type=2, scale=TRUE), 2, sort, decreasing=TRUE)

# Plot random forest error rates across trees
plot(RF_shop)

# stability test

pred_shop_20_RF <- predict(RF_shop, newdata=V_set_20)
no_na_20_RF <- na.omit(round(pred_BG_shop_20_RF) - V_set_20$Revenue)
mse_20_RF <- mean((no_na_BG_20_RF)^2)

pred_shop_10_RF <- predict(RF_shop, newdata=V_set_10)
no_na_10_RF <- na.omit(round(pred_shop_10_RF) - V_set_10$Revenue)
mse_10_RF <- mean((no_na_10_RF)^2)

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



#----- Cole



shop_drop <- shopper_clean[, !(names(shopper_clean) %in% c("ProductRelated_Duration", "Administrative_Duration", "Informational_Duration","BounceRates"))]
shop_drop


#---SVM---- Clean

drop 
library(e1071) 
library(rpart)
#--Crossvalidation

set.seed(1115)
df_n <- nrow(shopper)
n_V_set <- floor(df_n * 0.3)         
n_T_set <- df_n - n_V_set

V_set <- sample(x=df_n, size=n_V_set, replace=FALSE)
T_set <- c(1:df_n)[-V_set]

shop_drop <- shop_drop[, !(names(shop_drop) %in% c("Weekend", "Browser","TrafficType", "Reigon"))]

#----K-Means-----

shop_drop <- shop_drop[complete.cases(shop_drop), ]
sum(is.na(shop_drop))  

shop_scaled <- scale(shop_drop[T_set,]) 

WSS <- NULL 
PVE <- NULL 

for (k in 1:10) { 
  KMC.tmp <- kmeans(scale(shop_drop), centers = k, nstart = 25) 
  WSS <- c(WSS, KMC.tmp$tot.withinss) 
  PVE <- c(PVE, KMC.tmp$betweenss / KMC.tmp$totss) 
} 

dev.new(height=800, width=800)

plot(PVE, ylim=c(0, 1),  
     col="blue", pch=16, cex=1.5, type="b", lwd=1.5, 
     xlab="Number of clusters", ylab="Percent variability explained", 
     col.axis="blue", col.lab="blue") 
points(WSS / max(WSS), col="red", pch=16, cex=1.5, type="b", lwd=1.5) 
axis(side=4, at = c(0, 0.25, 0.5, 0.75, 1), 
     labels = round(seq(0, max(WSS), length.out = 5)), 
     col.axis="red", col.ticks="red") 

mtext(side=4, text="Within-cluster sum of squares", line=3, col="red") 
legend("topright", legend=c("PVE", "WSS (scaled)"), 
       col=c("blue", "red"), pch=16, lty=1) 

set.seed(7673) 
KMC <- kmeans(scale(shop_drop), centers=8) 
KMC
table(KMC$cluster)


pairs(scale(shop_drop), pch=16, cex=0.8, gap=0, xaxt="n", yaxt="n",
      col=rainbow(8)[KMC$cluster])

#--PCA--

pca_result <- prcomp(x=shop_drop, scale. = TRUE) 
biplot(pca_result, scale = 0, main = "PCA Biplot")

pca_result$x

summary(pca_result)

pca_result$rotation


