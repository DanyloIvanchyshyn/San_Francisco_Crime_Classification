library(caret)
library(class)
library(dplyr)
library(psych)
library(e1071)
train <- read.csv("D:/train.csv", header = TRUE)
train <- train[1:3996,]
test3 <- train

test3$Dates <- strptime(test3$Dates, "%Y-%m-%d %H:%M:%S")
test3$Dates <- as.POSIXct(test3$Dates, format="%Y-%m-%d %H:%M:%S")
test3$Month <- factor(format(test3$Dates, "%m"))
test3$Year <- factor(format(test3$Dates, "%Y"))
test3$Day <- factor(format(test3$Dates,"%d"))
test3$Hour <- factor(format(test3$Dates,"%H"))

head(test3)
str(test3)
test3 <- test3[,-c(1, 3, 6, 7, 11)]
test3$Month <- ifelse(test3$Month == "05", 1, 0)
test4 <- test3

str(test4)
head(test4)
test4$Hour <- as.data.frame(dummy.code(test4$Hour))
test4$DayOfWeek <- as.data.frame(dummy.code(test4$DayOfWeek))
test4$PdDistrict <- as.data.frame(dummy.code(test4$PdDistrict))
test4$Day <- as.data.frame(dummy.code(test4$Day))

ran <- sample(1:nrow(test4), 0.75 * nrow(test4))
nor <- function(x) { (x -min(x))/(max(x)-min(x))   }
data_norm <- as.data.frame(lapply(test4[, c(2:8)], nor))
summary(data_norm)

rem <- nearZeroVar(data_norm)
data_norm <- data_norm[,-rem]
data_train <- data_norm[ran,]
data_test <- data_norm[-ran,]
data_target_category <- test4[ran,1]
data_test_category <- test4[-ran,1]

set.seed(1234)
library(caret)
library(lattice)

knn_method <- knn(train = data_train, test = data_test, cl = data_target_category, k=20)

knn_outcome <- data.frame(data_test_category)
class_comparison <- data.frame(knn_method, knn_outcome)
names(class_comparison) <- c("Predicted", "Observed")
head(class_comparison)


knn_pred_class <- train(data_train, data_target_category, method = "knn", 
                        preProcess = c("center","scale"))
knn_pred_class
plot(knn_pred_class)
prediction <- predict(knn_pred_class, newdata = data_test)

confusionMatrix(prediction, data_test_category)

pred <- knn(data_train,data_test,cl=data_target_category,k=23)
conf_matr <- table(pred,data_test_category)
ACC <- 100 * sum(data_test_category == pred)/NROW(data_test_category)
cat("Accuracy: ", ACC)
