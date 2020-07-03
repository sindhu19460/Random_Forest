library(party)
library(randomForest)
library(caret)
library(C50)
library(tree)
library(gmodels)
library(knitr)
#library(png)
Fraudcheck <- read.csv(file.choose())
View(Fraudcheck)
summary(Fraudcheck)
hist(Fraudcheck$Taxable.Income,col=c("blue","red","green","yellow"))
Risky_Good = ifelse(Fraudcheck$Taxable.Income <= 30000, "Risky", "Good")
#Risky_Good
FC <- data.frame(Fraudcheck,Risky_Good)
str(FC)
table(FC$Risky_Good)


##DATA PARTITION

set.seed(123)
ind <- sample(2,nrow(FC),replace = TRUE, prob = c(0.7,0.3))
train <- FC[ind == 1,]
test <- FC[ind ==2,]
set.seed(213)
rf <- randomForest(Risky_Good~., data = train)
rf
attributes(rf)

###TRAIN
pred <- predict(rf,train)
head(pred)
head(train$Risky_Good)
confusionMatrix(pred, train$Risky_Good)  # 100% accuracy on training data

####TEST

pred_test <- predict(rf,test)
head(pred_test)
head(test$Risky_Good)
confusionMatrix(pred_test,test$Risky_Good)  # 100% accurancy on test data

plot(rf)
hist(treesize(rf), main = "No of nodes", col = "yellow")
