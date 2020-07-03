library(randomForest)
library(MASS)
library(caret)

###Use the set.seed function so that we get some result each time
set.seed(123)
companydata <- read.csv(file.choose())
View(companydata)
summary(companydata)
hist(companydata$Sales, main = "sales of companydata", xlim = c(0,20),
     breaks=c(seq(10,20,30)), col=c("blue","red","green","yellow"))
mean(companydata$Sales)

highsales = ifelse(companydata$Sales<8, "No","YES")
company = data.frame(companydata[2:11],highsales)
str(company)
table(company$highsales)


###################DATA PARTITION###############
set.seed(123)
ind <- sample(2, nrow(company), replace = TRUE, prob = c(0.8,0.2))
###TRAIN AND TEST
train <- company[ind==1,]
test <- company [ind==2,]
set.seed(213)
##RANDOMfOREST
highsales
random <- randomForest(as.factor(highsales)~., data = train, ntree = 300)
random
attributes(random)
# confusion matrix 
#TRAIN
pred <- predict(random, train)
head(pred)
head(train$highsales)
confusionMatrix(pred, as.factor(train$highsales))
#TEST
pred1 <- predict(random, test)
confusionMatrix(pred1, test$highsales)
#ERROR RATE IN RANDOM FOREST MODEL
plot(random)

#####ERROR RATE IN RANDOM FOREST

plot(random)
legend("topright",colnames(random$err.rate),
       col=1:3,cex=0.8,fill=1:3)
acc_wbcd <- mean(company$highsales==predict(random))
acc_wbcd


varImpPlot(random)
varImpPlot(random ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
partialPlot(random,train, Price, "YES")
