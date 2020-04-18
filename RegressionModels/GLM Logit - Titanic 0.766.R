library(tidyverse)
library(reshape2)
library(gridExtra)
library(gmodels)
library(ROCR)

train <- read.csv("~/R/Kaggle/Titantic/train.csv", na.string=c(""))

t2 <- subset(train,select=c(2,3,5,6,7,8,10,12))
t2$Age[is.na(t2$Age)] <- mean(t2$Age,na.rm=T)

#2 embarked with NA, need to impute, will remove for now
t2 <- t2[!is.na(t2$Embarked),]
rownames(t2) <- NULL

# t3 <- data[1:800,]
# test1 <- data[801:889,]

model <- glm(Survived ~.,family=binomial(link='logit'),data=t2)
summary(model)

#test prediction
test <- read.csv("~/R/Kaggle/Titantic/test.csv", na.string=c(""))
sapply(test,function(x) sum(is.na(x)))
test$Age[is.na(test$Age)] <- mean(test$Age,na.rm=T)
test$Fare[is.na(test$Fare)] = mean(test$Fare, na.rm=T)

fitted.results <- predict(model,newdata=subset(test,select=c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarked")),type='response')
fitted.results <- ifelse(fitted.results > ,0.51,0)


misClasificError <- mean(fitted.results != test$Survived, na.rm=TRUE)
print(paste('Accuracy',1-misClasificError))

p <- predict(model, newdata=subset(test1,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test1$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


#submit prediction
test <- read.csv("~/R/Kaggle/Titantic/test.csv", na.string=c(""))
sapply(test,function(x) sum(is.na(x)))
test$Age[is.na(test$Age)] <- mean(test$Age,na.rm=T)
test$Fare[is.na(test$Fare)] = mean(test$Fare, na.rm=T)

fitted.results <- predict(model,newdata=subset(test,select=c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarked")),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)


subtitanic = as.data.frame(cbind(test$PassengerId, fitted.results))
subtitanic = setNames(submit, c("PassengerId", "Survived"))
write.csv(subtitanic, file="submit.csv", row.names = FALSE)



# CrossTable(train$Survived, train$Pclass)
# fisher.test(train$Survived, train$Pclass)
# t2 = melt(train, "Survived")
# p1 <- ggplot(t2, aes(value, Survived)) +  geom_point() + facet_wrap(~variable)
# p2 <- ggplot(train, aes(x = Survived)) + geom_histogram()
# grid.arrange(p1, p2, ncol=2)
