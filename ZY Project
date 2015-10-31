setwd("C:/Work/Projects/20150408ZY/rf")
data <- read.csv("randomForest2.csv")
library(randomForest)
set.seed(612)
logic <- runif(50)<0.9
data.train <- data2[logic,]
data.test <- data2[!logic,]
rf <- randomForest(as.factor(Cperform2)~Dominance+Decisiveness+Drive+Confidence+Resilience+Caring+Independence+Quality+Adaptability+Conformity+Response.Style,data=data.train,importance=T,mtry=3,ntree=2000)
rf <- randomForest(AOP~Dominance+Decisiveness+Drive+Confidence+Resilience
                   +Caring+Independence+Quality+Adaptability+Conformity
                   +Response.Style+Degree1+Marketing.Experience
                   +Training.Experience+Fortune500,data=data.train,
                   importance=T,mtry=4,ntree=2000)
importance(rf)[order(rf$importance[,2],decreasing=T),]
predict(rf,data.test)
data.test$Cperform2

set.seed(612)
logic <- runif(50)<0.9
data.train <- data[logic,]
data.test <- data[!logic,]
rf <- randomForest(as.factor(Status)~Dominance+Decisiveness+Drive+Confidence
                   +Resilience+Caring+Independence+Quality+Adaptability
                   +Conformity+Response.Style+Age+Gender+Marital.Status+Degree1
                   ,data=data,importance=T,
                   mtry=3,ntree=2000)
rf
predict(rf,data.test)
prop.table(table(c(predict(rf,data.test))-c(data.test$Status)))

#rpart
logic <- runif(50)<0.9
data.train <- data[logic,]
data.test <- data[!logic,]
partfit <- rpart(Status~Dominance+Decisiveness+Drive+Confidence+Resilience
                 +Caring+Independence+Quality+Adaptability+Conformity
                 +Response.Style,data=data.train,method="class",
                 control=rpart.control(minsplit=2,cp=0.01))
prop.table(table(c(predict(partfit,data.test,type="class"))-c(data.test$Status)))
