#Charity Data ML
#This is data webscrapped from Charitywatch.org. The data was already cleaned before being saved.
library(ggplot2)
library(caTools)
library(gsubfn)
library(rpart)
library(rpart.plot)
library(randomForest)
library(pracma)
library(partykit)
library(CHAID)


set.seed(99)
#Let's read in the data and look at the charity grades
setwd(file.path("C:","Users","soren","Desktop","charityPy"))
data <- read.csv("data.csv")
gradeCount <- with(data, table(grade))
ggplot(as.data.frame(gradeCount), aes(grade,Freq)) +     
  geom_col()

#A logistic regression would not be good here, since there are so many A's and the grades are in general heavily skewed toward the higher end.

#"top rated" does not just mean "A" and "A+" grades
unique(data[data["topRated"] == 0, "grade"])

#The "governance" factor is redundant
govTable <- data[data["governance"] == 1,c('govA', 'govB', 'govC', 'govD', 'govE', 'govF', 'govG', 'govH', 'govI')]
govTableSum <- rowSums(govTable)
govTotal9Bool <- govTableSum == 9
sum(govTotal9Bool) == dim(data[data["governance"] == 1,])[1]

#The "transparency" factor is redundant

sum(rowSums(data[data["transparency"] == 1,c('transA', 'transB')]) == 2) == dim(data[data["transparency"] == 1,])[1]

#Prepare dataset for modeling
predictorMat <- subset(data, select = -c(governance,transparency))
predictorMat$programPerc <- as.numeric(gsub("%", "", predictorMat$programPerc))
predictorMat$cost100 <- as.numeric(gsub("[$]", "", predictorMat$cost100))
predictorMat$topRated <- as.factor(predictorMat$topRated)

#PREDICT TOP RATED
#Let's first see if we can predict "top rated"

#Test,Train split
testTrainSplit <- function(data) {
  splitData <- sample.split(data, .2)
  trainData <- subset(data, splitData == FALSE)
  testData <- subset(data, splitData == TRUE)
  return (list(trainData,testData))
}

list[trainData1,testData1] <- testTrainSplit(predictorMat)

#Since these labels are created by human decisions, a decision tree should be able to do a good job at predicting

#First model
trainData1a <- trainData1
treeModel1a <- rpart(topRated ~ transA + transB + govA + govB + govC + govD + govE + govF + govG + govH + govI + privacy, trainData1a, control = rpart.control(minsplit = 1))

performance <- function(predictions, actual) {
  nWrong <- length(actual) - sum(predictions == actual)
  percRight <- sum(predictions == actual) / length(actual) * 100
  return(list(nWrong,percRight))
}

performance(predict(treeModel1a,trainData1a, "class"),trainData1a$topRated)

#We missed 2. Let's try adding "tax status".
trainData1b <- trainData1
trainData1b$c3 <- ifelse(trainData1b$taxStatus == "501(c)3",1,0) 
trainData1b$c4 <- ifelse(trainData1b$taxStatus == "501(c)4",1,0)
trainData1b$c6 <- ifelse(trainData1b$taxStatus == "501(c)6",1,0)
trainData1b$c19 <- ifelse(trainData1b$taxStatus == "501(c)19",1,0) 

treeModel1b <- rpart(topRated ~ transA + transB + govA + govB + govC + govD + govE + govF + govG + govH + govI + privacy + c3 + c4 + c6 +c19, trainData1b, control = rpart.control(minsplit = 1))
performance(predict(treeModel1b,trainData1b, "class"),trainData1b$topRated)

#We still missed 2. Tax status doesn't seem to help. Let's use "Program Percentage" (amount spent on programs relative to overhead)
trainData1c <- trainData1

treeModel1c <- rpart(topRated ~ transA + transB + govA + govB + govC + govD + govE + govF + govG + govH + govI + privacy + programPerc, trainData1c, control = rpart.control(minsplit = 1))
performance(predict(treeModel1c,trainData1c, "class"),trainData1c$topRated)

#We correctly predicted all labels. Let's look at the tree.
printcp(treeModel1c)
rpart.plot(treeModel1c)

#It looks like the only factors we really need are "programPerc","transB", and "govI". transB is probably more indicative of overall transparency, and govI is probably more indicative of overall governance.
trainData1d <- trainData1

treeModel1d <- rpart(topRated ~ transB + govI + programPerc, trainData1d, control = rpart.control(minsplit = 1))
performance(predict(treeModel1d,trainData1d, "class"),trainData1d$topRated)

#Let's now test the model
performance(predict(treeModel1d,testData1, "class"),testData1$topRated)

#It looks like our model was successfully able to label the test set

#PREDICT GRADE
#Let's now try to predict grade. Since we were able to predict "Top Rated", it is a redundant factor, so we will not be using it.
predictorMat <- subset(predictorMat, select = -c(topRated))
predictorMat$grade <- as.factor(predictorMat$grade)

#test,train split
list[trainData2,testData2] <- testTrainSplit(predictorMat)

#Let's use a similar model as before to predict grade
trainData2a <- trainData2

treeModel2a <- rpart(grade ~ transA + transB + govA + govB + govC + govD + govE + govF + govG + govH + govI + privacy + programPerc, trainData2a, control = rpart.control(minsplit = 1))
performance(predict(treeModel2a,trainData2a, "class"),trainData2a$grade)

#We failed to correctly label 38 charities. Let's add "cost100" (amount spent to raise $100 of contributions)
trainData2b <- trainData2

treeModel2b <- rpart(grade ~ transA + transB + govA + govB + govC + govD + govE + govF + govG + govH + govI + privacy + programPerc + cost100, trainData2b, control = rpart.control(minsplit = 1))
performance(predict(treeModel2b,trainData2b, "class"),trainData2b$grade)

#We were able to achieve 98% accuracy. Let's look at the tree.
printcp(treeModel2b)
rpart.plot(treeModel2b)

#It looks like the only factors we really need are "cost100, "programPerc", and "transB".
trainData2c <- trainData2

treeModel2c <- rpart(grade ~ transB + programPerc + cost100, trainData2c, control = rpart.control(minsplit = 1))
performance(predict(treeModel2c,trainData2c, "class"),trainData2c$grade)

#Let's now test the model
performance(predict(treeModel2c,testData2, "class"),testData2$grade)

#It looks like we weren't able to correctly label 6 charities. Fortunately, we actually know how these grades are chosen. The criteria can be found here (https://www.charitywatch.org/our-charity-rating-process). It seems like charity watch only uses Program Percentage and Cost to Raise 100.

#Predict Grade Using Criteria
#Test,Train split
list[trainData3,testData3] <- testTrainSplit(predictorMat)

#Let's train on only "programPerc" and "cost100".
trainData3a <- trainData3

treeModel3a <- rpart(grade ~ programPerc + cost100, trainData3a, control = rpart.control(minsplit = 1))
performance(predict(treeModel3a,trainData3a, "class"),trainData3a$grade)

#We were able to achieve 99% accuracy.
performance(predict(treeModel3a,testData3, "class"),testData3$grade)

#It looks like we only incorrectly labeled 2 charities, but we should have essentially done perfectly well. Let's look at what we got wrong.
table1 <- cbind(testData3[c("name","grade")],predict(treeModel3a,testData3, "class"),testData3[c("programPerc","cost100")])
colnames(table1) <- c("Name","Actual","Predicted","Program Percentage","Cost 100")
(table1 <- table1[table1$Actual != table1$Predicted,])

#We're not far off from predicting the grades correctly.
#This happens because decision trees can only use one factor at a time to make decisions. Let's see if training on each feature and then weighing them will solve this problem. We'll use a random forest in order to avoid overfitting a bit.
predictorMat$grade <- as.character(predictorMat$grade)
predictorMat[predictorMat["grade"] == "A+","grade"] <- 10
predictorMat[predictorMat["grade"] == "A","grade"] <- 9
predictorMat[predictorMat["grade"] == "A-","grade"] <- 8
predictorMat[predictorMat["grade"] == "B+","grade"] <- 7
predictorMat[predictorMat["grade"] == "B","grade"] <- 6
predictorMat[predictorMat["grade"] == "B-","grade"] <- 5
predictorMat[predictorMat["grade"] == "C+","grade"] <- 4
predictorMat[predictorMat["grade"] == "C","grade"] <- 3
predictorMat[predictorMat["grade"] == "C-","grade"] <- 2
predictorMat[predictorMat["grade"] == "D","grade"] <- 1
predictorMat[predictorMat["grade"] == "F","grade"] <- 0
predictorMat$grade <- as.factor(predictorMat$grade)
list[trainData4,testData4] <- testTrainSplit(predictorMat)

trainData4a <- trainData4
ppModel1 <- randomForest(grade ~ programPerc, data = trainData4a, sampsize = round(dim(trainData4a)[1] * .9))
c1Model1 <- randomForest(grade ~ cost100, data = trainData4a, sampsize = round(dim(trainData4a)[1] * .9))

#number incorrect for program percent model
performance(predict(ppModel1,trainData4a, "class"),trainData4a$grade)

#number incorrect for cost to raise 100 model
performance(predict(c1Model1,trainData4a, "class"),trainData4a$grade)

#Turn the predictions into a matrix, find the pseudoinverse and multiply by the actual labels in order to find the weights.
predMat1 <- cbind(as.numeric(as.character(predict(ppModel1,trainData4a, "class"))),as.numeric(as.character(predict(c1Model1,trainData4a, "class"))))
weights1 <- dot(as.numeric(as.character(trainData4a$grade)),t(pinv(predMat1)))
predicted1 <- round(dot(weights1,t(predMat1)))
performance(predicted1,trainData4a$grade)

#It looks like the simpler model does a much better job. However, our more complex model isn't far off from the correct labels.
table2 <- cbind(trainData4a[c("name","grade")],predicted1,trainData4a[c("programPerc","cost100")])
colnames(table2) <- c("Name","Actual","Predicted","Program Percentage","Cost 100")
(table2 <- table2[table2$Actual != table2$Predicted,])

#Although we didn't perform so well on the training data, let's see how we perform on the test data.
predMatTest1 <- cbind(as.numeric(as.character(predict(ppModel1,testData4, "class"))),as.numeric(as.character(predict(c1Model1,testData4, "class"))))
predictedTest1 <- round(dot(weights1,t(predMatTest1)))
performance(predictedTest1,testData4$grade)

#Let's look at where we went wrong.
table3 <- cbind(testData4[c("name","grade")],predictedTest1,testData4[c("programPerc","cost100")])
colnames(table3) <- c("Name","Actual","Predicted","Program Percentage","Cost 100")
(table3 <- table3[table3$Actual != table3$Predicted,])

#The flaw here is that this more complex model actually does not simulate human decision making. This is because we've found the weights using the entire matrix, but factors only need to be weighted when the labels disagree. Let's see if this insight improves our model.
predMat2 <- cbind(as.numeric(as.character(predict(ppModel1,trainData4a, "class"))),as.numeric(as.character(predict(c1Model1,trainData4a, "class"))))
disagreeMat1 <- predMat2[predMat2[,1] != predMat2[,2],]
weights2 <- dot(as.numeric(as.character(trainData4a[predMat2[,1] != predMat2[,2],"grade"])),t(pinv(disagreeMat1)))
predMat2[predMat2[,1] != predMat2[,2],] <- round(dot(weights2,t(disagreeMat1)))
predicted2 <- predMat2[,1]
performance(predicted2,trainData4a$grade)

#We've improved model performance. Let's see what we got wrong.
table4 <- cbind(trainData4a[c("name","grade")],predicted2,trainData4a[c("programPerc","cost100")])
colnames(table4) <- c("Name","Actual","Predicted","Program Percentage","Cost 100")
(table4 <- table4[table4$Actual != table4$Predicted,])

#Let's try our test data.
predMatTest2 <- cbind(as.numeric(as.character(predict(ppModel1,testData4, "class"))),as.numeric(as.character(predict(c1Model1,testData4, "class"))))
disagreeMatTest1 <- predMatTest2[predMatTest2[,1] != predMatTest2[,2],]
predMatTest2[predMatTest2[,1] != predMatTest2[,2],] <- round(dot(weights2,t(disagreeMatTest1)))
predictedTest2 <- predMatTest2[,1]
performance(predictedTest2,testData4$grade)

#Let's look at where we went wrong.
table4 <- cbind(testData4[c("name","grade")],predictedTest2,testData4[c("programPerc","cost100")])
colnames(table4) <- c("Name","Actual","Predicted","Program Percentage","Cost 100")
(table4 <- table4[table4$Actual != table4$Predicted,])

#It seems like the simpler model performed much better. Finally, since we have the actual levels of each feature used to give grades, let's see if we can predict using these levels.

#Predicting using levels of features

predictorMat$catProgramPerc <- cut(predictorMat$programPerc,c(0,35,49,55,60,64,67,71,74,79,89,100),c(0,1,2,3,4,5,6,7,8,9,10),include.lowest = TRUE,ordered_result = TRUE)
predictorMat$catCost100 <- cut(predictorMat$cost100,c(0,4,11,15,19,26,30,33,37,40,59,100),c(10,9,8,7,6,5,4,3,2,1,0),include.lowest = TRUE,ordered_result = TRUE)

list[trainData5,testData5] <- testTrainSplit(predictorMat)
trainData5a <-trainData5

#Let's try using CHAID since these are now ordered factors
chaidModel <- chaid(grade ~ catProgramPerc + catCost100, data = trainData5a)
performance(predict(chaidModel,trainData5a),trainData5a$grade)

#We incorrectly labeled 10 charities. Let's see what we got wrong.
table5 <- cbind(trainData5a[c("name","grade")],predict(chaidModel,trainData5a),trainData5a[c("programPerc","cost100")])
colnames(table5) <- c("Name","Actual","Predicted","Program Percentage","Cost 100")
(table5 <- table5[table5$Actual != table5$Predicted,])

#We grossly mislabeled "Grameen Foundation USA". We gave it an F, when it should have gotten a B+. Let's see if weights will fix this.
ppChaid <- chaid(grade ~ catProgramPerc, data = trainData5a)
performance(predict(ppChaid,trainData5a),trainData5a$grade)
plot(ppChaid,type = "simple")

c1Chaid <- chaid(grade ~ catCost100, data = trainData5a, control = chaid_control(alpha2 = .01, alpha4 = .01))
performance(predict(c1Chaid,trainData5a),trainData5a$grade)
plot(c1Chaid,type = "simple")

predMat3 <- cbind(as.numeric(as.character(predict(ppChaid,trainData5a))),as.numeric(as.character(predict(c1Chaid,trainData5a))))
disagreeMat2 <- predMat3[predMat3[,1] != predMat3[,2],]
weights3 <- dot(as.numeric(as.character(trainData5a[predMat3[,1] != predMat3[,2],"grade"])),t(pinv(disagreeMat2)))
predMat3[predMat3[,1] != predMat3[,2],] <- round(dot(weights3,t(disagreeMat2)))
predicted3 <- predMat3[,1]

performance(predicted3,trainData5a$grade)
performance(predicted2,trainData4a$grade)

#It seems that the CHAID model did not improve performance. Let's look at what we got wrong.
table6 <- cbind(trainData5a[c("name","grade")],predicted3,trainData5a[c("programPerc","cost100")])
colnames(table6) <- c("Name","Actual","Predicted","Program Percentage","Cost 100")
(table6 <- table6[table6$Actual != table6$Predicted,])

#We performed better on "Grameen Foundation USA", but we performed worse overall. It seems that, once again, the simplest model was the best model.


#summary(treeModel1a)
#print(treeModel1a)
#rpart.plot(treeModel1a)
#printcp(treeModel1a)









        