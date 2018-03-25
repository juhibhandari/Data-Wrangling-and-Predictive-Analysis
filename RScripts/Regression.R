
#3.Regression
#loading the TourneyCompactResults dataset
TourneyCompactResults <- read_csv("TourneyCompactResults.csv")
View(TourneyCompactResults)
regression_ds = TourneyCompactResults
winning_tourney = regression_ds[, c(1,3,4)]
View(winning_tourney)
losing_tourney = regression_ds[, c(1,5,6)]
View(losing_tourney)
summary(winning_tourney)
summary(losing_tourney)

#filtering the winningteam data; getting the data for season > 2003
wt = winning_tourney[ which(winning_tourney$Season>= 2003),]

#filtering the losingteam data; getting the data for season > 2003
lt = losing_tourney[ which(losing_tourney$Season>= 2003),]
View(wt)
View(lt)

#renaming the columns of winning team
colnames(wt) = c("Season", "Team", "Score")

#renaming the columns of losing team
colnames(lt) = c("Season", "Team", "Score")

#merging both winningteam and losingteam data 
merged = rbind(wt,lt)
View(merged)

#merging the above dataset generated from TourneyCompactResults dataset
#and the dataset containing average statistics generated in Data Wrangling part
total = merge(merged, myDataSet_avg, by= c("Season","Team"))
total_1 = total[, c(-4)]

newdata = total_1[,3:16]
View(newdata)
dim(newdata)
train = newdata[1:1462,]
y_test = newdata[1463:1828,1]
test = newdata[1463:1828,2:14]


#creating the model based on all the statistics in the newly generated dataset total_1
#building and predicting using data split method
model = lm(Score ~ . , data = train)
sm = summary(model)
sm

# MSE for lm model
mselm <- mean(sm$residuals^2)
mselm
#Residual standard error: 11.01 on 1814 degrees of freedom
#Multiple R-squared:  0.1616,	Adjusted R-squared:  0.1556 
#F-statistic:  26.9 on 13 and 1814 DF,  p-value: < 2.2e-16
abline(model)
anova(model)
#new will loop on all the values of statistics so that it can be used to predict score for all the values of Season and Team

predicted = predict(model, test)
write.csv(predicted, file="predicted.csv")


#building and predicting lm model using cross validation
train_control <- trainControl(method="cv", number=10)
cvmodel <- train(Score~., data=newdata, trControl=train_control, method="lm")
summary(cvmodel)

predictedcv = predict(model, newdata)
write.csv(predictedcv, file="predictedcv.csv")

predictedDataSet <- read.csv("predictedcv.csv")
roundpredictedDataSet = lapply(predictedDataSet,as.integer)

finalpredictedDataSet = cbind(total_1,roundpredictedDataSet)[,-17]
View(finalpredictedDataSet)
write.csv(finalpredictedDataSet, file="finalpredictedDataSet.csv")


#building and comparing mse of glm model
glm.fit = glm(Score ~ ., data=newdata)
cv.out = cv.glm(newdata, glm.fit, K=10)
cv.out$delta[1]
summary(newdata$Score)

predictedglm = predict(glm.fit, newdata)
write.csv(predictedglm, file="predictedglm.csv")

predictedglmDataSet <- read.csv("predictedglm.csv")
roundpredictedglmDataSet = lapply(predictedglmDataSet,as.integer)

finalpredictedglmDataSet = cbind(total_1,roundpredictedglmDataSet)[,-17]
View(finalpredictedglmDataSet)
write.csv(finalpredictedglmDataSet, file="finalpredictedglmDataSet.csv")

#GLMNetModel
x=model.matrix(Score~.,newdata)[,-1]
y=newdata$Score

#Ridge
cv.out=cv.glmnet(x,y,alpha=0)
cv.out$lambda.min
cv.out$cvm
min(cv.out$cvm)

#Lasso
cv.out=cv.glmnet(x,y,alpha=1)
min(cv.out$cvm)


# building pcr model and comparing mse
pcr.fit = pcr(Score ~ ., data=newdata, scale=TRUE, validation="CV")
pcr_pred = predict(pcr.fit,test,ncomp=2)
View(pcr_pred)
mean((pcr_pred-y_test)^2)

# The option 'validation' allows us to view CV results on the
# different numbers of components
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)
