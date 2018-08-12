install.packages("rpart")
install.packages("rpart.plot")

library(readxl)
library(Hmisc)
library(rpart)
library(rpart.plot)
library(plyr)

setwd("C:/Users/hitpr/Desktop/MSBA/1st semester/Business Analytics/Homework")

getwd()

organics <- read.csv("organics.csv", header=TRUE)

set.seed(42)

#Examine the data
str(organics)
table(organics$TargetBuy)
head(organics)

row.names(organics)<-organics[,1]
organics<-organics[,-c(1,4,13)]
head(organics)




nrow(organics[is.na(organics$DemAge),])
nrow(organics[is.na(organics$DemAffl),])
nrow(organics[organics$DemClusterGroup=="",])
nrow(organics[organics$DemGender=="",])
nrow(organics[organics$DemReg=="",])
nrow(organics[organics$DemTVReg=="",])
nrow(organics[organics$PromClass=="",])
nrow(organics[is.na(organics$PromSpend),])
nrow(organics[is.na(organics$PromTime),])

#Other than PromClass and PromSpend all other variables contain null values
#We need to impute values for all continuous variables using regression

OrganicsNotNull<-organics[!(row.names(organics) %in% row.names(organics[c(is.na(organics$DemAge)|is.na(organics$DemAffl)|is.na(organics$PromTime)|organics$DemClusterGroup==""|organics$DemGender==""|organics$DemReg==""|organics$DemTVReg==""),])),]
OrganicsAgeNull<-organics[is.na(organics$DemAge),]
dim(OrganicsNotNull)
dim(OrganicsAgeNull)

head(OrganicsNotNull)

#Changing categorical variables containing characters to numeric variables to run regression

OrganicsNotNull$PromClass <- as.numeric(factor(OrganicsNotNull$PromClass , levels=c("Tin" ,
"Silver", "Gold","Platinum")))

OrganicsNotNull$DemTVReg <- as.numeric(factor(OrganicsNotNull$DemTVReg , levels=c("Wales & West",	"Midlands",	"N West",	"East",	"N East",	
                                                                                    "S & S East",	"London",	"S West",	
                                                                                    "Yorkshire",	"Border",	"C Scotland",	"N Scot")))

OrganicsNotNull$DemReg <- as.numeric(factor(OrganicsNotNull$DemReg , levels=c("Midlands",	"North",	"South East",	"South West",	"Scottish")))

OrganicsNotNull$DemGender <- as.numeric(factor(OrganicsNotNull$DemGender , levels=c("M","F","U")))

OrganicsNotNull$DemClusterGroup <- as.numeric(factor(OrganicsNotNull$DemClusterGroup , levels=c("A",	"B",	"C",	"D",	"E",	"F",	"U")))

OrganicsDemAge <- lm(DemAge ~., data=OrganicsNotNull)
summary(OrganicsDemAge)

OrganicsDemAge <- lm(DemAge ~DemAffl+DemClusterGroup+DemTVReg+PromClass+PromTime+TargetBuy, data=OrganicsNotNull)
summary(OrganicsDemAge)

nrow(OrganicsAgeNull[is.na(OrganicsAgeNull$DemAffl),])
nrow(OrganicsAgeNull[OrganicsAgeNull$DemClusterGroup=="",])
nrow(OrganicsAgeNull[OrganicsAgeNull$DemTVReg=="",])
nrow(OrganicsAgeNull[OrganicsAgeNull$PromClass=="",])
nrow(OrganicsAgeNull[is.na(OrganicsAgeNull$PromTime),])

#The rows for which we want to predict DemAge should not contain null values for any column used in the prediction model
#The rows that have missing must be removed
#Changing categorical variables containing characters to numeric variables to run prediction
OrganicsAgeNull1<-OrganicsAgeNull[!(row.names(OrganicsAgeNull) %in% row.names(OrganicsAgeNull[c(is.na(OrganicsAgeNull$DemAffl)|is.na(OrganicsAgeNull$PromTime)|OrganicsAgeNull$DemClusterGroup==""|OrganicsAgeNull$DemTVReg==""),])),]

OrganicsAgeNull1$PromClass <- as.numeric(factor(OrganicsAgeNull1$PromClass , levels=c("Tin" ,
                                                                                    "Silver", "Gold","Platinum")))

OrganicsAgeNull1$DemTVReg <- as.numeric(factor(OrganicsAgeNull1$DemTVReg , levels=c("Wales & West",	"Midlands",	"N West",	"East",	"N East",	
                                                                                  "S & S East",	"London",	"S West",	
                                                                                  "Yorkshire",	"Border",	"C Scotland",	"N Scot")))

OrganicsAgeNull1$DemReg <- as.numeric(factor(OrganicsAgeNull1$DemReg , levels=c("Midlands",	"North",	"South East",	"South West",	"Scottish")))

OrganicsAgeNull1$DemGender <- as.numeric(factor(OrganicsAgeNull1$DemGender , levels=c("M","F","U")))

OrganicsAgeNull1$DemClusterGroup <- as.numeric(factor(OrganicsAgeNull1$DemClusterGroup , levels=c("A",	"B",	"C",	"D",	"E",	"F",	"U")))

#Predict and round the value as DemAge cannot be in decimals.
#Assign the predicted value to DemAge in organics data frame for relevant rows

organics[row.names(organics) %in% row.names(OrganicsAgeNull1), ]$DemAge<-round(predict(OrganicsDemAge,OrganicsAgeNull1),digits=0)

nrow(organics[is.na(organics$DemAge),])



############################################################################

OrganicsNotNull<-organics[!(row.names(organics) %in% row.names(organics[c(is.na(organics$DemAge)|is.na(organics$DemAffl)|is.na(organics$PromTime)|organics$DemClusterGroup==""|organics$DemGender==""|organics$DemReg==""|organics$DemTVReg==""),])),]
OrganicsAfflNull<-organics[is.na(organics$DemAffl),]
dim(OrganicsNotNull)
dim(OrganicsAfflNull)

head(OrganicsNotNull)

#Changing categorical variables containing characters to numeric variables to run regression

OrganicsNotNull$PromClass <- as.numeric(factor(OrganicsNotNull$PromClass , levels=c("Tin" ,
                                                                                    "Silver", "Gold","Platinum")))

OrganicsNotNull$DemTVReg <- as.numeric(factor(OrganicsNotNull$DemTVReg , levels=c("Wales & West",	"Midlands",	"N West",	"East",	"N East",	
                                                                                  "S & S East",	"London",	"S West",	
                                                                                  "Yorkshire",	"Border",	"C Scotland",	"N Scot")))

OrganicsNotNull$DemReg <- as.numeric(factor(OrganicsNotNull$DemReg , levels=c("Midlands",	"North",	"South East",	"South West",	"Scottish")))

OrganicsNotNull$DemGender <- as.numeric(factor(OrganicsNotNull$DemGender , levels=c("M","F","U")))

OrganicsNotNull$DemClusterGroup <- as.numeric(factor(OrganicsNotNull$DemClusterGroup , levels=c("A",	"B",	"C",	"D",	"E",	"F",	"U")))

OrganicsDemAffl <- lm(DemAffl ~., data=OrganicsNotNull)

summary(OrganicsDemAffl)

OrganicsDemAffl <- lm(DemAffl ~ DemAge+TargetBuy, data=OrganicsNotNull)

nrow(OrganicsAfflNull[is.na(OrganicsAfflNull$DemAge),])
nrow(OrganicsAfflNull[is.na(OrganicsAfflNull$TargetBuy),])

#The rows for which we want to predict DemAffl should not contain null values for any column used in the prediction model
#The rows that have missing must be removed
#Changing categorical variables containing characters to numeric variables to run prediction

OrganicsAfflNull1<-OrganicsAfflNull[!(row.names(OrganicsAfflNull) %in% row.names(OrganicsAfflNull[c(is.na(OrganicsAfflNull$DemAge)),])),]

OrganicsAfflNull1$PromClass <- as.numeric(factor(OrganicsAfflNull1$PromClass , levels=c("Tin" ,
                                                                                        "Silver", "Gold","Platinum")))

OrganicsAfflNull1$DemTVReg <- as.numeric(factor(OrganicsAfflNull1$DemTVReg , levels=c("Wales & West",	"Midlands",	"N West",	"East",	"N East",	
                                                                                      "S & S East",	"London",	"S West",	
                                                                                      "Yorkshire",	"Border",	"C Scotland",	"N Scot")))

OrganicsAfflNull1$DemReg <- as.numeric(factor(OrganicsAfflNull1$DemReg , levels=c("Midlands",	"North",	"South East",	"South West",	"Scottish")))

OrganicsAfflNull1$DemGender <- as.numeric(factor(OrganicsAfflNull1$DemGender , levels=c("M","F","U")))

OrganicsAfflNull1$DemClusterGroup <- as.numeric(factor(OrganicsAfflNull1$DemClusterGroup , levels=c("A",	"B",	"C",	"D",	"E",	"F",	"U")))

organics[row.names(organics) %in% row.names(OrganicsAfflNull1), ]$DemAffl<-round(predict(OrganicsDemAffl,OrganicsAfflNull1),digits=0)

nrow(organics[is.na(organics$DemAffl),])

#############################################################################

OrganicsNotNull<-organics[!(row.names(organics) %in% row.names(organics[c(is.na(organics$DemAge)|is.na(organics$DemAffl)|is.na(organics$PromTime)|organics$DemClusterGroup==""|organics$DemGender==""|organics$DemReg==""|organics$DemTVReg==""),])),]
OrganicsPromTimeNull<-organics[is.na(organics$PromTime),]
dim(OrganicsNotNull)
dim(OrganicsPromTimeNull)

head(OrganicsNotNull)

#Changing categorical variables containing characters to numeric variables to run regression

OrganicsNotNull$PromClass <- as.numeric(factor(OrganicsNotNull$PromClass , levels=c("Tin" ,
                                                                                    "Silver", "Gold","Platinum")))

OrganicsNotNull$DemTVReg <- as.numeric(factor(OrganicsNotNull$DemTVReg , levels=c("Wales & West",	"Midlands",	"N West",	"East",	"N East",	
                                                                                  "S & S East",	"London",	"S West",	
                                                                                  "Yorkshire",	"Border",	"C Scotland",	"N Scot")))

OrganicsNotNull$DemReg <- as.numeric(factor(OrganicsNotNull$DemReg , levels=c("Midlands",	"North",	"South East",	"South West",	"Scottish")))

OrganicsNotNull$DemGender <- as.numeric(factor(OrganicsNotNull$DemGender , levels=c("M","F","U")))

OrganicsNotNull$DemClusterGroup <- as.numeric(factor(OrganicsNotNull$DemClusterGroup , levels=c("A",	"B",	"C",	"D",	"E",	"F",	"U")))

OrganicsPromTime <- lm(PromTime ~., data=OrganicsNotNull)

summary(OrganicsPromTime)

OrganicsPromTime <- lm(PromTime ~ DemAge+DemClusterGroup+DemReg+PromClass+PromSpend, data=OrganicsNotNull)

summary(OrganicsPromTime)

nrow(OrganicsPromTimeNull[is.na(OrganicsPromTimeNull$DemAge),])
nrow(OrganicsPromTimeNull[OrganicsPromTimeNull$DemClusterGroup=="",])
nrow(OrganicsPromTimeNull[OrganicsPromTimeNull$DemReg=="",])
nrow(OrganicsPromTimeNull[OrganicsPromTimeNull$PromClass=="",])
nrow(OrganicsPromTimeNull[is.na(OrganicsPromTimeNull$PromSpend),])

#The rows for which we want to predict PromTime should not contain null values for any column used in the prediction model
#The rows that have missing must be removed
#Changing categorical variables containing characters to numeric variables to run prediction

OrganicsPromTimeNull1<-OrganicsPromTimeNull[!(row.names(OrganicsPromTimeNull) %in% row.names(OrganicsPromTimeNull[c(is.na(OrganicsPromTimeNull$DemAge), OrganicsPromTimeNull$DemClusterGroup=="", OrganicsPromTimeNull$DemReg==""),])),]

OrganicsPromTimeNull1$PromClass <- as.numeric(factor(OrganicsPromTimeNull1$PromClass , levels=c("Tin" ,
                                                                                                "Silver", "Gold","Platinum")))

OrganicsPromTimeNull1$DemTVReg <- as.numeric(factor(OrganicsPromTimeNull1$DemTVReg , levels=c("Wales & West",	"Midlands",	"N West",	"East",	"N East",	
                                                                                              "S & S East",	"London",	"S West",	
                                                                                              "Yorkshire",	"Border",	"C Scotland",	"N Scot")))

OrganicsPromTimeNull1$DemReg <- as.numeric(factor(OrganicsPromTimeNull1$DemReg , levels=c("Midlands",	"North",	"South East",	"South West",	"Scottish")))

OrganicsPromTimeNull1$DemGender <- as.numeric(factor(OrganicsPromTimeNull1$DemGender , levels=c("M","F","U")))

OrganicsPromTimeNull1$DemClusterGroup <- as.numeric(factor(OrganicsPromTimeNull1$DemClusterGroup , levels=c("A",	"B",	"C",	"D",	"E",	"F",	"U")))

organics[row.names(organics) %in% row.names(OrganicsPromTimeNull1), ]$PromTime<-round(predict(OrganicsPromTime,OrganicsPromTimeNull1),digits=0)

nrow(organics[is.na(organics$PromTime),])



##############################################################################
#Imputed values for DemAge, DemAffl, PromTime. These are the continuous variables missing maximum values.
#Since the loss of rows is minimized to an extent we can remove the remaining rows with missing values for other columns (including categorical variables) and proceed further

organics<-organics[!(row.names(organics) %in% row.names(organics[c(is.na(organics$DemAge)|is.na(organics$DemAffl)|is.na(organics$PromTime)|organics$DemClusterGroup==""|organics$DemGender==""|organics$DemReg==""|organics$DemTVReg==""),])),]

nrow(organics[c(is.na(organics$DemAge)|is.na(organics$DemAffl)|is.na(organics$PromTime)|organics$DemClusterGroup==""|organics$DemGender==""|organics$DemReg==""|organics$DemTVReg==""),])

#Randomize the data and give the probabilities of partitions to specify the partition size 
ind<- sample(2, nrow(organics),
             replace=TRUE,
             prob=c(0.5,0.5))

organicstrain<- organics[ind==1, ]
dim(organicstrain)

organicstest<- organics[ind==2, ]
dim(organicstest)

options("scipen"=100, "digits"=5)

#Proportion of indivisuals who purchased organic products
print(nrow(organics[organics$TargetBuy==1,])/nrow(organics))
#In percentage
print((nrow(organics[organics$TargetBuy==1,])*100)/nrow(organics))


#Build decision tree
organicstree <- rpart(TargetBuy ~ ., data = organicstrain, method = "class")
organicstree
summary(organicstree)

rpart.plot(organicstree)

#Confusion matrix for train data
organicstrain$pred<-predict(organicstree,organicstrain,type="class")

install.packages("caret")
install.packages("e1071")

library(caret)
library(e1071)


#############################################################################

### Test Model Performance on train data - Creates a 2X2 confusion matrix and associated metrics
testModelPerformance <- function(model, dataset, target, prediction) {
  if(missing(prediction))
  {
    print("here")
    dataset$pred <- predict(model, dataset, type = "class")
  }
  else
  {
    print("here2")
    dataset$pred <- prediction
  }}
  
  Performancetrain <- testModelPerformance(organicstree, organicstrain, organicstrain$TargetBuy)
  
  writeLines("PERFORMANCE EVALUATION FOR")
  writeLines(paste("Model:", deparse(substitute(organicstree))))
  writeLines(paste("Target:", deparse(substitute(organicstrain))))
  
  writeLines("\n\nConfusion Matrix:")
  confMatrix <- table(Actual = organicstrain$TargetBuy, Predicted = organicstrain$pred)
  truePos <- confMatrix[2,2]
  falseNeg <- confMatrix[2,1]
  falsePos <- confMatrix[1,2]
  trueNeg <- confMatrix[1,1]
  print(confMatrix)
  writeLines("\n\n")
  
  accuracy <- (truePos + trueNeg)/(truePos + falseNeg + falsePos + trueNeg)
  sensitivity <- truePos/(truePos + falseNeg)
  specificity <- trueNeg/(falsePos + trueNeg)
  falsePosRate <- falsePos/(falsePos + trueNeg)
  falseNegRate <- falseNeg/(truePos + falseNeg)
  precision <- truePos/(truePos + falsePos)
  
  writeLines(paste("Accuracy:", round(accuracy, digits = 4)))
  writeLines(paste("Sensitivity:", round(sensitivity, digits = 4)))
  writeLines(paste("Specificity:", round(specificity, digits = 4)))
  writeLines(paste("False Positive Rate:", round(falsePosRate, digits = 4)))
  writeLines(paste("False Negative Rate:", round(falseNegRate, digits = 4)))
  writeLines(paste("Precision:", round(precision, digits = 4)))
  
  ###Test model performance on test data
  
  organicstest$pred<-predict(organicstree,organicstest,type="class")
  Performancetest <- testModelPerformance(organicstree, organicstest, organicstest$TargetBuy)
  
  writeLines("PERFORMANCE EVALUATION FOR")
  writeLines(paste("Model:", deparse(substitute(organicstree))))
  writeLines(paste("Target:", deparse(substitute(organicstest))))
  
  writeLines("\n\nConfusion Matrix:")
  confMatrix <- table(Actual = organicstest$TargetBuy, Predicted = organicstest$pred)
  truePos <- confMatrix[2,2]
  falseNeg <- confMatrix[2,1]
  falsePos <- confMatrix[1,2]
  trueNeg <- confMatrix[1,1]
  print(confMatrix)
  writeLines("\n\n")
  
  accuracy <- (truePos + trueNeg)/(truePos + falseNeg + falsePos + trueNeg)
  sensitivity <- truePos/(truePos + falseNeg)
  specificity <- trueNeg/(falsePos + trueNeg)
  falsePosRate <- falsePos/(falsePos + trueNeg)
  falseNegRate <- falseNeg/(truePos + falseNeg)
  precision <- truePos/(truePos + falsePos)
  
  writeLines(paste("Accuracy:", round(accuracy, digits = 4)))
  writeLines(paste("Sensitivity:", round(sensitivity, digits = 4)))
  writeLines(paste("Specificity:", round(specificity, digits = 4)))
  writeLines(paste("False Positive Rate:", round(falsePosRate, digits = 4)))
  writeLines(paste("False Negative Rate:", round(falseNegRate, digits = 4)))
  writeLines(paste("Precision:", round(precision, digits = 4)))
  
  
  #Compare performance on testing and training data
  
  organicstrain$correct <- organicstrain$TargetBuy == organicstrain$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
  traincorrectcount <- length(which(organicstrain$correct))
  trainincorrectcount <- nrow(organicstrain) - traincorrectcount
  trainerrorrate <- trainincorrectcount/nrow(organicstrain)
  trainaccuracy <- 1-trainerrorrate
  
  organicstest$correct <- organicstest$TargetBuy == organicstest$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
  testcorrectcount <- length(which(organicstest$correct))
  testincorrectcount <- nrow(organicstest) - testcorrectcount
  testerrorrate <- testincorrectcount/nrow(organicstest)
  testaccuracy <- 1-testerrorrate
  
  #Compare
  paste("TRAIN: Error Rate (", trainerrorrate, ") Accuracy (", trainaccuracy, ")")
  paste("TEST: Error Rate (", testerrorrate, ") Accuracy (", testaccuracy, ")")
  

##############################################################################

#Build logistic regression
organicstrain$pred <- NULL
organicstrain$correct <- NULL

organicstest$pred <- NULL
organicstest$correct <- NULL

logit.reg <- glm(TargetBuy ~ ., data = organicstrain, family = binomial(link = "logit"))
summary(logit.reg)

logit.reg <- glm(TargetBuy ~ DemAffl+DemAge+DemGender, data = organicstrain, family = binomial(link = "logit"))
summary(logit.reg)

confint.default(logit.reg) #Build confidence intervals
exp(coef(logit.reg)) #Calculate odds ratio

#Calculate Chi-Square
devdiff <- with(logit.reg, null.deviance - deviance) #difference in deviance between null and this model
dofdiff <- with(logit.reg, df.null - df.residual) #difference in degrees of freedom between null and this model
pval <- pchisq(devdiff, dofdiff, lower.tail = FALSE )
paste("Chi-Square: ", devdiff, " df: ", dofdiff, " p-value: ", pval)

#Calculate psuedo R square
install.packages("pscl")
library(pscl)

pR2(logit.reg)
#pr2=1-(residual deviance/null deviance). 
#First value in pR2 result is residual deviance
#Second value in pR2 result is null deviance
resid.dev<-pR2(logit.reg)[1]
null.dev<-pR2(logit.reg)[2]
pr2 <- 1-(resid.dev/null.dev)
paste("Psuedo R2: ", pr2)
#This is the same as the fourth value-McFadden rho-squared which is already displayed in the result of the pR2 function
pR2(logit.reg)[4]





#Predict training data
organicstrain$probTargetBuy <- predict(logit.reg, newdata = organicstrain, type = "response")
#Convert probability in to a 0 or 1 prediction by rounding (cutoff = 0.5)
head(organicstrain)
organicstrain$pred <- round(organicstrain$probTargetBuy)

#Evaluate model performance
install.packages("gains")
library(gains)
gain <- gains(organicstrain$TargetBuy, organicstrain$pred, groups=length(organicstrain$pred))

#plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(organicstrain$TargetBuy))~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(organicstrain$TargetBuy))~c(0, dim(organicstrain)[1]), lty=2)
# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(organicstrain$TargetBuy)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9),
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
# add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

#Confusion Matrix
writeLines("PERFORMANCE EVALUATION FOR")
writeLines(paste("Model:", deparse(substitute(logit.reg))))
writeLines(paste("Target:", deparse(substitute(organicstrain))))

writeLines("\n\nConfusion Matrix:")
confMatrix <- table(Actual = organicstrain$TargetBuy, Predicted = organicstrain$pred)
truePos <- confMatrix[2,2]
falseNeg <- confMatrix[2,1]
falsePos <- confMatrix[1,2]
trueNeg <- confMatrix[1,1]
print(confMatrix)
writeLines("\n\n")

accuracy <- (truePos + trueNeg)/(truePos + falseNeg + falsePos + trueNeg)
sensitivity <- truePos/(truePos + falseNeg)
specificity <- trueNeg/(falsePos + trueNeg)
falsePosRate <- falsePos/(falsePos + trueNeg)
falseNegRate <- falseNeg/(truePos + falseNeg)
precision <- truePos/(truePos + falsePos)

writeLines(paste("Accuracy:", round(accuracy, digits = 4)))
writeLines(paste("Sensitivity:", round(sensitivity, digits = 4)))
writeLines(paste("Specificity:", round(specificity, digits = 4)))
writeLines(paste("False Positive Rate:", round(falsePosRate, digits = 4)))
writeLines(paste("False Negative Rate:", round(falseNegRate, digits = 4)))
writeLines(paste("Precision:", round(precision, digits = 4)))

#############################################################################

#Predict test data



#Prediction on test data
#Convert probability in to a 0 or 1 prediction by rounding (cutoff = 0.5)
organicstest$probTargetBuy <- predict(logit.reg, newdata = organicstest, type = "response")

head(organicstest)
organicstest$pred <- round(organicstest$probTargetBuy)

#Evaluate model performance on test data
gain <- gains(organicstest$TargetBuy, organicstest$pred, groups=length(organicstest$pred))

#plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(organicstest$TargetBuy))~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(organicstest$TargetBuy))~c(0, dim(organicstest)[1]), lty=2)
# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(organicstest$TargetBuy)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9),
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
# add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

#Confusion Matrix

writeLines("PERFORMANCE EVALUATION FOR")
writeLines(paste("Model:", deparse(substitute(logit.reg))))
writeLines(paste("Target:", deparse(substitute(organicstest))))

writeLines("\n\nConfusion Matrix:")
confMatrix <- table(Actual = organicstest$TargetBuy, Predicted = organicstest$pred)
truePos <- confMatrix[2,2]
falseNeg <- confMatrix[2,1]
falsePos <- confMatrix[1,2]
trueNeg <- confMatrix[1,1]
print(confMatrix)
writeLines("\n\n")

accuracy <- (truePos + trueNeg)/(truePos + falseNeg + falsePos + trueNeg)
sensitivity <- truePos/(truePos + falseNeg)
specificity <- trueNeg/(falsePos + trueNeg)
falsePosRate <- falsePos/(falsePos + trueNeg)
falseNegRate <- falseNeg/(truePos + falseNeg)
precision <- truePos/(truePos + falsePos)

writeLines(paste("Accuracy:", round(accuracy, digits = 4)))
writeLines(paste("Sensitivity:", round(sensitivity, digits = 4)))
writeLines(paste("Specificity:", round(specificity, digits = 4)))
writeLines(paste("False Positive Rate:", round(falsePosRate, digits = 4)))
writeLines(paste("False Negative Rate:", round(falseNegRate, digits = 4)))
writeLines(paste("Precision:", round(precision, digits = 4)))

##########################################################################

#Compare performance on testing and training data

organicstrain$correct <- organicstrain$TargetBuy == organicstrain$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
traincorrectcount <- length(which(organicstrain$correct))
trainincorrectcount <- nrow(organicstrain) - traincorrectcount
trainerrorrate <- trainincorrectcount/nrow(organicstrain)
trainaccuracy <- 1-trainerrorrate

organicstest$correct <- organicstest$TargetBuy == organicstest$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
testcorrectcount <- length(which(organicstest$correct))
testincorrectcount <- nrow(organicstest) - testcorrectcount
testerrorrate <- testincorrectcount/nrow(organicstest)
testaccuracy <- 1-testerrorrate

#Compare
paste("TRAIN: Error Rate (", trainerrorrate, ") Accuracy (", trainaccuracy, ")")
paste("TEST: Error Rate (", testerrorrate, ") Accuracy (", testaccuracy, ")")
