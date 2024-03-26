## Use the attached "titanic.csv" data and do as follows in R Studio with R script:
library(psych) 
library(caret)
 
## 1. Read the titanic.csv data with base R function and save it as "data" and remove the name column and save again as data
data<- read.csv("E:\\Milan\\MDS\\R by Sital Bhandary\\assignmnt\\4.0 assignment\\Milan Paudel - titanic.csv")
data<- data[,-3]
View(data)
str(data)

###########################################################################################################################
## 2. Fit binary logistic regression model with "Survived" variable as dependent variable and rest of variables as independent variables using "data", get summary of the model, check VIF and interpret the results carefully

data_lrc<- data
data_lrc$Pclass<- as.factor(data_lrc$Pclass)
data_lrc$Sex<- as.factor(data_lrc$Sex)
data_lrc$Survived<- as.factor(data_lrc$Survived)

binaryLogisticRegression<- glm(Survived ~.,data= data_lrc, family = "binomial")
summary(binaryLogisticRegression)

library(car)
vif(binaryLogisticRegression)
###########################################################################################################################
## 3. Randomly split the data into 70% and 30% with replacement of samples as "train" and "test" data
set.seed(100)
spliting<- sample(2,nrow(data),replace = TRUE, prob = c(0.7,0.3))
trainData<- data[spliting==1,]
testData<- data[spliting==2,]

##########################################################################################################################
## 4. Fit binary logistic regression classifier, knn classifier, ann classifier, naive bayes classifier, svm classifier, decision tree classifier, decision tree bagging classifier, random forest classifier, tuned random forest classifier and random forest boosting classifier models using the "train" data

#Binary logistic regression classifier

traindata_lrc<- trainData

traindata_lrc$Pclass<- as.factor(traindata_lrc$Pclass)
traindata_lrc$Sex<- as.factor(traindata_lrc$Sex)
traindata_lrc$Survived<- as.factor(traindata_lrc$Survived)

testdata_lrc<- testData
testdata_lrc$Pclass<- as.factor(testdata_lrc$Pclass)
testdata_lrc$Sex<- as.factor(testdata_lrc$Sex)
testdata_lrc$Survived<- as.factor(testdata_lrc$Survived)

binaryLogisticRegression2<- glm(Survived~.,data= traindata_lrc, family = "binomial")

blrPredicted<- predict(binaryLogisticRegression2,testdata_lrc)
###############################################################################
#knn classifier

x_knn3Traindata<- trainData[,-1]
x_knn3Traindata$Age<- scale(x_knn3Traindata$Age)
x_knn3Traindata$Fare<- scale(x_knn3Traindata$Fare)
x_knn3Traindata$Sex<- ifelse(x_knn3Traindata$Sex=="male",1,0) 
y_knn3Traindata<- as.factor(trainData[,1])

x_knn3testdata<- testData[,-1]
x_knn3testdata$Age<- scale(x_knn3testdata$Age)
x_knn3testdata$Fare<- scale(x_knn3testdata$Fare)
x_knn3testdata$Sex<- ifelse(x_knn3testdata$Sex=="male",1,0) 
y_knn3testdata<- as.factor(testData[,1])

knnClassifier<- knn3(x= x_knn3Traindata,y= y_knn3Traindata)
knnPredicted<- predict(knnClassifier,x_knn3testdata)

###############################################################################
#ANN classifier
library(neuralnet)

#converting factors to numeric representation
ANNTraindata<- trainData
ANNTraindata$Age<- scale(ANNTraindata$Age)
ANNTraindata$Fare<- scale(ANNTraindata$Fare)
ANNTraindata$Sex<- ifelse(ANNTraindata$Sex=="male",1,0) 
ANNTraindata$Survived<- as.numeric(trainData[,1])

ANNTestdata<- testData
ANNTestdata$Age<- scale(ANNTestdata$Age)
ANNTestdata$Fare<- scale(ANNTestdata$Fare)
ANNTestdata$Sex<- ifelse(ANNTestdata$Sex=="male",1,0) 
ANNTestdata$Survived<- as.numeric(testData[,1])
str(ANNTestdata)

annClassifier<- neuralnet(Survived~., data =ANNTraindata , hidden = c(2),linear.output = FALSE, threshold = 0.001, stepmax = 1e+07)
ANNPredicted<- compute(annClassifier, ANNTestdata)
###############################################################################
library(e1071)

# Naive Bayes Classifier
NaiveBayesTrainingData<- trainData
NaiveBayesTestData<- testData

NaiveBayesTrainingData$Pclass<- as.factor(NaiveBayesTrainingData$Pclass)
NaiveBayesTrainingData$Sex<- as.factor(NaiveBayesTrainingData$Sex)
NaiveBayesTrainingData$Survived<- as.factor(NaiveBayesTrainingData$Survived)

NaiveBayesTestData<- testData
NaiveBayesTestData$Pclass<- as.factor(NaiveBayesTestData$Pclass)
NaiveBayesTestData$Sex<- as.factor(NaiveBayesTestData$Sex)
NaiveBayesTestData$Survived<- as.factor(NaiveBayesTestData$Survived)

set.seed(100)
naiveBayesClassifier<- naiveBayes(Survived~., data=NaiveBayesTrainingData)
naiveBayesPredicted<- predict(naiveBayesClassifier,NaiveBayesTestData)
###############################################################################
#SVM Classifier
svmTrainingData<- NaiveBayesTrainingData
svmTestData<- NaiveBayesTestData

svmClassifier<- svm(Survived~., data = svmTrainingData, type="C-classification", kernel="linear")
svmPredicted<- predict(svmClassifier, svmTestData)
###############################################################################
#Decision tree classifier
library(party)
dtTrainingData<- NaiveBayesTrainingData
dtTestData<- NaiveBayesTestData

dtClassifier<- ctree(Survived~., data= dtTrainingData)
plot(dtClassifier)                                        #shows over fitting

#Controlling the ctree
dtClassifier<- ctree(Survived~., data= dtTrainingData, controls= ctree_control(mincriterion = 0.99, minsplit = 300))
plot(dtClassifier)

dtPredicted<- predict(dtClassifier, dtTestData)
###############################################################################
#decision tree bagging classifier
library(ipred)

baggingTrainingData<- NaiveBayesTrainingData
baggingTestData<- NaiveBayesTestData

baggingClassifier<- bagging(Survived~., data= baggingTrainingData)
baggingPredicted<- predict(baggingClassifier,baggingTestData)
##############################################################################
#random forest classifier
library(randomForest)

randomForestTrainingData<- NaiveBayesTrainingData
randomForestTestData<- NaiveBayesTestData

set.seed(100)
randomForestClassifier<- randomForest(Survived~., data= randomForestTrainingData)
randomForestPredicted<- predict(randomForestClassifier, randomForestTestData)

#############################################################################
#tuned random forest classifier
plot(randomForestClassifier)
tuneRF(x= randomForestTrainingData[,-1],y= randomForestTrainingData[,1], stepFactor = 0.5, plot = TRUE, ntreeTry = 300, trace = TRUE, improve = 0.05)

randomForestClassifier2<- randomForest(Survived~., data= randomForestTrainingData, ntreeTry=300, mtry= 2, importance = TRUE, proximity = TRUE)
randomForestPredicted2<- predict(randomForestClassifier2, randomForestTestData)
#############################################################################
#random forest boosting classifier
library(gbm)

GBMtrainingData<- NaiveBayesTrainingData
GBMtestData<- NaiveBayesTestData

GBMclassifier<- train(Survived~., data = GBMtrainingData, method= "gbm", verbose=F )
GBMpredict<- predict(GBMclassifier, GBMtestData)
####################################################################################################################################

## 5. Get confusion matrix and accuracy/misclassification error for all the classifier models and interpret them carefully
#Binary logistic regression

blrCM<- predict(binaryLogisticRegression2, traindata_lrc)

blrCM<- ifelse(blrCM>0,1,0)
blrCMTable<- table(blrCM, traindata_lrc$Survived)
confusionMatrix(blrCMTable)
accuracyBLRcm<- sum(diag(blrCMTable))/sum(blrCMTable)
1-accuracyBLRcm
##############################################################################
#Knn classifier
knnCMpredit<- predict(knnClassifier, x_knn3Traindata)

knnCMpredit<- ifelse(knnCMpredit>0.5,1,0)
y_knn3TraindataDummycode<- dummy.code(y_knn3Traindata)
knnCMtable<- table(knnCMpredit, y_knn3TraindataDummycode)
confusionMatrix(knnCMtable)
knnCMaccuracycm<- sum(diag(knnCMtable))/sum(knnCMtable)
1 - knnCMaccuracycm
##############################################################################
#ANN classifier
ANNcmPredicted<- compute(annClassifier, ANNTraindata)

ANNcmProbability<- ifelse(ANNcmPredicted$net.result> 0.5,1,0)
ANNcmTable<- table(ANNcmProbability,ANNTraindata$Survived)
confusionMatrix(ANNcmTable)
ANNcmAccuracycm<- sum(diag(ANNcmTable))/sum(ANNcmTable)
1- ANNcmAccuracycm
###############################################################################
##Naive bayes classifier

naiveBayesPredictedcm<- predict(naiveBayesClassifier,NaiveBayesTrainingData)

naiveBayesTablecm<- table(naiveBayesPredictedcm, NaiveBayesTrainingData$Survived)

confusionMatrix(naiveBayesTablecm)
accuracyNaiveBayescm<- sum(diag(naiveBayesTablecm))/sum(naiveBayesTablecm)
1- accuracyNaiveBayescm

###############################################################################
#svm classifier

svmPredictedcm<- predict(svmClassifier, svmTrainingData)

svmTablecm<- table(svmPredictedcm, svmTrainingData$Survived)

confusionMatrix(svmTablecm)
accuraySVMcm<- sum(diag(svmTablecm))/sum(svmTablecm)
1- accuraySVMcm
###############################################################################
#decision tree classifier

dtPredictedcm<- predict(dtClassifier, dtTrainingData)

dtTablecm<- table(dtPredictedcm, dtTrainingData$Survived)

confusionMatrix(dtTablecm)
accuracyDTcm<- sum(diag(dtTablecm))/sum(dtTablecm)
1- accuracyDTcm
###############################################################################
#decision tree bagging classifier

baggingPredictedcm<- predict(baggingClassifier,baggingTrainingData)

baggingTablecm<- table(baggingPredictedcm, baggingTrainingData$Survived)

confusionMatrix(baggingTablecm)
accuracyBaggingcm<- sum(diag(baggingTablecm))/sum(baggingTablecm)
1- accuracyBaggingcm
###############################################################################
#random forest classifier

randomForestPredictedcm<- predict(randomForestClassifier, randomForestTrainingData)

randomForestTablecm<- table(randomForestPredictedcm, randomForestTrainingData$Survived)

confusionMatrix(randomForestTablecm)
randomForestAccuracycm<- sum(diag(randomForestTablecm))/sum(randomForestTablecm)
1- randomForestAccuracycm
###############################################################################
#tuned random forest classifier

randomForestPredicted2cm<- predict(randomForestClassifier2, randomForestTrainingData)

randomForestTable2cm<- table(randomForestPredicted2cm, randomForestTrainingData$Survived)

confusionMatrix(randomForestTable2cm)
randomForestAccuracy2cm<- sum(diag(randomForestTable2cm))/sum(randomForestTable2cm)
1- randomForestAccuracy2cm
###############################################################################
#random forest boosting classifier

GBMpredictcm<- predict(GBMclassifier, GBMtrainingData)

GBMtableCM<- table(GBMpredictcm, GBMtrainingData$Survived)

confusionMatrix(GBMtableCM)
GBMaccuracyCM<- sum(diag(GBMtableCM))/sum(GBMtableCM)
1- GBMaccuracyCM

############################################################################################################################################################################
## 6. Get confusion matrix and accuracy/misclassification error for all the predicted models and interpret them carefully
#Binary logistic regression

blrProbability<- ifelse(blrPredicted>0.5,1,0)
blrTable<- table(testData$Survived,blrProbability)

confusionMatrix(blrTable)
accuracyBinaryLR<- sum(diag(blrTable))/sum(blrTable)
1- accuracyBinaryLR
###############################################################################
#knn classifier

knnProbability<- ifelse(knnPredicted>0.5,1,0)
y_knn3testdataDummyCode<- dummy.code(y_knn3testdata)

knnTable<- table(y_knn3testdataDummyCode, knnProbability)

confusionMatrix(knnTable)
accuracyKNN<- sum(diag(knnTable))/sum(knnTable)
1- accuracyKNN
###############################################################################
#ANN Classifier

annProbability<- ifelse(ANNPredicted$net.result>0.5,1,0)
annTable<- table(annProbability, ANNTestdata$Survived)

confusionMatrix(annTable)
accuracyANN<- sum(diag(annTable))/sum(annTable)
1- accuracyANN
###############################################################################
#Naive Bayes classifier

naiveBayesTable<- table(naiveBayesPredicted, NaiveBayesTestData$Survived)

confusionMatrix(naiveBayesTable)
accuracyNaiveBayes<- sum(diag(naiveBayesTable))/sum(naiveBayesTable)
1 - accuracyNaiveBayes
##############################################################################
#svm classifier

svmTable<- table(svmPredicted, svmTestData$Survived)

confusionMatrix(svmTable)
accuraySVM<- sum(diag(svmTable))/sum(svmTable)
1- accuraySVM
##############################################################################
#Decision tree

dtTable<- table(dtPredicted, dtTestData$Survived)

confusionMatrix(dtTable)
accuracyDT<- sum(diag(dtTable))/sum(dtTable)
1- accuracyDT
##############################################################################
#bagging classifier

baggingTable<- table(baggingPredicted, baggingTestData$Survived)

confusionMatrix(baggingTable)
accuracyBagging<- sum(diag(baggingTable))/sum(baggingTable)
1- accuracyBagging
##############################################################################
#Random Forest classifier

randomForestTable<- table(randomForestPredicted, randomForestTestData$Survived)

confusionMatrix(randomForestTable)
randomForestAccuracy<- sum(diag(randomForestTable))/sum(randomForestTable)
1- randomForestAccuracy
##############################################################################
#tuned random forest classifier

randomForestTable2<- table(randomForestPredicted2, randomForestTestData$Survived)

confusionMatrix(randomForestTable2)
randomForestAccuracy2<- sum(diag(randomForestTable2))/sum(randomForestTable2)
1- randomForestAccuracy2
###############################################################################
#random forest boosting classifier

GBMpredict<- predict(GBMclassifier, GBMtestData)

GBMtable<- table(GBMpredict, GBMtestData$Survived)

confusionMatrix(GBMtable)
GBMaccuracy<- sum(diag(GBMtable))/sum(GBMtable)
1- GBMaccuracy
#############################################################################################################################

## 7. Compare accuracy and misclassification error of predicted models based on "test" data to decide the "best" model
accuracyErrorTable<- data.frame(c(accuracyBinaryLR,accuracyKNN,accuracyANN,accuracyNaiveBayes,accuraySVM,accuracyDT,accuracyBagging,
  randomForestAccuracy, randomForestAccuracy2, GBMaccuracy),c(errorBinaryLR,errorKNN,errorANN,errorNaiveBayes,errorSVM,errorDT,errorbagging,
    randomForestError, randomForestError2, GBMerror))

rownames(accuracyErrorTable)<- c("Binary Logistic Regression","KNN Classification","ANN Classification","Naive Bayes","SVM","Decision Tree",
                                 "Bagging","Random Forest","Tuned Random Forest","Random Forest Boosting")

colnames(accuracyErrorTable)<- c("Accuracy","Error")
accuracyErrorTable
###########################################################################################################################
## 8. Write a reflection on your own word focusing on "what did I learn from this assignment?"