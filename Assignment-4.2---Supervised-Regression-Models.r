#1. Generate a 1000 random data with 10 variables [five continuous: age (18 to 90 years), height (150 - 180 cm), weight (50 - 90 kg), 
#income (10000 - 200000), diastolic blood pressure (70 - 170 mm Hg) and five categorical: sex (male/female), education (no education, primary, 
#secondary, tertiary), place of residence (rural/urban), socio-economic status (low/medium/high) and exercise (yes/no)] using 
#set.seed(your roll number and save it as SR object
set.seed(18)
age<- sample(18:90,1000, replace = TRUE)

set.seed(18)
height<- sample(seq(150, 180, by = 0.5), 1000, replace = TRUE)

set.seed(18)
weight<- sample(50:90,1000, replace=TRUE)

set.seed(18)
income<- sample(seq(10000,200000, by= 500),1000, replace = TRUE)

set.seed(18)
blood_pressure<- sample(70:170,1000, replace = TRUE)

set.seed(18)
sex<- as.factor(sample(c("Male","Female"),1000, replace=TRUE))

set.seed(18)
education<- as.factor(sample(c("No_Education","Primary","Secondary","Tertiary"),1000, replace=TRUE))

set.seed(18)
place_of_residence<- as.factor(sample(c("Rural","Urban"),1000, replace = TRUE))

set.seed(18)
socio_economic_status<- as.factor(sample(c("Low","Medium","High"),1000, replace = TRUE))

set.seed(18)
exercise<- as.factor(sample(c("Yes","No"),1000, replace = TRUE))

SR<- data.frame(age,height,weight,income,blood_pressure,sex,education,place_of_residence,socio_economic_status,exercise)
colnames(SR)<- c("age","height","weight","income","bloodPressure","sex","education","place_of_residence","socio_economic_status","exercise")
View(SR)

# 2. Randomly split the SR object data as SR.train (70%) and SR.test (30%) with replacement sampling and fit multiple linear regression with
#diastolic blood pressure as dependent variable and rest of variables as independent variable and get fit indices (R-Square, MSE, RMSE and MAE)
#for the SR.test data

library(caret)

#splitting data
split<- sample(2,nrow(SR),replace = TRUE, prob = c(0.7,0.3))

#training and testing data
SR.train<- SR[split==1,]
SR.test<- SR[split==2,]


#linear model
mlr<- lm(bloodPressure~.,data=SR.train)

#predicted value
yhat<- predict(mlr,SR.test)

#calculating R-square error
r_square<-  R2(yhat,SR.test$bloodPressure)

stats<- data.frame(
#calculation of mean square error
mse= mean((yhat- SR.test$bloodPressure)^2),

#calculation of root mean square error
rmse= RMSE(yhat,SR.test$bloodPressure),

#calculation of mean absolute error
mae= MAE(yhat, SR.test$bloodPressure))

# 3. Fit the multiple linear regression model with Leave One Out Cross-Validation, k-fold cross validation, repeated k-fold cross validation methods 
#and get fit indices for SR.test data and, compare the fit indices of supervised regression models fitted in step 2 and 3 above with careful 
#interpretation

#Leave one out cross-validation linear regression
trainingControl<- trainControl(method = "loocv")
trained<- train(bloodPressure~., data = SR, trControl=trainingControl, method="lm")
predictLOOCV<- predict(trained,SR.test)

loocvDataFrame<- data.frame(
#calculating R-square error
r_square= R2(predictLOOCV,SR.test$bloodPressure),

#calculation of mean square error
mse= mean((predictLOOCV- SR.test$bloodPressure)^2),

#calculation of root mean square error
rmse= RMSE(predictLOOCV,SR.test$bloodPressure),

#calculation of mean absolute error
mae= MAE(predictLOOCV,SR.test$bloodPressure))
rownames(loocvDataFrame)<- "loocv"
##################################################################
#k-fold cross validation method
kControl<- trainControl(method="cv",number = 10)
kTrain<- train(bloodPressure ~., data = SR, model="lm", trControl= kControl )
kPredict<- predict(kTrain, SR.test)

kDataFrame<- data.frame(
  #calculating R-square error
  r_square=  R2(kPredict,SR.test$bloodPressure),
  
  #calculation of mean square error
  mse= mean((kPredict- SR.test$bloodPressure)^2),
  
  #calculation of root mean square error
  rmse= RMSE(kPredict,SR.test$bloodPressure),
  
  #calculation of mean absolute error
  mae= MAE(kPredict,SR.test$bloodPressure))
rownames(kDataFrame)<- "k-fold cv"
######################################################################
#Repeated k-fold cross validation method

repeatedkControl<- trainControl(method="repeatedcv",number = 10, repeats=3 )
repeatedkTrain<- train(bloodPressure ~., data = SR, model="lm", trControl= repeatedkControl )
repeatedkPredict<- predict(repeatedkTrain, SR.test)
repeatedkPredictkDataFrame<- data.frame(
  #calculating R-square error
  r_square=  R2(repeatedkPredict,SR.test$bloodPressure),
  
  #calculation of mean square error
  mse= mean((repeatedkPredict- SR.test$bloodPressure)^2),
  
  #calculation of root mean square error
  rmse= RMSE(repeatedkPredict,SR.test$bloodPressure),
  
  #calculation of mean absolute error
  mae= MAE(repeatedkPredict,SR.test$bloodPressure))
rownames(repeatedkPredictkDataFrame)<- "repeated k-fold cv"
statsDF<- rbind(loocvDataFrame,kDataFrame,repeatedkPredictkDataFrame) 
statsDF

# 4. Fit KNN regression, Decision Tree regression, SVM regression and Neural Network regression using the same dependent and independent variables, 
#get and compare fit indices of these models for SR.test data
#KNN regression

##KNN Regression
x_datatrain<- (SR.train[,1:4])
x_datatrain<- scale(x_datatrain)[,]
y_datatrain<- (SR.train)[,5]
x_datatrainSex<- (data.frame(sex= ifelse(SR.train$sex=="Male",1,0)))
x_datatrainplace_of_residence<- data.frame(residence= ifelse(SR.train$place_of_residence== "Urban",1,0))
x_datatrainExercise<- data.frame(exercise= ifelse(SR.train$exercise == "Yes",1,0))

#creating dummy data for train data
educationDummy<- as.data.frame(psych::dummy.code(SR.train$education))
socio_economicDummy<- as.data.frame(psych::dummy.code(SR.train$socio_economic_status))
x_datatrain<- cbind(x_datatrain,educationDummy,socio_economicDummy )

#creating data for test data
x_dataTest<- SR.test[,1:4]
x_dataTest<- scale(x_dataTest)
y_dataTest<- SR.test[,5]
x_dataTestSex<- (data.frame(sex= ifelse(SR.test$sex=="Male",1,0)))
x_dataTestplace_of_residence<- data.frame(residence= ifelse(SR.test$place_of_residence== "Urban",1,0))
x_dataTestExercise<- data.frame(exercise= ifelse(SR.test$exercise == "Yes",1,0))

#creating dummy data for test data
educationDummyTest<- as.data.frame(psych::dummy.code(SR.test$education)) 
socio_economicDummyTest<- as.data.frame(psych::dummy.code(SR.test$socio_economic_status))
x_dataTest<- cbind(x_dataTest,educationDummyTest,socio_economicDummyTest)

knnModel<- knnreg(x=x_datatrain, y= y_datatrain)
y_predict<- predict(knnModel,x_dataTest)

#fit indices of model test data
KNNr2<- R2(y_predict,y_dataTest)
KNNmse<- mean((y_dataTest- y_predict)^2)
KNNrmse<- RMSE(y_predict,y_dataTest)
KNNmae<- MAE(y_predict,y_dataTest)
KNNr2
KNNmse
KNNrmse
KNNmae
####################################################################################
#SVM regression

library(e1071)
library(psych)

svmTrain<- SR.train[,-(6:10)]
#svmTrain<- scale(svmTrain)
svmTrainSex<- x_datatrainSex
svmtrainplace_of_residence<- x_datatrainplace_of_residence
svmtrainExercise<- x_datatrainExercise
svmtrainEducation<- educationDummy
svmtrainSocioEconomy<- socio_economicDummy
svmTrain<- cbind(svmTrain,svmTrainSex,svmtrainplace_of_residence,svmtrainExercise,svmtrainEducation,svmtrainSocioEconomy)
View(svmTrain)

svmTest<- SR.test[,-(6:10)]
#svmTest<- scale(svmTest)
svmTestSex<- x_dataTestSex
svmTestplace_of_residence<- x_dataTestplace_of_residence
svmTestExercise<- x_dataTestExercise
svmTestEducation<- educationDummyTest
svmTestSocioEconomy<- socio_economicDummyTest
svmTest<- cbind(svmTest,svmTestSex,svmTestplace_of_residence,svmTestExercise,svmTestEducation,svmTestSocioEconomy)
View(svmTest)

svmmodel<-svm(bloodPressure ~., data = svmTrain, kernel="linear", type="eps-regression")
svmPredct<- predict(svmmodel,svmTest)

svmR2<- R2(svmPredct, svmTest$bloodPressure)
svmMSE<- mean((svmPredct - svmTest$bloodPressure)^2)
svmRMSE<- RMSE(svmPredct, svmTest$bloodPressure)
svmMAE<- MAE(svmPredct, svmTest$bloodPressure)
svmR2
svmMSE
svmRMSE
svmMAE
#######################################################################
#Neural Network Regression
library(neuralnet)

NNTraindata<- SR.train[,-(6:10)]
#NNTraindata<- (NNTraindata- min(NNTraindata))/(max(NNTraindata)- min(NNTraindata))

NNTraindata<- scale(NNTraindata)
NNTraindataSex<- x_datatrainSex
NNTraindataplace_of_residence<- x_datatrainplace_of_residence
NNTraindataExercise<- x_datatrainExercise
NNTraindataEducation<- educationDummy
NNTraindataSocioEconomy<- socio_economicDummy
NNTraindata<- cbind(NNTraindata,NNTraindataSex,NNTraindataplace_of_residence,NNTraindataExercise,NNTraindataEducation,NNTraindataSocioEconomy)

NNTestdata<- SR.test[,-(6:10)]
#NNTestdata<- (NNTestdata- min(NNTestdata))/(max(NNTestdata)- min(NNTestdata))
NNTestdata<- scale(NNTestdata)
NNTestdataSex<- x_dataTestSex
NNTestdataplace_of_residence<- x_dataTestplace_of_residence
NNTestdataExercise<- x_dataTestExercise
NNTestdataEducation<- educationDummyTest
NNTestdataSocioEconomy<- socio_economicDummyTest
NNTestdata<- cbind(NNTestdata,NNTestdataSex,NNTestdataplace_of_residence,NNTestdataExercise,NNTestdataEducation,NNTestdataSocioEconomy)

NNmodel<- neuralnet(bloodPressure ~., data= svmTrain, hidden= c(3,4,3), linear.output= FALSE, threshold = 0.001 )
NNpredict<- predict(NNmodel,NNTestdata)

deviation<- (NNTestdata$bloodPressure- NNpredict)/NNTestdata$bloodPressure
accuracy<- abs(mean(deviation))
error<- 1- accuracy

nnR2<- R2(NNpredict, NNTestdata$bloodPressure)
nnMSE<- mean((NNpredict - NNTestdata$bloodPressure)^2)
nnRMSE<- RMSE(NNpredict, NNTestdata$bloodPressure)
nnMAE<- MAE(NNpredict, NNTestdata$bloodPressure)
nnR2
nnMSE
nnRMSE
nnMAE
####################################################################
#Decision tree regression
library(rpart)
DecissionTreeTraindata<- SR.train[,-(6:10)]

DecissionTreeTraindataSex<- x_datatrainSex
DecissionTreeTraindataplace_of_residence<- x_datatrainplace_of_residence
DecissionTreeTraindataExercise<- x_datatrainExercise
DecissionTreeTraindataEducation<- educationDummy
DecissionTreeTraindataSocioEconomy<- socio_economicDummy
DecissionTreeTraindata<- cbind(DecissionTreeTraindata,DecissionTreeTraindataSex,DecissionTreeTraindataplace_of_residence,DecissionTreeTraindataExercise,DecissionTreeTraindataEducation,DecissionTreeTraindataSocioEconomy)

DecissionTreeTestdata<- SR.test[,-(6:10)]
DecissionTreeTestdataSex<- x_dataTestSex
DecissionTreeTestdataplace_of_residence<- x_dataTestplace_of_residence
DecissionTreeTestdataExercise<- x_dataTestExercise
DecissionTreeTestdataEducation<- educationDummyTest
DecissionTreeTestdataSocioEconomy<- socio_economicDummyTest
DecissionTreeTestdata<- cbind(DecissionTreeTestdata,DecissionTreeTestdataSex,DecissionTreeTestdataplace_of_residence,DecissionTreeTestdataExercise,DecissionTreeTestdataEducation,DecissionTreeTestdataSocioEconomy)

DecissionTreeModel<- rpart(bloodPressure ~., DecissionTreeTraindata, method = "anova", minsplit=5, minbucket=2)
DecissionTreeModel

DecissionTreePrediction<- predict(DecissionTreeModel,DecissionTreeTestdata)
plot(DecissionTreeModel)

# 5. Which supervised regression model is the best model for doing prediction in the SR.test data? Why?

# 6. Predict diastolic blood pressure of a person with 50 years, 175mm height, 80 kg weight, 90000 income, male, tertiary level education, living in 
#urban area, medium socio-economic status and no exercise and interpret the result carefully

# 7. Write a reflection of the assignment on your own words focusing on "what did I learn with this assignment?"

                                                                                                                                                                                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                                                                                                                                                                               