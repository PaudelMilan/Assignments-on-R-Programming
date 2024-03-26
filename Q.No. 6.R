setwd("C:/Users/melan/OneDrive/Desktop/Milan_Paudel_Roll_18")

## Q.No 6

### 6a
set.seed(18)
age<- sample(18:99,100, replace = TRUE)

set.seed(18)
sex<- sample(c("Male","Female"),100, replace= TRUE)

set.seed(18)
education_level<- sample(c("No_Education","Primary","Secondary","Beyond_Secondary"),100, replace= TRUE)

set.seed(18)
socio_economic_status<- sample(c("Low","Middle","High"),100, replace = TRUE)

set.seed(18)
body_mass_index<- sample(seq(14,38, by= 0.1), 100, replace = TRUE)

data<- data.frame(age, sex, education_level, socio_economic_status, body_mass_index)

### 6b
library(ggplot2)
ggplot(data= data, aes(x= age)) + geom_line()

### 6c
ggplot(data= data, aes(x= age, y= body_mass_index)) + geom_point()

### 6d
ggplot(data= data)+ coord_polar()

### 6e
ggplot((data= data, aes()))


####################################################################################################

## Q.No 7

### 7.a
library(carData)

bfoxData<- Bfox 
View(bfoxData)
str(bfoxData)

split_Data<- sample(2, nrow(bfoxData), replace= TRUE, prob = c(0.7,0.3))
train.data<- bfoxData[split_Data ==1,] 
test.data<- bfoxData[split_Data ==2,]

### 7.b
library(caret)
train_x<- train.data[c(1:4,6)]
train_y<- train.data[5]
test_x<-  test.data[c(1:4,6)]
test_y<- test.data[5]

linear_model<- lm(debt~., data = train.data)
knn_model<- knnreg(x=train_x, y = train_y)

### 7.c
summary(linear_model)
summary(knn_model)

linear_model_r2<- R2(linear_model$)

knn_model_r2<- R2(knn_model$) 


### 7.d
linear_predict<- predict(linear_model, data= test.data)
knn_model_predict<- predict(knn_model, data = test_x)

### 7.e
test_linear_model_r2<- R2(linear_predict, test.data$debt)
test_linear_model_rmse<- RMSE(linear_predict, test.data$debt)
test_linear_model_mse<- mean((linear_predict- test.data$debt)^2)

test_knn_model_r2<- R2(knn_model_predict, test_y)
test_knn_rmse<- RMSE(knn_model_predict, test_y)
test_knn_mse<- mean((knn_model_predict- test_y)^2)

############################################################################################
## 8 

### 8.a
arrestsData<- Arrests
str(arrestsData)
arrestsData$released <- ifelse(arrestsData$released=="Yes",1,0)

split_mtcars<- sample(2, nrow(arrestsData), replace = TRUE, prob = c(0.8,0.2))
arrests_train<- arrestsData[split_mtcars == 1,]
arrests_test<- arrestsData[split_mtcars ==2,]

### 8.b
library(stats)
library(e1071)

logistic_model<- glm(released ~ colour+age+employed+citizen, data= arrests_train, family = "binomial")
naive_bayes_model<- naiveBayes(released ~ colour+age+employed+citizen, data= arrests_train)

logistic_model_predicted<- predict(logistic_model, arrests_train)
naiveBayes_predicted<- predict(naive_bayes_model, arrests_train)

logistic_prob<- ifelse(logistic_model_predicted>0.5,1,0)

logistic_table<- table()
