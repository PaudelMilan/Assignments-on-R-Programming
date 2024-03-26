##### 1. Get this data in R, make it tidy and do the necessary transformations: https://www.mohfw.gov.in/ (Table: COVID-19 Statewise Status (Click to expand))
#Loading packages rvest and dplyr
library(rvest)
library(dplyr)

#Reading json data
getHTMLCovid <-  jsonlite::fromJSON("https://www.mohfw.gov.in/data/datanew.json")   #Reading Html file from link

#converting to data frame
covidTable<- as.data.frame(getHTMLCovid)
View(covidTable)

#removing last row
covidTable<- covidTable[-37,]
View(covidTable)
str(covidTable)

#changing the data types of columns to numeric
covidTable <- mutate_at(covidTable,c(1,3:14),as.numeric)
str(covidTable)

#2. Get this data in R, make it tidy and do the necessary transformation: https://covid19.who.int/table

#Extracting Data from json file
getTable1<- jsonlite::fromJSON("https://covid19.who.int/page-data/sq/d/3713876948.json")[[1]][[1]][[1]][[1]]
str(getTable1)

#Replacing "\" in char
getTable1<- gsub(pattern = "[\\]","",getTable1)

#Reading and extracting json code into dataframe
str(getTable1)
getTable1<- jsonlite::fromJSON(getTable1)

View(getTable1)

#Replacing spaces in column header with "_"
colnames(getTable1)   <- gsub("[ ] ","_",colnames(getTable1))

#Replacing table with only needed data
getTable1 <- transmute(getTable1,Country,Confirmed,Cases_Last_7_Days,Deaths,
          Deaths_Last_7_Days,Doses_Per_100,Persons_fully_vaccinated_with_last_dose_of_primary_series,Boosters_per_100)
View(getTable1)



#Second Part
library(readr)
library(tidyverse)

#7. Set your working directory as "Documents/imdb" with R script/code to import these datasets in R Studio
setwd("C:/Users/melan/OneDrive/Documents/imdb")
getwd()

#8. Import the "title.ratings.tsv" file using read_tsv function as "df_ratings
df_ratings <- read_tsv(file="C:\\Users\\melan\\OneDrive\\Documents\\imdb\\title.ratings.tsv\\data.tsv")
View(df_ratings)

#9. Check the structure and get summary of all the variables and interpret them carefully
str(df_ratings)
summary(df_ratings)
boxplot(df_ratings$averageRating)

#10. Is this dataset "tidy"? Why?

#11. Get scatterplot of "averageRating" and "numVotes" and interpret it carefully
plot(df_ratings$averageRating)
plot(df_ratings$numVotes)
plot(df_ratings$averageRating,df_ratings$numVotes)

#12. Import the "title.basics.tsv" file using read_tsv function as "df_ basics"
df_basics<- read_tsv(file="C:\\Users\\melan\\OneDrive\\Documents\\imdb\\title.basics.tsv\\data.tsv")
View(df_basics)

#13. Check the structure and get summary of all the variables and interpret them carefully
str(df_basics)
summary(df_basics)


#14. Is this dataset "tidy"? Why?

#15. Join "df_basics" variables in "df_ratings" using "left_join" function (name of the joined data must be df_ratings)
df_ratings<- left_join(df_ratings,df_basics)
View(df_ratings)

#16. Get scatterplot of runtimeMinutes and averageRating variable and interpret then carefull
plot(df_ratings$runtimeMinutes,df_ratings$averageRating,na.omit=TRUE)

#17. Get frequency table of "genres" variable using "plyr" package, if required and interpret it carefully
library(plyr)
plyr::count(df_ratings,"genres")


library(dplyr)
#18. Create a "by_genres" object using group_by function with df_ratings data and genres variable
by_genres <- group_by(df_ratings,genres)

#19. Get mean of runtimeMinutes variables using summarise function by genres, remove NA in runtimeMinutes if required
by_genres$runtimeMinutes<- as.numeric(by_genres$runtimeMinutes,na.omit=TRUE)
summarise(by_genres,Mean=mean(runtimeMinutes,na.rm=TRUE))

#20. Get mean of runtimeMinutes variables using summarise by genres without creating "by_genres" object
summarise(df_ratings,Mean = mean(as.numeric(df_ratings$runtimeMinutes),na.rm= TRUE))

#21. What is difference between step 19 and step 20? Which one do you prefer? Why? Interpret the result of your choice.

#22. Filter "df_ratings" data with runtimeMinutes less than 150 minutes and save it as "df_ratings_movie150m"
df_ratings$runtimeMinutes<- as.numeric(df_ratings$runtimeMinutes,na.omit(TRUE))
df_ratings_movie150m <- filter(df_ratings,runtimeMinutes<150)

#23. Get scatterplot of runtimeMinutes and averageRating variable for this new data and interpret then carefully
plot(df_ratings_movie150m$runtimeMinutes,df_ratings_movie150m$averageRating)

#24. Arrange the df_rating_movie150m data in descending order by averageRating and save it as "best_worst_movies"
best_worst_movies <- df_ratings_movie150m[order(df_ratings_movie150m$averageRating,decreasing = TRUE),]

#25. Show the top 6 and last 6 movies based on the arranged dataset above
head(best_worst_movies)
tail(best_worst_movies)

#26. Get the averageRating of adult movies (isAdult variable) using mutate function and interpret it carefully
select(group_by(df_ratings_movie150m,isAdult),isAdult,averageRating,) %>% mutate(Avg= averageRating) 


#27. Divide the "df_ratings_movies150m" into training and testing dataset with 80% and 20% split with slice function
trainingDataSet<- slice_sample(df_ratings_movie150m,prop = 0.8)
testingDataSet<- slice_sample(df_ratings_movie150m,prop=0.2)
View(trainingDataSet)
class(trainingDataSet)


#28. Get mean, standard deviation, median and interquartile range of averageRating, numVotes and runtimeMinutes variable of training and testing data and interpret them carefully
selectedTrainingDataSet<-trainingDataSet %>% select(averageRating,numVotes,runtimeMinutes) 
summarise(selectedTrainingDataSet,meanOfData=mean(averageRating),standardDeviation= stats::sd(averageRating),median=median(averageRating),InterQuartileRange= stats::IQR(averageRating))
summarise(selectedTrainingDataSet,meanOfData=mean(numVotes),standardDeviation= stats::sd(numVotes),median=median(numVotes),InterQuartileRange= stats::IQR(numVotes))
summarise(selectedTrainingDataSet,meanOfData=mean(runtimeMinutes),standardDeviation= stats::sd(runtimeMinutes),median=median(runtimeMinutes),InterQuartileRange= stats::IQR(runtimeMinutes))

selectedTestingDataSet<-trainingDataSet %>% select(averageRating,numVotes,runtimeMinutes) 
summarise(selectedTestingDataSet,meanOfData=mean(averageRating),standardDeviation= stats::sd(averageRating),median=median(averageRating),InterQuartileRange= stats::IQR(averageRating))
summarise(selectedTestingDataSet,meanOfData=mean(numVotes),standardDeviation= stats::sd(numVotes),median=median(numVotes),InterQuartileRange= stats::IQR(numVotes))
summarise(selectedTestingDataSet,meanOfData=mean(runtimeMinutes),standardDeviation= stats::sd(runtimeMinutes),median=median(runtimeMinutes),InterQuartileRange= stats::IQR(runtimeMinutes))

#29. Get histogram of averageRating, numVotes and runtimeMinutes variables of training and testing data; compare them and interpret them carefully
hist(selectedTrainingDataSet$averageRating)
hist(selectedTrainingDataSet$numVotes)
hist(selectedTrainingDataSet$runtimeMinutes)

hist(selectedTestingDataSet$averageRating)
hist(selectedTestingDataSet$numVotes)
hist(selectedTestingDataSet$runtimeMinutes)

#30. Get boxplot of averageRating, numVotes and runtimeMinutes variables of training and testing data; compare them and interpret them carefully
boxplot(selectedTrainingDataSet$averageRating)
boxplot(selectedTrainingDataSet$numVotes)
boxplot(selectedTrainingDataSet$runtimeMinutes)

boxplot(selectedTestingDataSet$averageRating)
boxplot(selectedTestingDataSet$numVotes)
boxplot(selectedTestingDataSet$runtimeMinutes)

