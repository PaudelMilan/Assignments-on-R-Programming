## 1. Create a barplot of cyl variable of mtcars data with and without using factor argument

df<- mtcars             #creating df object that has mtcars dataframe data
barplot(table(df$cyl))  #getting frequency of data in df$cyl and plotting in barplot

plot(factor(df$cyl))    #plotting graph using plot function with factor argument

################################################################################################################
## 2. Locate median of mpg variable of mtcars data graphically and compare the median value with in-built function of R

newmtcarmpg<-sort(mtcars$mpg,decreasing = FALSE)                                   #sorting mpg attribute values in ascending order
cumulativeSum<- cumsum(newmtcarmpg)                                                #calculating cumulative sum frequencies
plot(newmtcarmpg,cumulativeSum,type = "l",xlab = "MPG",ylab = "Cumulative Sum")    #plot mpg vs cumulative summ
lines(newmtcarmpg,rev(cumulativeSum))                                              #adding more than ogive curve
abline(v=median(mtcars$mpg),col="red")                                             #getting vertical line
text(median(mtcars$mpg)+05,250,paste("Median = ",median(mtcars$mpg),sep = " "),col="red")

##############################################################################################################
## 3. Locate mode of mpg variable of mtcars data graphically and compare the mode value with the in-built/custom function of R
myHist<- hist(mtcars$mpg)                                                     #creating object of histogram
modeOfData<- 3*median(mtcars$mpg) -2*mean(mtcars$mpg)                         #calculation for mode
lines(x=myHist$breaks[c(2,3,2,3)],y=myHist$counts[c(3,2,2,3)],col="red")      #plotting lines that intersect each other at 17.42
abline(v=modeOfData,lty=16,lwd=3,col="green")                                 #ploting vertical line
text(26,10,paste("Mode = ",round(modeOfData,2),sep = ""),col="green")         #setting text
#############################################################################################################
#4. Create a scatterplot of mgp (dependent) and wt (independent) variable of mtcars data, add ablines at mean plus-minus 2 standard
#deviation and show the name of the cars with mpg > 2 standard deviation from mean inside the scatterplot obtained above getting plot of
#wt as independent variable and milege as dependent variable
plot(df$wt,df$mpg)

#adding lines for mean of df$mpg and +- twice standard deviation
abline(h=mean(df$mpg),lwd=4,lty=3,col="blue")
abline(h=mean(df$mpg)+2*sd(df$mpg),lwd=3,lty=4,col="blue")
abline(h=mean(df$mpg)-2*sd(df$mpg),lwd=3,lty=4,col="blue")

#getting row names of data in which mpg is greater than sum of mean and twice standard deviation
lists<- (row.names(df)[df$mpg>(2*sd(df$mpg)+mean(df$mpg))])
numbers<- which(row.names(df) %in% lists)                     #getting row number
text(df$wt[numbers]+0.45,df$mpg[numbers],labels = row.names(df)[numbers],col = "blue")  #setting text for scatter plot

#######################################################################################################
## 5. Create a x variable with 100 random numbers and y variable with x + 100 random numbers;
#create a factor variable with 100 monthly random observations, create a time series data with 100 random values starting from January 1970; 
#create a date variable with 100 random values starting from 1970/01/01 increasing each day; create a new variable z with square of x variable
set.seed(1)
x<- sample(100,replace = FALSE)
y<- x+100
monthRandom<- factor(sample(month.name[1:12],100,replace=T))
my_timeseries<- ts(sample(100,replace = FALSE),start = c(1970,1),frequency=12)
date<- seq(as.Date("2005/1/1"), by ="day", length = 100)
z<- x^2

#######################################################################################################
## 6. Create a 2 x 3 plot window with scatterplot, barplot, boxplot, time series plot, date plot and square function plot
par(mfrow=c(2,3))
plot(x,y,main="Scatter Plot")
barplot(table(monthRandom),main="Bar Plot")
boxplot(monthRandom,main="Box Plot")
plot(my_timeseries,main="Time Series")
plot(date,main="Date")
plot(z,main="Square Function")
par(mfrow=c(1,1))
##############################################################################################################
## 7. Perform log transformation on x and z variables i.e. log of x, log of z and log of x and z in the plot command and interpret the results carefully 
plot(log(x))
plot(log(z))
plot(log(z),log(x))
##############################################################################################################
## 8. Create a 1x1 plot window with correlation matrix plot of first three numerical variable of mtcars data (cyl is not numeric, its factor!)
View(mtcars)
par(mfrow=c(1,1))
plot(mtcars[,c(1,3,4)],main="Correlation matrix plot")
###########################################################################################################################################
## 9. Interpret the result of the correlation matrix plot obtained above with respect to the correlation coefficient to be used for each pair
cor(mtcars$mpg,mtcars$disp)
cor(mtcars$mpg,mtcars$hp)
cor(mtcars$disp,mtcars$hp)
#############################################################################################################################################
## 10. Write advantage and limitations of plots created using R base packages based on these nine steps

