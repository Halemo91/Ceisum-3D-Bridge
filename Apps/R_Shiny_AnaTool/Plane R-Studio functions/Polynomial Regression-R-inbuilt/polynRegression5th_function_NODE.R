### 5th Polynomial regression FUNCTION #########################
### Ro Neumann, 2015 ###########################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: tr5 = trendline --> 1 column matrix, data values
###         y_detrended5 --> 1 column matrix, data values



polynRegression5th__function<-function (data) {
  
# data=read.table("Exercise3-3.txt", sep=" ")

t = as.numeric(unlist(data[1]))
y = as.numeric(unlist(data[2]))


trend5  <- lm(y~poly(t,5,raw=TRUE))
e1 = summary(trend5)$coefficients[1,1]
e2 = summary(trend5)$coefficients[2,1]
e3 = summary(trend5)$coefficients[3,1]
e4 = summary(trend5)$coefficients[4,1]
e5 = summary(trend5)$coefficients[5,1]
# e6 = summary(trend5)$coefficients[6,1]


tr5 = e1 + e2*t + e3*t^2 + e4*t^3 + e5*t^4 #+ e6*t^5

y_detrended5 = y-tr5


# PLOT Original data and trendline
plot(t,y,type="l",col="blue")
lines(t,tr5, col="red") 

# PLOT detrended data in new window
plot(t,y_detrended5,type="l",col="black")


}