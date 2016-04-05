### 6th Polynomial regression FUNCTION ###########################
### Ro Neumann, 2015 #############################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: tr6 = trendline --> 1 column matrix, data values
###         y_detrended6 --> 1 column matrix, data values


polynRegression6th__function<-function (data) {
  
t = as.numeric(unlist(data[1]))
y = as.numeric(unlist(data[2]))



trend6  <- lm(y~poly(t,6,raw=TRUE))
f1 = summary(trend6)$coefficients[1,1]
f2 = summary(trend6)$coefficients[2,1]
f3 = summary(trend6)$coefficients[3,1]
f4 = summary(trend6)$coefficients[4,1]
f5 = summary(trend6)$coefficients[5,1]
f6 = summary(trend6)$coefficients[6,1]
# f7 = summary(trend6)$coefficients[7,1] # out of bounds


tr6 = f1 + f2*t + f3*t^2 + f4*t^3 + f5*t^4 + f6*t^5 #+ f7*t^6

y_detrended6 = y-tr6



# PLOT Original data and trendline
plot(t,y,type="l",col="blue")
lines(t,tr6, col="red") 

# PLOT detrended data in new window
plot(t,y_detrended6,type="l",col="black")

}
