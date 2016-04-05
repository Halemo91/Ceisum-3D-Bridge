### 4th Polynomial regression FUNCTION ##################
### Ro Neumann, 2015 ####################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: tr4 = trendline --> 1 column matrix, data values
###         y_detrended4 --> 1 column matrix, data values


polynRegression4th_function<-function (time, data) {


t = as.numeric(unlist(time))
y = as.numeric(unlist(data))




trend4 <- lm(y~poly(t,4,raw=TRUE))
d1 = summary(trend4)$coefficients[1,1]
d2 = summary(trend4)$coefficients[2,1]
d3 = summary(trend4)$coefficients[3,1]
d4 = summary(trend4)$coefficients[4,1]
d5 = summary(trend4)$coefficients[5,1]

tr4 = d1 + d2*t + d3*t^2 + d4*t^3 + d5*t^4

y_detrended4 = y-tr4


# PLOT Original data and trendline
# plot(t,y,type="l",col="blue")
# lines(t,tr4, col="red") 

# PLOT detrended data in new window
plot(t,y_detrended4,type="l",col="black", xlab ="time", ylab = "Data", main = "Polynomial Regression Of 4th Order")
lines(t,y,type="l",col="blue")
lines(t,tr4, col="red") 

}
