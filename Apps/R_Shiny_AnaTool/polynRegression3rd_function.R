### 3rd Polynomial regression - FUNCTION ######################
### Ro Neumann, 2015 ##########################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: tr3 = trendline --> 1 column matrix, data values
###         y_detrended3 --> 1 column matrix, data values


polynRegression3rd_function<-function (time, data) {
  
# data=read.table("Exercise3-3.txt", sep=" ")

t = as.numeric(unlist(time))
y = as.numeric(unlist(data))



trend3 <- lm(y~poly(t,3,raw=TRUE))
c1 = summary(trend3)$coefficients[1,1]
c2 = summary(trend3)$coefficients[2,1]
c3 = summary(trend3)$coefficients[3,1]
c4 = summary(trend3)$coefficients[4,1]

tr3 = c1 + c2*t + c3*t^2 + c4*t^3

y_detrended3 = y-tr3




# PLOT Original data and trendline
# plot(t,y,type="l",col="blue")
# lines(t,tr3, col="red") 

# PLOT detrended data in new window
plot(t,y_detrended3,type="l",col="black", xlab ="time", ylab = "Data", main = "Polynomial Regression Of 3rd Order")
lines(t,y,type="l",col="blue")
lines(t,tr3, col="red") 

}
