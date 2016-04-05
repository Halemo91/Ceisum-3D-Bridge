### 7th Polynomial regression FUNCTION ##########################
### Ro Neumann, 2015 ############################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: tr7 = trendline --> 1 column matrix, data values
###         y_detrended7 --> 1 column matrix, data values


polynRegression7th_function<-function (time, data) {
  

t = as.numeric(unlist(time))
y = as.numeric(unlist(data))


trend7  <- lm(y~poly(t,7,raw=TRUE))
g1 = summary(trend7)$coefficients[1,1]
g2 = summary(trend7)$coefficients[2,1]
g3 = summary(trend7)$coefficients[3,1]
g4 = summary(trend7)$coefficients[4,1]
g5 = summary(trend7)$coefficients[5,1]
g6 = summary(trend7)$coefficients[6,1]
# g7 = summary(trend7)$coefficients[7,1] # out of bounds
# g8 = summary(trend7)$coefficients[8,1] # out of bounds

tr7 = g1 + g2*t + g3*t^2 + g4*t^3 + g5*t^4 + g6*t^5 #+ g7*t^6 #+ g8*t^7

y_detrended7 = y-tr7


# PLOT Original data and trendline
# plot(t,y,type="l",col="blue")
# lines(t,tr7, col="red") 

# PLOT detrended data in new window
plot(t,y_detrended7,type="l",col="black", xlab ="time", ylab = "Data", main = "Polynomial Regression Of 7th Order")
lines(t,y,type="l",col="blue")
lines(t,tr7, col="red") 
}
