### S-curve REGRESSION ##########################################################
### Ro Neumann, 2015 ###########################################################

### INPUT: 1. Time series with first column = time, 2nd  = data
###        2. Initial value for a --> graphically determined at upper boundary
###        3. Initial value for b --> graphically determined at y-intercept at time t=0
### OUTPUT: 1. trendline as y-values
###         2. y_reduced --> reduced data as y-values
###         3. X_hat with coefficients a, b and beta


Scurve_function<-function (time, data,a) {
  
#data=read.table("Exercise4_3.txt", sep=",") # Exponential growth

time = as.numeric(unlist(time))
y = as.numeric(unlist(data))

L = y
lengthX = length(time)

# Initial value for a --> graphically determined at upper boundary
#a = 1.557

# New factor b, y-intercept at time t=0, = 0.7845 roughly, graphically determined
b=a/0.7854-1  #  maybe 0,8053

# Beta from rearranging functional model
beta = -log (a/y[1]/b-1/b)/time[1]


X_initial = c(a, b, beta)

maxInf = Inf
delta = 10^-9
iteration = 0
X_0 = X_initial

while (maxInf > delta){
 
  a1 = 1/(1+b*exp(-beta*time))
  a2 = -a*exp(-beta*time)/(1+b*exp(-beta*time))^2
  a3 = a*b*time*exp(-beta*time)/(1+b*exp(-beta*time))^2
  A= cbind(a1,a2,a3)

  
  
  
  # Observations for reduction
  l=y-a/(1+b*exp(-beta*time))
  

  N=t(A)%*%A  
  n= solve(N)
  x=n %*% (t(A) %*% l)
  
  # Update
  X_hat = X_0 + x
  X_0 = X_hat
  a = X_hat [1]
  b = X_hat [2]
  beta = X_hat [3]
  maxInf = max(abs(x))
  iteration = iteration +1

}  

r = length(y) -length(X_initial)

v = (A%*%x)-y


# Detrend data
trendline = a/(1+b*exp(-beta*time))
y_reduced = y-trendline

par(mfrow=c(2,1))

# PLOT Original data and trendline
plot(time,y,type="l",col="blue")
lines(time,trendline, col="red") 

# PLOT detrended data in new window
plot(time,y_reduced,type="l",col="black")


}
######################################################################
