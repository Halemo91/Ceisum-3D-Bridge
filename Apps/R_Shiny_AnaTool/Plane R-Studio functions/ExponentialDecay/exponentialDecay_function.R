### EXPONENTIAL DECAY FUNCTION ###############################################
### Ro Neumann, 2015 ###########################################################

### INPUT: 1. Time series with first column = time, 2nd  = data
###        2. Initial value for alpha --> value the curve converges to, determined from plot by user!
### OUTPUT: 1. trendline as y-values
###         2. Reduced data as y-values
###         3. X_hat with coefficients alpha & beta


exponentialGrowth_function<-function (data, alpha) {
  

# data=read.table("Exercise4_2.txt", sep=",") # Exponential growth

time = as.numeric(unlist(data[1]))
y = as.numeric(unlist(data[2]))

lengthX = length(time)

# Initial value for alpha --> graphically determined, at upper boundary
# alpha = 3.124
# hlp = alpha-y[1]/alpha
# hlp2 = log(hlp)

# Calculation beta
beta = -(hlp2/ time[1])
# beta = 0.0118

X_initial = c(alpha, beta)
X_0 = X_initial
maxInf = Inf
# is.infinite(maxInf)
delta = 10^-5
iteration = 0


while (maxInf > delta){
  
  a1 = 1-(exp(-beta*time))
  a2 = alpha * time * exp(-beta*time)
  A= cbind(a1,a2)
  # A= [1-exp(-beta*t) alpha*t.*exp(-beta*t)];
  # Reduced vector of observations
  l=y-alpha*(1-exp(-beta*time));
  N=t(A)%*%A  
  
  #n=(A'*y)
  n= solve(N); # DONT use N^-1!!
  x=n %*% (t(A) %*% l)
  
  
  # Update
  X_hat = X_0 + x
  X_0 = X_hat
  alpha = X_hat [1]
  beta = X_hat [2]
  maxInf = max(abs(x))
  iteration = iteration +1
}


r = length(y) -length(X_initial)

v = (A%*%x)-y

# Qxx = solve(N)
# vTPv = t(v) %*% v
# s_0_2 = vTPv/r
# s0 = s_0_2[1] # Due to numerical problem: is assigned as matrix, not scalar
# S_XX= s0 * Qxx
# sX = sqrt(diag(S_XX))




# Detrend data
trendline = alpha*(1-exp(-beta*time))
y_reduced = y-trendline


# 
# 
# PLOT Original data and trendline
plot(time,y,type="l",col="blue")
lines(time,trendline, col="red") 

# PLOT detrended data in new window
plot(time,y_reduced,type="l",col="black")

######################################################################
### EXPONENTIAL GROWTH STANDALONE ##########################################################
### Ro Neumann, 2015 ###########################################################

### INPUT: 1. Time series with first column = time, 2nd  = data
###        2. Initial value for alpha --> value the curve converges to, determined from plot by user!
### OUTPUT: 1. "trendline" as one y-column --> to plot
###         2. Reduced data y_reduced --> to plot
###         3. X_hat with coefficients alpha & beta



exponentialDecay_function<-function (data, alpha) {

  # data=read.table("Exercise4_2.txt", sep=",") # Exponential growth

time = as.numeric(unlist(data[1]))
y = as.numeric(unlist(data[2]))

lengthX = length(time)

# Initial value for alpha -->cgraphically determined, at upper boundary
# alpha = 3.124


hlp = alpha-y[1]/alpha
hlp2 = log(hlp)
# beta = -(hlp2/ time[1])
beta = 0.0118
X_initial = c(alpha, beta)
X_0 = X_initial
maxInf = Inf
# is.infinite(maxInf)
delta = 10^-5
iteration = 0


while (maxInf > delta){

  a1 = 1-(exp(-beta*time))
  a2 = alpha * time * exp(-beta*time)
  A= cbind(a1,a2)
  # A= [1-exp(-beta*t) alpha*t.*exp(-beta*t)];
 # Reduced vector of observations
  l=y-alpha*(1-exp(-beta*time));
  N=t(A)%*%A  
  
  #n=(A'*y)
  n= solve(N); # DONT use N^-1!!
  x=n %*% (t(A) %*% l)


  # Update
  X_hat = X_0 + x
       X_0 = X_hat
       alpha = X_hat [1]
       beta = X_hat [2]
       maxInf = max(abs(x))
       iteration = iteration +1
}


r = length(y) -length(X_initial)

v = (A%*%x)-y

# Qxx = solve(N)
# vTPv = t(v) %*% v
# s_0_2 = vTPv/r
# s0 = s_0_2[1] # Due to numerical problem: is assigned as matrix, not scalar
# S_XX= s0 * Qxx
# sX = sqrt(diag(S_XX))




# Detrend data
trendline = alpha*(1-exp(-beta*time))
y_reduced = y-trendline


# 
# 
# PLOT Original data and trendline
plot(time,y,type="l",col="blue")
lines(time,trendline, col="red") 

# PLOT detrended data in new window
plot(time,y_reduced,type="l",col="black")

}

}



######################################################################
