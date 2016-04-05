### EXPONENTIAL GROWTH STANDALONE ##########################################################
### Ro Neumann, 2015 ###########################################################

### INPUT: 1. Time series with first column = time, 2nd  = data
###        2. Initial value for alpha --> value the curve converges to, determined from plot by user!
### OUTPUT: 1. trendline as y-values
###         2. Reduced data as y-values
###         3. X_hat with coefficients alpha & beta


data=read.table("Exercise4_1.txt", sep=",") # Exponential growth

time = as.numeric(unlist(data[1]))
y = as.numeric(unlist(data[2]))

L = y
lengthX = length(time)

# Initial value for alpha --> y-axis offset; graphically determined for x=0
alpha = 3.124


# Initial value for beta --> rearranged functional model
hlp = log(y[1]/alpha)
beta = hlp/ time[1]

X_initial = c(alpha, beta)
X_0 = X_initial

maxInf = Inf
delta = 10^-6
iteration = 0


while (maxInf > delta){

  
  a1 = exp(beta*time)
  a2 = alpha * time * exp(beta*time)
  A= cbind(a1,a2)

    # Observations for reduction
  L_0 = alpha*exp(beta*time)
  
  # Reduced vector of observations
  l= L-L_0


  N=t(A)%*%A  
  n= t(A)%*%l
  Qxx=solve(N)
  x_hat = Qxx%*%n
  
  # Adjust unknowns
  X_hat = X_0 + x_hat
  
  # Update
  X_0 = X_hat
  alpha = X_hat [1]
  beta = X_hat [2]
  maxInf = max(abs(x_hat))
  iteration = iteration +1
  
}  

r = length(y) -length(X_initial)

v = (A%*%x_hat)-y

# Qxx = solve(N)
# vTPv = t(v) %*% v
# s_0_2 = vTPv/r
# s0 = s_0_2[1] # Due to numerical problem: is assigned as matrix, not scalar
# S_XX= s0 * Qxx
# sX = sqrt(diag(S_XX))

# Detrend data
trendline = alpha*exp(beta*time)
y_reduced = y-trendline


# PLOT Original data and trendline
plot(time,y,type="l",col="blue")
lines(time,trendline, col="red") 

# PLOT detrended data in new window
plot(time,y_reduced,type="l",col="black")

######################################################################
