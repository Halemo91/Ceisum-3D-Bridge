### LINEAR REGRESSION FUNCTION #################################################
### Ro Neumann, 2015 ###########################################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: trendline --> 1 column matrix, data values
###         y_detrended --> 1 column matrix, data values


polynRegression2nd_function<-function (time, data) {

# data=read.table("Exercise3-2.txt", sep=" ")

time = as.numeric(unlist(time))
t = time/10000 # For numeric stability! Time stamps -> 32000, measurements -> 0.005 etc


y = as.numeric(unlist(data))

lengthX = length(t)

#entries = matrix(1,nrow = length(t), ncol = 1)

A = matrix(1, nrow = lengthX, ncol = 3)

A [,1] = t^2
A[,2] = t

# Assumign P = I


# N = A'PA
N=t(A)%*%A  

#n=(A'*y)
n = t(A) %*% y

Qxx = solve(N)

#x=Qxx*n

x=Qxx%*%n

# Redundancy
r = length(y)-length(x)

#Residuals
v = (A%*%x)-y

# St.dev. of unknowns from  vtv, s_0, S_XX
vTPv = t(v) %*% v
s_0_2 = vTPv/r
s0 = s_0_2[1] # Due to numerical problem: is assigned as matrix, not scalar
S_XX= s0 * Qxx
sX = sqrt(diag(S_XX))


# Trendline
trendline= x[1]*t^2+x[2]*t+x[3];


# Detrending the data
y_detrended = y-trendline; 
t = t*10000 # Reconvert, numeric stability issue from above

# par(mfrow=c(2,1))
# PLOT Original data and trendline
# plot(t,y,type="l",col="blue")
# lines(t,trendline, col="red") 

# PLOT detrended data in new window
plot(t,y_detrended,type="l",col="black", xlab ="time", ylab = "Data", main = "Polynomial Regression Of 2nd Order")
lines(t,y,type="l",col="blue")
lines(t,trendline, col="red") 


}