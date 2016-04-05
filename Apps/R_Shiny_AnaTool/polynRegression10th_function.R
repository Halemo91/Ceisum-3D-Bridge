### LINEAR REGRESSION STANDALONE ##########################################################
### Ro Neumann, 2015 ###########################################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: trendline --> 1 column matrix, data values
###         y_detrended --> 1 column matrix, data values


polynRegression10th_function<-function (time, data) {
  
#data=read.table("Exercise3-3.txt", sep=" ")
  
t = as.numeric(unlist(time))
y = as.numeric(unlist(data))

lengthX = length(t)

#entries = matrix(1,nrow = length(t), ncol = 1)

A = matrix(1, nrow = lengthX, ncol = 11)

A [,1] = t^10
A[,2] = t^9
A[,3] = t^8
A[,4] = t^7
A[,5] = t^6
A[,6] = t^5
A[,7] = t^4
A[,8] = t^3
A[,9] = t^2
A[,10] = t

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
trendline= x[1]*t^10+x[2]*t^9+x[3]*t^8+x[4]*t^7+x[5]*t^6+x[6]*t^5+x[7]*t^4+x[8]*t^3+x[9]*t^2+x[10]*t+x[11];


# Detrending the data
y_detrended = y-trendline; 

# par(mfrow=c(2,1))

# PLOT Original data and trendline
# plot(t,y,type="l",col="blue")
# lines(t,trendline, col="red") 

# PLOT detrended data in new window
plot(t,y_detrended,type="l",col="black", xlab ="time", ylab = "Data", main = "Polynomial Regression Of 10th Order")
lines(t,y,type="l",col="blue")
lines(t,trendline, col="red") 
}