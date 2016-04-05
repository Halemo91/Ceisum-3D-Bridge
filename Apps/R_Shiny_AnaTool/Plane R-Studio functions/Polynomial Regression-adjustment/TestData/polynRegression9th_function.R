### LINEAR REGRESSION FUNCTION ########################################
### Ro Neumann, 2015 ###########################################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: trendline --> 1 column matrix, data values
###         y_detrended --> 1 column matrix, data values


polynRegression9th__function<-function (data) {
  
# data=read.table("Exercise3-3.txt", sep=" ")

t = as.numeric(unlist(data[1]))
y = as.numeric(unlist(data[2]))

lengthX = length(t)

#entries = matrix(1,nrow = length(t), ncol = 1)

A = matrix(1, nrow = lengthX, ncol = 10)

A [,1] = t^9
A[,2] = t^8
A[,3] = t^7
A[,4] = t^6
A[,5] = t^5
A[,6] = t^4
A[,7] = t^3
A[,8] = t^2
A[,9] = t

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
trendline= x[1]*t^9+x[2]*t^8+x[3]*t^7+x[4]*t^6+x[5]*t^5+x[6]*t^4+x[7]*t^3+x[8]*t^2+x[9]*t+x[10];


# Detrending the data
y_detrended = y-trendline; 


# PLOT Original data and trendline
plot(t,y,type="l",col="blue")
lines(t,trendline, col="red") 

# PLOT detrended data in new window
plot(t,y_detrended,type="l",col="black")

}
