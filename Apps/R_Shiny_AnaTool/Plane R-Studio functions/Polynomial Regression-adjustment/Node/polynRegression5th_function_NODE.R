### LINEAR REGRESSION FUNCTION ########################################
### Ro Neumann, 2015 ##################################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: trendline --> 1 column matrix, data values
###         y_detrended --> 1 column matrix, data values



polynRegression5th__function<-function (data) {
  
# data=read.table("Exercise3-3.txt", sep=" ")

t = as.numeric(unlist(data[1]))
y = as.numeric(unlist(data[2]))
t = time/100000 # For numeric stability! Time stamps -> 32000, measurements -> 0.005 etc


lengthX = length(t)

#entries = matrix(1,nrow = length(t), ncol = 1)

A = matrix(1, nrow = lengthX, ncol = 6)

A [,1] = t^5
A[,2] = t^4
A[,3] = t^3
A[,4] = t^2
A[,5] = t
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
trendline= x[1]*t^5+x[2]*t^4+x[3]*t^3+x[4]*t^2+x[5]*t+x[6];


# Detrending the data
y_detrended = y-trendline; 
t = t*100000 # Reconvert, numeric stability issue from above


# PLOT Original data and trendline
plot(t,y,type="l",col="blue")
lines(t,trendline, col="red") 

# PLOT detrended data in new window
plot(t,y_detrended,type="l",col="black")


}