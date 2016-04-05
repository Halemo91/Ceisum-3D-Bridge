### LINEAR REGRESSION FUNCTION ##########################################################
### Ro Neumann, 2015 ###########################################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: trendline --> 1 column matrix, data values
###         y_detrended --> 1 column matrix, data values


linearRegression_function<-function (data) {

# data=read.table("Exercise3-1.txt", sep=" ")

t = as.numeric(unlist(data[1]))
y = as.numeric(unlist(data[2]))

lengthX = length(t)

#entries = matrix(1,nrow = length(t), ncol = 1)

A = matrix(1, nrow = lengthX, ncol = 2)

A [,1] = t

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

#s_0_a=sqrt(vTPv_a/r);
s_0_2 = vTPv/r
s0 = s_0_2[1] # numerical problem, is assigned as matrix, not scalar
S_XX= s0 * Qxx
sX = sqrt(diag(S_XX))

# Trendline
trendline= x[1]*t+x[2];


# Detrending the data
y_detrended = y-x[1]*t-x[2]; 


# PLOT Original data and trendline
plot(t,y,type="l",col="blue")
lines(t,trendline, col="red") #--> works!


# PLOT detrended data in new window
plot(t,y_detrended,type="l",col="black")

}
