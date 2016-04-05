### LINEAR REGRESSION STANDALONE ##########################################################
### Ro Neumann, 2015 ###########################################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: trendline --> 1 column matrix, data values
###         y_detrended --> 1 column matrix, data values


#data=read.table("Exercise3-1.txt", sep=" ")

data = read.csv(file="C:/users/akshara/Desktop/analysis with R/Node383.csv", header = TRUE, nrow = 500)# read csv file 

date = as.character(lapply(strsplit(as.character(data[,1]), split=" "), "[", 1))
hms = as.character(lapply(strsplit(as.character(data[,1]), split=" "), "[", 2))
a = as.character(lapply(strsplit(as.character(hms), split="+"), "[", 1))
b = as.character(lapply(strsplit(as.character(hms), split="+"), "[", 2))
h = as.numeric(lapply(strsplit(hms, split=":"), "[", 1))
m = as.numeric(lapply(strsplit(hms, split=":"), "[", 2))
s = as.numeric(lapply(strsplit(hms, split=":"), "[", 3))

time = NULL
for(i in 1:length(data[,1]))
{
  time[i] = h[i]*3600 + m[i]*60 + s[i]
}

t=time
#t = as.numeric(unlist(data[1]))
y = as.numeric(unlist(data[3]))

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
s_0_2 = vTPv/r
s0 = s_0_2[1] # Due to numerical problem: is assigned as matrix, not scalar
S_XX= s0 * Qxx
sX = sqrt(diag(S_XX))


# Trendline
trendline= x[1]*t+x[2];


# Detrending the data
y_detrended = y-x[1]*t-x[2]; 

# Create a grid to plot the different values of "a"
par(mfrow=c(2,1))

# PLOT Original data and trendline
plot(t,y,type="l",col="blue")
lines(t,trendline, col="red") 

# PLOT detrended data in new window
plot(t,y_detrended,type="l",col="black")
