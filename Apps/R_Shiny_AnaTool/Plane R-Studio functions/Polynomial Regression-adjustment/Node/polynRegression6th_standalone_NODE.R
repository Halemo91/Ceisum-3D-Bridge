### LINEAR REGRESSION FUNCTION #################################################
### Ro Neumann, 2015 ###########################################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: trendline --> 1 column matrix, data values
###         y_detrended --> 1 column matrix, data values

# data=read.table("Exercise3-3.txt", sep=" ")
# t = as.numeric(unlist(data[1]))
# y = as.numeric(unlist(data[2]))


data = read.csv(file="Node383sm.csv", header = TRUE, skip = 15)
time = unlist(data[1])
index = as.numeric(unlist(data[2]))
Channel1 = as.numeric(unlist(data[3]))

time=toString(time) 
timeSplit = strsplit(time, ",") 
timeSplit = unlist(timeSplit) 
timeSplit = gsub(" ", "", timeSplit, fixed = TRUE)    # remove redundant spaces in data
### DISSECT HOURS MINUTES SECONDS

dateComp = substr(timeSplit, 1, 10)   # cut out date
timeComp = substr(timeSplit, 11, 28)  # cut out time

hours <- numeric(0)   # create empty arrays to be filled
minutes <-numeric(0)
seconds <- numeric(0)


lT = length(timeComp) # get array length for loop

for (i in 1:lT) {
  timex = timeComp[[i]]    # take first element of string vector
  h = substr(timex, 1, 2)  # Cut string
  h = as.numeric(h)        # convert to hour value
  hours[i]=h   # add UTC component
  
  m = substr(timex, 4, 5)
  m = as.numeric(m)
  minutes[i]=m 
  
  s = substr(timex, 7, 15) 
  s = as.numeric(s)
  seconds[i]=s
}



timeInSeconds  <- numeric(0) 

for (i in 1:lT) {
  timeInSeconds[i]= (hours[i]*60*60) + (minutes[i]*60)+seconds[i]
}

# y = as.numeric(unlist)
# write.table(timeInSeconds, "C:/Users/ro/Desktop/TU-WS14-15 - 2015-04-09/GIS_project/R_coding/R_Ro/timestamps.txt", sep="\t") 
# write.table(Channel1, "C:/Users/ro/Desktop/TU-WS14-15 - 2015-04-09/GIS_project/R_coding/R_Ro/Channel1.txt", sep="\t") 
# # 
# stamps=read.table("timestamps.txt", sep=" ")
# dats=read.table("Channel1.txt", sep=" ")
# t = as.numeric(unlist(stamps))
# y = as.numeric(unlist(dats))

# t = timeInSeconds/100000
t = timeInSeconds
subtr = 25000
t = t-subtr
# t = t^(1/6)
Channel1 = Channel1 
y = Channel1
y = y-0.1


lengthX = length(t)

A = matrix(1, nrow = lengthX, ncol = 7)

A [,1] = t^6
A[,2] = t^5
A[,3] = t^4
A[,4] = t^3
A[,5] = t^2
A[,6] = t

# Assumign P = I

# Tried: 1000, 10000, 100 000. 
# Dividing t while mutliplying y with 10, 100
# Normailzation of t, y and ty & y failed

# A matrix

# N = A'PA
N=t(A)%*%A  

#n=(A'*y)
n = t(A) %*% y

Qxx = solve(N) # Solves for t /100000 & t^6/1000!!

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

# x[1]=x[1]*1000
# Trendline
trendline= x[1]*t^6+x[2]*t^5+x[3]*t^4+x[4]*t^3+x[5]*t^2+x[6]*t+x[7];


# Detrending the data
y_detrended = y-trendline; 
t = t*1000


# PLOT Original data and trendline
plot(t,y,type="l",col="blue")
lines(t,trendline, col="red") 

# PLOT detrended data in new window
plot(t,y_detrended,type="l",col="black")

