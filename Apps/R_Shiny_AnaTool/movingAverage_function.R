### MOVING AVERAGE FUNCTION  ##########################################
### Ro Neumann, 2015 ###########################################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: 1: "mvg" as two-column matrix
###         2: "y_reduced"as two-column matrix
### Both result to be plotted & made available for data export



movingAverage_function<-function (time, data, tau) {
  
# Load test data
#data=read.table("Exercise5_1.txt", sep=",")

t = as.numeric(unlist(time))
y = as.numeric(unlist(data))



# Define filter height for moving average --> passed on to function from app!
#tau_array = [5, 10, 15, 20, 25];
#tau = 8


n = length(t);


#NOTES:::
# moduls x %%y

# MATRIX
# name[row,column]
# data[2,1] gives second entry of first column
# data[1,2] gives first entry of second column
# tail(data, n = -6L) gets columns from frist 6 elements onwards till end
# tail(data) getas last 6 elements by default
# tail(data, n = 1) gets very last element!

### CALCULATE MOVING AVERAGE

# EVEN CASE
modTest = tau %% 2

if(modTest == 0){
  m = n-tau
  mvg = matrix(0,nrow=m, ncol=2)
  last = n-(tau/2)
  first = tau/2+1
  move = t[first:last] 
  mvg[,1] = move
  w = matrix(1, nrow= tau+1, ncol =1)

  w[1] = 0.5
  w[tau+1] = 0.5
  range = n-tau
  
  for (i in 1:range){
    p=w/tau
    low = i+tau
    mvg[i,2]=sum (y[i:low]%*%p)
  }

  
  
  
# ODD CASE   
  } else {
     m=n-(tau-1)
     mvg = matrix(nrow=m, ncol=2)
     first = floor(tau/2)+1
     last = n-floor(tau/2)
     move = t[first:last]
     mvg[,1] = move
     range = n-(tau-1)
     
     for (i in 1:range){
     mvg[i,2]=mean (y[i:i+tau-1])
     }
  }

# REDUCING original signal by filtered signal
lengthMov = length(mvg[,1])



#Adapting length of old time series i norder ot subtract shorter movAvg column
#For EVEN case

if(modTest== 0){
  m=tau/2
  y_reduced = matrix(nrow=lengthMov, ncol=2)
  y_reduced[,1] = mvg[,1]
  up=m+1
  low=n-m
  y_reduced[,2] = y[up:low]-mvg[,2]

# For ODD case 
} else {
  m= floor(tau/2)
  y_reduced = matrix(nrow=lengthMov, ncol=2)
  y_reduced[,1] = mvg[,1]
  up=m+1
  low=n-m
  y_reduced[,2] =y[up:low]-mvg[,2]
}

# Create a grid to plot the different values of "a"
#par(mfrow=c(2,1))


# PLOT 1. Original data time vs y1 & time vs y2
# plot(t,y,type="l",col="blue")

# Plot Moving average on top of original data
# lines(mvg[,1],mvg[,2],col="red") #--> works!

# Plot de-trended data
plot(y_reduced[,1],y_reduced[,2],type="l",col="black", xlab ="time", ylab = "Data", main = "Moving average Plot")
lines(t,y,type="l",col="blue")
lines(mvg[,1],mvg[,2],col="red") #--> works!

}


