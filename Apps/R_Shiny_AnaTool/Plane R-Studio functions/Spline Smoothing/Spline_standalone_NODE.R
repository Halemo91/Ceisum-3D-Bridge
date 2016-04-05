### SPLINE-Interpolation  ####################################
### Ro Neumann, 2015 #########################################

### INPUT: 1. Time series with first column = time, 2nd  = data
###        2. Initial value for alpha --> value the curve converges to, determined from plot by user!
### OUTPUT: 1. trendline as y-values
###         2. Reduced data as y-values
###         3. X_hat with coefficients alpha & beta


# data=read.table("Exercise3-3.txt", sep=" ") # Exponential growth
# 
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
# write.table(timeInSeconds, "C:/Users/ro/Desktop/TU-WS14-15 - 2015-04-09/GIS_project/R_coding/R_Ro/Singularity Problem/timestamps.txt", sep="\t") 
# write.table(Channel1, "C:/Users/ro/Desktop/TU-WS14-15 - 2015-04-09/GIS_project/R_coding/R_Ro/Singularity Problem/Channel1.txt", sep="\t") 


t = timeInSeconds

y = Channel1

dof = length(y)


#Run spline
ppp <- smooth.spline(data, y = NULL, w = NULL, dof, spar = NULL)

# Get x & y values
splinex = unlist(ppp[1])
spliney = unlist(ppp[2])


# Detrend data
# y_reduced = y-spliney


# PLOT Original data and trendline
plot(t,y,type="l",col="blue")

plot(splinex,spliney, type="l",col="red")

# PLOT detrended data in new window
# plot(t,y_reduced,type="l",col="black")

######################################################################
