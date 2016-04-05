### LINEAR REGRESSION STANDALONE ##########################################################
### Ro Neumann, 2015 ###########################################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: trendline --> 1 column matrix, data values
###         y_detrended --> 1 column matrix, data values


# data=read.table("Exercise3-3.txt", sep=" ")
# 
# t = as.numeric(unlist(data[1]))
# y = as.numeric(unlist(data[2]))


data = read.csv(file="Node383.csv", header = TRUE, skip = 15)
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

write.table(timeInSeconds, "C:/Users/ro/Desktop/TU-WS14-15 - 2015-04-09/GIS_project/R_coding/R_Ro/Singularity Problem/timestamps.txt", sep="\t") 
write.table(Channel1, "C:/Users/ro/Desktop/TU-WS14-15 - 2015-04-09/GIS_project/R_coding/R_Ro/Singularity Problem/Channel1.txt", sep="\t") 


trend3 <- lm(y~poly(t,3,raw=TRUE))
c1 = summary(trend3)$coefficients[1,1]
c2 = summary(trend3)$coefficients[2,1]
c3 = summary(trend3)$coefficients[3,1]
c4 = summary(trend3)$coefficients[4,1]

tr3 = c1 + c2*t + c3*t^2 + c4*t^3

y_detrended3 = y-tr3


# PLOT Original data and trendline
plot(t,y,type="l",col="blue")
lines(t,tr3, col="red") 

# PLOT detrended data in new window
plot(t,y_detrended3,type="l",col="black")
