### 7th Polynomial regression STANDALONE #######################
### Ro Neumann, 2015 ###########################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: tr7 = trendline --> 1 column matrix, data values
###         y_detrended7 --> 1 column matrix, data values


# data=read.table("Exercise3-3.txt", sep=" ")
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


t = timeInSeconds
y = Channel1







trend7  <- lm(y~poly(t,7,raw=TRUE))
g1 = summary(trend7)$coefficients[1,1]
g2 = summary(trend7)$coefficients[2,1]
g3 = summary(trend7)$coefficients[3,1]
g4 = summary(trend7)$coefficients[4,1]
g5 = summary(trend7)$coefficients[5,1]
g6 = summary(trend7)$coefficients[6,1]
# g7 = summary(trend7)$coefficients[7,1] # out of bounds
# g8 = summary(trend7)$coefficients[8,1] # out of bounds

tr7 = g1 + g2*t + g3*t^2 + g4*t^3 + g5*t^4 + g6*t^5 #+ g7*t^6 #+ g8*t^7

y_detrended7 = y-tr7


# PLOT Original data and trendline
plot(t,y,type="l",col="blue")
lines(t,tr7, col="red") 

# PLOT detrended data in new window
plot(t,y_detrended7,type="l",col="black")
