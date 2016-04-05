# INTERPOLATION FUNCTION
interpolation_function<-function (data1, data2) {
# Takes 2 sets of data with x -> time column and y -> data column each.
  
  
# Called when the datasets for cross-correlation show differing time stamp intervals.
# E.g. 1 set measured with sampling rate of 128Hz, the second measured with 512Hz or similar.

# approx   (x, y = NULL, xout, method = "linear", n = 50,
#          yleft, yright, rule = 1, f = 0, ties = mean)
# Where x is data towards which you want y to be interpolated
# xout are the new time stamp values for the shorter time series.

  
  
# To manually call data:  
# data1=read.table("tls.txt", sep=" ") # 5912 entries
# data2=read.table("acceleration.txt", sep=" ") # 4577 entries


  
# Check which dataset is longer to avoid information loss

if (length(data1[1]) > length (data2[2])) {
# if first dataset is larger than second, use that to interpolate towards
  time1 = as.numeric(unlist(data1[1]))
  time2 = as.numeric(unlist(data2[1]))
  y1 = as.numeric(unlist(data1[2]))
  y2 = as.numeric(unlist(data2[2]))
  
} else {
# Else use time2 as to not loose information 
  time1 = as.numeric(unlist(data2[1]))
  time2 = as.numeric(unlist(data1[1]))
  y1 = as.numeric(unlist(data2[2]))
  y2 = as.numeric(unlist(data1[2]))
}


# Perform interpolation

y_interp = approx   (time2, y2, time1, method = "linear") 
# --> gives out a list of 2 columns, new time x and interpolated data y
time2 = as.numeric(unlist(y_interp[1]))
y2 = as.numeric(unlist(y_interp[2]))

# OUtpUT::: Shorter data series with new values y2 and time stamps time2 of longer series


# Code for plotting on two axis, if required
# Source: http://robjhyndman.com/hyndsight/r-graph-with-two-y-axes/

# par(mar=c(5,4,4,5)+.1)
# plot(time1,y2,type="l",col="red")
# par(new=TRUE)
# plot(time1, y1,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
  # axis(1) # only shows x and first y-axis
  # axis(2) # overlays first y-axis with second
  # axis(3) # would display a second x-axis up top
# axis(4) # would display a second x-axis up top
# mtext("y2",side=4,line=3)
# legend("topleft",col=c("red","blue"),lty=1,legend=c("y1","y2"))
}