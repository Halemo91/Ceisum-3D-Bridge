# INTERPOLATION FUNCTION

# This function is called when dataset for cross-correlation show differing time stamp intervals
# E.g. 1 set measured with sampling rate of 128Hz, the second measured with 512Hz or similar

# approx   (x, y = NULL, xout, method = "linear", n = 50,
#          yleft, yright, rule = 1, f = 0, ties = mean)
# Where x is data towards which you want y to eb interpolated
# xout is the new time stamp values

data1=read.table("tls.txt", sep=" ") # 5912 entries
data2=read.table("acceleration.txt", sep=" ") # 4577 entries

time1 = as.numeric(unlist(data1[1]))
time2 = as.numeric(unlist(data2[1]))
y1 = as.numeric(unlist(data1[2]))
y2 = as.numeric(unlist(data2[2]))

y_2interp = approx   (time2, y2, time1, method = "linear") 
# --> gives out a list of 2 columns, new time x and interpolated data y
time2 = as.numeric(unlist(y_2interp[1]))
y2 = as.numeric(unlist(y_2interp[2]))

plot(time2,y2, type="l", col="blue", lwd=1)
#plot(time2,y2,col="green", lwd=1)  #--> works!!! :D


# Plotting on two axis
# Source: http://robjhyndman.com/hyndsight/r-graph-with-two-y-axes/

par(mar=c(5,4,4,5)+.1)
plot(time1,y1,type="l",col="red")
par(new=TRUE)
plot(time2, y2,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
# axis(1) # only shows x and first y-axis
# axis(2) # overlays first y-axis with second
# axis(3) # would display a second x-axis up top
axis(4) # would display a second x-axis up top
mtext("y2",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("y1","y2"))
