### SPLINE-Interpolation  ####################################
### Ro Neumann, 2015 #########################################

### INPUT: 1. Time series with first column = time, 2nd  = data
###        2. Weight --> one is default
###        3. dof --> degrees of freedom
###        4. Smoothing -->usually between 0 and 1
### OUTPUT: Spline curve

spline_function<-function (data, dof, weight, smoothing) {
  
dataNum = as.numeric(unlist(data))


# dof = length(y)


#Run spline
ppp <- smooth.spline(dataNum, y = NULL, w = weight, dof, spar = smoothing)

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

}
######################################################################
