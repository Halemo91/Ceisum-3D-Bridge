# CROSS-CORRELATION FUNCTION - real data
# Takes 2 sets of data with time and data column *each*
crossCorr_function<-function (data1, data2, chosenLag) {


# # load data from txt, space-delimited
# data1=read.table("tls.txt", sep=" ")
# data2=read.table("ibis.txt", sep=" ")
# # --> tab delimited would be: ex2txt=read.table("Exercise2-1.txt", sep="\t")


# Compare time stamps of both datasets
stamptest = data1[1]-data2[1]
testsum = sum (stamptest, na.rm = FALSE)


# If time stamps not equal .. ( CAREFUL: Position of else and curly brackets is vital!!!!)
if (testsum > 0) {
  stop('Time stamps are not of equal intervals! Interpolation required, calculation stopped.')
} else {
  print("Time stamps equal, calculation proceeded normally.")
  
  
  
  
  
  # If they *are* equal though, proceed with calculations ....
  
  # Set columns of time data
  time = as.numeric(unlist(data1[1]))
  y1 = as.numeric(unlist(data1[2]))
  y2 = as.numeric(unlist(data2[2]))
  
  
  
  ##### PERFORMING CROSS CORRELATION
  
  # Set column length
  lengthTime = length(time)
  
  # Define time lag tau
  tau = round((lengthTime/chosenLag), digits = 0)
  
  # Get sampling frequency
  lastValueOfTime = tail(time, n=1)
  # Also, first value of array: firstValueOftime = head (x, n=1)
  # changing n will determine how many entries are grabbed from the top 
  # or bottom of an array!
  
  # Calculate sampling frequency --> how many measurements per unit time
  sampleF = lengthTime/lastValueOfTime
  
  # Create a grid to plot the different values of "a"
  par(mfrow=c(2,2))
  
  # Calculate cross correlation with fixed tau 
  # Maybe we should think about allowing user to decide between value or default length/4.
  crosscorrel = ccf(y1,y2,tau) # is working fine (y)
  
  # --> output of ccf is a LIST (!) of 6: 1st column-> correlation values, 2nd -> character types,
  #   3rd-> length, 4th-> lags, 5th-> time column, 6th-> the two original input datasets
  
  # Get correlation values in extra column
  ccvalues = crosscorrel[[1]]
  
  # Get time lags from ccf output n extra column
  ccLags = crosscorrel[[4]] 
  
  
  ##### FINDING TIME SHIFT between original and correlation values
  
  # Get max to have a basis to which one can "attach" the time shift
  maximum = max(abs(ccvalues))
 # minimum = min(ccvalues)
  # Find index of max correlation value, so that we can find associated (integer) time lag
  indMax = match(maximum, abs(ccvalues), nomatch = NA_integer_, incomparables = NULL)
 # indMin = match(minimum, ccvalues, nomatch = NA_integer_, incomparables = NULL)
  
  #indMax = 1529
  # Get time difference --> Make actual time value of according time lag  
  delta_t = ccLags [[indMax]]/sampleF;
  
  # Convert from list to numeric to allow math operations
  sampleF = as.numeric(sampleF) # convert from object to numeric
  cctimeNum = as.numeric(ccLags)
  
  # For printing of corss-corr. function, re-calculate the actual times for 
  # the integer lags of cross-corr 
  ccLagsPrint = cctimeNum/sampleF
  
  # Shift the time series according to calculated delta_t
  time_y1_shifted = as.numeric(unlist(time)) - as.numeric(delta_t)
  
  
  # PLOT 1. Original data time vs y1 & time vs y2
  plot(time,y1,type="l",col="blue")
  lines(time,y2,col="black") #--> works!
  
  
  # PLOT 2. Cross-correlation function - ccPrint on x-axis, ccvalues on y-axis
  plot(ccLagsPrint,ccvalues,type="l",col="red") # --> works awesomely!!!
  
  
  # PLOT 3. Time series 2 and shift-corrected time series 1
  plot(time,y1, type="l", col="blue", lwd=1)
  lines(time_y1_shifted,y2,col="green", lwd=1)  #--> works!!! :D
  
} }
