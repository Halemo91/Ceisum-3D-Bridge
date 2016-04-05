### STATIONARITY TEST ##########################################################
### Ro Neumann, 2015 ###########################################################

### INPUT: Time series with first column = time, 2nd 3rd & 4th column = data
### OUTPUT: 3 lists: text1 --> Number of tested intervals
###                  text2 --> Number of intervals with differing mean
###                  text3 --> Assessment, stationary or non-stationary

### PROCEDURE:
### 1. Partitions it into a number of intervals depending on the amount of cell entries
### 2. Performs 2-sample Student-test to compare the means of each interval to the first
### 3. If one differs from the first --> Not stationary & vice versa




# Loading test data from txt, space-delimited
#data=read.table("Exercise1-1.txt", sep=" ")
data = read.csv(file="C:/users/akshara/Desktop/analysis with R/Node383.csv", header = TRUE, nrow = 500)# read csv file 

# Set time and data columns
x= as.numeric(unlist(data[3]))
# y= as.numeric(unlist(data[2])) # non-stationary
# y= as.numeric(unlist(data[3])) # non-stationary
# y= as.numeric(unlist(data[4])) # stationary

# Set array length
n = length(y)

# Define number of subarrays depending on number of measurements
if (n <= 50){
div = 3;
} else if (n > 50 && n<=200){
  div = 4;
} else if (n>200 && n<=2000){
  div = 6;
} else if (n>2000 && n<= 5000){
  div = 100;
} else {
  div = 1000;
}


# Divide data column into sub arrays
sub = round(n/div)


# Create vector with intervals
numbers = matrix();
for (i in 1:div){
numbers[i]= sub*i;
}

numbers=c(1, numbers)


# Initialize matrix for storing interval value
columns = matrix(nrow=sub+1, ncol = length(numbers))

# Initialize interval to avoid override in loop --> top boundary of array
interval = 1


for (i in 1:length(numbers)-1){
  interval2 = interval + sub; # set lower boundary of array --> 251 values
  t=y[interval:interval2];  # cut part out of  data column y
  columns [,i]=t; # fill interval into columns matrix
  i = i+1; #increment i
  interval = interval + sub; # set new upper boundary for next interval to be cut out
}

# Exclude half-filled columns that contain NA-entries
colLength = length(columns[1,])

for (i in colLength:1){
  check = mean(columns[,i])
  if (is.na(check)){
    columns<-columns[,-i]
  }
}

# Re-define column length in case some were removed
colLength = length(columns[1,])


#Perform 2-Sample T-test: Comparing interval contents with the first column
# h = 1 --> hyp rejected, series is not stationary
# ttest is by default two-tailed in R!

xxx= matrix()
counter = 0;

for (i in 1:colLength){

  xxx=t.test(columns[,1], columns[,i])
  #standardDev = sd(x, na.rm = FALSE) not needed right now
  testStatistic = xxx$statistic # get test statistic from
  confIntervals = qt(c(.025, .975), df=length(columns[,1]))
  
 # If testStatistic within confIntervals --> accepted --> stationary 1, else not stationary
    if (testStatistic >= confIntervals[1] && testStatistic <= confIntervals[2] ){
        counter = counter+1
    }
}


### Create list output for display in Shiny
diff = colLength-counter
text1 <- c('Intervals tested:',div)
text2 <- c('Number of intervals significantly different from the first:',diff)
if (counter == colLength){
    text3 = 'Time series is stationary.'
  }else{
    text3 = 'Time series is not stationary.'
  }

###############################################################################