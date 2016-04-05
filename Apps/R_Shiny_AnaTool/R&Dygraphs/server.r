library(shiny)
library(ggplot2)
library(TTR)
library(httr)
library(dygraphs)
library(datasets)
library(xts)
library(plyr)


# sensor = "Node_383"
# channels = "accelerationx,accelerationy,accelerationz,air:temperature"
# time_start = "2014-11-23T07:56:48.006750000+0100"
# time_end = "2014-11-25T07:57:00.256750000+0100"
# 
# url <- paste0("http://quader.igg.tu-berlin.de/istsos/bridgedemoservice?service=SOS&request=GetObservation&offering=temporary&procedure=",sensor,"&observedProperty=",channels,"&responseFormat=text/plain&version=1.0.0&",time_start,"/",time_end)
# dataset1 <- read.csv(file=url, header = TRUE)# read csv fil

dataset1<- read.csv(file="Node383.csv", header = TRUE, stringsAsFactors = F)  # read csv file 
dataset2 <- read.csv(file="Node384.csv", header = TRUE, skip=16,stringsAsFactors = F )  # read csv file 
dataset3 <- read.csv(file="Node573.csv", header = TRUE, skip=16,stringsAsFactors = F )  # read csv file 
dataset4 <- read.csv(file="Node574.csv", header = TRUE, skip=15,stringsAsFactors = F)  # read csv file
#
#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------
dataset1$Timestamp..GMT. <- sub("\\.\\d*$","",dataset1$Timestamp..GMT.)
dataset2$Timestamp..GMT. <- sub("\\.\\d*$","",dataset2$Timestamp..GMT.)
dataset3$Timestamp..GMT. <- sub("\\.\\d*$","",dataset3$Timestamp..GMT.)
dataset4$Timestamp..GMT. <- sub("\\.\\d*$","",dataset4$Timestamp..GMT.)
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
dataset1.summary <- ddply(dataset1, "Timestamp..GMT.", summarise,
                          chan1 = (Channel.1),
                          chan2 = (Channel.2),
                          chan3 = (Channel.3),
                          chan4 = (Channel.4))

dataset2.summary <- ddply(dataset2, "Timestamp..GMT.", summarise,
                          chan1 = (Channel.1),
                          chan2 = (Channel.2),
                          chan3 = (Channel.3),
                          chan4 = (Channel.4))

dataset3.summary <- ddply(dataset3, "Timestamp..GMT.", summarise,
                          chan1 = (Channel.1),
                          chan2 = (Channel.2),
                          chan3 = (Channel.3),
                          chan8 = (Channel.8))

dataset4.summary <- ddply(dataset4, "Timestamp..GMT.", summarise,
                          chan1 = (Channel.1),
                          chan2 = (Channel.2),
                          chan8 = (Channel.8))
#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
# Convert first column to ts:
z <- strptime(dataset1.summary$Timestamp..GMT., "%d/%m/%Y %H:%M:%OS")
dataset1.summary$Timestamp..GMT. <- z # replace data frame info

y <- strptime(dataset2.summary$Timestamp..GMT., "%d/%m/%Y %H:%M:%OS")
dataset2.summary$Timestamp..GMT. <- y # replace data frame info

x <- strptime(dataset3.summary$Timestamp..GMT., "%d/%m/%Y %H:%M:%OS")
dataset3.summary$Timestamp..GMT. <- x # replace data frame info

w <- strptime(dataset4.summary$Timestamp..GMT., "%d/%m/%Y %H:%M:%OS")
dataset4.summary$Timestamp..GMT. <- w # replace data frame info
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
# Make individual time series:
test1 <- xts(dataset1.summary$chan1, dataset1.summary$Timestamp..GMT.)
test2 <- xts(dataset1.summary$chan2, dataset1.summary$Timestamp..GMT.)
test3 <- xts(dataset1.summary$chan3, dataset1.summary$Timestamp..GMT.)
test4 <- xts(dataset1.summary$chan4, dataset1.summary$Timestamp..GMT.)

test21 <- xts(dataset2.summary$chan1, dataset2.summary$Timestamp..GMT.)
test22 <- xts(dataset2.summary$chan2, dataset2.summary$Timestamp..GMT.)
test23 <- xts(dataset2.summary$chan3, dataset2.summary$Timestamp..GMT.)
test24 <- xts(dataset2.summary$chan4, dataset2.summary$Timestamp..GMT.)

test31 <- xts(dataset3.summary$chan1, dataset3.summary$Timestamp..GMT.)
test32 <- xts(dataset3.summary$chan2, dataset3.summary$Timestamp..GMT.)
test33 <- xts(dataset3.summary$chan3, dataset3.summary$Timestamp..GMT.)
test34 <- xts(dataset3.summary$chan8, dataset3.summary$Timestamp..GMT.)

test41 <- xts(dataset4.summary$chan1, dataset4.summary$Timestamp..GMT.)
test42 <- xts(dataset4.summary$chan2, dataset4.summary$Timestamp..GMT.)
test48 <- xts(dataset4.summary$chan8, dataset4.summary$Timestamp..GMT.)


#---------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------


##---end of functions-------
## Define server logic required to generate and plot a random distribution
shinyServer(function(input,output) {

  ##---functions----------
  linear_regression <- function(x,y){
    plot(y ~ x, xlab="acceleration.x",ylab ="acceleration.y", main="linear regression plotting")
    model = lm(y ~ x)
    abline(lm(y ~ x),col=2,lwd=2)
    return (model)
 }
  

  #dygraph(test, main = "Node383",) %>% 
  
#--datset chooseing--

dataset <- reactive({
  if(input$radio == "Node 383")
    #dataset1 <- cbind(test1,test2,test3,test4)
    data <- dataset1
  
  else if(input$radio == "Node 384")
   # dataset2<- cbind(test21,test22,test23,test24)
    data <- dataset2
   
  
  else if(input$radio == "Node 573")
    #dataset3<- cbind(test31,test32,test33,test34)
       data <- dataset3
    
  else if(input$radio == "Node 574")
    # dataset4 <- cbind(test41,test42,test43,test44)
      data <- dataset4
     
  else("Error selecting a Node")
      })
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
myData <- function(){
  if(input$radio == "Node 383"){
    test <- cbind(test1,test2,test3,test4)
  }
  else if(input$radio == "Node 384") {
    test <- cbind(test21,test22,test23,test24)
  }
  
  else if(input$radio == "Node 573")
    test <- cbind(test31,test32,test33,test34)
  
 else if(input$radio == "Node 574")
    test <- cbind(test41,test42)
  #else if
  
  else("Error selecting a Node")
}


#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
time<- reactive({
  timestamp = dataset()[1:input$samplesize,1]
  date = as.character(lapply(strsplit(as.character(timestamp), split=" "), "[", 1))
  hms = as.character(lapply(strsplit(as.character(timestamp), split=" "), "[", 2))
  h = as.numeric(lapply(strsplit(hms, split=":"), "[", 1))
  m = as.numeric(lapply(strsplit(hms, split=":"), "[", 2))
  s = as.numeric(lapply(strsplit(hms, split=":"), "[", 3))
  
  time_sec = NULL
  for(i in 1:length(timestamp))
  {
    time_sec[i] = h[i]*3600 + m[i]*60 + s[i]
  }
  time_sec
#   
#   date = as.character(lapply(strsplit(as.character(timestamp), split="T"), "[", 1))
#   hms = as.character(lapply(strsplit(as.character(timestamp), split="T"), "[", 2))
#   h = as.numeric(lapply(strsplit(hms, split=":"), "[", 1))
#   m = as.numeric(lapply(strsplit(hms, split=":"), "[", 2))
#   # s = as.numeric(lapply(strsplit(hms, split=":"), "[", 3)) # gave NAs introduced by coercion 
#   
#   # Cutting characters manually
#   s = as.numeric(substr(hms, 7, 15)) # characters 7 to 15
#   
#   # Gets UTC component, also cut manually
#   utcComp = as.numeric(substr(hms, 17, 18)) 
#   
#   h=h+utcComp # adding UTC to hours value
#   
#   # Filling time vector
#   time_sec = NULL
#   for(i in 1:length(timestamp))
#   {
#     time_sec[i] = h[i]*3600 + m[i]*60 + s[i]
#   }
#   time_sec
})

channel1<- reactive({
 dataset()$Channel.1
})

channel2<- reactive({
  dataset()$Channel.2
})

channel3<- reactive({
  dataset()$Channel.3
})

channel4<- reactive({
  dataset()$Channel.4
})
#----datset---------    
output$plot <- renderPlot(function(){

  p <- ggplot(dataset(),aes_string(x=input$x, y=input$y))+geom_point()

  if(input$color != 'None')
    p <- p + aes_string(color=input$color)

  if (input$shape != 'None')
    p <- p + aes_string(shape=input$shape)

  facets <- paste(input$facet_row, '~', input$facet_col)

  if (facets != '. ~ .')
    p <- p + facet_grid(facets)

  if (input$jitter)
    p <- p + geom_jitter()

  if (input$smooth)
    p <- p + geom_smooth()


  print(p)

})
#--------summary tab-----------------
output$dataname<- renderText({
  paste("summary of dataset", input$radio)
})

output$summary <- renderPrint({ 
  summary(dataset()[,3:6]) 
})

#-------end of summary-------------
#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------
#---------------------Plot Tab--------------------------
output$plot <- renderDygraph({ 
  print(summary(myData()))
  dygraph(myData()) %>% 
    dyRoller(rollPeriod = 10)%>% 
    dyOptions(drawPoints = TRUE, pointSize= 1.3  ,colors = RColorBrewer::brewer.pal(4, "Set2"),)%>%
    dyLegend(width= 600)%>%
    dySeries("..1", axis = "y" , label = "Accelation-x")%>%
    dySeries("..2", axis = "y" , label = "Accelation-y")%>%
    dySeries("..3", axis = "y" , label = "Accelation-z")%>%
    dySeries("..4", axis = "y2" , label = "Temperature")%>%
    dyAxis("y", label = "Accelation-x y, and z")%>%
    dyAxis("y2", label = "Temperature")%>%
    dyRangeSelector()
})


#output$plot1 <- renderDygraph({
#  test2 <- cbind(test21,test22,test23,test24)
#  dygraph(test2, main = "Node384",)%>%
#    dyRangeSelector()
#})

#output$plot2 <- renderDygraph({
#  test3 <- cbind(test31, test32)
#  dygraph(test3, main = "Sensor 3",) %>% 
#    dyRangeSelector()
#})

#output$plot3 <- renderDygraph({
#  test4 <- cbind(test41, test42)
#  dygraph(test4, main = "Sensor 4",) %>% 
 #   dyRangeSelector()
#})


#----------------- end of Plot Tab----------------------
#--------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------

#--------data tab----------------
output$datadf <- renderTable({ 
  dataset()
})
output$data <- renderTable({ 
  head(get(dataset()), n=input$samplesize)
})
#-----end of data------------------

#--------statistic tab----------------
output$samplesize<- renderText({
  paste(input$samplesize, "observations of dataset", input$radio)
})

output$stats <- renderPrint({ 
  summary(dataset()[input$samplesize,3:6]) 
})
#-----end of statistic------------------

#----similarity tab--------
output$plot_acf <- renderPlot({
data<-channel1()[1:input$samplesize]
 ac_res <- acf(data,length(data)/input$acf_lag, type = input$acf_options, na.action = na.pass, col="red") # type should be "correlation", "covariance", "partial"
})
#output$plot_acf <- renderDygraph({
 # data<-test1()[1:input$samplesize]
 # gygraph(ac_res <- acf(data,length(data)/input$acf_lag, type = input$acf_options, na.action = na.pass, col="red")) # type should be "correlation", "covariance", "partial"
#})

output$plot_pacf <- renderPlot({
 source('crossCorr_standAlone.r')
})

output$plot_ccf <- renderPlot({
data1= data.frame(time()[1:input$samplesize], channel1()[1:input$samplesize])
data2= data.frame(time()[1:input$samplesize], channel2()[1:input$samplesize])
source('crossCorr_function.r')
crossCorr_function(data1, data2, input$ccf_lag)
})

output$datadf <- renderTable({ 
  data1<- data.frame(time()[1:input$samplesize], dataset()$Channel.1[1:input$samplesize])
  data2<- data.frame(time()[1:input$samplesize], dataset()$Channel.2[1:input$samplesize])
  data1
})
#-----end of similarity tab----

##----test tab-------
output$info<- renderText({
  paste("Information",br(),
        "The selected sensor is",names(dataset()),"and the selected channel is",input$radio)
})
output$str <- renderPrint({
  channel1<-dataset()$Channel.1
  channel2<-dataset()$Channel.2
  a <- linear_regression(channel1,channel2)
  summary(a)
})
output$plot_lm <- renderPlot({
#   channel1<-dataset()$Channel.1[1:input$samplesize]
#   channel2<-dataset()$Channel.2[1:input$samplesize]
#   linear_regression(channel1,channel2)  
  if(input$reg_options == "LM")
    source('REGRESSIONS/linearRegression_standalone.r')
  else if(input$reg_options == "PR2")
    source('REGRESSIONs/linearRegression_standalone.r')
  else if(input$reg_options == "PR3")
    source('REGRESSIONs/linearRegression_standalone.r')
  else if(input$reg_options == "PR4")
    source('REGRESSIONs/linearRegression_standalone.r')
  else if(input$reg_options == "PR5")
    source('REGRESSIONs/linearRegression_standalone.r')
  else if(input$reg_options == "PR6")
    source('REGRESSIONs/linearRegression_standalone.r')
  else if(input$reg_options == "PR7")
    source('REGRESSIONs/linearRegression_standalone.r')
  else if(input$reg_options == "PR8")
    source('REGRESSIONs/linearRegression_standalone.r')
  else if(input$reg_options == "PR9")
    source('REGRESSIONs/linearRegression_standalone.r')
  else if(input$reg_options == "PR10")
    source('REGRESSIONs/linearRegression_standalone.r')
}) 

st_test <- eventReactive(input$stat_check, {
  source('stationarity_standalone.r')
})

output$st_text <- renderText({
  st_test()
})
output$down <- downloadHandler(
  filename=function(){
    paste("untitled",".png",sep=" ")
  }, 
  content=function(file){
    png(file)
    plot(dataset()$Channel.1,dataset()$Channel.2)
    dev.off()
  })
#--------end of test tab

#------filter tab-----
output$plot_sma <- renderPlot({
  channel1<-dataset()$Channel.1[1:input$samplesize]
  channel2<-dataset()$Channel.2[1:input$samplesize]
  sma(channel1,10)  
})

output$text = renderText({
 sensor = "NNode383"
 channels = "accelerationx,accelerationy,accelerationz,air:temperature"
 time_start = "2014-05-13T07:56:48.006750000+0100"
 time_end = "2014-05-13T07:57:00.256750000+0100"

 url <- paste0("http://quader.igg.tu-berlin.de/istsos/test?service=SOS&request=GetObservation&offering=temporary&procedure=",sensor,"&observedProperty=",channels,"&responseFormat=text/plain&version=1.0.0&",time_start,"/",time_end)

})

output$plot_ma <- renderPlot({
  
   source('movingAverage_standalone.r')#%>%
})
#---end of filter tab-----

##-------info tab--------
#output$text = renderText({paste("Function Definitions", "\n", "You have chosen a range that goes from", input$range[1], "to", input$range[2])})

#---------end of info tab--------
})



#source('sample.r')

