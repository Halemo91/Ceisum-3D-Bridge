library(shiny)
library(dygraphs)
library(datasets)
library(xts)
library(plyr)

# sensor = "Node_383_new"
# channels = "acceleration-x,acceleration-y,acceleration-z,air:temperature"
# date_start = input$date_start#"2015-03-26"
# time_start = input$date_end#"13:16:01"
# date_end = input$time_start#"2015-03-26"
# time_end = input$time_end#"13:17:15"
# 
# url <- paste0("http://quader.igg.tu-berlin.de/istsos/test?service=SOS&request=GetObservation&offering=temporary&procedure=",sensor,
#               "&observedProperty=",channels,
#               "&responseFormat=text/plain&version=1.0.0&eventTime=", date_start,"T",time_start,"+0100/",date_end,"T",time_end,"+0100")
# Node_383 <- read.csv(file=url, header = TRUE)# read csv file


##--renaming for data from istsos
# urn = as.character(lapply(strsplit(names(Node_383), split="1.0."), "[", 1))
# colnames = as.character(lapply(strsplit(names(Node_383), split="1.0."), "[", 2))
# names(Node_383)<-paste(colnames)
# names(Node_383)[2]<- paste("procedure")


Node_383 <- read.csv(file="Node383.csv", header = TRUE)# read csv file 
Node_384 <- read.csv(file="Node384.csv", header = TRUE, skip =15)# read csv file 
Node_573 <- read.csv(file="Node573.csv", header = TRUE, skip =15)# read csv file 
Node_574 <- read.csv(file="Node574.csv", header = TRUE, skip =15)# read csv file 

##---functions----------
## any functions goes here
##---end of functions-------

## Define server logic required to generate and plot a random distribution
shinyServer(function(input,output) {
  
  
  #--datset chooseing--
  
  dataset <- reactive({
    if(input$radio == "Node_383")
      data <- Node_383
    
    else if(input$radio == "Node_384")
      data <- Node_384
    
    else if(input$radio == "Node_573")
      data <- Node_573
    
    else if(input$radio == "Node_574")
      data <- Node_574
    
    #      else if(input$file)
    #      fileinput <- input$file
    #      #if(is.null(fileinput)){return()}
    #      read.table(file=file$datapath)
    
    else("Error selecting a Node")
  })
  #----end of datset---------    
  
  output$datasize <- renderUI({
    sliderInput('samplesize','Data Size', min=10, max=nrow(dataset()),
                value=min(500,nrow(dataset())),
                step=50,
                round=0)
  })
  
  output$dateRangeText  <- renderText({
    paste("input$dateRange is", 
          paste(as.character(input$dateRange), collapse = " to ")
    )
  })
  #-------time conversion----------
  time<- reactive({
    timestamp = dataset()[,1]
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
      
#       date = as.character(lapply(strsplit(as.character(timestamp), split="T"), "[", 1))
#       hms = as.character(lapply(strsplit(as.character(timestamp), split="T"), "[", 2))
#       h = as.numeric(lapply(strsplit(hms, split=":"), "[", 1))
#       m = as.numeric(lapply(strsplit(hms, split=":"), "[", 2))
#       # s = as.numeric(lapply(strsplit(hms, split=":"), "[", 3)) # gave NAs introduced by coercion 
#       
#       # Cutting characters manually
#       s = as.numeric(substr(hms, 7, 15)) # characters 7 to 15
#       
#       # Gets UTC component, also cut manually
#       utcComp = as.numeric(substr(hms, 17, 18)) 
#       
#       h=h+utcComp # adding UTC to hours value
#       
#       # Filling time vector
#       time_sec = NULL
#       for(i in 1:length(timestamp))
#       {
#         time_sec[i] = h[i]*3600 + m[i]*60 + s[i]
#       }
#       time_sec
  })
  
  #----end of time conversion---------      

#--------summary tab-----------------
output$dataname<- renderText({
  paste("summary of dataset", input$radio)
})

# output$choose_plot <- renderUI({
#   radioButtons(inputId="choose_plot",
#     label="Choose a type of plot for visualization ",
#     choices=list(
#       "Plot" = "plot",
#       "Dygraphs Plot" = "dyplot"),
#     selected="plot")
# })

output$plot_raw <- renderPlot({
  if(input$radio == "node_574"){
    ch1<- dataset()$Channel.1[1:input$samplesize]
    ch2<- dataset()$Channel.2[1:input$samplesize]
    
    plot(range(time()[1:input$samplesize], na.rm=T), range(c(ch1, ch2)), na.rm=T, type='n', title ="Plot of accelerations of Raw data")
    lines(time()[1:input$samplesize], ch1, col="red")
    lines(time()[1:input$samplesize], ch2, col="green")}
  else {
    ch1<- dataset()$Channel.1[1:input$samplesize]
    ch2<- dataset()$Channel.2[1:input$samplesize]
    ch3<- dataset()$Channel.3[1:input$samplesize]
    
    plot(range(time()[1:input$samplesize], na.rm=T), range(c(ch1, ch2, ch3)), na.rm=T, type='n', main ="Plot of accelerations of Raw data")
    lines(time()[1:input$samplesize], ch1, col="red")
    lines(time()[1:input$samplesize], ch2, col="green")
    lines(time()[1:input$samplesize], ch3, col="blue")}
})

output$raw_data <- renderUI({
  if(input$radio != "node_574")
    selectInput("raw_channel","select any data for ploting and Staionarity Check", choices = names(dataset()[,3:6]))
  else
    selectInput("raw_channel","select any data for ploting and Staionarity Check", choices = names(dataset()[,3:5]))
  
})


output$plot_temp <- renderPlot({
  attach(get(input$radio))
  ch<-get(input$raw_channel)
  
  if(input$radio == "Node_574"){
   title<-paste(input$raw_channel," plot")
    plot(time()[1:input$samplesize],ch[1:input$samplesize], type="l",col="red", main = title)}
  
  else{
    title<-paste(input$raw_channel," plot")
    plot(time()[1:input$samplesize],ch[1:input$samplesize], type="l",col="red", main =title)}
})

outpt_text <- renderText({paste("Stationarity Test")})

output$st_test <- renderText({
  attach(get(input$radio))
  data<-get(input$raw_channel)
  
  source('stationarity_function.r')
  stationarity_function(time()[1:input$samplesize],data[1:input$samplesize])
})

output$plot <- renderDygraph({ 
  #test <- cbind(test1,test2,test3,test4)
  dygraph(dataset()[,3:6], main = "Node383",) %>% 
    dyOptions(drawPoints = TRUE, pointSize= 1.3  ,colors = RColorBrewer::brewer.pal(4, "Set1"),)%>%
    dyLegend(width= 550)%>%
    dySeries("..1", axis = "y" , label = "Channel 1")%>%
    dySeries("..2", axis = "y" , label = "Channel 2")%>%
    dySeries("..3", axis = "y" , label = "Channel 3")%>%
    dySeries("..4", axis = "y2" , label = "Channel 4")%>%
    dyAxis("y", label = "Channel 1, 2 and 3")%>%
    dyAxis("y2", label = "Channel 4")%>%
    dyRangeSelector()
  
  
})
#-------end of summary-------------

#--------data tab----------------

output$data <- renderTable({ 
  head(dataset(),n = input$samplesize)
})

output$downloadData <- downloadHandler(
  filename=function(){
    paste("data-","txt", sep=".")
  },
  content=function(file){
   data=head(dataset(),n=input$samplesize)
    write.table(data, file, sep="\t", row.names=FALSE)
  }
)
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
output$acf_data <- renderUI({
  if(input$radio != "Node_574")
  selectInput("acf_channel","selecting the channels", choices = names(dataset()[,3:6]))
  else
  selectInput("acf_channel","selecting the channels", choices = names(dataset()[,3:5]))
  
})

output$plot_acf <- renderPlot({
  attach(get(input$radio))
  data<-get(input$acf_channel)[1:input$samplesize]
  ac_res <- acf(data,length(data)/input$acf_lag, type = input$acf_options, na.action = na.pass, col="red") # type should be "correlation", "covariance", "partial"
})

output$ccf_data1 <- renderUI({
  if(input$radio != "Node_574")
    selectInput("ccf_channel1","selecting the channels", choices = names(dataset()[,3:6]))
  else
    selectInput("ccf_channel1","selecting the channels", choices = names(dataset()[,3:5]))
  
})

output$ccf_data2 <- renderUI({
  if(input$radio != "Node_574")
    selectInput("ccf_channel2","selecting the channels", choices = names(dataset()[,3:6]), selected = names(dataset()[4]))
  else
    selectInput("ccf_channel2","selecting the channels", choices = names(dataset()[,3:5]), selected = names(dataset()[4]))
  
})

output$plot_ccf <- renderPlot({
  attach(get(input$radio))
  ch1<-get(input$ccf_channel1)
  ch2<-get(input$ccf_channel2)
  data1= data.frame(time()[1:input$samplesize], ch1[1:input$samplesize])
  data2= data.frame(time()[1:input$samplesize], ch2[1:input$samplesize])

  source('crossCorr_function.r')
  crossCorr_function(data1, data2, input$ccf_lag)
})

#-----end of similarity tab----

##----refression tab-------

output$reg <- renderUI({
  selectInput("reg","choose a Regression Model", choices = c("Linear and Polynomial"="reg","Exponential Growth"="grow","Exponential Decay"="decay", "S Curve"="scurve"))
})

output$reg_data <- renderUI({
  if(input$radio != "Node_574")
    selectInput("reg_channel","selecting the channels", choices = names(dataset()[,3:6]))
  else
    selectInput("reg_channel","selecting the channels", choices = names(dataset()[,3:5]))
  
})

coef_test <- eventReactive(input$coef_test, {
  attach(get(input$radio))
  data<-get(input$reg_channel)
  
  source('coefficienttesting_function.r')
  coefficienttesting_function(time()[1:input$samplesize],data[1:input$samplesize])
})

output$coef_table <- renderTable({
  coef_test()
})

output$plot_reg <- renderPlot({
  attach(get(input$radio))
  data<-get(input$reg_channel)
  
  if(input$reg=="reg"){
    if(input$reg_options == "LM"){
      source('linearRegression_function.r')
      linearRegression_function(time()[1:input$samplesize],data[1:input$samplesize])}
    else if(input$reg_options == "PR2"){
      source('polynRegression2nd_function.r')
      polynRegression2nd_function(time()[1:input$samplesize],data[1:input$samplesize])}
    else if(input$reg_options == "PR3"){
      source('polynRegression3rd_function.r')
      polynRegression3rd_function(time()[1:input$samplesize],data[1:input$samplesize])}
    else if(input$reg_options == "PR4"){
      source('polynRegression4th_function.r')
      polynRegression4th_function(time()[1:input$samplesize],data[1:input$samplesize])}
    else if(input$reg_options == "PR5"){
      source('polynRegression5th_function.r')
      polynRegression5th_function(time()[1:input$samplesize],data[1:input$samplesize])}
    else if(input$reg_options == "PR6"){
      source('polynRegression6th_function.r')
      polynRegression6th_function(time()[1:input$samplesize],data[1:input$samplesize])}
    else if(input$reg_options == "PR7"){
      source('polynRegression7th_function.r')
      polynRegression7th_function(time()[1:input$samplesize],data[1:input$samplesize])}
    else if(input$reg_options == "PR8"){
      source('polynRegression8th_function.r')
      polynRegression8th_function(time()[1:input$samplesize],data[1:input$samplesize])}
    else if(input$reg_options == "PR9"){
      source('polynRegression9th_function.r')
      polynRegression9th_function(time()[1:input$samplesize],data[1:input$samplesize])}
    else if(input$reg_options == "PR10"){
      source('polynRegression10th_function.r')
      polynRegression10th_function(time()[1:input$samplesize],data[1:input$samplesize])}}
  
  else if(input$reg=="grow"){
    source('exponentialGrowth_function.r')
    exponentialGrowth_function(time()[1:input$samplesize],data[1:input$samplesize], input$reg_options)
  }

  else if(input$reg=="decay"){
    source('exponentialDecay_function.r')
    exponentialDecay_function(time()[1:input$samplesize],data[1:input$samplesize], input$reg_options)
  }
  
  else if(input$reg=="scurve"){
    source('Scurve_function.r')
    Scurve_function(time()[1:input$samplesize],data[1:input$samplesize], input$reg_options)
  }
  
})


output$reg_options <- renderUI({
if(input$reg=="reg"){
  selectInput("reg_options","Operations",
              c("Linear Regression" = "LM",
                "2nd order polyn Regression"= "PR2",
                "3nd order polyn Regression"= "PR3",
                "4nd order polyn Regression"= "PR4",
                "5nd order polyn Regression"= "PR5",
                "6nd order polyn Regression"= "PR6",
                "7nd order polyn Regression"= "PR7",
                "8nd order polyn Regression"= "PR8",
                "9nd order polyn Regression"= "PR9",
                "10nd order polyn Regression"= "PR10"),
              selected="LM")
  }
else if(input$reg =="grow"){
  numericInput("reg_options","Alpha", 3.5 , min=2, max =50)
}

else if(input$reg =="decay"){
  numericInput("reg_options","Alpha", 3.5 , min=2, max =50)
}

else if(input$reg =="scurve"){
  numericInput("reg_options","Coefficient a", 3.5 , min=2, max =50)
}
})



coef_test <- eventReactive(input$coef_test, {
  attach(get(input$radio))
  data<-get(input$reg_channel)
  
  source('coefficienttesting_function.r')
  coefficienttesting_function(time()[1:input$samplesize],data[1:input$samplesize])
})

output$coef_table <- renderTable({
  coef_test()
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


output$filter_data <- renderUI({
  if(input$radio != "Node_574")
    selectInput("filter_channel","selecting the channels", choices = names(dataset()[,3:5]))
  else
    selectInput("filter_channel","selecting the channels", choices = names(dataset()[,3:4]))
  
})

output$filter_args <- renderUI({
  if(input$filter_options=="MA"){
  numericInput("filter_args","Tau", 8 , min=2, max =50)   
  }
  else if(input$filter_options=="SP"){
    numericInput("filter_args","Smoothing value", 0.9 , min=2, max =50)   
  }
})

output$plot_filter <- renderPlot({
  attach(get(input$radio))
  data<-get(input$filter_channel)
  
  if(input$filter_options=="MA"){

    source('movingAverage_function.r')
    movingAverage_function(time()[1:input$samplesize],data[1:input$samplesize],input$filter_args)}
  
  else if(input$filter_options=="SP"){
    
    source('Spline_function.r')
    Spline_function(time()[1:input$samplesize],data[1:input$samplesize],input$filter_args)}
  
  else if(input$filter_options=="Kalman"){
    
#     source('.r')
#     _function(time()[1:input$samplesize],data[1:input$samplesize],input$tau)
  }
})
#---end of filter tab-----

##-------info tab--------
#output$st_text = renderText({paste(""Stationariy Test")})

#---------end of info tab--------
})



#source('sample.r')
