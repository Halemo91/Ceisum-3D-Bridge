library(shiny)
library(ggplot2)
library(TTR)
library(dygraphs)
library(datasets)
library(xts)
library(plyr)


dataset1<- read.csv(file="Node383.csv", header = TRUE, stringsAsFactors = F)  # read csv file 
dataset2 <- read.csv(file="Node384.csv", header = TRUE, skip=16,stringsAsFactors = F )  # read csv file 
dataset3 <- read.csv(file="Node573.csv", header = TRUE, skip=16,stringsAsFactors = F )  # read csv file 
dataset4 <- read.csv(file="Node574.csv", header = TRUE, skip=15,stringsAsFactors = F)  # read csv file
#-----------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
shinyUI(pageWithSidebar(

  ## Application title
  titlePanel(title = h4("Analysis", align = "center")),

  ## Sidebar with a slider input for number of observations

  sidebarPanel (
  
   radioButtons(
    inputId="radio",
    label="Selection a Node:",
    choices=list(
      "Node 383",
      "Node 384",
	    "Node 573",
	    "Node 574"),
    #selectInput("plot","plot1","plot2","plot3"),
    selected="Node 383"),
	fileInput("file","upload a file"),
	helpText("Max. file size is 5MB"),

    sliderInput('samplesize','Select Sample Size', min=1, max=nrow(dataset1),
                value=min(500,nrow(dataset2)),
                step=50,
                round=0),

    selectInput('x','X',names(dataset1[c(3,4,5,6)]), selectize=FALSE),
    selectInput('y','Y',names(dataset1[c(3,4,5,6)]), names(dataset1)[[4]], selectize=FALSE),
  width=2

  ),

  ## Show a plot of the generated distribution

  mainPanel(
    tabsetPanel(type="tab",
               #tabPanel("test",  verbatimTextOutput("str"), plotOutput('plot_lm',height="400")),
                tabPanel("Summary", h4(textOutput("dataname")),
                         dygraphOutput("plot") , verbatimTextOutput("summary")),
                #tabPanel("Data",h4(textOutput("samplesize")), tableOutput("datadf")),
                tabPanel("Statistics",h4(textOutput("samplesize")), verbatimTextOutput("stats")),
                tabPanel("Similarity Measures",
                        helpText("Identify the model of the simulated data using the correlograms, the", 
                                 strong("Autocorrelation Function"), "(ACF), ", 
                                 strong("Crosscorrelation Function"), "(CCF) and the", 
                                 strong("Partial Autocorrelation Function"), "(PACF).",
                                 br(),br()),
                        tabsetPanel(type = "tab",
                                    tabPanel("ACF", 
                                             fluidRow (style= "padding-bottom: 20px",
                                               column(3, selectInput("acf_yvalue","Channel",names(dataset1[c(3,4,5,6)]))),
                                               column(3, numericInput("acf_lag","TimeLag", 4 , min=2, max =50)),
                                               column(3, radioButtons(
                                                 inputId="acf_options",label="Operations",
                                                 choices=c("Auto Covariance" = "covariance",
                                                           "Auto Correlation"= "correlation",
                                                           "partial Auto Correlation" ="partial"),
                                                 selected="covariance"))),
                                             #fluidRow(dygraphOutput("plot_acf"))),
                                    fluidRow(plotOutput("plot_acf"))),
                                    tabPanel("CCF", 
                                             fluidRow (style= "padding-bottom: 20px",
                                                       column(4, selectInput("ccf_y2value","Choose data 2",names(dataset1[c(3,4,5,6)]), names(dataset1)[[4]])),
                                                     column(4, numericInput("ccf_lag","TimeLag", 4 , min=2, max =50)),
                                                       width=2),
                                             fluidRow(plotOutput("plot_ccf"), tableOutput("datadf"))),
                                             #fluidRow(dygraphtOutput("plot_ccf"), tableOutput("datadf"))),
                                    tabPanel("Differenced Series",
                                             helpText("Unless differencing is applied,
                                                      you will see nothing in this tab.",br(), 
                                                      "Not needed for our project may be"))
                                    #     checkboxInput(
                                    #       inputId="dfdat",
                                    #       label = "Apply Differencing?",
                                    #       value = FALSE),
                                    #     conditionalPanel(
                                    #       condition = "input.dfdat == true",
                                    #       selectInput(
                                    #         inputId = "dfd",
                                    #         label = "",
                                    #         choices = list(
                                    #           "First-Order" = 1, 
                                    #           "Second-Order" = 2),
                                    #         selected = "First-Order")),
                                    #     value = 2
                                             )),
                tabPanel("Regression analysis",
                         fluidRow (style= "padding-bottom: 20px",
                                   column(5, selectInput("acf_yvalue","Channel",names(dataset1[c(3,4,5,6)]))),
                                   column(5, radioButtons(
                                     inputId="reg_options",label="Operations",
                                     choices=c("Linear Regression" = "LM",
                                               "2nd order polyn Regression"= "PR2",
                                               "3nd order polyn Regression"= "PR3",
                                               "4nd order polyn Regression"= "PR4",
                                               "5nd order polyn Regression"= "PR5",
                                               "6nd order polyn Regression"= "PR6",
                                               "7nd order polyn Regression"= "PR7",
                                               "8nd order polyn Regression"= "PR8",
                                               "9nd order polyn Regression"= "PR9",
                                               "10nd order polyn Regression"= "PR10"),
                                     selected="LM"))),
                         fluidRow(plotOutput("plot_lm"), downloadButton(outputId="down",label="Download Plot"), actionButton(inputId="stat_check", label="Stationarity check"), verbatimTextOutput("st_text"), verbatimTextOutput("str"))
                         ),
                tabPanel("Filters", textOutput("text"), plotOutput("plot_ma")),
               #tabPanel("Filters", textOutput("text"), dygraphOutput("plot_ma")),
                tabPanel("Information",
                         #---function definition---#
                         h2("Function Definitions", align="center"), 
                         h3("Correlation coefficient"), 
                         p("Input:   timestamp (t), data (y)"),
                         code("Formula: r=(1/n)*(sum((t-t_mean).*(y-y_mean)))/(st*sy)"),
                         p("Output: -> 1 value: correlation coefficient r",br(),"-> 5 values: Correlation coefficient r, mean_x, mean_y, sx, sy"),
                         h3("Stationarity test"),
                         p("Input:   	-> Only data of a time series",br(),
                         "-> Desired number of intervals"),
                         code("Formula: interval=round(length(data)/i)"),
                         p("Output: 2 String-values- Hypothesis test result for T-Test and F-Test (Null-hypothesis failed to reject - Time series is stationary or Null-hypothesis rejected - Time series is not stationary)"),
                         h3("Auto Covariance and Corelation"),
                         p("Input:  	Time Series with timestamp (t) and data (y)"),
                         code("Formula: acf=zeros(round(n/4)+1,2)"),
                         p("Output: -> A shorter time series with time intervals (not time stamps) in t and auto covariance values in y"),
                         h3("Cross Corelation"),
                         p("Input:    2 Time Series with timestamp (t)(must be of same length) and data (y)"),
                         code("Formula: tau=round(length(t)/4)"),
                         p("Output: -> Cross correlation function with Lags-column and XCF-column",br(),"-> Time shifted version of second time series with data column y and corrected time-value column t_shift"),
                         h3("Regressions"),
                         p("Input:    Time Series with timestamp (t) and data (y)"),
                         h3("Moving Average"),
                         h3("Event Detection"),
                         a("http://shiny.rstudio.com/tutorial/lesson2/"))
    ),width=10
)
)

)

