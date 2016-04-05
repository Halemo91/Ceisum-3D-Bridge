library(datasets)
library(shiny)
library(dygraphs)
library(xts)
library(shinythemes)



Node_383 <- read.csv(file="Node383.csv", header = TRUE)# read csv file 
Node_384 <- read.csv(file="Node384.csv", header = TRUE, skip =15)# read csv file 
Node_573 <- read.csv(file="Node573.csv", header = TRUE, skip =15)# read csv file 
Node_574 <- read.csv(file="Node574.csv", header = TRUE, skip =15)# read csv file 


shinyUI(fluidPage(theme = shinytheme("cerulean"),
  
  ## Application title
  titlePanel(title = h2("Data Analysis and Visualisation", align = "center")),
  
  ## Sidebar with a slider input for number of observations
  
  sidebarPanel (
    
    radioButtons(
      inputId="radio",
      label="Selection a Node:",
      choices=list(
        "Node_383" = "Node_383",
        "Node_384" = "Node_384",
        "Node_573" = "Node_573",
        "Node_574" = "Node_574"),
      selected="Node_383"),
    fileInput("file","upload a file"),
    helpText("Max. file size is 5MB"),
    
    uiOutput("datasize"),
    
    dateInput('date_start',label = 'Start Date',"2015-03-27",format = "yyyy-mm-dd"),#start = Sys.Date() - 2, end = Sys.Date() + 2),
    dateInput('date_end',label = 'End Date',"2015-03-27",format = "yyyy-mm-dd"),
    textInput("time_start","Start Time", "13:16:01"),
    textInput("time_end","End Time", "13:17:01"),
    helpText("Please enter in the format 'HH:MM:SS' for start and end time"),
    
    width=2
    
  ),
  
  ## Show a plot of the generated distribution
  
  mainPanel(
    tabsetPanel(type="tab",
                #tabPanel("test",uiOutput("button"), textOutput("button_text")),
                tabPanel("Summary", h4(textOutput("dataname")),uiOutput("choose_plot"), plotOutput("plot_raw")),
                tabPanel("Data",downloadButton("data_down","Download Data (.csv file)"), tableOutput("data")),
                tabPanel("Statistics",h4(textOutput("samplesize")), verbatimTextOutput("stats"), uiOutput("raw_data"), h3(textOutput("st_text")), h5(textOutput("st_test")), plotOutput("plot_temp")),
                tabPanel("Similarity Measures",
                         helpText("Identify the model of the simulated data using the correlograms, the", 
                                  strong("Autocorrelation Function"), "(ACF), ", 
                                  strong("Crosscorrelation Function"), "(CCF) and the", 
                                  strong("Partial Autocorrelation Function"), "(PACF).",
                                  br(),br()),
                         tabsetPanel(type = "tab",
                                     tabPanel("ACF", 
                                              fluidRow (style= "padding-bottom: 20px",
                                                        column(3, uiOutput("acf_data")),
                                                        column(3, numericInput("acf_lag","TimeLag", 4 , min=2, max =50)),
                                                        column(3, radioButtons(
                                                          inputId="acf_options",label="Operations",
                                                          choices=c("Auto Covariance" = "covariance",
                                                                    "Auto Correlation"= "correlation",
                                                                    "partial Auto Correlation" ="partial"),
                                                          selected="covariance"))),
                                              fluidRow(plotOutput("plot_acf"))),
                                     tabPanel("CCF", 
                                              fluidRow (style= "padding-bottom: 20px",
                                                        column(3, uiOutput("ccf_data1")),
                                                        column(3, uiOutput("ccf_data2")),
                                                        column(3, numericInput("ccf_lag","TimeLag", 4 , min=2, max =50)),
                                                        width=2),
                                              fluidRow(plotOutput("plot_ccf")))                    
                                          )),
                tabPanel("Regression analysis",
                         fluidRow(style= "padding-bottom: 20px",
#                                   column(3,actionButton(inputId="coef_test", label="Coefficient testing"),tableOutput("coef_table")),
                                  column(3,uiOutput("reg")),
                                  column(3,uiOutput("reg_data")),
                                  column(3, uiOutput("reg_options"))),
fluidRow(actionButton(inputId="coef_test", label="Coefficient testing"),tableOutput("coef_table")),

                         fluidRow(plotOutput("plot_reg"), downloadButton(outputId="down",label="Download Plot"))),
                tabPanel("Filters", fluidRow (style= "padding-bottom: 20px",
                                              column(3, uiOutput("filter_data")),
                                              column(3, uiOutput("filter_args")),
                                              column(3, radioButtons(
                                                inputId="filter_options",label="Operations",
                                                choices=c("Moving Average" = "MA",
                                                          "Spline"= "SP"#," Kalman" ="Kalman"
                                                          ),
                                                selected="MA"))),
                         fluidRow(plotOutput("plot_filter"))),
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
                         ),width=9
  )
)

)
