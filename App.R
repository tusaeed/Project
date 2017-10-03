library(shiny)
library(forecast)
library(xts)
library(forecast)
library(urca)
library(tseries)
library(lmtest)
library(astsa)



# Define UI 
ui<-shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Demand Forecasting"),
  
  # Sidebar with controls to select the dataset and forecast ahead duration
  sidebarPanel(
    selectInput("variable", "Variable:",
                list("Product A" = "product_a", 
                     "Product B" = "product_b",
                     "Product C" = "product_c",
                     "Product D" = "product_d",
                     "Product E" = "product_e")),
    numericInput("ahead", "Months to Forecast Ahead:", 12),
    
    #  sliderInput("ahead", "Months to Forecast Ahead:",
    #             min = 1, max = 12,
    #            value = 6, step = 1),
    
    
    
    
    submitButton("Update View")
  ),
  
  
  
  # Show the caption and forecast plots
  mainPanel(
    h3(textOutput("caption")),
    
    tabsetPanel(
      tabPanel("Arima Forecast", plotOutput("arimaForecastPlot")),
      tabPanel("Timeseries Decomposition", plotOutput("dcompPlot"))
    )
  )
))

server<- shinyServer(function(input, output) {
  

  aa <- "https://raw.githubusercontent.com/tusaeed/Project/master/A.csv"
  bb <- "https://raw.githubusercontent.com/tusaeed/Project/master/B.csv"
  cc <- "https://raw.githubusercontent.com/tusaeed/Project/master/C.csv"
  dd <- "https://raw.githubusercontent.com/tusaeed/Project/master/D.csv"
  ee <- "https://raw.githubusercontent.com/tusaeed/Project/master/E.csv"
  
  ### Data for Product A
  a <- as.numeric(read.table(file = aa, sep=",", header=F))
  product_a <- ts(a, start=c(2013,5),end=c(2017,5), frequency = 12)
  Atrain <- window(product_a, start = c(2013,5), end = c(2016,5))
  Atest <- window(product_a, start =c(2016,6))
  
  
  ### Data for Product B
  b <- as.numeric(read.table(file = bb, sep=",", header=F))
  product_b <- ts(b, start=c(2013,5), end=c(2017,5), frequency = 12)
  Btrain <- window(product_b, start = c(2013,5), end = c(2016,5))
  Btest <- window(product_b, start =c(2016,6))
  
  
  ### Data for Product C
  c <- as.numeric(read.table(file = cc, sep=",", header=F))
  product_c <- ts(c, start=c(2013,5), end=c(2017,5), frequency = 12)
  Ctrain <- window(product_c, start = c(2013,5), end = c(2016,5))
  Ctest <- window(product_c, start =c(2016,6))
  
  ### Data for Product D
  d <- as.numeric(read.table(file = dd, sep=",", header=F))
  product_d <- ts(d, start=c(2013,5), end=c(2017,5), frequency = 12)
  Dtrain <- window(product_d, start = c(2013,5), end = c(2016,5))
  Dtest <- window(product_d, start =c(2016,6))
  
  ### Data for Product E
  e <- as.numeric(read.table(file = ee, sep=",", header=F))
  product_e <- ts(e, start=c(2013,5), end=c(2017,5), frequency = 12)
  Etrain <- window(product_e, start = c(2013,5), end = c(2016,5))
  Etest <- window(product_e, start =c(2016,6))
  
  ### Model for product A
  plot(Atrain, type = "l")
  adf.test(Atrain)  ### Non-stationary data
  dAtrain <- diff(Atrain) 
  plot(dAtrain)
  acf2(dAtrain)
  sarima(Atrain, p = 1, d = 1, q = 0, P = 0, D = 1, Q = 0, S=12)
  sarima.for(Atrain, n.ahead=12, p = 1, d = 1, q = 0, P = 0, D = 1, Q = 0, S=12)
  arash<-lines(Atest)
  
  
  ### Model for product B
  plot(Btrain, type="l")
  adf.test(Btrain)
  dBtrain <- diff(Btrain)
  plot(dBtrain)
  acf2(dBtrain)
  auto.arima(dBtrain)
  sarima(Btrain, 0,0,1,0,0,0,12)
  sarima.for(Btrain, n.ahead=12,0,0,1,0,0,0,12)
  eric<-lines(Btest)
  
  
  ### Model for Product C
  plot(Ctrain, type="l")
  adf.test(Ctrain)
  dCtrain <- diff(Ctrain)
  plot(dCtrain)
  acf2(dCtrain)
  auto.arima(dCtrain)
  sarima(Ctrain, 2,0,1,0,1,0,12)
  sarima.for(Ctrain, n.ahead=12,2,0,1,0,1,0,12)
  matt<-lines(Ctest)
  
  ### Model for Product D
  plot(Dtrain, type="l")
  adf.test(Dtrain)
  dDtrain <- diff(Dtrain)
  plot(dDtrain)
  acf2(dDtrain)
  auto.arima(dDtrain)
  sarima(Dtrain, 2,0,1,0,1,0,12)
  sarima.for(Dtrain, n.ahead=12,2,0,1,0,1,0,12)
  Sona<-lines(Dtest)
  
  ### Model for Product E
  plot(Etrain, type="l")
  adf.test(Etrain)
  dEtrain <- diff(Etrain)
  plot(dEtrain)
  acf2(dEtrain)
  auto.arima(dEtrain)
  sarima(Etrain, 2,0,1,0,1,0,12)
  sarima.for(Etrain, n.ahead=12,2,0,1,0,1,0,12)
  Gagan<-lines(Etest)
  
  getDataset <- reactive({
    if (input$variable=="product_a")
    {
      return(product_a)
    }
    else if (input$variable=="product_b")
    {
      return(product_b)
    }
    else if (input$variable=="product_c")
    {
      return(product_c)
    }
    else if (input$variable=="product_d")
    {
      return(product_d)
    }
    else
    {
      return(product_e)
    }
  })
  
  
  
  output$dcompPlot <- renderPlot({
    ds_ts <- ts(getDataset(), frequency=12)
    f <- decompose(ds_ts)
    plot(f)
  })
  
  output$arimaForecastPlot <- renderPlot({
    fit <- auto.arima(getDataset())
    plot(forecast(fit, h=input$ahead))
  })
  
  
})

shinyApp(ui = ui, server = server)
