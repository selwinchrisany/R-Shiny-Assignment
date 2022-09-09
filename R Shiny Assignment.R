library(shiny)
library(reshape)
library(dplyr)
library(data.table)
library(ggplot2)
library(matrixStats)

# UI --------------------------------
ui <- fluidPage(
  titlePanel("IBNR Chain Ladder"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("loss_year", label = "Input Loss Year", value = 0, min = 1900, max = 2500, step = 1),
      numericInput("development_year", label = "Input Development Year", value = 1, min = 0, max = 100, step = 1),
      numericInput("claims_paid", label = "Input Amount of Claims Paid", value = 0, min = 0, step = 1),
      sliderInput("tail_factor", label = "Input Tail Factor", min = 1, max = 2, value = 1),
      actionButton("submitInputData", "Input Data", icon("Input") , width = NULL,
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      actionButton("calculate", "Calculate", icon("Input") , width = NULL,
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Input Data",
                 dataTableOutput("input_data"),
                 dataTableOutput("cumulative")
        ), 
        tabPanel("Calculation", dataTableOutput("calculation")), 
        tabPanel("Plot", 
                 plotOutput("line_graph")) 
        
      )
    )
  )
)


#Server -----------------------------
server <- function(input, output, session){
  tableValues <- reactiveValues(df = data.frame(loss_year = as.numeric(), development_year = as.numeric(),
                                                claims_paid = as.numeric(), check.names = FALSE))
  
  cumulativeData <- reactiveValues(cumulative_data = data.frame(loss_year = as.numeric(), claims_paid.1 = as.numeric(),
                                                                claims_paid.2 = as.numeric(), claims_paid.3 = as.numeric(),
                                                                check.names = FALSE))
  calcData <- reactiveValues(df = data.frame(loss_year = as.numeric(), claims_paid.1 = as.numeric(),
                                             claims_paid.2 = as.numeric(), claims_paid.3 = as.numeric(),
                                             claims_paid.4 = as.numeric(), check.names = FALSE))
  
  #OBSERVE EVENT INPUT DATA
  observeEvent(input$submitInputData, {
    
    temp <- tableValues$data
    newRow <- data.frame(loss_year = input$loss_year, development_year = input$development_year,
                         claims_paid = input$claims_paid, check.names = FALSE)
    temp <- rbind(temp, newRow)
    temp <- dplyr::arrange(temp, loss_year, development_year)
    
    tableValues$data <- temp
  })
  
  
  #FIRST OUTPUT
  output$input_data <- renderDataTable({
    req(tableValues$data)
    #names(tableValues$data) <- c("Loss Year", "Development Year", "Amount of Claims Paid")
    tableValues$data
  })
  
  
  #SECOND OUTPUT UPPER TRIANGLE
  output$cumulative <- renderDataTable({
    
    req(tableValues$data)
    cumulative_data <- tableValues$data
    
    cumulative_wide <- reshape(cumulative_data, idvar = "loss_year", timevar = "development_year", 
                               v.names = c("claims_paid"),  direction = "wide")
    
    cumulative_wide[-1] <- rowCumsums(as.matrix(cumulative_wide[-1]))
    
    cumulativeData$value <- cumulative_wide
    #names(cumulative_wide) <- c("Loss Year", "Development Year 1", "Development Year 2",
    #                          "Development Year 3")
    cumulative_wide
  })
  
  
  #OBSERVE EVENT CALCULATE
  observeEvent(input$calculate, {
    
    setDT(cumulativeData$value)
    dt <- cumulativeData$value
    
    n_row <- NROW(dt)
    
    for (j in 3L:NCOL(dt)) {
      row_i <- n_row - j + 3L
      set(
        dt, i = n_row:row_i, j = j,
        value = sum(dt[1L:(row_i - 1L), ..j]) /
          sum(dt[1L:(row_i - 1L), j - 1L, with = FALSE]) *
          dt[n_row:row_i, j - 1L, with = FALSE]
      )
    }
    
    dt$"claims_paid.4" <- dt[,4] * input$tail_factor
    calcData$lower <- dt
  })
  
  
  #SECOND OUTPUT
  output$calculation <- renderDataTable({
    req(calcData$lower)
    #names(calcData$lower) <- c("Loss Year", "Development Year 1", "Development Year 2",
    #                           "Development Year 3", "Development Year 4")
    calcData$lower
  })
  
  
  #THIRD OUTPUT
  test1 <<- new.env()
  output$line_graph <- renderPlot({
    req(calcData$lower)
    new.dt <- reshape(data = calcData$lower, direction = "long", idvar = "Loss Year", varying = list(2:5), v.names = "value")
    new.dt <- new.dt[with(new.dt, order(new.dt$"Loss Year"))]
    
    names(new.dt) <- c("lossyear", "time", "value", "remove")
    new.dt[, c("remove"):=NULL]
    
    d <- ggplot() +
      geom_line(data=new.dt, aes(x=time, y=value, group=lossyear, color=factor(lossyear), fill=factor(lossyear))) +
      xlab("Development Year") + ylab("Claims Paid ($)") +
      ggtitle("Cumulative Paid Claims ($)")
    
    d + scale_color_manual(values=c("#00FF00", "#FFA500", "#0000FF"))
    #scale_color_manual(values=c('green', 'orange', 'blue'))
  })
}

shinyApp(ui = ui, server = server)


