library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(stringr)
library(DT)
library(dplyr)
library(shiny)
library(gridExtra)
library(tidyr)

orders <- fread('https://media.githubusercontent.com/media/mlforsachid/MSDSQ4/master/Data608/HW-6/data/InstaCart/orders.csv/orders.csv')

orders = orders[1:1000,]
shinyApp(
  ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "shinythemes",
      tabPanel("Introduction",
               
               fluidPage(
                 
                 titlePanel("Data 608 HW-6 : Market Basket Analysis - Instacart"),
                 
                 fluidRow(
                   
                   column(10,
                          includeHTML("include.html")
                   )
                 )
               )
               
               
      ),
      tabPanel("Orders",
               
               fluidPage(
                 
                 titlePanel('When do people order?'),
                 sidebarPanel(
                   selectInput('ordertime', 'Time to Order', c('Day', 'Week'), selected='Day')
                  ),
                 
                 
                 mainPanel(
                   fluidRow(
                     column(10, textOutput('ordertext')),
                     column(8, plotOutput('orderplot', width="600px", height="800px")),
                     column(10, textOutput('reordertext')),
                     plotOutput('reorderplot', width="600px", height="800px")
                   )
                 )
               )
               
               
               
      ),
      tabPanel("Products", 
               fluidPage(
                 
                 titlePanel('Which products are best sellers?'),
                 sidebarPanel(
                   selectInput('topproduct', 'Show Top', c('5', '10', '15'), selected='5')
                 ),
                 
                 
                 mainPanel(
                   fluidRow(
                     column(10, textOutput('topproducttext')),
                     column(8, plotOutput('topproductplot', width="600px", height="800px")),
                     column(10, textOutput('productreordertext')),
                     plotOutput('productreorderplot', width="600px", height="800px")
                   )
                 )
               )
               
               )
    )
  ),
  server <- shinyServer(function(input, output, session) {
    
    selectedData <- reactive({
      dfSlice  =  orders
      
      if(input$ordertime == "Day")
      {
        dfSlice$ordertime = dfSlice$order_hour_of_day
      }else{
        dfSlice$ordertime = dfSlice$order_dow
      }
      return(dfSlice)
      
    })
    
    compareData <- reactive({
      dffilter <- df %>%
        filter(ICD.Chapter == input$cse, State %in% (unique(selectedData()$State)))
      dfSlice = dffilter%>%group_by(State) %>% summarise(State.Avg = mean(Crude.Rate))
      nat.df.filter = df %>% filter(ICD.Chapter == input$cse)
      Nat.Avg = sum(nat.df.filter$Population * nat.df.filter$Crude.Rate)/sum(nat.df.filter$Population)
      dfSlice$Nat.Avg = round(Nat.Avg,2)
      dfSlice$State.Avg = round(dfSlice$State.Avg,2)
      dfSlice = gather(dfSlice, AvgType, value, -State)
      return(dfSlice)
      
    })
    
    
    output$orderplot <- renderPlot({
      
      selectedData() %>% 
        ggplot(aes(x=ordertime)) + 
        geom_histogram(stat="count",fill="blue")
    })
    
    output$ordertext = renderText({
      paste("Customers order time in a ", input$ordertime)
      
    })
    
    output$reorderplot <- renderPlot({
      
      orders %>% 
        ggplot(aes(x=days_since_prior_order)) + 
        geom_histogram(stat="count",fill="green")
    })
    
    output$reordertext = renderText({
      paste("When do customers reorder?")
      
    })
   
    
  })
  
)