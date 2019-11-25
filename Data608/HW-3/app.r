library(ggplot2)
library(dplyr)
library(shiny)
library(gridExtra)
library(tidyr)

df <- read.csv('https://media.githubusercontent.com/media/mlforsachid/MSDSQ4/master/Data608/HW-3/data/HW-3_Data.csv', stringsAsFactors = FALSE)

ui <- fluidPage(
  headerPanel('Data 608 HW-3'),
  sidebarPanel(
    selectInput('cse', 'Cause', unique(df$ICD.Chapter), selected='Neoplasms'),
    selectInput('yre', 'Year', unique(df$Year), selected='2010'),
    selectInput('tp', 'Show Top', seq(10,50, by = 5), selected='10')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot",
               fluidRow(
                 column(8, plotOutput('plot1', width="600px", height="800px")),
                 column(8, verbatimTextOutput('stats')),
                 plotOutput('plot2', width="600px", height="800px")
               )
               
      )
    )
  )
)

server <- shinyServer(function(input, output, session) {
  
  selectedData <- reactive({
    dfSlice <- df %>%
      filter(Year == input$yre, ICD.Chapter == input$cse)
    dfSlice = dfSlice[order(dfSlice$Crude.Rate, decreasing = TRUE)[1:input$tp],]
    
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
  
  
  output$plot1 <- renderPlot({
    
  ggplot(selectedData(), aes(x = reorder(State, (Crude.Rate)), y = Crude.Rate, fill = State)) +
     geom_bar(stat='identity') +
      labs(title="Mortality rate per state", 
           subtitle="State wise mortality rate", 
           caption="Mortality rate dataset", x='States', y='Mortality Rate') +
      geom_text(aes(y=Crude.Rate-20, label=Crude.Rate), color='white', size=5) +
      coord_flip()
      #coord_polar("y", start=0)
  })
  
  output$plot2 <- renderPlot({
    
    ggplot(compareData(), aes(fill = AvgType, y=value, x=AvgType)) +
      geom_bar(position="dodge", stat='identity') +
      labs(title="National vs State mortality rate average comparisons", 
           subtitle="Mortality rate comparison grouped by states", 
           caption="Mortality rate dataset", x='States', y='Mortality Rate') +
      geom_text(aes(y=value-10, label=value), color='white', size=3) +
      facet_wrap(~State) 
  })
  
  output$stats <- renderPrint({
    dfSliceTier <- selectedData()
    
    summary(dfSliceTier$Crude.Rate)
  })
  
})

shinyApp(ui = ui, server = server)