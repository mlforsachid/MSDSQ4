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
library(curl)

orders <- fread('https://media.githubusercontent.com/media/mlforsachid/MSDSQ4/master/Data608/HW-6/data/InstaCart/orders.csv/orders.csv', header = TRUE, sep = ",")
products <- fread('https://media.githubusercontent.com/media/mlforsachid/MSDSQ4/master/Data608/HW-6/data/InstaCart/products.csv/products.csv' , header = TRUE, sep = ",")
order_products <- fread('https://media.githubusercontent.com/media/mlforsachid/MSDSQ4/master/Data608/HW-6/data/InstaCart/order_products__train.csv/order_products__train.csv' , header = TRUE, sep = ",")

orders = orders[1:1000,]
shinyApp(
  ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "Select themes to personalize",
      tabPanel("Introduction",
               
               fluidPage(
                 
                 titlePanel("Data 608 HW-6 : Market Basket Analysis - Instacart"),
                 
                 fluidRow(
                   
                   column(10, includeHTML("include.html")
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
                     column(8, textOutput('reordertext')),
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
                     column(8, plotOutput('productreorderplot', width="600px", height="800px")),
                     column(10, textOutput('noproducttext')),
                     column(8, plotOutput('noproductplot', width="600px", height="800px"))
                   )
                 )
               )
      ),
      tabPanel("Customers", 
              fluidPage(
                
                titlePanel('Customer Behaviour'),
                mainPanel(
                  fluidRow(
                    column(10, includeHTML("ordernoproba.html")),
                    column(8, plotOutput('ordernoprobaplot', width="600px", height="800px")),
                    column(10, includeHTML("reorderproba.html")),
                    column(8, plotOutput('reorderprobaplot', width="600px", height="800px"))
                    
                    
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
    
    bestsellers = reactive({
      tmp <- order_products %>% 
        group_by(product_id) %>% 
        summarize(count = n()) %>% 
        top_n(as.integer(input$topproduct), wt = count) %>%
        left_join(select(products,product_id,product_name),by="product_id") %>%
        arrange(desc(count)) 
      return(tmp)
    })
    
    productreorder = reactive({
      tmp <-order_products %>% 
        group_by(product_id) %>% 
        summarize(proportion_reordered = mean(reordered), n=n()) %>% 
        filter(n>40) %>% 
        top_n(as.integer(input$topproduct),wt=proportion_reordered) %>% 
        arrange(desc(proportion_reordered)) %>% 
        left_join(products,by="product_id")
      return(tmp)
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
    
    output$topproductplot <- renderPlot({
      
      bestsellers() %>% 
        ggplot(aes(x=reorder(product_name,-count), y=count))+
        geom_bar(stat="identity",fill="orange")+
        theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())
    })
    
    output$topproducttext = renderText({
      paste("Top", input$topproduct, "best selling products")
      
    })
    
    output$noproductplot <- renderPlot({
      
      order_products %>% 
        group_by(order_id) %>% 
        summarize(n_items = last(add_to_cart_order)) %>%
        ggplot(aes(x=n_items))+
        geom_histogram(stat="count",fill="orange") + 
        geom_rug()+
        coord_cartesian(xlim=c(0,80))
    })
    
    output$noproducttext = renderText({
      paste("No of products in an order")
      
    })
    
    output$productreorderplot <- renderPlot({
      
      productreorder() %>% 
        ggplot(aes(x=reorder(product_name,-proportion_reordered), y=proportion_reordered))+
        geom_bar(stat="identity",fill="red")+
        theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.85,0.95))
      
    })
    
    output$productreordertext = renderText({
      paste("Top", input$topproduct, "reordered products")
      
    })
    
    output$reorderprobaplot <- renderPlot({
      
      order_products %>% 
        left_join(orders,by="order_id") %>% 
        group_by(days_since_prior_order) %>%
        summarize(mean_reorder = mean(reordered)) %>%
        ggplot(aes(x=days_since_prior_order,y=mean_reorder))+
        geom_bar(stat="identity",fill="red")
    })
    
    output$ordernoprobaplot <- renderPlot({
      
      order_products %>% 
        group_by(product_id) %>% 
        summarize(proportion_reordered = mean(reordered), n=n()) %>%
        ggplot(aes(x=n,y=proportion_reordered))+
        geom_point()+
        geom_smooth(color="red")+
        coord_cartesian(xlim=c(0,2000))
    })
    
  })
  
)