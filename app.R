library(rsconnect)
library(shiny)
library(shinythemes)
library(caret)
library(nnet)
library(DT)
library(MASS)

data2 <- read.csv("deploy1.csv")

model2 <-readRDS("model2.rds")

ui <- fluidPage(theme = shinytheme("simplex"),
                
                navbarPage(#----------->NAVBAR
                  theme = "united",
                  "NPS Prediction",
                  
                  tabsetPanel(
                    tabPanel("Home",icon = icon("home", lib = "glyphicon") ,
                             br(),
                             sidebarPanel(
                               br(),
                               br(),
                             
                               img(src = "sitelLogo.png", height = 100, width = 220),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               img(src = "airbnb.png", height = 80, width = 300),
                             ),
                             
                             mainPanel(
                               br(),
                               h1("Welcome to ",strong("Sitel's Machine Learning", style = "color:red")),
                               br(),
                               h4("This is a new tool so you can be able to predict your team's", strong("NPS result for a future week", style = "color:green")),
                               br(),
                               h4("Please check our", em('How it works'), "article to understand how you can run this to prepare your team to the future"),
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               img(src = "aiAirbnb.png")
                             )
                    ),
                    tabPanel("How this works",
                             br(),
                             sidebarPanel(
                               img(src = "HIW.png", width = "100%"),
                             ),
                             
                             mainPanel(
                               br(),
                               h1("How this Works?"),
                               br(),
                               h4("This machine will Forecast the total NPS result for the future week"),
                               br()
                               ,
                               div(includeMarkdown("about.md"),
                                   align="justify")
                             )
                    ),
                    
                    tabPanel("NPS Forecast", icon = icon("table"),
                             br(),
                             sidebarPanel(
                               br(),
                               tags$label(h3('Input parameters')),
                               
                               selectInput("Siter", "Choose a Site",
                                           choices = c(unique(data2$Site))
                               ),
                               selectInput("Tierr", 
                                           label = "Choose a Tier",
                                           choices = c(unique(data2$Tier))
                               ),
                               selectInput("TktLangr", 
                                           label = "Choose the Ticket Language",
                                           choices = c(unique(data2$Language))
                               ),
                               selectInput("Channelr", 
                                           label = "Choose the agent Channel",
                                           choices = c(unique(data2$Channel))
                               ),
                               
                               numericInput("Yearr",
                                            "Choose the Year to be Forecasted",
                                            2021,2023,value = 2021,step = 1
                               ),
                               
                               sliderInput("TTRr",
                                           "Choose the TTR",
                                           0,264,value = 2, step = 1
                               ),
                               numericInput("Weekr",
                                            "Choose the Week to be Forecasted",
                                            1,53,value = 30, step = 1
                               ),
                               
                               actionButton("submitbuttonr", "Predict",icon = icon("refresh"), 
                                            class = "btn btn-primary")
                             ),
                             
                             mainPanel(
                               br(),
                               fluidRow(
                                 h1("Please, Choose the options you need to be predicted on the left panel:", style = ("color:red")),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 column(2, ),
                                 column(8,align="center",
                                        wellPanel(   
                                          tableOutput('regresionModel')
                                        ) 
                                 )
                               )
                               
                             )
                    )
                    
                    
                  )
                )
) # fluidPage







server <- function(input, output, session) {
  
  
  
  #NPS ML Input
  updateSelectInput(session, "Siter",
                    label = "Choose a Site",
                    choices = c(unique(data2$Site))
                    # ,
                    # selected = tail(unique(data2$Site), 1)
  )
  
  updateSelectInput(session, "Tierr",
                    label = "Choose a Tier",
                    choices = c(unique(data2$Tier))
                    # ,
                    # selected = tail(unique(data2$Tier), 1)
  )
  
  updateSelectInput(session, "TktLangr",
                    label = "Choose the Language",
                    choices = c(unique(data2$Language))
                    # ,
                    # selected = tail(unique(data2$Language), 1)
  )
  updateSelectInput(session, "Channelr",
                    label = "Choose a Channel",
                    choices = c(unique(data2$Channel))
                    # ,
                    # selected = tail(unique(data2$Channel), 1)
  )
  updateNumericInput(session, "Yearr",
                     label = "Choose the Year",
                     value = 2021,
                     min = 2021,
                     max = 2022
  )
  updateSliderInput(session, "TTRr",
                    label = "Choose TTR",
                    value = 29,
                    min = 0,
                    max = 260
                    
  )
  updateNumericInput(session, "Weekr",
                     label = "Choose the Week time",
                     value = 40,
                     min = 1,
                     max = 53
                     
  )
  
  
  
  #result NPS ML
  observeEvent(input$submitbuttonr,{
    
    site = input$Siter
    tire = input$Tierr
    tktLang = input$TktLangr
    channel = input$Channelr
    year = input$Yearr
    week = input$Weekr
    tTR = input$TTRr
    
    newdata = data.frame( Site =site, Tier = tire, Language = tktLang,  Channel=channel, Year = year, Week = week:(week + 3), TTR=tTR)
    
    rresulter = predict(model2, newdata = newdata)
    
    output$regresionModel <- renderTable({
      
      data.frame(result = paste(round(rresulter, digits=3)))
      
    })
    
  })
  
}

shinyApp(onStart = function() { 
  
},ui = ui, server = server)