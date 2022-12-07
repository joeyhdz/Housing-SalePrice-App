library(shiny)
library(dplyr)
library(rlang)
library(ggplot2)
library(shinythemes)
library(datasets)
library(treemapify)
library(ggcorrplot)
library(naniar)
library(tidyverse)
library(ggplot2)
library(plyr)
library(ggcorrplot)
library(gridExtra)
library(ggthemes)


ui <- shinyUI(fluidPage(
  titlePanel("Home Sale Price v. Ground Living Area"),
  tabsetPanel(
    tabPanel("Upload Ames Home Data File",
             h4("The dataset can be found here:",
                tags$a(href="https://github.com/joeyhdz/Housing-SalePrice-App/tree/master/app", "Download Here!"),
                tags$br(),tags$br(),
                "Once downloaded, please upload it below:"),
             titlePanel("File Upload"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Upload CSV File Here:',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 
                 #tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
                 
               ),
               mainPanel(
                 tableOutput('contents')
               )
             )
    ),
    
    tabPanel("Neighborhood Sale Price Scatterplot",
             h4("This page will display a Scatterplot for Sale Price v Ground Living Area",
                tags$br(),tags$br(),"You may select to visualize all neighborhoods, or dive into 3 of
                your target locations."),
             pageWithSidebar(
               headerPanel("Home Sales Scatterplot"),
               sidebarPanel(
                 radioButtons("neighbor", "Choose Neighborhood:",
                              c("All" = "nothing",
                                "Edwards" = "Edwards",
                                "BrkSide" = "BrkSide",
                                "NAmes" = "NAmes")),
                 # could enter a space here if desired. 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'X Variable:', "X"),
                 selectInput('ycol', 'Y Variable:', "Y"),
                 selectInput('color', "Analyze All Neighborhoods By:","-"),
                 
               ),
               
               mainPanel(
                 plotOutput('MyPlot')
               )
             )
    )
  )
)
)

server <- shinyServer(function(input, output, session) {
  
  N <- reactive({
    dfs <- switch(input$dfs,
                  Edwards = Edwards,
                  BrkSide = BrkSide,
                  NAmes = NAmes,
                  Edwards)
    dfs(input$n)
  })
  
  output$plot1 <- renderPlot({
    dfs <- input$dfs
    n <- input$n
  })
  
  data <- reactive({ 
    req(input$file1) ## req #  require that the input is available
    inFile <- input$file1 
    
    
    AmesHouesingDataFrame <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    
    AmesHouesingDataFrame$LogSalePrice = log(AmesHouesingDataFrame$SalePrice)
     AmesHouesingDataFrame$LogX1stFlrSF = log(AmesHouesingDataFrame$X1stFlrSF + 1)
     AmesHouesingDataFrame$LogX2ndFlrSF = log(AmesHouesingDataFrame$X2ndFlrSF + 1)
     AmesHouesingDataFrame$LogGrLivArea = log(AmesHouesingDataFrame$GrLivArea + 1)
    #
     AmesHouesingDataFrame$LogTotalBsmtSF = log(AmesHouesingDataFrame$TotalBsmtSF + 1)
     AmesHouesingDataFrame$LogBsmtUnfSF = log(AmesHouesingDataFrame$BsmtUnfSF + 1)
    #
     AmesHouesingDataFrame$LogWoodDeckSF = log(AmesHouesingDataFrame$WoodDeckSF + 1)
     AmesHouesingDataFrame$LogOpenPorchSF = log(AmesHouesingDataFrame$OpenPorchSF + 1)
     AmesHouesingDataFrame$LogX3SsnPorch = log(AmesHouesingDataFrame$X3SsnPorch + 1)
     AmesHouesingDataFrame$LogScreenPorch = log(AmesHouesingDataFrame$ScreenPorch + 1)
    #
     AmesHouesingDataFrame$LogLotArea = log(AmesHouesingDataFrame$LotArea + 1)
     AmesHouesingDataFrame$LogLotFrontage = log(AmesHouesingDataFrame$LotFrontage + 1)
     AmesHouesingDataFrame$LogPoolArea = log(AmesHouesingDataFrame$PoolArea + 1)
     AmesHouesingDataFrame$LogGarageArea = log(AmesHouesingDataFrame$GarageArea + 1)
     AmesHouesingDataFrame$LogMasVnrArea = log(AmesHouesingDataFrame$MasVnrArea + 1)


     AmesHouesingDataFrame$Fence <- as.character(AmesHouesingDataFrame$Fence)
     AmesHouesingDataFrame$Fence[is.na(AmesHouesingDataFrame$Fence)] <- "None"
     AmesHouesingDataFrame$Fence <- as.factor(AmesHouesingDataFrame$Fence)
    #
     AmesHouesingDataFrame$MSZoning <- as.character(AmesHouesingDataFrame$MSZoning)
     AmesHouesingDataFrame$MSZoning[is.na(AmesHouesingDataFrame$MSZoning)] <- "None"
     AmesHouesingDataFrame$MSZoning <- as.factor(AmesHouesingDataFrame$MSZoning)
    #
     AmesHouesingDataFrame$Exterior1st <- as.character(AmesHouesingDataFrame$Exterior1st)
     AmesHouesingDataFrame$Exterior1st[is.na(AmesHouesingDataFrame$Exterior1st)] <- "None"
     AmesHouesingDataFrame$Exterior1st <- as.factor(AmesHouesingDataFrame$Exterior1st)
    #
     AmesHouesingDataFrame$Exterior2nd <- as.character(AmesHouesingDataFrame$Exterior2nd)
     AmesHouesingDataFrame$Exterior2nd[is.na(AmesHouesingDataFrame$Exterior2nd)] <- "None"
     AmesHouesingDataFrame$Exterior2nd <- as.factor(AmesHouesingDataFrame$Exterior2nd)
    #
     AmesHouesingDataFrame$GarageType <- as.character(AmesHouesingDataFrame$GarageType)
     AmesHouesingDataFrame$GarageType[is.na(AmesHouesingDataFrame$GarageType)] <- "None"
     AmesHouesingDataFrame$GarageType <- as.factor(AmesHouesingDataFrame$GarageType)
    #
     AmesHouesingDataFrame$FireplaceQu <- as.character(AmesHouesingDataFrame$FireplaceQu)
     AmesHouesingDataFrame$FireplaceQu[is.na(AmesHouesingDataFrame$FireplaceQu)] <- "None"
     AmesHouesingDataFrame$FireplaceQu <- as.factor(AmesHouesingDataFrame$FireplaceQu)
    #
     AmesHouesingDataFrame$Alley <- as.character(AmesHouesingDataFrame$Alley)
     AmesHouesingDataFrame$Alley[is.na(AmesHouesingDataFrame$Alley)] <- "None"
     AmesHouesingDataFrame$Alley <- as.factor(AmesHouesingDataFrame$Alley)
    #
     AmesHouesingDataFrame$BsmtQual <- as.character(AmesHouesingDataFrame$BsmtQual)
     AmesHouesingDataFrame$BsmtQual[is.na(AmesHouesingDataFrame$BsmtQual)] <- "None"
     AmesHouesingDataFrame$BsmtQual <- as.factor(AmesHouesingDataFrame$BsmtQual)
    #
     AmesHouesingDataFrame$BsmtCond <- as.character(AmesHouesingDataFrame$BsmtCond)
     AmesHouesingDataFrame$BsmtCond[is.na(AmesHouesingDataFrame$BsmtCond)] <- "None"
     AmesHouesingDataFrame$BsmtCond <- as.factor(AmesHouesingDataFrame$BsmtCond)
    #
     AmesHouesingDataFrame$BsmtExposure <- as.character(AmesHouesingDataFrame$BsmtExposure)
     AmesHouesingDataFrame$BsmtExposure[is.na(AmesHouesingDataFrame$BsmtExposure)] <- "None"
     AmesHouesingDataFrame$BsmtExposure <- as.factor(AmesHouesingDataFrame$BsmtExposure)
    #
     AmesHouesingDataFrame$BsmtFinType1 <- as.character(AmesHouesingDataFrame$BsmtFinType1)
     AmesHouesingDataFrame$BsmtFinType1[is.na(AmesHouesingDataFrame$BsmtFinType1)] <- "None"
     AmesHouesingDataFrame$BsmtFinType1 <- as.factor(AmesHouesingDataFrame$BsmtFinType1)
    #
     AmesHouesingDataFrame$BsmtFinType2 <- as.character(AmesHouesingDataFrame$BsmtFinType2)
     AmesHouesingDataFrame$BsmtFinType2[is.na(AmesHouesingDataFrame$BsmtFinType2)] <- "None"
     AmesHouesingDataFrame$BsmtFinType2<- as.factor(AmesHouesingDataFrame$BsmtFinType2)
    #
     AmesHouesingDataFrame$GarageFinish <- as.character(AmesHouesingDataFrame$GarageFinish)
     AmesHouesingDataFrame$GarageFinish[is.na(AmesHouesingDataFrame$GarageFinish)] <- "None"
     AmesHouesingDataFrame$GarageFinish <- as.factor(AmesHouesingDataFrame$GarageFinish)
    #
     AmesHouesingDataFrame$GarageQual <- as.character(AmesHouesingDataFrame$GarageQual)
     AmesHouesingDataFrame$GarageQual[is.na(AmesHouesingDataFrame$GarageQual)] <- "None"
     AmesHouesingDataFrame$GarageQual <- as.factor(AmesHouesingDataFrame$GarageQual)
    #
     AmesHouesingDataFrame$GarageCond <- as.character(AmesHouesingDataFrame$GarageCond)
     AmesHouesingDataFrame$GarageCond[is.na(AmesHouesingDataFrame$GarageCond)] <- "None"
     AmesHouesingDataFrame$GarageCond <- as.factor(AmesHouesingDataFrame$GarageCond)
    
     # MiscFeature: Miscellaneous feature not covered in other categories
     AmesHouesingDataFrame$MiscFeature <- as.character(AmesHouesingDataFrame$MiscFeature)
     AmesHouesingDataFrame$MiscFeature[is.na(AmesHouesingDataFrame$MiscFeature)] <- "None"
     AmesHouesingDataFrame$MiscFeature <- as.factor(AmesHouesingDataFrame$MiscFeature)
    
     # MasVnrType: Masonry veneer type
     AmesHouesingDataFrame$MasVnrType <- as.character(AmesHouesingDataFrame$MasVnrType)
     AmesHouesingDataFrame$MasVnrType[is.na(AmesHouesingDataFrame$MasVnrType)] <- "None"
     AmesHouesingDataFrame$MasVnrType <- as.factor(AmesHouesingDataFrame$MasVnrType)
    
    #
    

    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = c("Log Sale Price" = "LogSalePrice"),
                      selected = names(AmesHouesingDataFrame))
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = c("Log Gr. Living Area" = "LogGrLivArea"),
                      selected = names(AmesHouesingDataFrame))
    
    updateSelectInput(session, inputId = "color", label = "Analyze All Neighborhoods By:",
                      choices = c("Neighborhood" = "Neighborhood",
                                  "Zoning Class" = "MSZoning",
                                  "Sale Condition" = "SaleCondition"))
    
    
    return(AmesHouesingDataFrame)
  })
  
  output$contents <- renderTable({
    data()
  })
  
  output$MyPlot <- renderPlot({
    
    ggplot(data(), aes_string(x = input$xcol, y = input$ycol)) +
      geom_point(show.legend = FALSE)
    
    if (input$neighbor != "nothing"){
    filtered <- data() %>% filter(Neighborhood == input$neighbor)
    plot <- filtered %>%
      ggplot(aes_string(x = input$xcol, y = input$ycol))+
      geom_point(show.legend = FALSE) + xlab("Log: Ground Living Area Square Feet") +
      ylab("Log: Sale Price")
    
    return(plot)
    }
     else{
       ggplot(data(), aes_string(x = input$xcol, y = input$ycol)) +
         geom_point(show.legend = TRUE, aes(color = data()[[input$color]]))+
         xlab("Log: Ground Living Area Square Feet") +
         ylab("Log: Sale Price")
     }
    
    
    
  })
  
}) # tags end the server side


shinyApp(ui, server)