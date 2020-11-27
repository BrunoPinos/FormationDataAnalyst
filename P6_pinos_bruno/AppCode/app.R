#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(caret)



ui <- fluidPage(
    
    # App title ----
    titlePanel("Uploading Files"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
            
            
            
        ),
        
    mainPanel(
 
        titlePanel("Money detector"),
        img(src = "billet.jpg"), align = "center",
    br(), br(), br(),br(),
    h1("Let's go !"),
    actionButton("random", "start_lda"),
    actionButton("random2", "start_glm"),
    tableOutput("text1"),
    tableOutput("text2"),
    tableOutput("contents")
)
)
)

server <- function(input, output){
    v <- reactiveValues(data = NULL)
    w <- reactiveValues(data = NULL)
    load("models")
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        
    })   
    
    observeEvent(input$random, {
        df<-read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
        
        df.transformed <- preproc.param %>% predict(df)
        
        
        predictions <- model_lda %>% predict(df.transformed)
        
        prob<-predictions$posterior%>%round(2)*100
        
        prob<-prob[,2]%>%data.frame()%>%select("prob_true_lda" = "." )
        
        
        c<-predictions$class%>%as.data.frame()%>%cbind(df$id,.)%>%
            select("id" = "df$id", "is_genuine" =".")%>%
            cbind(prob)
        
        v$data <- c })
    
    
    observeEvent(input$random2, {
        df<-read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
        
        df.transformed <- preproc.param %>% predict(df)
        
        
        probabilities <- model %>%
            predict(df.transformed, type = "response")
        
        predicted.classes <- ifelse(probabilities > 0.5, "True", "False")
        
        
        prob<-probabilities%>%round(2)*100
        
        prob<-prob%>%data.frame()%>%select("prob_true_glm" = "." )
        
        
        c<-predicted.classes%>%as.data.frame()%>%cbind(df$id,.)%>%
            select("id" = "df$id", "is_genuine" =".")%>%
            cbind(prob)
        
        w$data <- c })
    
    
    output$text1 <- renderTable({v$data})
    output$text2 <- renderTable({w$data})
    
}

shinyApp(ui, server)


