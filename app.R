library(shiny)
library(PerformanceAnalytics)
library(tabplot)
library(mvtsplot)
library(skimr)
library(knitr)
library(pander)
library(DiagrammeR)
library(datamodelr)
library(esquisse)
library(onehot)
library(mlbench)
library(caret)
library(tidyverse)
library(ggplot2)
library(shinyWidgets)
library(shinythemes)
library(markdown)
library(PerformanceAnalytics)
library(knitr)
library(pander)
library(DiagrammeR)
library(datamodelr)
library(esquisse)
library(rpart.plot)
library(rpart)
library(e1071)
library(randomForest)
library(gbm)
library(markdown)
library(titanic)

#modules
source("modules/download.R")

# UI
ui <- fluidPage(theme = shinytheme("cerulean"),
tags$style("html, body {overflow: visible !important;"),

   
  navbarPage("InsightR",
  #-------------------------------Module 1 Sidebar Panel-------------------------------#
             tabPanel("Module 1",
                      sidebarLayout(
                        sidebarPanel(
                          conditionalPanel(condition="input.conditionedPanels1 == 'Database'",
                            fileInput("file","Upload the file", multiple = TRUE),
                            helpText("Default max. file size is 10MB"),
                            helpText("Select parameters for upload below"),
                            checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
                            checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                            radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                            hr(),
                            helpText("Or use Example:"),
                            actionButton("applyExample", "Use example datasets")
                          
                          ),
                          conditionalPanel(condition="input.conditionedPanels1 == 'Dataset' || input.conditionedPanels1 == 'Summary' || input.conditionedPanels1 == 'Visualization'",
                            uiOutput("selectfile")
                          ),
                          conditionalPanel(condition="input.conditionedPanels1 == 'Visualization'",
                            selectInput("selectvis", "Select Visualisation", choices=list("tabplot R-package" = 1, "mvtsplot R-package" = 2, "Esquisse R-package" = 3))
                          ),
                          conditionalPanel(condition="input.conditionedPanels1 == 'Merge Data'",
                                           uiOutput("mergefile1"),
                                           uiOutput("mergefile1.by"),
                                           uiOutput("mergefile2"),
                                           uiOutput("mergefile2.by"),
                                           radioButtons(inputId = 'join', label = 'Join:', choices = c("inner join", "left join", "right join", "full join"), selected = "inner join"),
                                           actionButton("applyJoin", label = 'Apply')
                          )
                        ),
  #-------------------------------Module 1 Main Panel-------------------------------# 
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Database", h4("Entity-Relationship-Diagram"),br(), grVizOutput(outputId='vis.3')),
                            tabPanel("Summary", h4("Dataset overview"),br(), DT::dataTableOutput("table"), h4("Summary of categorical variables"),br(), DT::dataTableOutput("fileob.1"),h4("Summary of numerical variables"),br(), DT::dataTableOutput("fileob.2") ,plotOutput("summ")),
                            tabPanel("Visualization", h4("Plot for visualization"),br(),
                                     conditionalPanel(condition = "['1'].indexOf(input.selectvis)>=0",plotOutput(outputId='vis.1')),
                                     conditionalPanel(condition = "['2'].indexOf(input.selectvis)>=0", plotOutput(outputId='vis.2')),
                                     conditionalPanel(condition = "['3'].indexOf(input.selectvis)>=0", tags$div(style = "height: 500px", esquisserUI(id = "esquisse", header = FALSE, choose_data = FALSE)))),
                            tabPanel("Merge Data",h4("Dataset"),br(), DT::dataTableOutput('mergetable')),
                            id = "conditionedPanels1"
                          ))
                      )
             ),
  #-------------------------------Module 2 Sidebar Panel-------------------------------#             
             tabPanel("Module 2",
                    sidebarLayout(
                      sidebarPanel(
                        conditionalPanel(condition="input.conditionedPanels2 == 'Feature-Engineering'",
                                         
                                         uiOutput("selectfile2"),
                                         hr(),
                                         helpText("Data Cleaning - Klick bottom of columns to plot"),
                                         sliderInput(inputId = "bins", label = "Number of bins:", min = 1, max = 50, value = 25),
                                         actionButton("deleteRows", "Delete Rows"),
                                         actionButton("deleteColumns", "Delete Columns"),
                                         hr(),
                                         helpText("Log-Transformation"),
                                         radioButtons(inputId = 'power', label = 'Power', choices = c("1"=1,"2"=2,"4"=4), selected = 2),
                                         actionButton("applyLog", "Apply"),
                                         hr(),
                                         helpText("Quantisierung"),
                                         radioButtons(inputId = 'quant', label = 'Quantisierung', choices = c("Feature Hashing"=1, "One-Hot-Encoding"=2), selected = 1),
                                         actionButton("applyQuant", "Apply"),
                                         hr(),
                                         helpText("Standardisierung und Normalisierung"),
                                         radioButtons(inputId = 'stand', label = 'Standardisierung und Normalisierung', choices = c("Standardisierung"=1, "Normalisierung 0-1" =2), selected = 2),
                                         actionButton("applyStand", "Apply")
                        ),
                        conditionalPanel(condition="input.conditionedPanels2 == 'Feature-Konstruktion'",
                                         uiOutput("cons.1"),
                                         radioButtons(inputId = 'consop', label = 'Operation:', choices = c("+"=1, "-" =2, "*"=3, "/" =4), selected = 2),
                                         uiOutput("cons.2"),
                                         actionButton("applyCons", "Create New Feature")
                        ),
                        conditionalPanel(condition="input.conditionedPanels2 == 'Feature-Selektion'",
                                         uiOutput("yval2"),
                                         uiOutput("xval2"),
                                         actionButton("applySelec", "Apply Feature-Selection"),
                                         hr(),
                                         helpText("Sampling"),
                                         sliderInput(inputId = "trainsplit", label = "Split of Trainingdata", min = 0, max = 1, value = 0.75),
                                         actionButton("applySamp", "Apply Sampling")

                        )
                      
                        
                      ),
                      
  #-------------------------------Module 2 Main Panel-------------------------------#
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Feature-Engineering", h4("Dataset"),br(), DT::dataTableOutput('datatab'),h4("Histogram and Boxplot"),br(), plotOutput("Hist")),             
                          tabPanel("Feature-Konstruktion", h4("Dataset"),br(), DT::dataTableOutput('datatabCons')),
                          tabPanel("Feature-Selektion",h4("Variable Importance"),br(), plotOutput("varImp"), h4("Train dataset"),br(),DT::dataTableOutput("datatabtrain")),
                          id = "conditionedPanels2"
                        ))
                    )
             ),
#-------------------------------Module 3 Sidebar Panel-------------------------------#  
                  tabPanel("Module 3",
                           sidebarLayout(
                             sidebarPanel(
                               helpText("Select Model according to user requirements"),
                               selectInput("task", "Choose Type of Task", choices = c("Regression" = "r", "Classification"="c")),
                               sliderInput(inputId = "accuracy", label = "Accuracy:", min = 1, max = 10, value = 5),
                               sliderInput(inputId = "robustness", label = "Robustness:", min = 1, max = 10, value = 5),
                               sliderInput(inputId = "easeofuse", label = "Ease of Use:", min = 1, max = 10, value = 5),
                               sliderInput(inputId = "interpretability", label = "Interpretability:", min = 1, max = 10, value = 5)
    
                  ),
#-------------------------------Module 3 Main Panel-------------------------------#
                        mainPanel(h4("Recommended ML-Model"),br(),uiOutput("modelchoice3"), uiOutput("modeltext")
                          )
                          )
                        ),
#-------------------------------Module 4 Sidebar Panel-------------------------------#
              tabPanel("Module 4",
                       sidebarLayout(
                         sidebarPanel(
                           conditionalPanel(condition="input.conditionedPanels4 == 'Modellauswahl und Training'",
                                            uiOutput("modelchoice4"), 
                                            hr(),
                                            uiOutput("yval3"),
                                            uiOutput("xval3"),
                                            helpText("Select values of tuning Parameters"),
                                            conditionalPanel(condition="input.modeltotrain == 'rpart'",
                                                             pickerInput(inputId = "cpval", label = "Select cp-Value (at least 2)",
                                                                         choices = c(0.0001, 0.001, 0.005,0.01,0.02,0.05,0.1,0.2,0.3,0.5), options = list(`actions-box` = TRUE),
                                                                         multiple = TRUE)
                                            ),
                                            conditionalPanel(condition="input.modeltotrain == 'svmLinear'",
                                                             pickerInput(inputId = "cost", label = "Select Cost (at least 2)",
                                                                         choices = c(0.1,0.25,0.5,0.75, 1, 1.25,2), options = list(`actions-box` = TRUE),
                                                                         multiple = TRUE)
                                            ),
                                            conditionalPanel(condition="input.modeltotrain == 'gbm'",
                                                             pickerInput(inputId = "n.trees", label = "Select n.trees (at least 2)",
                                                                         choices = c("0 to 50", "0 to 100","0 to 200"), options = list(`actions-box` = TRUE),
                                                                         multiple = TRUE),
                                                             pickerInput(inputId = "interaction.depth", label = "Select interaction.depth",
                                                                         choices = c(1, 3, 6, 9, 10), options = list(`actions-box` = TRUE),
                                                                         multiple = TRUE),
                                                             pickerInput(inputId = "shrinkage", label = "Select shrinkage",
                                                                         choices = c(0.0001, 0.001, 0.01, 0.1), options = list(`actions-box` = TRUE),
                                                                         multiple = TRUE),
                                                             pickerInput(inputId = "n.minobsinnode", label = "Select n.minobsinnode",
                                                                         choices = c(5,10,15,20), options = list(`actions-box` = TRUE),
                                                                         multiple = TRUE)
                                            ),
                                            actionButton("applyTrain", "Train"),
                                            hr(),
                                            helpText("Evaluation with test dataset"),
                                            uiOutput("metric"),
                                            actionButton("applyEval", "Evaluate"),
                                            hr(),
                                            selectInput("dataset", "Download Dataset", choices = c("bestModell", "results", "dfWorking", "dfTrain", "dfTest")),
                                            downloadObjUI(id = "download1")


                           ),


                           conditionalPanel(condition="input.conditionedPanels4 == 'Explore'",
                                            uiOutput("testout")

                           )

                         ),
#-------------------------------Module 4 Main Panel-------------------------------#
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Modellauswahl und Training", h4('Training'),br(), plotOutput("eval"),h4("Evaluation"),br(), verbatimTextOutput("test")),
                            tabPanel("Explore",h4("Adaptive prediction of trained ML-Model"),br(), tableOutput('a_out'), verbatimTextOutput("predict"), 
                                     conditionalPanel(condition = "['rpart'].indexOf(input.model)>=0", h4("Tree structure"),br(), plotOutput(outputId='rpartplot'))),
                            id = "conditionedPanels4"
                          ))
                       )
              ),
             tabPanel("About",
                      mainPanel(includeMarkdown("./resources/about.md"), width = 12
                      )
             )
             
  )
)
  
#-------------------------------Server-------------------------------#
#-------------------------------Server-------------------------------#
#-------------------------------Server-------------------------------#
server <- function(input, output, session) {
  options(shiny.maxRequestSize=10*1024^2) 
  
  #-------------------------------Server Module 1-------------------------------#

  
  reactdf <- reactiveValues(dflist = NULL)
  
  observeEvent(input$applyExample,{
    reactdf$dflist[["Iris Dataset"]] <- iris
    reactdf$dflist[["Titanic Dataset"]] <- titanic_train
  })
  
  output$selectfile <- renderUI({
    if(is.null(reactdf$dflist)) {return()}
    list(hr(), 
         helpText("Select the files for which you need to see data and Summary"),
         selectInput("Select", "Select DataTable", choices=names(reactdf$dflist))
    )
    
  })
  
  #Read in the Data
  observeEvent(input$file,{
    for(i in 1:nrow(input$file)){
      tempdf <- as.data.frame(read.table(file=input$file$datapath[input$file$name==input$file$name[i]],
                                         sep=input$sep,
                                         header = input$header,
                                         stringsAsFactors = input$stringAsFactors))
      reactdf$dflist[[input$file$name[i]]] <- tempdf
    }
  })
  
  #Datamodel
  output$vis.3 <- renderGrViz({
    validate(
      need(!is.null(reactdf$dflist), "Please upload data or use example datasets")
    )
    req(reactdf$dflist)
    if(is.null(reactdf$dflist)){return()}
    erd_1()
    
  })
  
  
  erd_1 <- reactive({
    req(reactdf$dflist)
    
    dm.model <- dm_from_data_frames(reactdf$dflist)
    dm_render_graph(dm_create_graph(dm.model, rankdir = "BT", col_attr = c("column", "type")))
    
  })
  
  #Dataset
  output$table <- DT::renderDataTable(server = TRUE, options = list(pageLength = 5),{ 
    reactdf$dflist[[input$Select]]
    
  })
  
  #Summary
  output$fileob.1 <- DT::renderDataTable(server = TRUE,{
    validate(
      need(input$Select!="", "")
    )
    str_input.1()
    
  })
  output$fileob.2 <- DT::renderDataTable(server = TRUE,{
    validate(
      need(input$Select!="", "Please upload data or use example datasets")
    )
    str_input.2()
    
  })
  
  str_input.1 <- reactive({
    skim_to_list(reactdf$dflist[[input$Select]])[[1]]
  }) 
  str_input.2 <- reactive({
    skim_to_list(reactdf$dflist[[input$Select]])[[2]]
  }) 
  
  
  #Visualization
  output$vis.1 <- renderPlot({
    validate(
      need(input$Select!="", "Please upload data or use example datasets")
    )
    req(reactdf$dflist)
    if(input$selectvis==1){
      tableplot_1()
    }
    
  })
  
  tableplot_1 <- reactive({
    if(input$selectvis==1){
      tableplot(reactdf$dflist[[input$Select]], fontsize = 8, legend.lines = 8,  fontsize.title = 12)
    }
  }) 
  
  output$vis.2 <- renderPlot({
    validate(
      need(input$Select!="", "Please upload data or use example datasets")
    )
    req(reactdf$dflist)
    if(input$selectvis==2){
      mvtsplot_1()
    }
  })
  
  mvtsplot_1 <- reactive({
    if(input$selectvis==2){
      mvtsplot(reactdf$dflist[[input$Select]])
    }
  }) 
  
  #esquisse
  data_r <- reactiveValues(data = data.frame("Please Upload Data" = 0), name = "Please Upload Data")
  
  observeEvent(input$Select, {
    if (is.null(reactdf$dflist)){
      data_r$data <- data.frame("Please Upload Data" = 0)
      data_r$name <- "Please Upload Data"
    } else{
      
      req(reactdf$dflist)
      data_r$data <- reactdf$dflist[[input$Select]]
      data_r$name <- "reactdf$dflist$name"
    }
  })
  
  
  callModule(module = esquisserServer, id = "esquisse", data = data_r)
  
  #Merge Data
  output$mergefile1 <- renderUI({
    list(hr(), 
         helpText("Select 1st Datatable"),
         selectInput("merge1", "Select left DataTable", choices=names(reactdf$dflist))
    )
  })
  
  output$mergefile1.by <- renderUI({
    list(hr(), 
         selectInput("merge1.by", "Select Matching Column left", choices=names(reactdf$dflist[[input$merge1]]))
    )
  })
  
  
  output$mergefile2 <- renderUI({
    list(hr(), 
         helpText("Select 2nd Datatable"),
         selectInput("merge2", "Select right DataTable", choices=names(reactdf$dflist))
    )
  })
  
  output$mergefile2.by <- renderUI({
    list(hr(), 
         selectInput("merge2.by", "Select Matching Column right", choices=names(reactdf$dflist[[input$merge2]]))
    )
  })
  
  observeEvent(input$applyJoin,{
    if(input$join=="inner join"){
      reactdf$dflist[["consolidated.DT"]] <- merge(reactdf$dflist[[input$merge1]], reactdf$dflist[[input$merge2]],
                                          by.x = input$merge1.by, by.y = input$merge2.by)
    }
    if(input$join=="left join"){
      reactdf$dflist[["consolidated.DT"]] <- merge(reactdf$dflist[[input$merge1]], reactdf$dflist[[input$merge2]],
                                          by.x = input$merge1.by, by.y = input$merge2.by, all.x = TRUE)
    }
    if(input$join=="right Join"){
      reactdf$dflist[["consolidated.DT"]] <- merge(reactdf$dflist[[input$merge1]], reactdf$dflist[[input$merge2]],
                                          by.x = input$merge1.by, by.y = input$merge2.by, all.y = TRUE)
    }
    if(input$join=="full Join"){
      reactdf$dflist[["consolidated.DT"]] <- merge(reactdf$dflist[[input$merge1]], reactdf$dflist[[input$merge2]],
                                          by.x = input$merge1.by, by.y = input$merge2.by, all = TRUE)
    }
  })
  
  output$mergetable <- DT::renderDataTable(server = TRUE, options = list(pageLength = 5),{ 
    validate(
      need(!is.null(reactdf$dflist[["consolidated.DT"]]), "Join uploaded datatables by applying join methods from the sidebar")
    )
    reactdf$dflist[["consolidated.DT"]]
    
  })
  
  #-------------------------------Server Module 2-------------------------------#
  values <- reactiveValues(dfWorking = NULL)
  
  output$selectfile2 <- renderUI({
    list(hr(), 
         helpText("Select the files for which you need to see data and Summary"),
         selectInput("Select2", "Select", choices=names(reactdf$dflist))
    )
    
  })
  
  observeEvent(input$Select2,{
    values$dfWorking <- reactdf$dflist[[input$Select2]]
  })
  
  
  #FE
  output$Hist <- renderPlot({
    
    validate(
      need(input$Select2!="", "Please upload data or use example datasets"),
      need(input$datatab_columns_selected !="", "Klick on the bottom of a column to plot")
    )
    req(reactdf$dflist)
    par(mfrow=c(1,2))
    hist_df()
    
  })
  
  hist_df <- reactive({
    req(reactdf$dflist)

    hist_df <- isolate(values$dfWorking)
    mainname <- names(hist_df)[input$datatab_columns_selected]
    
    if(length(unique(hist_df[[input$datatab_columns_selected]]))==1){
      hist(as.numeric(as.factor(hist_df[[input$datatab_columns_selected]])), main =mainname , xlab="",col = "#75AADB", border = "grey",warn.unused = TRUE)
      boxplot(as.numeric(as.factor(hist_df[[input$datatab_columns_selected]])), main =mainname, xlab="", col = "#75AADB", border = "grey", outcol=c("red"))
    }else{
      if(class(hist_df[[input$datatab_columns_selected]])=="character"){
        bins <- seq(min(as.numeric(as.factor(hist_df[[input$datatab_columns_selected]])), na.rm =TRUE), max(as.numeric(as.factor(hist_df[[input$datatab_columns_selected]])), na.rm =TRUE), length.out = input$bins + 1)
        hist(as.numeric(as.factor(hist_df[[input$datatab_columns_selected]])), main =mainname, xlab="", breaks = bins, col = "#75AADB", border = "grey",warn.unused = TRUE)
        boxplot(as.numeric(as.factor(hist_df[[input$datatab_columns_selected]])), main =mainname, xlab="", col = "#75AADB", border = "grey", outcol=c("red"))
      }
      if(class(hist_df[[input$datatab_columns_selected]])=="factor"){
        bins <- unique(seq(min(as.numeric(hist_df[[input$datatab_columns_selected]]), na.rm =TRUE), max(as.numeric(hist_df[[input$datatab_columns_selected]]), na.rm =TRUE), length.out = input$bins + 1))
        hist(as.numeric(hist_df[[input$datatab_columns_selected]]), main =mainname, xlab="", breaks = bins, col = "#75AADB", border = "grey",warn.unused = TRUE)
        boxplot(as.numeric(hist_df[[input$datatab_columns_selected]]), main =mainname, xlab="", col = "#75AADB", border = "grey", outcol=c("red"))
      }
      if(class(hist_df[[input$datatab_columns_selected]])=="integer" || class(hist_df[[input$datatab_columns_selected]])=="numeric"){
        bins <- seq(min(hist_df[[input$datatab_columns_selected]], na.rm =TRUE), max(hist_df[[input$datatab_columns_selected]], na.rm =TRUE), length.out = input$bins + 1)
        hist(hist_df[[input$datatab_columns_selected]], main =mainname, xlab="", breaks = bins, col = "#75AADB", border = "grey", warn.unused = TRUE)
        boxplot(hist_df[[input$datatab_columns_selected]], main =mainname, xlab="", col = "#75AADB", border = "grey", outcol=c("red"))
      }
      if(!is.null(input$deleteRows)){
        if(class(hist_df[[input$datatab_columns_selected]])=="character"){
          bins <- seq(min(as.numeric(as.factor(hist_df[[input$datatab_columns_selected]]))), max(as.numeric(as.factor(hist_df[[input$datatab_columns_selected]]))), length.out = input$bins + 1)
          hist(as.numeric(as.factor(hist_df[[input$datatab_columns_selected]])), main =mainname, xlab="", breaks = bins, col = "#75AADB", border = "grey",warn.unused = TRUE)
          boxplot(as.numeric(as.factor(hist_df[[input$datatab_columns_selected]])), main =mainname, xlab="", col = "#75AADB", border = "grey", outcol=c("red"))
        }
        if(class(hist_df[[input$datatab_columns_selected]])=="integer" || class(hist_df[[input$datatab_columns_selected]])=="numeric"){
          bins <- seq(min(hist_df[[input$datatab_columns_selected]], na.rm =TRUE), max(hist_df[[input$datatab_columns_selected]], na.rm =TRUE), length.out = input$bins + 1)
          hist(hist_df[[input$datatab_columns_selected]], main =mainname, xlab="",breaks = bins, col = "#75AADB", border = "grey", warn.unused = TRUE)
          boxplot(hist_df[[input$datatab_columns_selected]], main =mainname, xlab="", col = "#75AADB", border = "grey", outcol=c("red"))
        }
      }
    }

  })
  
  # Data Table
  output$datatab <- DT::renderDataTable(server = TRUE,options = list(pageLength = 5), selection = list(target = 'row+column'), filter = "top",{

      values$dfWorking 
    }
    
  )

  
  proxy <- reactive({
    req(reactdf$dflist)
    dataTableProxy('datatab')
  }) 
  
  observeEvent(input$select1, {
    proxy() %>% selectRows(as.numeric(input$rows))
  })
  
  observeEvent(input$select2, {
    proxy() %>% selectColumns(input$col)
  })
  
  observeEvent(input$clear1, {
    proxy() %>% selectRows(NULL)
  })
  
  observeEvent(input$clear2, {
    proxy() %>% selectColumns(NULL)
  })
  
  observeEvent(input$add, {
    proxy() %>% addRow(iris[sample(nrow(iris), 1), , drop = FALSE])
  })
  
  observeEvent(input$deleteRows,{
    
    if (!is.null(input$datatab_rows_selected)) {
      
      values$dfWorking <- values$dfWorking[-as.numeric(input$datatab_rows_selected),]
    }
  })
  
  observeEvent(input$deleteColumns,{
    
    if (!is.null(input$datatab_columns_selected)) {
      
      values$dfWorking <- values$dfWorking[,-as.numeric(input$datatab_columns_selected)]
    }
  })
  
  # Log Transformation
  observeEvent(input$applyLog, {
    if(class(values$dfWorking[,as.numeric(input$datatab_columns_selected)])=="numeric"){
      if(input$power==1){
      values$dfWorking[,as.numeric(input$datatab_columns_selected)] <- log(values$dfWorking[,as.numeric(input$datatab_columns_selected)], 
                                                                           (max(values$dfWorking[,as.numeric(input$datatab_columns_selected)])
                                                                            -min(values$dfWorking[,as.numeric(input$datatab_columns_selected)]))^1)
      }
      if(input$power==2){
        values$dfWorking[,as.numeric(input$datatab_columns_selected)] <- log(values$dfWorking[,as.numeric(input$datatab_columns_selected)], 
                                                                             (max(values$dfWorking[,as.numeric(input$datatab_columns_selected)])
                                                                              -min(values$dfWorking[,as.numeric(input$datatab_columns_selected)]))^2)
      }
      if(input$power==4){
        values$dfWorking[,as.numeric(input$datatab_columns_selected)] <- log(values$dfWorking[,as.numeric(input$datatab_columns_selected)], 
                                                                             (max(values$dfWorking[,as.numeric(input$datatab_columns_selected)])
                                                                              -min(values$dfWorking[,as.numeric(input$datatab_columns_selected)]))^4)
      }
    }
    
  })
  
  # Quantisierung
  observeEvent(input$applyQuant, {
    if(input$quant==1){
      #Feature Hashing
      values$dfWorking[,as.numeric(input$datatab_columns_selected)] <- as.numeric(as.factor(values$dfWorking[,as.numeric(input$datatab_columns_selected)]))
    }
    if(input$quant==2){
      #One Hot Encoding
      encoder <- onehot(data = as.data.frame(values$dfWorking[,as.numeric(input$datatab_columns_selected)]), stringsAsFactors = TRUE, addNA = TRUE, max_levels = 20)
      temp_df <- predict(encoder, as.data.frame(values$dfWorking[,as.numeric(input$datatab_columns_selected)]))
      nam_df <- as.data.frame(temp_df[0,])
      names(temp_df) <- names(nam_df)
      
      for(i in 1:ncol(temp_df)){
        values$dfWorking[[names(nam_df)[i]]] <- temp_df[,i]
      }
      values$dfWorking <- values$dfWorking[,-as.numeric(input$datatab_columns_selected)]
    }
  })
  
  # Normalisierung und Standardisierung
  observeEvent(input$applyStand, {
    if(input$stand==1){
      #Standardisierung
      values$dfWorking[,as.numeric(input$datatab_columns_selected)] <- scale(values$dfWorking[,as.numeric(input$datatab_columns_selected)])
    }
    if(input$stand==2){
      #Normalisierung
      range01 <- function(x){(x-min(x))/(max(x)-min(x))}
      values$dfWorking[,as.numeric(input$datatab_columns_selected)] <- range01(values$dfWorking[,as.numeric(input$datatab_columns_selected)])
    }
  })
  
  
  # FC
  output$cons.1 <- renderUI({
    req(reactdf$dflist)
    if(is.null(reactdf$dflist)) {return()}
    list(hr(),
         helpText(""),
         selectInput("cons.1", "Select 1", choices=names(values$dfWorking))
    )
  })
  
  output$cons.2 <- renderUI({
    req(reactdf$dflist)
    if(is.null(reactdf$dflist)) {return()}
    list(hr(),
         helpText(""),
         selectInput("cons.2", "Select 2", choices=names(values$dfWorking))
    )
  })
  
  observeEvent(input$applyCons, {
    if(input$consop==1){
      #+
      values$dfWorking[[paste(input$cons.1,input$consop,input$cons.2)]] <- values$dfWorking[[input$cons.1]]+values$dfWorking[[input$cons.2]]
    }
    if(input$consop==2){
      #-
      values$dfWorking[[paste(input$cons.1,input$consop,input$cons.2)]] <- values$dfWorking[[input$cons.1]]-values$dfWorking[[input$cons.2]]
    }
    if(input$consop==3){
      #*
      values$dfWorking[[paste(input$cons.1,input$consop,input$cons.2)]] <- values$dfWorking[[input$cons.1]]*values$dfWorking[[input$cons.2]]
    }
    if(input$consop==4){
      #/
      values$dfWorking[[paste(input$cons.1,input$consop,input$cons.2)]] <- values$dfWorking[[input$cons.1]]/values$dfWorking[[input$cons.2]]
    }
    names(values$dfWorking) <- make.names(names(values$dfWorking))
    })
    
    # Data Table
    output$datatabCons <- DT::renderDataTable(server = TRUE, selection = list(target = 'row+column'), filter = "top",{
      validate(
        need(values$dfWorking!="", "Please upload data or use example datasets")
      )
      consVals()
    })
    
    consVals <- reactive({
      req(values$dfWorking)
      values$dfWorking
    })
  
  # FS
  output$yval2 <- renderUI({
    req(reactdf$dflist)
    if(is.null(reactdf$dflist)) {return()}
    list(hr(),
         helpText(""),
         selectInput("yval2", "Select Target Value", choices=names(values$dfWorking))
    )
  })
  
  output$xval2  <- renderUI({
    
    pickerInput(inputId = "xval2", 
                label = "Select the Independent Variable(s)", 
                choices = names(values$dfWorking[!(names(values$dfWorking) %in% input$yval2)]), options = list(`actions-box` = TRUE), 
                multiple = TRUE)
    
  })
  
  output$varImp <- renderPlot({
    require(reactdf$dflist)
    validate(
      need(input$xval2!="", "Please select independent variables")
    )
    importance()
  })
  
  importance <- reactive({
    require(reactdf$dflist)
    #prep input
    control <- trainControl(method="repeatedcv", number=10, repeats=3)
    xval2_be <- toString(gsub(",","+",toString(input$xval2)))
    formula_model <- as.formula(paste(input$yval2, ' ~ ',  xval2_be))
    # train model
    model <- train(formula_model, data=values$dfWorking, method = "rpart", trControl = control, na.action = na.omit)
    # varimp
    importance <- varImp(model, scale=FALSE)
    varImp(model)$importance %>% 
      as.data.frame() %>%
      rownames_to_column() %>%
      arrange(Overall) %>%
      mutate(rowname = forcats::fct_inorder(rowname )) %>%
      ggplot()+
      geom_col(aes(x = rowname, y = Overall))+
      coord_flip()+
      theme_bw()
  })
  
  
  # Selektion
  observeEvent(input$applySelec, {
    namevec <- rbind(input$xval2,input$yval2)
    values$dfWorking <- values$dfWorking[ , (names(values$dfWorking) %in% namevec)]
    
  })
  
  # Selektion
  observeEvent(input$applySamp, {
    trainIndex <- createDataPartition(values$dfWorking[,1], p = input$trainsplit, 
                                      list = FALSE, times = 1)
    
    values$dfTrain  <- values$dfWorking[trainIndex,]
    values$dfTest   <- values$dfWorking[-trainIndex,]
    
    
  })
  
  # Data Table
  output$datatabtrain = DT::renderDataTable(
    trainvals(), server = TRUE, selection = list(target = 'row+column'), filter = "top"
  )
  
  trainvals <- reactive({
    req(input$applySamp)
    values$dfTrain
  })
  

  #-------------------------------Server Module 3-------------------------------#
  
  output$modelchoice3 <- renderUI({
    selectInput("model", "Model based on Choice",  choices = modelchoice())
  })
  
  output$modelchoice4 <- renderUI({
    selectInput("modeltotrain", "Model to Train",choices = input$model)
  })
  
  modelchoice <- reactive({
    choosemodel <- readRDS("resources/choosemodel.RData")
    toString(predict(choosemodel, data.frame("tas"= input$task, "acc"=input$accuracy,"rob"= input$robustness,"eas"= input$easeofuse, "int"=input$interpretability)))
  })
  
  output$modeltext <- renderUI({
    req(input$model)
    if(input$model=="rpart"){
      path <- file.path("resources/dt.md")
    }
    if(input$model=="svmLinear"){
      path <- file.path("resources/svmLinear.md")
    }
    if(input$model=="gbm"){
      path <- file.path("resources/gbm.md")
    }
    includeMarkdown(path)
  })
  

    
  #-------------------------------Server Module 4-------------------------------#
  
  output$yval3 <- renderUI({
    req(values$dfTrain)
    if(is.null(values$dfTrain)) {return()}
    list(hr(),
         helpText(""),
         selectInput("yval3", "Select Target Value", choices=names(values$dfTrain))
    )
  })
  
  nmdfTrain <- reactive({
    req(input$yval3)
    names(values$dfTrain)[!(names(values$dfTrain) %in% input$yval3)]
  })
  
  output$xval3  <- renderUI({
    
    pickerInput(inputId = "xval3", 
                label = "Select independent variable(s)", 
                choices = nmdfTrain(), options = list(`actions-box` = TRUE), 
                multiple = TRUE)
    
  })
  
  output$eval <- renderPlot({
    validate(
      need(input$yval3!= "","Choose target variable, independent variables, hyperparameter settings and and start training of ML-model")
    )
    eval()
  })
  
  
  
  eval <- eventReactive(input$applyTrain,{
    xval3_be <- toString(gsub(",","+",toString(input$xval3)))
    formula_model <- as.formula(paste(input$yval3, ' ~ ',  xval3_be))
    metric <- input$metric
    
    #Rpart
    if (input$modeltotrain=="rpart"){
      modellist.cart <- list()
        for (cp in input$cpval){
          grid <- expand.grid(cp=cp)
          fit.cart <- train(formula_model, data=isolate(values$dfTrain) , method=input$modeltotrain , tuneGrid = grid, na.action = na.omit)
          key <- paste("cp :", toString(cp))
          modellist.cart[[key]] <- fit.cart
        
      }
      values$modellist <- modellist.cart
    }
    #SVMRad
    if (input$modeltotrain=="svmLinear"){
      modellist.svmRad <- list()
        for (cost in input$cost){
          grid <- expand.grid(cost = cost)
          fit.svm <- train(formula_model,data= isolate(values$dfTrain) , method="svmLinear2", tuneGrid = grid, na.action = na.omit)
          key <- paste("C:", toString(cost))
          modellist.svmRad[[key]] <- fit.svm
          
      }
      values$modellist <- modellist.svmRad
    }
    #GBM
    if (input$modeltotrain=="gbm"){
      modellist.gbm <- list()
      n.trees.tune <-data.frame("0 to 50" = c((0:10)*5),
                                "0 to 100" = c((0:10)*10),
                                "0 to 200" = c((0:10)*20))
      
      for (n.trees in input$n.trees){
        for (interaction.depth in input$interaction.depth){
          for (shrinkage in input$shrinkage){
            for (n.minobsinnode in input$n.minobsinnode){
              grid <- expand.grid(.n.trees = n.trees.tune[, (names(n.trees.tune) %in% input$n.trees)], .interaction.depth = interaction.depth, .shrinkage=shrinkage, .n.minobsinnode=n.minobsinnode)
              fit.gbm <- train(formula_model, data = isolate(values$dfTrain) , method="gbm", verbose = FALSE, na.action = na.omit)
              key <- paste("N.Trees:", toString(n.trees), "I.Depth", toString(interaction.depth),"Shrinkage", toString(shrinkage), "n.Minobsinnode", toString(n.minobsinnode) )
              modellist.gbm[[key]] <- fit.gbm
            }
          }
        }
      }
      values$modellist <- modellist.gbm
    }

    
    # #compare results
    my.settings <- list(
      strip.background =list(col="#A0A0A0"),
      strip.border=list(col="black")
    )
    
    dplot <- dotplot(resamples(values$modellist), par.settings = my.settings)
    if(values$modellist[[levels(dplot$panel.args[[1]]$y)[1]]]$modelType=="Classification"){
      values$bestModell <- values$modellist[[levels(dplot$panel.args[[1]]$y)[tail(dplot$panel.args[[1]]$y, n = 1)]]]
    }
    if(values$modellist[[levels(dplot$panel.args[[1]]$y)[1]]]$modelType=="Regression"){
      values$bestModell <- values$modellist[[levels(dplot$panel.args[[1]]$y)[1]]]
    }
    dplot
  })
  
  #Evaluate
  output$metric <- renderUI({
    req(values$bestModell)
    if(values$bestModell$modelType=="Classification"){
      choices <- c("Accuracy")
    }
    if(values$bestModell$modelType=="Regression"){
      choices <- c("MAE", "RMSE")
    }
      
    selectInput("metric", "Choose Metric:" , choices = choices)
  })
  
  output$test <- renderPrint({
    testing()
  })
  
  testing <- eventReactive(input$applyEval,{
    if(input$metric=="MAE"){
      mae <- mean(abs(predict(values$bestModell, values$dfTest)-values$dfTest[[input$yval3]]))
      values$results <- paste(input$metric, ": ", mae)
    }
    if(input$metric=="RMSE"){
      rmse <- sqrt((mean(predict(values$bestModell, values$dfTest)-values$dfTest[[input$yval3]]))^2)
      values$results <- paste(input$metric, ": ", rmse)
    }
    if(input$metric=="Accuracy"){
      confMat <- confusionMatrix(predict(values$bestModell,values$dfTest), as.factor(values$dfTest[[input$yval3]]))
      values$results <- list(confMat$overall[1],confMat$table)
    }
    print(values$results)
  })
  
  #Predict
  output$predict <- renderPrint({
    validate(
      need(values$bestModell!="","")
    )
    paste(input$yval3,": ",pred.depl()[1])
  })
  
  output$testout <- renderUI({
    validate(
      need(values$bestModell!="","")
    )
    lapply(2:length(names(values$bestModell$trainingData)), function(i) {
      if(class(values$bestModell$trainingData[,i])=="character"){
        selectInput(paste0('a', i), paste0(names(values$bestModell$trainingData)[i]),
                    choices = values$bestModell$trainingData[,i])
      }
      if(class(values$bestModell$trainingData[,i])=="numeric" || class(values$bestModell$trainingData[,i])=="integer"){
        sliderInput(paste0('a', i), paste0(names(values$bestModell$trainingData)[i]),
                    min = min(values$bestModell$trainingData[,i], na.rm = TRUE), max = max(values$bestModell$trainingData[,i], na.rm = TRUE),
                    value = median(values$bestModell$trainingData[,i], na.rm = TRUE))
      }else{
        selectInput(paste0('a', i), paste0(names(values$bestModell$trainingData)[i]),
                    choices = values$bestModell$trainingData[,i])
      }
      
    })
  })
  
  output$a_out <- renderTable({
    validate(
      need(values$bestModell!="","Please train a ML-model first")
    )
    a_out()
  })
  
  pred.depl <- reactive({
    predict(isolate(values$bestModell), a_out())
  })
  
  a_out <- reactive({
    res <- lapply(2:length(names(values$bestModell$trainingData)), function(i){
      
      
      input[[paste0('a', i)]]
    } )
    input_df <- as.data.frame(res)
    names(input_df) <- names(values$bestModell$trainingData)[2:length(names(values$bestModell$trainingData))]
    input_df
  })
  
  output$rpartplot <- renderPlot({
    if(input$model=="rpart"){
      rpartplot.1()
    }
    
  })
  
  rpartplot.1 <-   eventReactive(input$applyTrain,{
    rpart.plot(isolate(values$bestModell$finalModel))
  })
  
  
  
  
  # #Download
  my_data <- reactive({ 
    switch(input$dataset,
           "dfWorking" = isolate(values$dfWorking),
            "dfTrain" = isolate(values$dfTrain),
            "dfTest" = isolate(values$dfTest))
    
  })
  
  callModule(downloadObj, id = "download1", data = my_data, extension = ".csv")
  
  
}

#-------------------------------RunApp-------------------------------#
#-------------------------------RunApp-------------------------------#
#-------------------------------RunApp-------------------------------#

# Run app
shinyApp(ui = ui, server = server)

