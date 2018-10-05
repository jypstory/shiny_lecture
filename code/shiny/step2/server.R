library(rvest)
library(httr)
library(KoNLP)
library(stringr)
library(tm)
library(qgraph)
library(xml2)
library(dplyr)
library(networkD3)
library(DT)

library(igraph)
library(tidyverse)
library(threejs)
library(readxl)
library(ggpubr)
library(forcats)
library(extrafont)
loadfonts()

shinyServer(function(input, output) {
  
  ## --------------------------------------------------------------------##
  ## Tab1 :: IRIS, IRIS_
  IRIS_selectedData <- reactive({
    iris[, c(input$IRIS_xcol, input$IRIS_ycol)]
  })
  
  IRIS_kclusters <- reactive({
    kmeans(IRIS_selectedData(), input$IRIS_clusters)
  })
  
  output$IRIS_plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    #par(mar = c(5.1, 4.1, 0, 1))
    plot(IRIS_selectedData(),
         col = IRIS_kclusters()$cluster,
         pch = 20, cex = 3)
    points(IRIS_kclusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  ## --------------------------------------------------------------------##
  ## Tab2 :: Data EDA,  prefix - EDA_
  function_eda_dataset <- eventReactive(input$EDA_action, {
    df <- read.csv(input$EDA_file1$datapath,
                   header = input$EDA_header,
                   sep = input$EDA_sep,
                   quote = input$EDA_quote)
    
    result <- list()
    result$df <- df
    
    return(result)
  })
  
  output$EDA_contents <- renderTable({
    
    tryCatch(
      {
        df <- function_eda_dataset()$df
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$EDA_disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  output$EDA_summary <- renderPrint({
    df <- function_eda_dataset()$df
    summary(df)
  })
  
  output$EDA_view <- renderTable({
    head(function_eda_dataset()$df, n = input$EDA_obs)
  })
})

