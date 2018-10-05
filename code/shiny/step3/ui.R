#install.packages(c('rvest','httr','KoNLP','stringr','tm','qgraph','xml2','dplyr','networkD3'))
#install.packages(c('igraph','tidyverse','threejs','readxl','ggpubr','forcats','extrafont'))

library(shiny)
library(shinythemes)
library(DT)
library(networkD3)
library(threejs)

shinyUI(
  navbarPage("My Shiny",
             #theme = shinytheme("united"),
             #shinythemes::themeSelector(),  
             
             ## Tab1 :: IRIS,  prefix - IRIS_
             tabPanel("Iris Data Example",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('IRIS_xcol', 'X Variable', names(iris)),
                          selectInput('IRIS_ycol', 'Y Variable', names(iris),
                                      selected=names(iris)[[2]]),
                          numericInput('IRIS_clusters', 'Cluster count', 3,
                                       min = 1, max = 9)
                        ),
                        mainPanel(
                          plotOutput('IRIS_plot1')
                        )
                      )
             ),
             
             ## Tab2 :: Data EDA,  prefix - EDA_
             tabPanel("Data EDA",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          # Input: Select a file ----
                          fileInput("EDA_file1", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Checkbox if file has header ----
                          checkboxInput("EDA_header", "Header", TRUE),
                          
                          # Input: Select separator ----
                          radioButtons("EDA_sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ","),
                          
                          # Input: Select quotes ----
                          radioButtons("EDA_quote", "Quote",
                                       choices = c(None = "",
                                                   "Double Quote" = '"',
                                                   "Single Quote" = "'"),
                                       selected = '"'),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Select number of rows to display ----
                          radioButtons("EDA_disp", "Display",
                                       choices = c(Head = "head",
                                                   All = "all"),
                                       selected = "head"),
                          
                          numericInput(inputId = "EDA_obs",
                                       label = "Number of observations to view:",
                                       value = 10),
                          
                          actionButton("EDA_action", "Action", class = "btn-primary")
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          tabsetPanel(
                            tabPanel("View Data", # Output: Data file ----
                                     h3("View Data"),
                                     tableOutput("EDA_contents")),
                            
                            tabPanel("Summary Data", 
                                     h3("Summary"),
                                     
                                     # Output: Verbatim text for data summary ----
                                     verbatimTextOutput("EDA_summary"),
                                     
                                     # Output: HTML table with requested number of observations ----
                                     tableOutput("EDA_view")
                            ), 
                            tabPanel("Table", 
                                     h3("Todo... "),
                                     tableOutput("table")
                            )
                          )
                        )
                      )
             ),
             
             ## --------------------------------------------------------------------##
             ## Tab3 :: Text Mining ,  prefix - TM_
             navbarMenu("Text Mining",
                        tabPanel("Crawling Movie",
                                 sidebarLayout(
                                   sidebarPanel(
                                     textInput("TM_txt_url", "Input URL:", "https://movie.daum.net/moviedb/grade?movieId=99611&type=netizen&page="),
                                     
                                     sliderInput("TM_crawling_page_slider", "Crawling Page:", 1, 500, 200),
                                     
                                     # tags$h5("Deafult actionButton:"),
                                     # actionButton("TM_default_action", "Search"),
                                     
                                     tags$h5("Start Crawling:"),
                                     actionButton("TM_start_action", "Crawling Action", class = "btn-primary")
                                   ),
                                   
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Reviews", DT::dataTableOutput("TM_tbl_reviews")), 
                                       tabPanel("Draw qgraph",  plotOutput("TM_qgraph")), 
                                       tabPanel("Draw networkD3 ", forceNetworkOutput("TM_nw3d"))
                                     )
                                   )
                                 )
                        ),
                        tabPanel("Crawling Facebook")
             ),
             
             ## --------------------------------------------------------------------##
             ## Tab4 :: mers ,  prefix - MRS_
             ## Tab2 :: Data EDA,  prefix - EDA_
             tabPanel("MERS Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          # Input: Select a file ----
                          fileInput("MRS_file", "Choose File",
                                    multiple = FALSE
                          ),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          actionButton("MRS_action", "Action", class = "btn-primary")
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          tabsetPanel(
                            tabPanel("View Data",
                                     h3("View Data"),
                                     scatterplotThreeOutput("MERS_graphjs", width = "100%", height = "800px")
                            )
                          )
                        )
                      )
             )
  )
)