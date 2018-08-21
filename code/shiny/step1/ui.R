library(shiny)

shinyUI(navbarPage("My Shiny",
                   tabPanel("Component 1",
                            sidebarLayout(
                              sidebarPanel(
                                textInput("text", label = h3("Text input"), value = "Enter text...")
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Plot", plotOutput("plot")), 
                                  tabPanel("Summary", verbatimTextOutput("summary")), 
                                  tabPanel("Table", tableOutput("table"))
                                )
                              )
                            )
                   ),
                   tabPanel("Component 2"),
                   navbarMenu("More",
                              tabPanel("Sub-Component A",
                                       sidebarLayout(
                                         
                                         sidebarPanel(
                                           # Inputs excluded for brevity
                                         ),
                                         
                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("Plot2", plotOutput("plot2")), 
                                             tabPanel("Summary2", verbatimTextOutput("summary2")), 
                                             tabPanel("Table2", tableOutput("table2"))
                                           )
                                         )
                                       )
                                       
                                       
                              ),
                              tabPanel("Sub-Component B")
                   )
))

