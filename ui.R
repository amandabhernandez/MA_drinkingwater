#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# set up page structure 
shinyUI(navbarPage("MA Drinking Water", 
                   tabPanel("What's in my water?", 
                            fluidPage(
                              #set theme
                              theme = shinytheme("flatly"),
                              tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                              #tags$head(tags$link(href = "button.js")),
                              #add analytics
                              #tags$head(includeHTML(("html/google-analytics.html"))),
                              fluidRow(
                                column(4,
                                #        #add panel with text on the left side of the page
                                       wellPanel(
                                #          h3("What are PFAS?"),
                                #          htmlOutput("pfas_exp"),
                                #          h3("Health Effects"),
                                #          htmlOutput("health_effects"),
                                         h3("What's in my water?"),
                                         uiOutput("instructions"),
                                         htmlOutput("summary"),
                                #          h3("What can I do?"),
                                #          p("There are many steps you can take to reduce your exposure to PFAS."),
                                #          htmlOutput("treatment"),
                                #          htmlOutput("community")
                                #          )
                                       )),
                                       #add input options 
                                       column(8,
                                              h2("My Water Report"),
                                              
                                              h4(""),
                                              fluidRow(column(6,uiOutput("town"),
                                                              uiOutput("chemicals")),
                                                       column(2, uiOutput("year"),
                                                              #button does not currently work, requires Rmd file 
                                                              actionButton("download", 
                                                                           "Download Report", 
                                                                           icon = icon("download")))),
                                              #add graph and table content 
                                              tabsetPanel(
                                                tabPanel("Graphs",
                                                         htmlOutput("hint"),
                                                         plotlyOutput("dw")),
                                                tabPanel("Table", DT::dataTableOutput("dw_table")))
                                       )
                                )
                              )
                              
                            ),
                            tabPanel(title = "FAQ",
                                     htmlOutput("FAQ_text")),
                            tabPanel("About",
                                     htmlOutput("about")))
)
