### Author: AHz
### Date published: 


# set up page structure 
shinyUI(navbarPage("",
                   id = "nav",
                   tabPanel("What's in my water?", icon = fontawesome::fa_i("tint"), 
                            fluidPage(
                              #set theme
                              theme = shinythemes::shinytheme("flatly"),
                              tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                              #add analytics
                              tags$head(includeHTML(("html/google-analytics.html"))),
                              #image from: https://unsplash.com/photos/wfqdDQY429U
                              fluidRow(HTML("
                                     <section class='banner'>
                                     <h1 class='ban_text'>What's in my water?</h1>
                                     <h2 class='ban_text'> Massachusetts Drinking Water Quality Tool</h2>
                                     </section>")),
                              fluidRow(column(8,offset = 2, 
                                              includeHTML("html/landing_text.html"),
                                              hr()),
                              br(),
                              ),
                              fluidRow(column(4),
                                       column(4, actionButton("start", "Get Started", width = 300))),
                              fluidRow(br())
                            )),
                   
                   tabPanel("My Water Report", value = "report", 
                            fluidPage(
                              fluidRow(
                                column(2, offset = 10, actionButton("help", "Instructions", 
                                                                    icon =fontawesome::fa_i("question"))),
                                column(12, h3("My Water Report", align = "center"),
                                ),
                                
                                column(12, hr()), 
                                
                                column(4,
                                       #add panel with text on the left side of the page
                                       wellPanel(
                                         h4("What's in my water?"),
                                         uiOutput("instructions"),
                                         
                                         htmlOutput("summary"),
                                       )),
                                #add input options 
                                column(8,
                                       #h2("My Water Report"),
                                       br(), 
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
                                                  h4(""),
                                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                                   tags$div("Loading...",id="loadmessage")
                                                  ),
                                                  plotlyOutput("dw")),
                                         tabPanel("Table", DT::dataTableOutput("dw_table")))
                                )
                              )
                            )
                            
                   ),
                   tabPanel(title = "FAQ",
                            h3("FAQ", align = "center"), 
                            br(), 
                            column(7, includeHTML("html/FAQ.html")),
                            column(1),
                            column(4, wellPanel(h4("Key definitions", align = "center"),
                                                includeHTML("html/definitions.html")))
                            ),
                   tabPanel("About",
                            includeHTML("html/about.html")),
                   
                   div(
                     HTML("<footer></footer>")
                   )
)
)
