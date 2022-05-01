### Author: AHz
### Date published: 


# set up page structure 
shinyUI(navbarPage("",
                   tabPanel("What's in my water?", icon = fontawesome::fa_i("tint"), 

                            #image from: https://unsplash.com/photos/wfqdDQY429U
                            # titlePanel(title = div(img(src='banner.jpg', height="75%", width="100%")),
                            #            h1("What's in my water?", align = "center")),
                            fluidPage(
                              #set theme
                              theme = shinythemes::shinytheme("flatly"),
                              tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                              #add analytics
                              tags$head(includeHTML(("html/google-analytics.html"))),
                              fluidRow(HTML("
                                     <section class='banner'>
                                     <h1 class='ban_text'>What's in my water?</h1>
                                     </section>
                                     ")),
                              fluidRow(column(8,offset = 2, 
                                              includeHTML("html/landing_text.html"),
                                              hr()))
                              

                              # h1("What's in my water?", align = "center"),
                              # wellPanel(includeHTML("html/landing_text.html")),
                            )),
                   
                   tabPanel("My Water Report", 
                            fluidPage(
                              fluidRow(
                                column(12, h1("My Water Report", align = "center")
                                ),
                                column(12, hr()), 

                                column(4,
                                 #add panel with text on the left side of the page
                                       wellPanel(
                                         h3("What's in my water?"),
                                         uiOutput("instructions"),
                                         actionButton("help", "Show Instructions", 
                                                      icon =fontawesome::fa_i("question")), 
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
                                     includeHTML("html/FAQ.html")),
                            tabPanel("About",
                                     includeHTML("html/about.html")))
)
