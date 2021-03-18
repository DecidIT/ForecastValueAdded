#------------------------------------------------#
#-------------- DEMO FVA - DecidIT --------------#
#------------------------------------------------#
source("Code/pack_ui.txt")
source("Code/func_ui.txt")
library(dygraphs)

display.mode="showcase"

navbarPage("PROTOTYPE: Forecast Improvement", #("HiFi : HIghway to Forecast Improvement",
           theme = shinythemes::shinytheme("sandstone"),
#---------------------------------#      
#--------  MENU SESSION ----------#
#---------------------------------#       
tabsetPanel(selected = "FVA", # Tempo1
           tabPanel("Session",
                    fluidPage(
                      titlePanel("Session"),
                      sidebarLayout(
                        sidebarPanel(
                           imageOutput("ImageLogo1", width = "100%", height = 50),
                           tabsetPanel(id = "SideTab", selected = "Inputs",  type = "pills",
                           tabPanel("Inputs",
                           br(),
                           tags$div(
                             actionLink("SampleDataLink", "Use sample data"),
                             br(),
                             tags$strong("Choose csv file to upload"),
                           ),
                           fileInput("UserFile", NULL,
                                     accept=c('text/csv',
                                              'text/comma-separated-values,text/plain',
                                              '.csv')),
                           # checkboxInput("header", "Header", TRUE),
                           radioButtons("sep", "Separator",
                                        c(Comma=',', Semicolon=';', Tab='\t'), ';'),
                           #  radioButtons("quote", "Quote",
                           #              c(None='', 'Double Quote'='"', 'Single Quote'="'"),
                           #              '"'),
                           br(),
                           uiOutput("DisplayType"),
                           uiOutput("NbBins"),
                           br(),
                           fluidRow(themeSelector()),  #fluidRow
                           hr(),
                           p(em("Developed by"),br("DecidIT LTD"),style="text-align:center; font-family: times")
                           
                           ) #, #tabPanel("Inputs"
                         #tabPanel("Filters"),
                        # tabPanel("How-To")
                          ) #tabsetPanel
                        ), #sidebarPanel
#-
          mainPanel(
            tabsetPanel(
#---              
#--- TAB "Business data" ---#
#---              
              tabPanel("Business data",
                       tags$style(".fa-database {color:#E87722}"),
                       h3(p(em("Dataset "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                        br(),
                        fluidRow(
                          DT::dataTableOutput('ContentTable')),
                        br(),
                        fluidRow(
                           plotOutput("BusinessDataPlot"))
                       #fluidRow(
                       #    dygraphOutput('ContentPlot')),
                      ),# Temporary,
#---              
#--- TAB "Application" ---#
#---               
            tabPanel("Application",
            tags$style(".fa-glasses {color:#E87722}"),
            h3(p(em("Description"),icon("book-reader",lib = "font-awesome"),style="color:black;text-align:center")),
            br(),
            imageOutput("FileFormat", width = "100%", height = 50),         
         )  # TtabPanel "Application"

            )   #tabsetPanel
         )  #MainPanel
                    )  #sidebarLayout
              )  #fluidPage
      ), #tabPanel("Session")
          
#-----------------------------#      
#--------  MENU FVA ----------#
#-----------------------------#
           tabPanel("FVA",
                    fluidPage(
                     titlePanel("Forecast Value Added (FVA)"),
                      sidebarLayout(
                        sidebarPanel(
                          imageOutput("ImageLogo2", width = "100%", height = 50),
                          tabsetPanel(id = "SideTab", selected = "Inputs",  type = "pills",
                            tabPanel("Inputs",
                          br(),
                          uiOutput("ErrorMetric"),

                          #tags$style(".fa-wikipedia-w {color:black}"),
                          #p("Read more about normal distribution here → ", a(href="https://en.wikipedia.org/wiki/Normal_distribution", icon("wikipedia-w"),target="_blank"),style="color:black;text-align:center"),
                          uiOutput("ReadWiki01"),
                          uiOutput("NaiveModel"),
                          uiOutput("SnFrequ"),
                          uiOutput("MaOrder")
                            ), #tabPanel("Inputs",
                        tabPanel("Filters",
                                 br(),
                                 uiOutput('Filter1'),
                                 uiOutput('Filter2')
                        ),
                        tabPanel("How-To",
                                 includeMarkdown("Texts/HowToFVA.Rmd"),
                                 #shinythemes::themeSelector()
                        )
                         ) #tabsetPanel
                       ), #sidebarPanel
                         mainPanel(
                           tabsetPanel(id = "FVATab", selected = "Metrics", type = "pills",
#---              
#--- TAB "Metrics" ---#
#--- 
tabsetPanel(selected = "Application", #tempo02
            
                                        tabPanel("Metrics",
                                                 tags$style(".fa-chart-bar {color:#E87722}"),
                                                 h3(p(em("Results"),icon("chart-bar",lib = "font-awesome"),style="color:black;text-align:center")),
                                                 br(),
                                               tableOutput("ResultFVA"),
                                               #$$$ Test
                                               #tableOutput("MaseResults"),
                                               #$$$ Test
                                               br(),
                                               fluidRow(
                                                 tags$head(tags$style("#ExplainFVA{color: brown;
                                                      font-size: 15px;
                                                             font-style: italic;
                                                             font-weight: bold;
                                                             text-align: center
                                                             }")),
                                               column(width=1, icon("share","fa-igloo"),align="center"),
                                               column(
                                               textOutput('ExplainFVA'),
                                               width = 8,style="background-color:white;border-left:8px solid brown"
                                               )
                                               ),
                                               br(),
                                               fluidRow(
                                                 conditionalPanel(
                                                    condition = "input.ErrorMetric == 'MASE'",
                                                    plotOutput('ContentMASE')
                                                 ),
                                                 dygraphOutput('ContentError')
                                                 ),
                                               br(),
                                               fluidRow(
                                                 plotOutput('ContentFVA')
                                               )
                                              ),
#---              
#--- TAB "Analysis" ---#
#--- 
                                    tabPanel("Analysis",
                                             tags$style(".fa-glasses {color:#E87722}"),
                                             h3(p(em("Insight"),icon("glasses",lib = "font-awesome"),style="color:black;text-align:center")),
                                             br(),
                                            fluidRow(
                                              column(4,
                                                 uiOutput('StepA')
                                                    ),
                                             column(4,
                                                 uiOutput('StepB')
                                                   )
                                            ),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.ErrorMetric == 'MASE'",
                                                plotOutput('CompareMASE1') 
                                              ),
                                              conditionalPanel(
                                                condition = "input.ErrorMetric != 'MASE'",
                                              dygraphOutput('CompareError')
                                              ),
                                            ),
                                            
                                            br(),
                                            fluidRow(
                                              tags$head(tags$style("#ExplainError{color: brown;
                                                      font-size: 15px;
                                                             font-style: italic;
                                                             font-weight: bold;
                                                             text-align: center
                                                             }")),
                                              column(width=1, icon("share","fa-igloo"),align="center"),
                                              column(
                                                htmlOutput("ExplainError"),
                                                width = 8,style="background-color:white;border-left:8px solid brown"
                                              )
                                            ),
                                            br(),
                                            
                                           fluidRow(
                                             conditionalPanel(
                                               condition = "input.ErrorMetric == 'MASE'",
                                               plotOutput('CompareMASE2')
                                            ),
                                              dygraphOutput('AnalysFVA')
                                            ),
                                          
                                           ),
#---              
#--- TAB "Inferences" ---#
#--- 
                                    tabPanel("Inferences",
                                             tags$style(".fa-microscope {color:#E87722}"),
                                             h3(p(em("Insight"),icon("microscope",lib = "font-awesome"),style="color:black;text-align:center")),
                                             br(),                                               
                                             fluidRow( column(width=1, icon("share","fa-igloo"),align="center"),
                                                      column(
                                                        p("To measure the forecasts process efficiency, it is necessary to establish 
                                     that the errors mean generated by one forecast step is significantly different from the errors'mean generated by another forecast step
                                                          and specifically by the naive forecast",style="color:black;text-align:justify"),
                                                        withMathJax(),
                                                        p('$$H_0: ~ \\mu1 ~-~ \\mu2 ~=~ 0$$',style="color:black;border:1px solid black;background-color:white"),
                                    #                     p("In our case we will take as a response variable", strong(em("Personal injuries")), "since it represents a big 
                                    # safety problem where the physical integrity of the people is threatened. We will try to explain this 
                                    # variable through education issues, others related to sports and even through other safety problems. All of these,
                                    # represented through the other variables in the dataset",style="color:black;text-align:justify"),
                                                         width=11,style="background-color:lavender;border-radius: 10px")
                                              ),
                                             br(),                                            
                                    fluidRow(
                                      column(4,
                                             uiOutput('StepC')
                                      ),
                                      column(4,
                                             uiOutput('StepD')
                                      )
                                    ),
                                          fluidRow(
                                            column(br(),plotOutput("InfHist"),br(),width=4,style="border:1px solid black"),
                                            column(br(),plotOutput("InfBplot"),br(),width=4,style="border: 1px solid black;border-left: none"),
                                            column(br(),plotOutput("InfQqplot"),br(),width=4,style="border:1px solid black;border-left:none")
                                          ),
                                          br(),
                                          fluidRow(
                                            tags$head(tags$style("#ExplainInference{color: brown;
                                                      font-size: 15px;
                                                             font-style: italic;
                                                             font-weight: bold;
                                                             text-align: center
                                                             }")),
                                            br(),
                                            column(width=1, icon("share","fa-igloo"),align="center"),
                                            column(
                                            htmlOutput("ExplainInference"),
                                            width = 8,style="background-color:white;border-left:8px solid brown"
                                            )
                                          ),
                                          br(),
                                          ),
                                tabPanel("Application",
                                    tags$style(".fa-book-reader {color:#E87722}"),
                                    h3(p(em("Explanation"),icon("book-reader",lib = "font-awesome"),style="color:black;text-align:center")),
                                    br(),
                                    includeMarkdown("Texts/FVA.Rmd"),
                                    imageOutput("ImageHelp1", height = 150),
                                    br(),
                                    #pre(includeText("Texts/FVAfig1.txt")),
                                    includeMarkdown("Texts/FVA2.Rmd")
                                ) #tabPanel "Application"
                              ) #tabSetPanel #Temp02
                           ) #tabsetPanel
                         ) #mainPanel  
                      ) #sidebarLayout
                    ) #fluidPage  
                  ) #tabPanel FVA
                ),  #tabSetPanel #temp01
#      navbarMenu("Forecast",   #See ?naive
           # tabPanel("Bias",
           # fluidRow(
           #   column(4, plotOutput("SalesBoxPlot")
           #          ),
           #   column(8, plotOutput("SalesFcst")
           #          )
           # ),
           # fluidRow(
           #   plotOutput("BiasPlot")
           # )
           # ),
           # tabPanel("Seasonality")
#      ), #navbarMenu

id = "NavBar") #navbarPage
