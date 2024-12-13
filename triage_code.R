
ui <- fluidPage(
  br(),
  
  # change visual elements
  tags$style(shiny_css),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(width = 4,
                 style = "background: white; border:0; ",
                 span(textOutput("text1"), style = "font-weight:bold; font-size: 20px;"),
                 br(),
                 selectInput(inputId = "category",
                             label = h4(strong("Select category:")),
                             choices = c("Vulnerabilities",
                                         "Threats",
                                         "Engagement with news",
                                         "Engagement with news outlets",
                                         "Engagement with politicians"),
                             selected = "Vulnerabilities"),
                 uiOutput("ui"),
                 
                 br(),
                 
                 wellPanel(style = "background: white; border-width:0; box-shadow:0px; border-top:0px; ",
                           tableOutput("table"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # add a text output
      span(textOutput("TITLE"), style = "color:#467742; font-style: bold; font-size: 25px;"),
      span(textOutput("subtitle"), style = "color:FAF9F6; font-style: italic; font-size: 18px;"),
      
      #plot output
      plotlyOutput("plot"),
      br(),
      
      span(htmlOutput("text2"), style = "color:#467742; font-size: 16px;"),
      textOutput("MEANING"),
      
      hr(style="border-color: #828282;"),
      
      span(htmlOutput("text3"), style = "color:#467742; font-size: 16px;"),
      textOutput("MEASUREMENT"),
      
      hr(style="border-color: #828282;"),
      
      br(),
      br(),
      
      textOutput("footer"),
      br()
      
    )
    
  )
)


server <- function(input, output, session) {
  
}

shinyApp(ui, server)


