
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(h2("Characteristics of Ecosystem Health Over Time", align = "center")),
  
  br(),
  br(),
  
  # change visual elements
  tags$style(shiny_css),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # sidebar panel style
      style = "background-color: white; ",
      
      # Dropdown 1
      selectInput(
        inputId = "title",
        label = h4("VULNERABILITIES"),
        choices = c("Inequality",
                    "Segmentation",
                    "Insularity",
                    "Toxic speech",
                    "Division",
                    "News Avoidance",
                    "Trust",
                    "Chilled speech"),
        selected = "Inequality"
      ),
      
      # add space
      br(),
      
      # Dropdown 2
      selectInput(
        inputId = "title",
        label = h4("THREATS"),
        choices = c("Misinformation",
                    "Foreign Influence",
                    "Artificial Intelligence"),
        selected = FALSE
      ),
      
      #add space
      br(),
      
      # Dropdown 3
      selectInput(
        inputId = "title",
        label = h4("ENGAGEMENT WITH NEWS"),
        choices = c("News seeking",
                    "News sharing",
                    "News consumption"),
        selected = FALSE
      ),
      
      # add space
      br(),
      
      # Dropdown 4
      selectInput(
        inputId = "title",
        label = h4("ENGAGEMENT WITH NEWS OUTLETS"),
        choices = c("Top 5 outlets",
                    "Local vs National outlets",
                    "Platforms"),
        selected = FALSE
      ),
      
      # add space
      br(),
      
      # Dropdown 5
      selectInput(
        inputId = "title",
        label = h4("ENGAGEMENT WITH POLITICIANS"),
        choices = c("Engagement with political leaders",
                    "Top social media platforms",
                    "Politicians vs news engagement"),
        selected = FALSE
      )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      # add a text output
      span(textOutput("TITLE"), style = "color:#467742; font-style: bold; font-size: 25px;"),
      span(textOutput("subtitle"), style = "color:FAF9F6; font-style: italic; font-size: 18px;"),
      
      #plot output
      plotlyOutput("plot"),
      
      hr(),
      
      textOutput("text1"),
      
      hr(),
      
      textOutput("text2"),
      
      hr()
      
    )
  )
)
