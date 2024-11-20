source('global.R')

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
        inputId = "category",
        label = h4("Ecosystem characteristics"),
        choices = c("Vulnerabilities",
                    "Threats",
                    "Engagement with news",
                    "Engagement with news outlets",
                    "Engagement with politicians"),
        selected = "Vulnerabilities"),
        
        # Dynamic ui output based on the first selection
        uiOutput("ui")
      ),

        # Show a plot of the generated distribution
        mainPanel(
          # add a text output
          span(textOutput("TITLE"), style = "color:#467742; font-style: bold; font-size: 25px;"),
          span(textOutput("subtitle"), style = "color:FAF9F6; font-style: italic; font-size: 18px;"),
          
          #plot output
          plotlyOutput("plot"),
          
          hr(),
          
          fluidRow(
            column(3,
                   absolutePanel(height = 500,
                                 wellPanel("Current Month: value \n6-Month Average: value"))),
          column(9, wellPanel(textOutput("text1")))
          ),
          
          hr(),
          
          textOutput("text2"),
          
          hr()
          
          )
      )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # ui output depending on the selection
  
  output$ui <- renderUI({
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$category,
           "Vulnerabilities" = awesomeRadio("title", "",
                                            choices = c("Inequality",
                                                        "Segmentation",
                                                        "Insularity",
                                                        "Toxic speech",
                                                        "Division",
                                                        "News avoidance",
                                                        "Trust",
                                                        "Chilled speech"),
                                            selected = "Inequality"),
           "Threats" = awesomeRadio("title", "",
                                   choices = c("Concern about misinformation",
                                               "Concern about foreign influence",
                                               "Concern about Generative AI"),
                                   selected = "Concern about misinformation"),
           "Engagement with news" = awesomeRadio("title", "",
                                    choices = c("News seeking",
                                                "News sharing",
                                                "News consumption"),
                                    selected = "News seeking"),
           "Engagement with news outlets" = awesomeRadio("title", "",
                                                 choices = c("Top 5 outlets",
                                                             "Local vs National news engagement",
                                                             "Platforms used by outlets"),
                                                 selected = "Top 5 outlets"),
           "Engagement with politicians" = awesomeRadio("title", "",
                                                         choices = c("Social media platforms",
                                                                     "Engagement with federal political party leadership",
                                                                     "Engagement with elected party members",
                                                                     "Politicians vs. news engagement"),
                                                         selected = "Social media platforms")
           )
  })
  
  
  #reactive elements
  rval_data <- reactive({
    df_app |>
      filter(title == input$title)
  })
  
  rval_text <- reactive({
    df_text |>
      filter(title == input$title)
  })
  
  # text output
  output$TITLE <- renderText({
    
    TITLE <- rval_text() |>
      select(title_upper) |>
      toString()
    
  })
  
  # text output
  output$subtitle <- renderText({
    
    subtitle <- rval_text() |>
      select(subtitle) |>
      toString()
    
  })
  
  # text output
  output$text1 <- renderText({
    
    text1 <- rval_text() |>
      select(text) |>
      toString() 
    
    })
  
  # text output
  output$text2 <- renderText({
    
    text2 <- rval_text() |>
      select(text) |>
      toString() 
    
  })
  
  output$text3 <- renderText({
    text3 <- "Current Month"
  })
  
  # plot output
  output$plot <- renderPlotly({
    
    plot <- rval_data() |>
      # add new month-year at the list every month
      mutate(month_year =  factor(month_year,levels = c("Jan-24",
                                                         "Feb-24",
                                                         "Mar-24",
                                                         "Apr-24",
                                                         "May-24",
                                                         "Jun-24",
                                                         "Jul-24",
                                                         "Aug-24",
                                                         "Sep-24",
                                                         "Oct-24")),
             ordered = TRUE) |>
      ggplot(aes(month_year, 
                 value,
                 text = paste0("Month: ", month,
                              "\nValue: ", value,
                              "\nMeasure: ", label))) +
      geom_line(aes(group = label,
                    color = label),
                linewidth = 0.5) +
      geom_point(aes(color = label),
                 size = 2.5,
                 stroke = 0.7)  +
      scale_color_manual(values = color_list) +
      labs(x = "",
           color = "") +
      theme_minimal(base_family = "poppins", base_size = 12) +
      theme(legend.position = "bottom")
    
    if (input$title == "Inequality") {
      plot <- plot +
        scale_y_continuous(limits = c(0,1),
                           breaks = seq(0, 1, 0.2)) +
        labs(y = "gini coefficient") 
      
    } else if (input$title == "Segmentation") {
      plot <- plot + 
        scale_y_continuous(limits = c(0,1),
                           breaks = seq(0, 1, 0.2)) +
        labs(y = "gini coefficient")
      
    } else if (input$title == "Insularity") {
      plot <- plot + 
        scale_y_continuous(limits = c(0,0.3),
                           breaks = seq(0, 0.3, 0.05)) +
        labs(y = "")
      
    } else if (input$title == "Toxic speech") {
      plot <- plot +
        scale_y_continuous(limits = c(0, 0.1),
                           breaks = seq(0, 0.1, 0.05)) +
        labs(y = "")
      
    } else if (input$title == "News avoidance") {
      plot <- plot +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0, 100, 20),
                           labels = label_percent(scale = 1)) +
        labs(y = "Percent of survey responders")
      
    } else if (input$title == "Chilled speech") {
      plot <- plot +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0, 100, 20),
                           labels = label_percent(scale = 1)) +
        labs(y = "Percent of survey responders")
      
    } else if (input$title == "Division") {
      plot <- plot +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0, 100, 20),
                           labels = label_percent(scale = 1)) +
        labs(y = "Percent of survey responders")
      
    } else {
      plot <- plot +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0, 100, 20),
                           labels = label_percent(scale = 1)) +
        labs(y = "Percent of survey responders")
    
    } 
    
    ggplotly(plot, tooltip = "text") |>
      layout(legend = list(orientation = 'h', x = 0.1, y = -0.1, hjust = 0.5),
        hoverlabel = list(bgcolor = "white",
                          font = list(size = 15, color = "black")))
  }) 
} 


# Run the application 
shinyApp(ui = ui, server = server)
