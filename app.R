source('global.R')


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(h1("Health of the Canadian Information Ecosystem (CIE)", align = "center")),
  
  br(),
  br(),
  br(),
  
  # change visual elements
  tags$style(shiny_css),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        radioButtons(
        inputId = "title",
        label = h2("Select vulnerability:"),
        choices = c("Inequality",
                    "Segmentation",
                    "Insularity",
                    "Toxic speech",
                    "Division",
                    "News Avoidance",
                    "Trust",
                    "Chilled speech"),
        selected = "Inequality",
        ),

        # add space
        br(),
        br(),
        
        # add a text output
        span(textOutput("text"), style = "color:FAF9F6; font-style: italic; font-size: 15px;"),
        
        # add text output
        span(textOutput("text1"), style = "color:#467742"),
        style = "background-color: black; border: black; "
      ),
        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("plot")
          )
      )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #reactive elements
  rval_data <- reactive({
    df_vulnerability |>
      filter(title == input$title)
  })
  
  rval_text <- reactive({
    df_text |>
      filter(title == input$title)
  })
  
  # text output
  output$text <- renderText({
    
    text <- rval_text() |>
      select(subtitle) |>
      toString()
    
  })
  
  # text output
  output$text1 <- renderText({
    
    text1 <- rval_text() |>
      select(text) |>
      toString() 
    
    })
  
  # plot output
  output$plot <- renderPlotly({
    
    plot <-  rval_data() |>
      mutate(month = factor(month, levels = month.name, ordered = TRUE)) |>
      ggplot(aes(month, 
                 value,
                 text = paste0("Month: ", month,
                              "\nValue: ", value,
                              "\nMeasure: ", label))) +
      geom_line(aes(group = label,
                    color = label),
                linewidth = 0.5) +
      geom_point(aes(color = label),
                 size = 2.5,
                 stroke = 0.7) +
      scale_color_manual(values = color_list) +
      labs(x = "",
           color = "") +
      theme_minimal(base_family = "poppins", base_size = 12) +
      theme(legend.position = "top")
    
    if (input$title == "Inequality") {
      plot <- plot +
        scale_y_continuous(limits = c(0,1),
                           breaks = seq(0, 1, 0.2)) +
        labs(y = "gini coefficient",
             title = "Inequality") 
      
    } else if (input$title == "Segmentation") {
      plot <- plot + 
        scale_y_continuous(limits = c(0,1),
                           breaks = seq(0, 1, 0.2)) +
        labs(y = "gini coefficient",
             title = "Segmentation")
      
    } else if (input$title == "Insularity") {
      plot <- plot + 
        scale_y_continuous(limits = c(0,0.3),
                           breaks = seq(0, 0.3, 0.05)) +
        labs(y = "",
             title = "Insularity")
      
    } else if (input$title == "Toxic speech") {
      plot <- plot +
        scale_y_continuous(limits = c(0, 0.1),
                           breaks = seq(0, 0.1, 0.05)) +
        labs(y = "",
             title = "Toxic speech")
      
    } else if (input$title == "News Avoidance") {
      plot <- plot +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0, 100, 20),
                           labels = label_percent(scale = 1)) +
        labs(y = "Percent of survey responders",
             title = "News avoidance")
      
    } else if (input$title == "Chilled speech") {
      plot <- plot +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0, 100, 20),
                           labels = label_percent(scale = 1)) +
        labs(y = "Percent of survey responders",
             title = "Chilled speech")
      
    } else if (input$title == "Division") {
      plot <- plot +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0, 100, 20),
                           labels = label_percent(scale = 1)) +
        labs(y = "Percent of survey responders",
             title = "Division")
      
    } else {
      plot <- plot +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0, 100, 20),
                           labels = label_percent(scale = 1)) +
        labs(y = "Percent of survey responders",
             title = "Trust")
    
    }
    
    ggplotly(plot, tooltip = "text") |>
      layout(legend = list(orientation = 'h', x = 0.2, y = 1.05),
        hoverlabel = list(bgcolor = "white",
                          font = list(size = 15, color = "black")))
  }) 
} 


# Run the application 
shinyApp(ui = ui, server = server)
