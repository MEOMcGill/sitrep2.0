source('global.R')


# Define UI for application that draws a histogram
ui <- fluidPage(
  # choose a theme

    # Application title
    titlePanel(h1("Health of the Canadian Information Ecosystem (CIE)")),
  
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
                    "Toxic speech"),
        selected = "Inequality"),

        # add space
        br(),
        br(),
        
        # add text output
        span(textOutput("text"), style = "color:#467742"),
      ),
        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("plot")
          )
      )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  # text output
  output$text <- renderText({
    if (input$title == "Inequality") {
      text <- inequality
    } else if (input$title == "Segmentation") {
      text <- segmentation
    } else if (input$title == "Insularity") {
      text <- insularity
    } else {
      text <- toxicity
      return(text) }
  })
  
  # plot output
  output$plot <- renderPlotly({
    
    plot <-  df_vulnerability |>
      filter(title == input$title) |>
      ggplot(aes(month, value)) +
      geom_line(aes(group = measure,
                    color = measure),
                linewidth = 0.5) +
      geom_point(aes(color = measure),
                 size = 2.5,
                 stroke = 0.7) +
      scale_color_manual(values = color_list) +
      theme_minimal(base_family = "poppins", base_size = 12) +
      theme(legend.position = "none")
    
    filtered_data <- reactive({
      df_end |>
        filter(title == input$title)
    })
    
    if (input$title == "Inequality") {
      plot <- plot +
        scale_y_continuous(limits = c(0,1),
                           breaks = seq(0, 1, 0.2)) +
        geom_text(data = filtered_data(),
                  aes(label = label,
                      color = measure),
                  nudge_y = -0.05) +
        labs(x = "",
             y = "gini coefficient",
             title = "Inequality: How skewed is the CIE towards a small set of accounts?")
        
    } else if (input$title == "Segmentation") {
      plot <- plot + 
        scale_y_continuous(limits = c(0,1),
                           breaks = seq(0, 1, 0.2)) +
        geom_text(data = filtered_data(),
                  aes(label = label,
                      color = measure),
                  nudge_y = 0.05) +
        labs(x = "",
             y = "gini coefficient",
             title = "Segmentation: How divided into distinct communities is the CIE?")
      
    } else if (input$title == "Insularity") {
      plot <- plot + 
        scale_y_continuous(limits = c(0,0.3),
                           breaks = seq(0, 0.3, 0.05)) +
        geom_text(data = filtered_data(),
                  aes(label = label,
                      color = measure),
                  nudge_y = 0.01) +
        labs(x = "",
             y = "",
             title = "Insularity: How insular are political parties relative to one another in the CIE?")
    } else {
      plot <- plot +
        scale_y_continuous(limits = c(0, 0.1),
                           breaks = seq(0, 0.1, 0.05)) +
        geom_text(data = filtered_data(),
                  aes(label = label,
                      color = measure),
                  nudge_y = 0.005) +
        labs(x = "",
             y = "",
             title = "Toxic speech: How prevalent is toxicity in the CIE?")
      return(plot)
    }
    
    ggplotly(plot)
  }) 
} 


# Run the application 
shinyApp(ui = ui, server = server)
