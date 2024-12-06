source('global.R')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(
      
      h2(strong("Characteristics of Ecosystem Health Over Time"), align = "center")),
  
  br(),
  br(),

  # change visual elements
  tags$style(shiny_css),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
      sidebarPanel(
        
        span(textOutput("text1"), style = "font-weight:bold; font-size: 20px;"),
        
        br(),
        
        # sidebar panel style
        style = "background-color: white; ",
        
        # Dropdown 1
        selectInput(
        inputId = "category",
        label = h4(strong("Select category:")),
        choices = c("Vulnerabilities",
                    "Threats",
                    "Engagement with news",
                    "Engagement with news outlets",
                    "Engagement with politicians"),
        selected = "Vulnerabilities"),
        
        # Dynamic ui output based on the first selection
        uiOutput("ui"),
        
        br(),
        
        tableOutput("table")
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
            column(width = 4,
                   wellPanel(htmlOutput("current", style = "text-align: center;")),
                   wellPanel(htmlOutput("average", style = "text-align: center; "))),
          column(8, span(htmlOutput("text2"), style = "color:#467742; font-size: 16px;"),
                              textOutput("MEANING"))
          ),
          
          hr(),
          
          span(htmlOutput("text3"), style = "color:#467742; font-size: 16px;"),
          textOutput("MEASUREMENT"),
          
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
           "Vulnerabilities" = radioButtons("title", h4(strong("Select characteristic:")),
                                            choices = c("Inequality",
                                                        "Segmentation",
                                                        "Insularity",
                                                        "Toxic speech",
                                                        "Division",
                                                        "News avoidance",
                                                        "Chilled speech",
                                                        "Trust"),
                                            selected = "Inequality"),
           "Threats" = radioButtons("title", h4(strong("Select characteristic:")),
                                   choices = c("Discussion about misinformation and foreign interference",
                                               "Concern about misinformation",
                                               "Links to known misinformation websites",
                                               "Concern about foreign influence",
                                               "Directed foreign influence",
                                               "Concern about Generative AI"),
                                   selected = "Discussion about misinformation and foreign interference"),
           "Engagement with news" = radioButtons("title", h4(strong("Select characteristic:")),
                                    choices = c("News seeking",
                                                "News sharing",
                                                "Mediums of weekly news",
                                                "Sources of weekly news",
                                                "Social media for news"),
                                    selected = "News seeking"),
           "Engagement with news outlets" = radioButtons("title", h4(strong("Select characteristic:")),
                                                 choices = c("Top 5 news outlets",
                                                             "Local vs National news engagement",
                                                             "Platforms used by news outlets"),
                                                 selected = "Top 5 news outlets"),
           "Engagement with politicians" = radioButtons("title", h4(strong("Select characteristic:")),
                                                         choices = c("Top social media platforms",
                                                                     "Engagement with party leaders",
                                                                     "Engagement with elected party members",
                                                                     "Engagement with Politicians vs News"),
                                                         selected = "Top social media platforms")
           )
  })
  
  
  #reactive elements
  rval_data <- reactive({
    df_app |>
      filter(title %in% input$title)
    })
  
  rval_text <- reactive({
    df_text |>
      filter(title %in% input$title)
    })
  
  rval_summary <- reactive({
    df_summary |>
      filter(title %in% input$title)
  })
  
  # text output
  output[["TITLE"]] <- renderText({
    req(input$title)
    
    TITLE <- rval_text() |>
      select(title_upper) |>
      toString()
    
  })
  
  output[["subtitle"]] <- renderText({
    req(input$title)
    
    subtitle <- rval_text() |>
      select(subtitle) |>
      toString()
    
  })
  
  output[["MEANING"]] <- renderText({
    req(input$title)
    
    MEANING <- rval_text() |>
      select(text) |>
      toString() 
    
    })
  
  output[["MEASUREMENT"]] <- renderText({
    req(input$title)
    
    MEASUREMENT <- rval_text() |>
      select(text) |>
      toString() 
    
  })
  
  output[["text1"]] <- renderText({
    text1 <- "ECOSYSTEM CHARACTERISTICS"
  })
  
  output[["text2"]] <- renderText({
    text2 <- HTML("MEANING<br>What does this data tell us?")
  })
  
  output[["text3"]] <- renderText({
    text3 <- HTML("MEASUREMENT<br>How do we generate these measures?")
  })
  
  output[["current"]] <- renderText({
    req(input$title)
    
    value <- rval_summary() |>
      pull(current_month) |>
      toString()
    
    current <- HTML("<b>Current Month</b><br>",
                    value)
  })
  
  output[["average"]] <- renderText({
    req(input$title)
    
    avg <- rval_summary() |>
      pull(average) |>
      toString()
    
    average <- HTML("<br><b>6-Month Average</b><br>",
                    avg)
  })
  
  output[["table"]] <- renderTable({
    req(input$title)
    
    table <- rval_summary() |>
      select(-title) |>
      gt::gt()
  })
  
  # plot output
  output[["plot"]] <- renderPlotly({
    req(input$title)
    
    P1 <- rval_data() |>
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
      ggplot(aes(x = month_year, 
                 y = value,
                 text = paste0("Month: ", month,
                              "\nValue: ", value,
                              "\nMeasure: ", label))) +
      geom_line(aes(group = label,
                    color = label),
                linewidth = 0.75,
                alpha = 0.7) +
      geom_point(aes(color = label),
                 size = 2.5,
                 stroke = 0.7,
                 alpha = 0.7)  +
      scale_color_manual(values = color_list) +
      labs(color = "") +
      theme_minimal(base_family = "poppins", base_size = 12) +
      theme(legend.position = "bottom",
            axis.title.x = element_blank())
    
    if (input$title %in% "Inequality") {
      plot <- P1 +
        scale_y_continuous(limits = c(0,1),
                           breaks = seq(0, 1, 0.2)) +
        labs(y = "Gini coefficient") 
      
      } else if (input$title %in% "Segmentation") {
      plot <- P1 +
        scale_y_continuous(limits = c(0,1),
                           breaks = seq(0, 1, 0.2)) +
        labs(y = "Segmentation index")
      
      } else if (input$title %in% "Insularity") {
      plot <- P1 + 
        scale_y_continuous(limits = c(0,0.25),
                           breaks = seq(0, 0.25, 0.05)) +
        labs(y = "Insularity index")
      
    } else if (input$title %in% "Toxic speech") {
      plot <- P1 +
        scale_y_continuous(limits = c(0, 0.1),
                           breaks = seq(0, 0.1, 0.05)) +
        labs(y = "Toxocity index")
      
    } else if (input$title %in% "Directed foreign influence") {
      plot <- P1 +
        scale_y_continuous(limits = c(0, 10),
                           breaks = seq(0, 10, 2),
                           labels = label_percent(scale = 1)) +
        labs(y = "Percent connections")
      
    } else if (input$title %in% "Links to known misinformation websites") {
      
      plot <- P1 +
        scale_y_continuous(limits = c(0, 25),
                           breaks = seq(0, 25, 5),
                           labels = label_percent(scale = 1)) +
        labs(y = "Percent of links to misinformation sites")
      
    } else if (input$title %in% "Discussion about misinformation and foreign interference") {
      
      plot <- P1 +
        scale_y_continuous(limits = c(0, 1),
                           breaks = seq(0, 1, 0.5),
                           labels = label_percent(scale = 1)) +
        labs(y = "Percent references")  
      
    } else if (input$title %in% c("Engagement with party leaders",
                                 "Engagement with elected party members",
                                 "Platforms used by news outlets", 
                                 "Top social media platforms")) {
      plot <- P1 +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0, 100, 20),
                           labels = label_percent(scale = 1)) +
        labs(y = "Percent engagement")  
      
            
    } else if (input$title %in% "Top 5 news outlets") {
      
      plot <- rval_data() |>
        mutate(month_year = factor(month_year,levels = c("Jan-24",
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
        group_by(month_year) |>
        arrange(month_year, desc(value)) |>
        mutate(rank = row_number(desc(value))) |>
        filter(rank < max_rank) |>
        ggplot(aes(x = month_year,
                   y = value,
                   text = paste0("Month: ", month,
                                 "\nValue: ", value,
                                 "\nMeasure: ", label))) +
        geom_col(aes(fill = label),
                 position = "stack",
                 width = 0.7,
                 color = "white",
                 alpha = 0.7) +
        scale_fill_manual(values = color_list) +
        labs(fill = "") +
        theme_minimal(base_family = "poppins", base_size = 12) +
        theme(legend.position = "bottom",
              axis.title.x = element_blank()) +
        scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20),
                     labels = label_percent(scale = 1)) +
        labs(y = "Proportion of percent engagement")
      
    } else if (input$title %in% c("Local vs National news engagement", 
                                  "Engagement with Politicians vs News")) {
      
      plot <- rval_data() |>
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
        ggplot(aes(x = month_year,
                   y = value,
                   text = paste0("Month: ", month,
                                 "\nValue: ", value,
                                 "\nMeasure: ", label))) +
        geom_col(aes(fill = label),
                 position = "stack",
                 width = 0.7,
                 color = "white",
                 alpha = 0.7) +
        scale_fill_manual(values = color_list) +
        labs(fill = "") +
        theme_minimal(base_family = "poppins", base_size = 12) +
        theme(legend.position = "bottom",
              axis.title.x = element_blank()) +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0, 100, 20),
                           labels = label_percent(scale = 1)) +
        labs(y = "Proportion of percent engagement") 

    } else {
      plot <- P1 +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0, 100, 20),
                           labels = label_percent(scale = 1)) +
        labs(y = "Percent of survey responders") 
      
    } 
    
    ggplotly(plot, 
             tooltip = "text") |>
      layout(legend = list(orientation = 'h', x = 0.01, y = -0.2, hjust = 0.5),
             annotations = list(x = 1,
                                y = -0.175,
                                text = ifelse(input$title %in% survey, 
                                              "Source: Survey data",
                                              "Source: Social media data"),
                                showarrow = F,
                                xref = "paper",
                                yref = "paper",
                                font = list(size = 12, color = "red")),
             hoverlabel = list(bgcolor = "white",
                               font = list(size = 15, color = "black")))
  }) 
} 


# Run the application 
shinyApp(ui = ui, server = server)

