


source('global.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
  br(),
  br(),

  # change visual elements
  tags$style(shiny_css),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
      sidebarPanel(width = 4,
        style = "background: white; border:0px; padding:0px; ",
        
        wellPanel(style = "background: white; border-width: thin; border-color: grey;",
          
        span(textOutput("text1"), style = "font-weight:bold; font-size: 20px;"),
        
        br(),
        
        # sidebar panel style
        
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
        uiOutput("ui")
        )
        ,
        
        br(),
        br(),

        wellPanel(style = "border:0px; padding: 0px; background: white; ",
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
                                                        "Trust in information gatekeepers"),
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
      select(meaning) |>
      toString() 
    
    })
  
  output[["MEASUREMENT"]] <- renderText({
    req(input$title)
    
    MEASUREMENT <- rval_text() |>
      select(measurement) |>
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
  
  output[["table"]] <- render_gt({
    req(input$title)
    
    table <- rval_summary() |>
      select(-title) |>
      gt() |>
      gt::cols_label(
        label = "",
        current_month = "<b>Current<br>Month<b>",
        six_month_average = "<b>6-month<br>Average<b>",
        .fn = md) |>
      opt_table_font(font = "Poppins") |>
      tab_header(title = HTML("<b>Summary<b>")) |>
      tab_options(table.font.size = 12,
                  table.width = pct(100),
                  heading.background.color = "#467742",
                  heading.border.bottom.color = "black",
                  container.padding.x = px(0)) |>
      opt_align_table_header(align = "left")
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
                                                         "Oct-24",
                                                        "Nov-24")),
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
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 45))
    
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
        labs(y = "Percent of links")
      
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
                                                   "Oct-24",
                                                   "Nov-24")),
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
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0, 100, 20),
                           labels = label_percent(scale = 1)) +
        labs(fill = "",
             y = "Percent engagement") +
        theme_minimal(base_family = "poppins", base_size = 12) +
        theme(legend.position = "bottom",
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 45))
       
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
                                                          "Oct-24",
                                                          "Nov-24")),
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
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0, 100, 20),
                           labels = label_percent(scale = 1)) +
        labs(fill = "",
             y = "Proportion of percent engagement") +
        theme_minimal(base_family = "poppins", base_size = 12) +
        theme(legend.position = "bottom",
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 45))

    } else {
      plot <- P1 +
        scale_y_continuous(limits = c(0, 100),
                           breaks = seq(0, 100, 20),
                           labels = label_percent(scale = 1)) +
        labs(y = "Percent of survey responders") 
      
    } 
    
    ggplotly(plot, 
             tooltip = "text") |>
      layout(legend = list(orientation = 'h', x = 0.01, y = -0.3, hjust = 0.5),
             annotations = list(x = 1,
                                y = -0.275,
                                text = ifelse(input$title %in% survey, 
                                              "Data source: Survey",
                                              "Data source: Social media"),
                                showarrow = F,
                                xref = "paper",
                                yref = "paper",
                                font = list(size = 12, color = "red")),
             hoverlabel = list(bgcolor = "white",
                               font = list(size = 15, color = "black")))
  }) 
  
  output$footer <- renderText({
    style = "color:#467742; font-style: bold; font-size: 15px; align: center;"
    "© Canadian Digital Media Research Network 2024"
  })
  
} 


# Run the application 
shinyApp(ui = ui, server = server)

