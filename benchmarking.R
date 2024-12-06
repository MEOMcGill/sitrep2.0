library(microbenchmark)
library(profvis)




save(df_text, file = "df_text.RData")
saveRDS(df_text, "df_text.rds")



benchmark <- microbenchmark(
  readCSV = utils::read.csv("df_text.csv"),
  readrCSV = readr::read_csv("df_text.csv"),
  vroom = vroom::vroom("df_text.csv"),
  loadRdata = base::load("df_text.RData"),
  readRds = base::readRDS("df_text.rds"),
  fread = data.table::fread("df_text.csv"),
  times = 10
)
print(benchmark, signif = 2)

autoplot(benchmark)


benchmark1 <- microbenchmark(
  library(systemfonts),
  library(extrafont),
  library(showtext),
  font_add_google("Poppins", "poppins"),
  showtext_auto(),
  showtext_opts(dpi = 300),
  times = 10
)

print(benchmark1, signif = 2)

profvis({
  readCSV = utils::read.csv("df_text.csv")
  readrCSV = readr::read_csv("df_text.csv")
  vroom = vroom::vroom("df_text.csv")
  loadRdata = base::load("df_text.RData")
  readRds = base::readRDS("df_text.rds")
  fread = data.table::fread("df_text.csv")
})



profvis({
  
  s <- suppressPackageStartupMessages({
    library(shiny)
    library(rsconnect)
    library(data.table)
    library(tidyverse)
    library(plotly)
    library(ggtext)
    library(systemfonts)
    library(extrafont)
    library(showtext)
    library(scales)
    library(gt)
  })
  
  df_app <- fread("df_app.csv")
  df_text <- fread("df_text.csv")
  df_summary <- fread("df_summary.csv")
  
  max_rank <- 6
  
  survey <- c("Division",
              "News avoidance",
              "Chilled speech",
              "Trust in information gatekeepers",
              "Concern about misinformation",
              "Concern about foreign influence",
              "Concern about Generative AI",
              "News seeking",
              "News sharing",
              "Mediums of weekly news",
              "Sources of weekly news",
              "Social media for news")
  
  
  # fonts
  # already imported from the url
  font_add_google("Poppins", "poppins")
  showtext_auto()
  showtext_opts(dpi = 300)
  
  # theme_set
  theme_set(theme_update(text = element_text(family = "Poppins")))
  
  # color palette
  
  party_colors = c("NDP" = "#f58220",
                   "Conservative" = "#142F52", 
                   "Liberal" = "#d71b1e",
                   "Russia" = "#E4181C",
                   "China" = "#ffcc00",
                   "India" = "#009933",
                   "Bloc Québécois" = "#19b1c2",
                   "Green" = "#3d9b35")
  
  meo_colors <- c("#467742",
                  "#6D4A4D",
                  "#434E7C",
                  "#272B26",
                  "#8B94B3",
                  "#69A849",
                  "#FF8200",
                  "#6BADC6",
                  "#F2E96B")
  
  color_list <- list(
    #vulnerability
    "Conservative" = party_colors[[2]],
    "Liberal" = party_colors[[3]],
    "NDP" = party_colors[[1]],
    "News inequality" = meo_colors[[1]],
    "Overall inequality" = meo_colors[[7]],
    "Segmentation" = meo_colors[[3]],
    "Toxicity" = meo_colors[[3]],
    "News avoidance" = meo_colors[[3]],               
    "Chilled speech" = meo_colors[[3]],                
    "Division" = meo_colors[[3]],
    "Information gatekeepers" = meo_colors[[2]],
    "Big Tech" = party_colors[[5]],
    "Elected officials" = party_colors[[4]],
    "Journalists" = meo_colors[[6]],
    "News Media" = meo_colors[[8]],
    
    #threat
    "Generative AI" = meo_colors[[3]],
    "Misinformation" = meo_colors[[3]],
    "Overall" = meo_colors[[2]],
    "UK" = meo_colors[[7]],
    "USA" = meo_colors[[8]],
    "China" = party_colors[[5]],
    "Russia" = party_colors[[4]],
    "India" = party_colors[[6]],
    "Percentage engagement" = meo_colors[[3]],
    "Misinformation references" = meo_colors[[3]],
    
    #engagement with news
    "News sharing" = meo_colors[[3]],
    "News seeking" = meo_colors[[3]],
    "International" = party_colors[[4]],
    "National" = meo_colors[[7]],
    "Local" = meo_colors[[1]],
    "Social media" = meo_colors[[8]],
    
    #engagement with news outlets
    "CTV" = meo_colors[[1]],
    "CBC" = party_colors[[2]],
    "Rebel News" = party_colors[[5]],
    "blogTO" = party_colors[[7]],
    "The Post Millennial" = meo_colors[[7]],
    "Toronto Star" = party_colors[[8]], 
    "Global News" = meo_colors[[8]],
    "Now Toronto" = meo_colors[[5]], 
    "Narcity" = meo_colors[[5]],
    "Instagram" = party_colors[[4]],
    "TikTok" = meo_colors[[7]],
    "X" = meo_colors[[1]],
    "YouTube" = meo_colors[[8]],
    "Digital" = party_colors[[4]],
    "Print" = meo_colors[[7]],
    "Radio" = meo_colors[[1]],
    "TV" = meo_colors[[8]],
    "Social media" = meo_colors[[3]],
    
    #engagement with politicians
    "Bloc Québécois" = party_colors[[7]],
    "Green" = party_colors[[8]],
    "News outlet" = meo_colors[[1]],
    "Politician" = meo_colors[[7]])
  
  
  ui <- fluidPage(
    br(),
    
    # change visual elements
    tags$style(shiny_css),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
      sidebarPanel(
        style = "background: white; border:0; ",
        
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
        ),
        
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
        tab_header(title = HTML("<b> <span style='color:green; '>Summary<b>")) |>
        tab_options(table.font.size = 15) |>
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
          labs(y = "Percent of misinformation site links")
        
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
  
  
  
})
