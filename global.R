#load packages 

suppressPackageStartupMessages({
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

# load data
df_app <- fread("df_app.csv")
df_text <- readRDS("df_text.rds")
df_summary <- fread("df_summary.csv")

# CSS formating
shiny_css <- "
  @import url('https://fonts.googleapis.com/css?family=Poppins');

body {
  background-color: white;
  color: black; 
  font-family: 'Poppins';
}
  sidebar {
  background-color: white;
  color: black;
  }
  
  label, input, button, select { 
  font-family: 'Poppins';
          color: black; 
  }
  
  div[data-value].active {
  background-color:#009900 !important;
  color: white;
  }"


# variables used in the app

max_rank <- 6

survey <- c("Division",
            "News avoidance",
            "Chilled speech",
            "Trust in information gatekeepers",
            "Concern about misinformation",
            "Concern about foreign influence",
            "Concern about generative AI",
            "News seeking",
            "News sharing",
            "Mediums of weekly news",
            "Sources of weekly news",
            "Social media for news")

# fonts
# already imported from the url
#font_add_google("Poppins", "poppins")
#showtext_auto()
#showtext_opts(dpi = 300)

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

social_media <- c("#c32aa3",
                  "#69c9d0",
                  "#1da1f2",
                  "#ff0000")

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
  "Narcity" = meo_colors[[6]],
  "APTN" = meo_colors[[4]],
  "Instagram" = social_media[[1]],
  "TikTok" = social_media[[2]],
  "X" = social_media[[3]],
  "YouTube" = social_media[[4]],
  "Digital" = party_colors[[4]],
  "Print" = meo_colors[[7]],
  "Radio" = meo_colors[[1]],
  "TV" = meo_colors[[8]],
  "Social media" = meo_colors[[3]],
  
  #engagement with politicians
  "Bloc Québécois" = party_colors[[7]],
  "Green" = party_colors[[8]],
  "News outlet" = meo_colors[[1]],
  "Politician" = meo_colors[[7]]
)

#=================================================================================







