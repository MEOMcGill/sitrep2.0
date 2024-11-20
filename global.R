#load packages 

suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(rsconnect)
  library(tidyverse)
  library(vroom)
  library(plotly)
  library(ggtext)
  library(systemfonts)
  library(extrafont)
  library(showtext)
  library(ggrepel)
  library(scales)
})

# load data
df_app <- vroom("df_app.csv")
df_text <- vroom("df_text.csv", delim = ",")


# fonts
font_add_google("Poppins", "poppins")
showtext_auto()
showtext_opts(dpi = 300)

# theme_set
theme_set(theme_update(text = element_text(family = "Poppins")))

# color palette

color_list <- list(
  "Conservative" = "#142F52",
  "Liberal" = "#d71b1e",
  "NDP" = "#f58220",
  "News inequality" = "#3eb1c8",
  "Overall inequality" = "#6ba539",
  "Segmentation" = "#66c1d4",
  "Toxicity" = "#8B94B3",
  "News avoidance" = "#3D4E80",               
  "Chilled speech" = "#647299",                
  "Division" = "#8B94B3",
  "Big Tech" = "#8EDD65",
  "Elected officials" = "#A4E484",
  "Information gatekeepers" = "#467742",
  "Journalists" = "#BBEAA2",
  "News Media" = "#D1F1C0",
  "Generative AI" = "#647299",
  "Misinformation" = "#8B94B3",
  "Overall" = "#467742",
  "UK" = "#8EDD65",
  "USA" = "#A4E484",
  "China" = "#BBEAA2",
  "Russia" = "#D1F1C0",
  "India" = "#E8F7E0"
)

#=================================================================================

shiny_css <- "
/* Get a fancy font from Google Fonts */
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
}


"










