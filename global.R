#load packages 

suppressPackageStartupMessages({
  library(shiny)
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
df_vulnerability <- vroom("df_vulnerability.csv")
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
  "Toxicity" = "#78494D",
  "News avoidance" = "#3D4E80",               
  "Chilled speech" = "#647299",                
  "Division" = "#8B94B3",
  "Big Tech" = "#467742",
  "Elected officials" = "#6C9269",
  "Information gatekeepers" = "#DAE4D9",
  "Journalists" = "#B6C9B3",
  "News Media" = "#90AD8E"  
)

####-------------------------------------------------------------------

shiny_css <- "
/* Get a fancy font from Google Fonts */
  @import url('https://fonts.googleapis.com/css?family=Poppins');

body {
  background-color: black;
  color: white; 
  font-family: 'Poppins';
}
  sidebar {
  background-color: black;
  color: white;
  }
  
  label, input, button, select { 
  font-family: 'Poppins';
          color: white; 
  }
"










