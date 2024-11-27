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
  "Big Tech" = party_colors[[5]],
  "Elected officials" = party_colors[[4]],
  "Information gatekeepers" = meo_colors[[2]],
  "Journalists" = meo_colors[[6]],
  "News Media" = meo_colors[[8]],
  
  #threat
  "Generative AI" = meo_colors[[3]],
  "Misinformation" = meo_colors[[3]],
  "Overall" = meo_colors[[2]],
  "UK" = meo_colors[[2]],
  "USA" = meo_colors[[8]],
  "China" = party_colors[[5]],
  "Russia" = party_colors[[4]],
  "India" = party_colors[[6]],
  
  #engagement with news
  "News sharing" = meo_colors[[3]],
  "News seeking" = meo_colors[[3]],
  "International" = meo_colors[[9]],
  "National" = meo_colors[[7]],
  "Local" = meo_colors[[1]],
  "Social media" = meo_colors[[8]],
  
  #engagement with news outlets
  "Instagram" = meo_colors[[9]],
  "TikTok" = meo_colors[[7]],
  "Twitter" = meo_colors[[1]],
  "YouTube" = meo_colors[[8]],
  
  #engagement with politicians
  "Bloc Québécois" = party_colors[[7]],
  "Green" = party_colors[[8]],
  "News outlet" = meo_colors[[1]],
  "Politician" = meo_colors[[7]]
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










