#load packages 

suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(tidyverse)
  library(vroom)
  library(plotly)
  library(ggtext)
  library(systemfonts)
  library(extrafont)
  library(showtext)
  library(ggrepel)
})


# load data
df <- vroom("sitrep_measures.csv")

# clean data for vulnerability
vulnerability <- c("Segmentation",
                   "Inequality",
                   "Insularity",
                   "Toxic speech")

df_vulnerability <- df |>
  filter(title %in% vulnerability) |>
  mutate(
         value = as.numeric(value),
         month = str_to_title(month),
         month = factor(month, levels = month.abb, labels = month.name)) |>
  arrange(month) |>
  select(-c(shown_value, caption, img_label))

# fonts
font_add_google("Poppins", "poppins")
showtext_auto()
showtext_opts(dpi = 300)

# theme_set
theme_set(theme_update(text = element_text(family = "Poppins")))

# color palette

color_list <- list(
  "insularity_cpc" = "#142F52",
  "insularity_lpc" = "#d71b1e",
  "insularity_ndp" = "#f58220",
  "news_inequality" = "#3eb1c8",
  "overall_inequality" = "#6ba539",
  "segmentation" = "#66c1d4",
  "toxicity" = "#78494D"
)


# create a dataframe for labels

df_end <- df_vulnerability |>
  filter(month == "April") |>
  mutate(label = case_when(
    measure == "segmentation" ~ "Segmentation",
    measure == "overall_inequality" ~ "Overall inequality",
    measure == "news_inequality" ~ "News inequality",
    measure == "toxicity" ~ "Toxicity",
    .default = label
  ))

# add explanatory text 

inequality <- str_wrap("We evaluate inequality using the gini coefficient 
(the extent influence within an information ecosystem deviates from a perfectly even distribution). 
The scale ranges from 0 (each entity has equal influence) to 1 (a single entity has all the influence). 
Fewer information sources (high value) severely restricts information flows, which can lead to faster spread 
and deeper penetration of misinformation.", 100, width = 2.5) 

segmentation <- str_wrap("We evaluate polarization through segmentation 
                         (the extent a network is divided into distinct communities, 
                         0=low segmentation and 1=high segmentation).", 100, width = 2.5)

insularity <- str_wrap("We evaluate insularity (the extent political party families 
                       engage with others outside their political party, with a higher number 
                       representing a more insular party family).", 100, width = 2.5)

toxicity <- str_wrap("We evaluate toxicity through toxic speech (presence of toxic speech among 
                     posts by political influencers, with lower values indicating lower toxicity).", 100, width = 2.5)

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
  background-color: white;
  }
  
  label, input, button, select { 
  font-family: 'Poppins';
          color: black; 
  }
"










