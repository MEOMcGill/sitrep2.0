#load packages 

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

# load data
df <- read_csv(here("sitrep_measures.csv")) |>
  group_by(month, year, measure) |>
  slice(1) 

# clean data for vulnerability
vulnerability <- c("Inequality",
                   "Segmentation",
                   "Insularity",
                   "Toxic speech",
                   "Division",
                   "News Avoidance",
                   "Trust",
                   "Chilled speech")

df_vulnerability <- df |>
  mutate(title = case_match(
    title,
    c("Trust in Information gatekeepers","Trust by Types of Information gatekeepers") ~ "Trust",
    "News Avoiding" ~ "News Avoidance",
    .default = title)) |>
    mutate(measure = case_match(
      measure,
      "trust" ~ "trust_big_tech",
      .default = measure)) |>
  filter(title %in% vulnerability) |>
  mutate(
    value = as.numeric(value),
    month = str_to_title(month),
    month = factor(month, levels = month.abb, labels = month.name, ordered = TRUE),
    value = round(value, 2),
    #update labels for the graph
    label = case_match(
      measure,
      "segmentation" ~ "Segmentation",
      "overall_inequality" ~ "Overall inequality",
      "news_inequality" ~ "News inequality",
      "toxicity" ~ "Toxicity",
      "trust_gatekeepers" ~ "Information gatekeepers",
      "chilled" ~ "Chilled speech",
      "division" ~ "Division",
      "avoidance" ~ "News avoidance",
      .default = label
    )) |>
  select(-c(shown_value, caption, img_label, section, number)) 


# save the latest csv and load the app directly from the clean .csv to make it faster
write_csv(df_vulnerability, "df_vulnerability.csv")

# create a df for text

df_text <- tibble(
  title = vulnerability,
  subtitle = c(
    "How skewed is the CIE towards a small set of accounts?",
    "How divided is the CIE into distinct communities?",
    "How insular are major political parties relative to one another?",
    "How prevalent is toxic speech in the CIE?",
    "How people feel about their own political party versus other parties?",
    "To what extent do people avoid political and public affairs news?",
    "How much do people trust information gatekeepers?",
    "To what extent do people avoid political discussion and opinion sharing?"
  ),
  text = c("We evaluate inequality using the Gini coefficient 
           (the extent influence within an information ecosystem deviates from a perfectly even distribution). 
           The scale ranges from 0 (each entity has equal influence) to 1 (a single entity has all the influence). 
           Fewer information sources (high value) severely restrict information flows, which can lead to faster spread 
           and deeper penetration of misinformation.", 
           "We evaluate polarization through segmentation (the extent a network is divided into distinct communities, 
           0=low segmentation and 1=high segmentation).",
           "We evaluate insularity (the extent political party families engage with others outside their political party, with a higher number 
            representing a more insular party family).",
           "We evaluate toxicity through toxic speech (presence of toxic speech among posts by political influencers, with lower values indicating lower toxicity).",
           "Division: Text TBD",
           "We evaluate news avoidance (extent people avoid the news all together, 0=low to 1=high).",
           "We evaluate trust by asking Canadians about their confidence in information gatekeepers (journalists, media organizations, politicians, and big tech).",
           "We evaluate chilled speech (extent people avoid online political discussion and opinion sharing, 0=low to 1=high).")
)

write_csv(df_text, "df_text.csv")

# optional color palette

color_list <- list(
  "insularity_cpc" = "#142F52",
  "insularity_lpc" = "#d71b1e",
  "insularity_ndp" = "#f58220",
  "news_inequality" = "#3eb1c8",
  "overall_inequality" = "#6ba539",
  "segmentation" = "#66c1d4",
  "toxicity" = "#78494D",
  "avoidance" = "#3D4E80",               
  "chilled" = "#647299",                
  "division" = "#8B94B3",
  "trust_big_tech" = "#467742",
  "trust_elected_officials" = "#6C9269",
  "trust_gatekeepers" = "#DAE4D9",
  "trust_journalists" = "#B6C9B3",
  "trust_news_media" = "#90AD8E"  
)

