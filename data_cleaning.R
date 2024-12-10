#load packages 

suppressPackageStartupMessages({
  library(tidyverse)
})
 
# data file from Aengus from December 5   
df_DT <- read_csv("sitrep_measures_DT.csv")

# data file from Chris from November 25
df_survey <- read_csv("sitrep_measures.csv") |>
  filter(source == "Survey") |>
  mutate(section = as.numeric(section),
         number = as.numeric(number)) |>
  # remove duplicate cells
  filter(measure != "trust") |>
  filter(!(measure == "trust_gatekeepers" &
             value %in% c(41.25117676,40.81673482))) |>
  filter(!(measure == "Misinformation_concern" &
             title == "Concern about generative AI")) |>
  # remove the news avoidance April data because the question changed in May
  filter(!(measure == "avoidance" &
             month == "apr")) |>
  distinct()
  
  

df <- df_DT |>
  bind_rows(df_survey)

# clean and prepare data for vulnerability section
vulnerability <- c("Inequality",
                   "Segmentation",
                   "Insularity",
                   "Toxic speech",
                   "Division",
                   "News avoidance",
                   "Trust in information gatekeepers",
                   "Chilled speech")

df_vulnerability <- df |>
  mutate(title = case_match(
    title,
    c("Trust in Information gatekeepers","Trust by Types of Information gatekeepers") ~ "Trust in information gatekeepers",
    "News Avoiding" ~ "News avoidance",
    .default = title)) |>
  mutate(measure = case_match(
    measure,
    "trust" ~ "trust_big_tech",
    .default = measure)) |>
  filter(title %in% vulnerability) |>
  mutate(
    value = as.numeric(value),
    month = str_to_title(month),
    month = factor(month, levels = month.abb, labels = month.abb, ordered = TRUE),
    year_new = str_replace_all(year,"20",""),
    month_year = paste0(month,"-",year_new),
    month_year = factor(month_year,
                        levels = c("Jan-24",
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
    value = ifelse(title == "Toxic speech", round(value,5), round(value,2)),
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
  select(-c(shown_value, caption, img_label, section, number)) |>
  distinct()


# save the latest csv and load the app directly from the clean .csv to make it faster
write_csv(df_vulnerability, "df_vulnerability.csv")

#===================================================================================================

# clean and prepare data for threats

threats <- c(c("Concern about misinformation",
               "Concern about foreign influence",
               "Concern about generative AI",
               "Directed foreign influence",
               "Links to known misinformation websites",
               "Discussion about misinformation and foreign interference"))

df_threats <- df |>
  mutate(title = case_match(
    title,
    "Concern about foreign influence by country" ~ "Concern about foreign influence",
    "Concern about Generative AI" ~ "Concern about generative AI",
    "Linking to known misinformation websites" ~ "Links to known misinformation websites",
    .default = title)) |>
  mutate(measure = case_match(
    measure,
    "FI_concern_country" ~ "FI_concern_china",
    .default = measure)) |>
  filter(title %in% threats) |>
  # IMPORTANT - Duplicate rows for the same measure with different label names. Remove one set of values 
  filter(measure != "misinfo_links") |>
  mutate(
    value = as.numeric(value),
    month = str_to_title(month),
    month = factor(month, levels = month.abb, labels = month.abb, ordered = TRUE),
    year_new = str_replace_all(year,"20",""),
    month_year = paste0(month,"-",year_new),
    month_year = factor(month_year,
                        levels = c("Jan-24",
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
    value = round(value, 2),
    #update labels for the graph
    label = case_match(
      measure,
      "AI_concern" ~ "Generative AI",
      "Misinformation_concern" ~ "Misinformation",
      "FI_concern_general" ~ "Overall",
      "misinfo_references" ~ "Misinformation references",
      .default = label
    ),
    label = factor(label, levels = c("Misinformation references", 
                                     "Percentage engagement",
                                     "Misinformation",
                                     "Generative AI",
                                     "China",
                                     "India",
                                     "Russia", 
                                     "UK",
                                     "USA",
                                     "Overall"                      
                                    ))) |>
  select(-c(shown_value, caption, img_label, section, number)) 


# save the latest csv and load the app directly from the clean .csv to make it faster
write_csv(df_threats, "df_threats.csv")

#======================================================================================================

# clean and prepare data for engagement with news

news_engagement <- c("News seeking",
                     "News sharing",
                     "Mediums of weekly news",
                     "Sources of weekly news",
                     "Social media for news")

df_news_engagement <- df |>
  mutate(title = case_match(
    title,
    "News Sharing" ~ "News sharing",
    "Mediums of Weekly News (digital, print, radio and TV)" ~ "Mediums of weekly news",
    "Sources of Weekly News" ~ "Sources of weekly news",
    "Social Media for News" ~ "Social media for news",
    .default = title)) |>
  filter(title %in% news_engagement) |>
  mutate(
    value = as.numeric(value),
    month = str_to_title(month),
    month = factor(month, levels = month.abb, labels = month.abb, ordered = TRUE),
    year_new = str_replace_all(year,"20",""),
    month_year = paste0(month,"-",year_new),
    month_year = factor(month_year,
                        levels = c("Jan-24",
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
    value = round(value, 2),
    #update labels for the graph
    label = case_match(
      measure,
      "sharing" ~ "News sharing",
      "seeking" ~ "News seeking",
      "socmed_news" ~ "Social media",
      .default = label
    )) |>
  select(-c(shown_value, caption, img_label, section, number)) |>
  distinct()


# save the latest csv and load the app directly from the clean .csv to make it faster
write_csv(df_news_engagement, "df_news_engagement.csv")

#=======================================================================================================

# clean and prepare data for engagement with news outlets

engagement_news_outlets = c("Top 5 news outlets",
                            "Local vs national news engagement",
                            "Top social media platforms - news outlets")

df_news_outlet <- df |>
  filter(measure != "top_news_platform_telegram") |>
  mutate(title = case_match(
    measure,
    "local_news_local" ~ "Local vs national news engagement",
    "local_news_national" ~ "Local vs national news engagement",
    c("top_news_platform_instagram",
      "top_news_platform_tiktok",
      "top_news_platform_twitter",
      "top_news_platform_youtube") ~ "Top social media platforms - news outlets",
    .default = title
  )) |>
  mutate(title = case_match(
    title,
    "Top 5 Canadian news outlets" ~ "Top 5 news outlets",
    .default = title
  )) |>
  filter(title %in% engagement_news_outlets) |>
  mutate(
    value = as.numeric(value),
    month = str_to_title(month),
    month = factor(month, levels = month.abb, labels = month.abb, ordered = TRUE),
    year_new = str_replace_all(year,"20",""),
    month_year = paste0(month,"-",year_new),
    month_year = factor(month_year,
                        levels = c("Jan-24",
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
    value = round(value, 2),
    label = case_match(
      measure,
      "top_news_platform_twitter" ~ "X",
      .default = label
    )) |>
  select(-c(shown_value, caption, img_label, section, number)) 


# save the latest csv and load the app directly from the clean .csv to make it faster
write_csv(df_news_outlet, "df_news_outlet.csv")

#=======================================================================================================

# clean and prepare data for engagement with politicians

engagement_politicians = c("Top social media platforms - politicians",
                           "Engagement with party leaders",
                           "Engagement with elected party members",
                           "Engagement with politicians vs news")

df_politicians <- df |>
  mutate(title = case_match(
    measure,
    c("politician_platform_Instagram",
      "politician_platform_TikTok",
      "politician_platform_Twitter",
      "politician_platform_YouTube") ~ "Top social media platforms - politicians",
    .default = title
  )) |>
  mutate(title = case_match(
    title,
    "Politicians vs. news engagement" ~ "Engagement with politicians vs news",
    .default = title
  )) |>
  filter(title %in% engagement_politicians) |>
  mutate(
    value = as.numeric(value),
    month = str_to_title(month),
    month = factor(month, levels = month.abb, labels = month.abb, ordered = TRUE),
    year_new = str_replace_all(year,"20",""),
    month_year = paste0(month,"-",year_new),
    month_year = factor(month_year,
                        levels = c("Jan-24",
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
    value = round(value, 2),
    label = str_replace(label, "News Outlet", "News outlet"),
    label = case_match(
      measure,
      "politician_platform_Twitter" ~ "X",
      .default = label
    )
  ) |>
  select(-c(shown_value, caption, img_label, section, number)) 


# save the latest csv and load the app directly from the clean .csv to make it faster
write_csv(df_politicians, "df_politicians.csv")

#=======================================================================================================

# Final dataset used in the app after data cleaning

df_app <- df_vulnerability |>
  bind_rows(df_threats) |>
  bind_rows(df_news_engagement) |>
  bind_rows(df_news_outlet) |>
  bind_rows(df_politicians)

write_csv(df_app, "df_app.csv")

