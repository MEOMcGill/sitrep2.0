#load packages 

suppressPackageStartupMessages({
  library(tidyverse)
})
 
# data file from Aengus from November 24   
df_DT <- read_csv("sitrep_measures_DT_latest.csv")

# data file from Chris from November 25
df_survey <- read_csv("sitrep_measures.csv") |>
  filter(source == "Survey") |>
  mutate(section = as.numeric(section),
         number = as.numeric(number)) |>
  filter(measure != "trust") |>
  filter(!(measure == "trust_gatekeepers" &
             value %in% c(41.25117676,40.81673482))) |>
  filter(!(measure == "Misinformation_concern" &
             title == "Concern about generative AI")) |>
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
                   "Trust",
                   "Chilled speech")

df_vulnerability <- df |>
  mutate(title = case_match(
    title,
    c("Trust in Information gatekeepers","Trust by Types of Information gatekeepers") ~ "Trust",
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
                                   "Oct-24")),
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

#===================================================================================================

# clean and prepare data for threats

threats <- c(c("Concern about misinformation",
               "Concern about foreign influence",
               "Concern about Generative AI",
               "Directed foreign influence",
               "Links to known misinformation websites",
               "Discussion about misinformation and foreign interference"))

df_threats <- df |>
  mutate(title = case_match(
    title,
    "Concern about foreign influence by country" ~ "Concern about foreign influence",
    "Concern about generative AI" ~ "Concern about Generative AI",
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
                                   "Oct-24")),
    value = round(value, 2),
    #update labels for the graph
    label = case_match(
      measure,
      "AI_concern" ~ "Generative AI",
      "Misinformation_concern" ~ "Misinformation",
      "FI_concern_general" ~ "Overall",
      "misinfo_references" ~ "Misinformation references",
      .default = label
    )) |>
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
                                   "Oct-24")),
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
                            "Local vs National news engagement",
                            "Platforms used by news outlets")

df_news_outlet <- df |>
  filter(measure != "top_news_platform_telegram") |>
  mutate(title = case_match(
    measure,
    "local_news_local" ~ "Local vs National news engagement",
    "local_news_national" ~ "Local vs National news engagement",
    c("top_news_platform_instagram",
      "top_news_platform_tiktok",
      "top_news_platform_twitter",
      "top_news_platform_youtube") ~ "Platforms used by news outlets",
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
                                   "Oct-24")),
    value = round(value, 2)
    ) |>
  select(-c(shown_value, caption, img_label, section, number)) 


# save the latest csv and load the app directly from the clean .csv to make it faster
write_csv(df_news_outlet, "df_news_outlet.csv")

#=======================================================================================================

# clean and prepare data for engagement with politicians

engagement_politicians = c("Top social media platforms",
                           "Engagement with party leaders",
                           "Engagement with elected party members",
                           "Engagement with Politicians vs News")

df_politicians <- df |>
  mutate(title = case_match(
    measure,
    c("politician_platform_Instagram",
      "politician_platform_TikTok",
      "politician_platform_Twitter",
      "politician_platform_YouTube") ~ "Top social media platforms",
    .default = title
  )) |>
  mutate(title = case_match(
    title,
    "Politicians vs. news engagement" ~ "Engagement with Politicians vs News",
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
                                   "Oct-24")),
    value = round(value, 2),
    label = str_replace(label, "News Outlet", "News outlet")
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


# create a df for text

df_text <- tibble(
  title = c(vulnerability,threats, news_engagement, engagement_news_outlets, engagement_politicians),
  subtitle = c(
    "How skewed is the CIE towards a small set of accounts?",
    "How divided is the CIE into distinct communities?",
    "How insular are major political parties relative to one another?",
    "How prevalent is toxic speech in the CIE?",
    "How people feel about their own political party versus other parties?",
    "To what extent do people avoid political and public affairs news?",
    "How much do people trust information gatekeepers?",
    "To what extent do people avoid political discussion and opinion sharing?",
    "How concerned are people about misinformation?",
    "How concerned are people about foreign influence?",
    "How concerned are people about artificial intelligence?",
    "To what extent are overt foreign influencers impacting CIE?",
    "How common and popular are web links to known misinformation websites?",
    "How often do people post about misinformation and foreign influence?",
    "How many people seek out information about news?",
    "How actively people share and post news on social media?",
    "How many people use each medium weekly for news?",
    "How many people use each source for weekly news?",
    "How many people use social media weekly for news?",
    "What are the top 5 Canadian news outlets? What is their share of total engagement?",
    "How skewed is preference for national vs local news?",
    "Which social media platforms are most popular for news?",
    "Which social media platforms are most popular for Canadian politicians?",
    "Which federal party leaders are most engaging with Canadians?",
    "Which elected party members are most engaging with Canadians?",
    "How skewed is engagement with politicians vs news outlets?"
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
           "We evaluate chilled speech (extent people avoid online political discussion and opinion sharing, 0=low to 1=high).",
           "We evaluate the threat of misinformation by measuring Canadians’ concern about misinformation.",
           "We evaluate the threat of foreign influence by measuring concern about foreign governments influencing Canadian media and politics (in general and by country).",
           "We assess the threat of generative AI by measuring concern about AI generated content misleading the general public.",
           "We assess how ‘close’ foreign accounts are to Canadian accounts in regards to the content they share: we can understand these accounts to be in close proximity to the CIE if they frequently post similar links or discuss certain topics at the same time and in the same way.",
           "We measure how much misinformation is circulated in the CIE by politically influential accounts. We do not measure how much misinformation in total there is in the CIE – that would require us to analyze every single post across multiple platforms in Canada.",
           "We analyze discussion about misinformation to assess the level of attention it is getting. Examining how often prominent accounts within the CIE provides an additional dimension to our analysis of concern about misinformation.",
           "We report the proportion of respondents who say “Occasionally seeking out”, “Often seeking out”, and “Constantly seeking out”.",
           "We calculate the proportion who say “Once or twice a week” or more and then average the proportions to generate an overall news sharing measure.",
           "We calculate the proportion who say “once or twice a week” or more to generate a news consumption score for each news medium.",
           "Text: TBD",
           "Text: TBD",
           "We first identify the five outlets that received the highest amount of engagement (as measured by the total number of likes, comments, and shares on their posts), sum them, then divide their engagement by the total amount of engagement received by Canadian news outlets online and multiplying by 100. We are then able to see how much of online Canadian news engagement is generated by the biggest outlets.",
           "We calculate the relative local news engagement by dividing the total amount of engagement generated by local news accounts across our platforms by the total amount of engagement generated by all news accounts.",
           "We evaluate on which platforms Canadian news outlets receive the most engagement to better understand where Canadians seek their online news content. We calculate the platform with the most engagement with Canadian news outlets by summing the total number of likes on posts by Canadian news outlets per platform, and reporting the platform with the most likes.",
           "We identify the most popular platform for engagement with politicians by adding up the total number of likes on posts by Canadian politicians across our platforms for the whole month, then identifying which platform generated the most likes.",
           "We compare the engagement rates of the leaders of Canadian federal political parties which currently hold seats in the House of Commons. This is calculated by summing the total amount of likes each leader receives on their posts across our platforms of interest, then representing these likes as a percentage of all likes received by all party leaders.",
           "We compare the engagement rates of all Canadian party ‘families’ (Liberal, NDP, Conservative, Bloc Quebecois, Green). We sum the total amount of engagement received by each party’s politicians, on both the federal and provincial level, then dividing that sum by the total amount of engagement received by Canadian politicians who are not party leaders.",
           "We report the percentage of engagement generated by Canadian politicians relative to Canadian news outlets to gain an idea of how prominent politicians are in the CIE. We calculate this percentage by dividing the total number of likes received by Canadian politicians by the total number of likes received by both news outlets and politicians.")
) |>
  mutate(title_upper = str_to_upper(title))

write_csv(df_text, "df_text.csv")

