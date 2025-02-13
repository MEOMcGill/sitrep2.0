# summary data

library(tidyverse)

this_month <- "Jan-25"

six_months <- c("Jan-25","Dec-24","Nov-24","Oct-24", "Sep-24", "Aug-24")

list_percent <- c("Division",
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
                  "Social media for news",
                  "Discussion about misinformation and foreign interference",
                  "Links to known misinformation websites",
                  "Directed foreign influence",
                  "Top 5 news outlets",
                  "Local vs national news engagement",
                  "Top social media platforms - news outlets",
                  "Top social media platforms - politicians",
                  "Engagement with party leaders",
                  "Engagement with elected party members",
                  "Engagement with politicians vs news")

df_summary <- df_app |>
  select(month_year, source, title, label, value) |>
  filter(month_year %in% six_months) |>
  group_by(title, label) |>
  mutate(current_month = value,
         avg = mean(value),
         avg = round(avg, 2))|>
  ungroup() |>
  filter(month_year == this_month) |>
  mutate(current_month = ifelse(title %in% list_percent, paste0(value, "%"), value),
         six_month_average = ifelse(title %in% list_percent, paste0(avg, "%"), avg)) |>
  # manually add the values for toxic speech and insularity averages
  mutate(six_month_average = ifelse(title == "Toxic speech", 0.026, six_month_average),
         six_month_average = case_when(
           (title == "Insularity") & (label == "Conservative") ~ "0.0616",
           (title == "Insularity") & (label == "Liberal") ~ "0.13",
           (title == "Insularity") & (label == "NDP") ~ "0.125",
           .default = six_month_average
         )) |>
  select(-c(value, avg, month_year, source)) |> 
  # remove the bottom 5 from the current month summary
  filter(!label %in% c("Rebel News",
                    "Toronto Sun",
                    "The National",
                    "blogTO",
                    "Narcity Quebec"))

write_csv(df_summary, "df_summary.csv")

# add toxicity and insularity values manually as the averages are not accurate due to rounding

df |>
  filter(title == "Toxic speech") |>
  filter(month %in% c(,"dec","nov","oct","sep","aug","jul")) |>
  select(value) |> 
  mutate(value = as.numeric(value)) |>
  mutate(avg = mean(value))

df_app |>
  filter(title == "Insularity") |>
  filter(month_year %in% six_months) |>
  group_by(label) |>
  select(value) |> 
  mutate(value = as.numeric(value)) |>
  mutate(avg = mean(value))

  
  
