# summary data

library(tidyverse)
library(glue)

this_month <- "Nov-24"

six_months <- c("Nov-24","Oct-24", "Sep-24", "Aug-24", "Jul-24", "Jun-24", "May-24")


df_summary <- df_app |>
  select(month_year, source, title, label, value) |>
  filter(month_year %in% six_months) |>
  group_by(title, label) |>
  mutate(current_month = value,
         avg = mean(value),
         avg = round(avg, 2))|>
  ungroup() |>
  filter(month_year == this_month) |>
  mutate(current_month = ifelse(source == "Survey", paste0(value, "%"), value),
         six_month_average = ifelse(source == "Survey", paste0(avg, "%"), avg)) |>
  select(-c(value, avg, month_year, source))

write_csv(df_summary, "df_summary.csv")


df_summary |>
  filter(title == "Inequality") |>
  pull(current_month) |>
  toString()

