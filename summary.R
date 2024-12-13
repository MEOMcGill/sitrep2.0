# summary data

library(tidyverse)

this_month <- "Nov-24"

six_months <- c("Nov-24","Oct-24", "Sep-24", "Aug-24", "Jul-24", "Jun-24")

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
  # manually add the values for toxic speech and insularity averages
  mutate(six_month_average = ifelse(title == "Toxic speech", 0.0266, six_month_average),
         six_month_average = case_when(
           (title == "Insularity") & (label == "Conservative") ~ "0.0652",
           (title == "Insularity") & (label == "Liberal") ~ "0.135",
           (title == "Insularity") & (label == "NDP") ~ "0.130",
           .default = six_month_average
         )) |>
  select(-c(value, avg, month_year, source))

write_csv(df_summary, "df_summary.csv")

# add toxicity and insularity values manually as the averages are not accurate due to rounding

df |>
  filter(title == "Toxic speech") |>
  filter(month %in% c("nov","oct","sep","aug","jul","jun")) |>
  select(value) |> 
  mutate(value = as.numeric(value)) |>
  mutate(avg = mean(value))

df |>
  filter(title == "Insularity") |>
  filter(month %in% c("nov","oct","sep","aug","jul","jun")) |>
  group_by(label) |>
  select(value) |> 
  mutate(value = as.numeric(value)) |>
  mutate(avg = mean(value))

  
  
