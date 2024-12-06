# text files

library(tidyverse)


df_text <- fread("text.csv") |>
  drop_na() |>
  mutate(title_upper = str_to_upper(title))

  
  
  
df_text |>
  filter(title == "Inequality") |>
  select(measurement) |>
  toString() 


save(df_text, file = "df_text.RData")
saveRDS(df_text, "df_text.rds")

write_csv(df_text, "df_text.csv")
