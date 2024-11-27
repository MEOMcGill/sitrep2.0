# load packages

library(tidyverse)
library(ggbump)

max_rank <- 11

top_5 <- df |>
  filter(title == "Top 5 Canadian news outlets") |>
  filter(number == 7) |>
  group_by(month) |>
  mutate(
    value = as.numeric(value),
    rank = row_number(desc(value)),
    percent = value / sum(value)
    ) |>
  ungroup() |>
  arrange(month, year, rank) 


col_pal <- c("CTV" = "#3eb1c8",
             "CBC" = "#6ba539",
             "Rebel News" = "#8B94B3",
             "blogTO" = "#f58220",
             "The Post Millennial" = "#8EDD65")


top_5 |>
  filter(rank < 6) |>
  mutate(rank = rank * (-1)) |>
  mutate(month = str_to_title(month),
         month = factor(month, levels = month.abb, ordered = TRUE)) |>
  ggplot(aes(month, rank, color = label)) +
  geom_point(size = 3) +
  geom_bump(linewidth = 0.5, 
            alpha = 0.7) +
  scale_color_manual(values = col_pal) +
  theme(legend.position = "none") +
  geom_text(aes(label = label),
            nudge_y = 0.25,
            size = 3) +
  theme_classic() +
  theme(legend.position = "bottom")

top_5 |>
  filter(rank < 6) |>
  mutate(month = str_to_title(month),
         month = factor(month, levels = month.abb, ordered = TRUE)) |>
  ggplot(aes(month, value, fill = label)) +
  geom_bar(position = "stack", stat = "identity") +
  theme(legend.position = "none")


bar_width <- 0.1

top_5 |>
  ggplot() +
  geom_rect(
    aes(
      xmin = month - bar_width, 
      xmax = month + bar_width,
      ymin = value,
      ymax = value,
      fill = state
    ),
    col = 'white'
  ) 




  

