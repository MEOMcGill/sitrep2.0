# load packages

library(tidyverse)
library(ggbump)


df_app |>
  filter(title == "Top 5 news outlets") |>
  mutate(month_year = factor(month_year,levels = c("Jan-24",
                                                   "Feb-24",
                                                   "Mar-24",
                                                   "Apr-24",
                                                   "May-24",
                                                   "Jun-24",
                                                   "Jul-24",
                                                   "Aug-24",
                                                   "Sep-24",
                                                   "Oct-24")),
         ordered = TRUE) |>
  group_by(month_year) |>
  arrange(month_year, desc(value)) |>
  mutate(rank = row_number(desc(value))) |>
  filter(rank < max_rank) |>
  ggplot(aes(x = month_year, 
             y = value,
             text = paste0("Month: ", month,
                           "\nValue: ", value,
                           "\nMeasure: ", label))) +
  geom_col(color = "white") +
  scale_fill_manual(values = color_list) +
  labs(fill = "") +
  theme_minimal(base_family = "poppins", base_size = 12) +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  labs(y = "Proportion of percent engagement")




top_5_b |>
  mutate(rank = rank * (-1)) |>
  mutate(month = str_to_title(month),
         month = factor(month, levels = month.abb, ordered = TRUE)) |>
  ggplot(aes(month, rank, color = label)) +
  geom_point(aes(size = value)) +
  geom_bump(linewidth = 0.5, 
            alpha = 0.7) +
  geom_text(aes(label = label)) +
  theme(legend.position = "none") +
  theme_classic() +
  theme(legend.position = "none")


top_5_b$label <- factor(top_5_b$label, levels = top_5_b$rank) 

top_5_b |>
  ggplot(aes(month, value, fill = label)) +
  geom_col(
           color = "white") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(keywidth = 0.5,
                             keyheight = 0.1))
  


