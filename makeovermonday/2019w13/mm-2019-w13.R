library(tidyverse)
library(lubridate)

df <- read_delim("https://query.data.world/s/neyo2euwotoeccn7ww3znwrdhxq4e6", delim = ",")
names(df) <- c("generation", "spending_category", "spending_proportion")

order_of_spending_category_vec <- df %>% 
  filter(generation == "Millenials") %>% 
  arrange(desc(spending_proportion)) %>% 
  pull(spending_category)

df <- df %>% 
  mutate(generation = factor(generation, 
                             levels = c("Millenials", "Generation X", "Baby Boomers", "Traditionalists")),
         spending_category = factor(spending_category, levels = order_of_spending_category_vec))

# Source: http://www.wmfc.org/uploads/GenerationalDifferencesChart.pdf
gens_and_age <- tibble(gen = c("Traditionalists", "Baby Boomers", "Generation X", "Millennials"),
                       year_from = c(1900, 1946, 1965, 1981),
                       year_to = c(1945, 1964, 1980, 2000),
                       years = paste(year_from, year_to, sep = "-"),
                       cur_age = paste(year(Sys.Date()) - year_to, year(Sys.Date()) - year_from, sep = "-"))

gens_and_age

gg_subtitle <- c("Because it is not fair to compare generations at different age")

df %>% 
  ggplot(aes(x = fct_rev(generation), 
             y = spending_proportion, 
             label = scales::percent(spending_proportion, accuracy = 1))) +
  geom_col() +
  coord_flip() +
  geom_text(hjust = "top", size = 3, nudge_y = -0.005, colour = "white") +
  facet_wrap(~spending_category, nrow = 2) +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = NULL, y = "Proportion of spendings", 
       title = "HOW TO LIE WITH CHARTS: a practical example from Bank of America Merrill Lynch",
       subtitle = paste(gg_subtitle, collapse = "\n"),
       caption = "Data source: Bank of America Merrill Lynch") +
  theme_bw()
