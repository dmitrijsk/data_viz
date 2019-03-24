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
         spending_category = factor(spending_category, levels = order_of_spending_category_vec),
         by_fill = generation == "Millenials" & spending_category %in% c("Restaurants", "Groceries", "Elec/Hobbies/Clothing"))

# Source: http://www.wmfc.org/uploads/GenerationalDifferencesChart.pdf
gens_and_age <- tibble(generation = c("Traditionalists", "Baby Boomers", "Generation X", "Millenials"),
                       year_from = c(1900, 1946, 1965, 1981),
                       year_to = c(1945, 1964, 1980, 2000),
                       years = paste(year_from, year_to, sep = "-"),
                       cur_age_from = year(Sys.Date()) - year_to,
                       cur_age_to = year(Sys.Date()) - year_from,
                       cur_age = paste(cur_age_from, cur_age_to, sep = "-")) %>% 
  mutate(generation = factor(generation,
                             levels = c("Millenials", "Generation X", "Baby Boomers", "Traditionalists")))

gens_and_age

gg_subtitle = c("", "It's not fair to compare generations at different age.",
                "This is evident from a step-wise nature of bars.", "")
df %>% 
  ggplot(aes(x = fct_rev(generation), 
             y = spending_proportion, 
             label = scales::percent(spending_proportion, accuracy = 1),
             fill = by_fill)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey70")) +
  coord_flip() +
  geom_text(data = filter(df, spending_proportion > 0.03), hjust = "top", size = 3, nudge_y = -0.002, colour = "white") +
  facet_wrap(~spending_category, nrow = 2) +
  scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = NULL, 
       y = NULL, 
       title = "Consumer spendings by four generations",
       subtitle = paste(gg_subtitle, collapse = "\n"),
       caption = "Data source: Bank of America Merrill Lynch") +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 0), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, color = "grey40"),
        plot.caption = element_text(size = 5, hjust = 0, color = "grey40"),
        strip.background = element_rect(fill = "white"), 
        strip.text = element_text(size = 11))

ggsave(filename = "spendings.png", dpi = 300, height = 12, width = 27, units = "cm")

gens_and_age %>% 
  ggplot(aes(y = cur_age_from, yend = cur_age_to, x = fct_rev(generation), xend = fct_rev(generation))) +
  geom_segment(size = 1.2) +
  geom_point(aes(y = cur_age_from), size = 2) +
  geom_point(aes(y = cur_age_to), size = 2) +
  geom_text(aes(y = cur_age_from - 3, label = cur_age_from), hjust = "right", size = 3.5) +
  geom_text(aes(y = cur_age_to + 3, label = paste(cur_age_to, "years")), hjust = "left", size = 3.5) +
  coord_flip() +
  labs(x = NULL, y = NULL, 
       title = "Current age of generations",
       subtitle = "In 2019") + 
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 0), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14, color = "grey40"))

ggsave(filename = "age.png", dpi = 300, height = 12, width = 20, units = "cm")

