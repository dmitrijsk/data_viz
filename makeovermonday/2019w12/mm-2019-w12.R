library(tidyverse)

# Source: https://www.weforum.org/agenda/2018/12/women-reykjavik-index-leadership

reykjavik_orig <- readxl::read_xlsx("makeovermonday/2019w12/Reykjavik Index.xlsx")
names(reykjavik_orig) <- c("country", "index")

reykjavik_average <- reykjavik_orig %>% 
  filter(country == "G7 Average")

reykjavik_orig %>% 
  filter(country != "G7 Average") %>% 
  mutate(country = reorder(country, index)) %>% 
  
  # Bar plot.
  ggplot(aes(x = country, y = index, label = index, fill = index > reykjavik_average$index)) +
  geom_col() +
  coord_flip(clip = "off") +
  geom_text(aes(color = index > reykjavik_average$index), hjust = "left", nudge_y = 2, size = 3.1) +
  
  # Scales.
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100), minor_breaks = NULL, position = "right") +
  scale_fill_manual(values = c("TRUE" = "grey70", "FALSE" = "#623460")) +
  scale_colour_manual(values = c("TRUE" = "grey30", "FALSE" = "#623460")) +
  
  # Labels.
  labs(title = "Study reveals prejudices against both men and women in leadership roles in professional life",
       subtitle = "Equality of attitudes to women in leadership in the G7 nations, scored out of 100",
       x = NULL, 
       y = NULL,
       caption = "Data source: The Reykjavik Index for Leadership") +
  
  # Teme elements.
  theme_classic() +
  theme(legend.position = "none", 
        plot.title = element_text(),
        plot.subtitle = element_text(colour = "grey30", size = 9), 
        plot.caption = element_text(hjust = 0, colour = "grey50", size = 8),
        axis.line.x = element_line(color = "grey30"), 
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank()) +
  
  # Annotations for upper 4 countries.
  annotate(label = "Less than 30%", "text", colour = "grey30", x = 6, y = 80, size = 3.5, hjust = "left", fontface = 2) +
  annotate(label = "of", "text", colour = "grey30", x = 6, y = 94, size = 3.5, hjust = "left") +
  annotate(label = "\npeople don't think men\nand women are equally\nsuited in leadership\nroles", "text", colour = "grey30", x = 5.3, y = 80, size = 3.5, hjust = "left") +
  
  # Annotations for lower 3 countries.
  annotate(label = "Up to 39%", "text", colour = "#623460", x = 2.5, y = 80,   size = 3.5, hjust = "left", fontface = 2) +
  annotate(label = "of people", "text", colour = "#623460", x = 2.5, y = 90, size = 3.5, hjust = "left") +
  annotate(label = "\ndon't think men and\nwomena re equally\nsuited in leadership roles", "text", colour = "#623460", x = 1.9, y = 80,  size = 3.5, hjust = "left")

# Export the plot.
ggsave(filename = "makeovermonday/2019w12/mm-2019-w12-r.png", width = 23, height = 12, units = "cm", dpi = 500)


