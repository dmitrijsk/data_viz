
# Packages and functions.
library(tidyverse)
library(httr)
library(readxl)
library(ggrepel)

# function provided by Erwan Le Pennec for the radar coord. 
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

# COLOURS_OVERALL <- c("Top Five" = "#00787E", 
#                      "Above Average" = "#1DA7AA",
#                      "Average" = "grey90", # originally "#91C6BE"
#                      "Below Average" = "grey90", # originally "#EC9247"
#                      "Bottom Five" = "#E3643B")

COLOURS_OVERALL <- c("Top Ten" = "#1DA7AA", 
                     "Average" = "grey90",
                     "Bottom Ten" = "#EC9247")

# Get data.
GET("https://query.data.world/s/43zkfzcdmvcgmp6bl3ju5kgwmxhnv6", write_disk(tf <- tempfile(fileext = ".xlsx")))
states_orig <- read_excel(tf)

# Explore data.
glimpse(states_orig)
skimr::skim(states_orig)

# Select rank columns.
rankings <- states_orig %>% 
  filter(Year == max(Year) & State != "Average") %>% 
  mutate(State_with_rank = paste(overallrank, State, sep = ". ") %>% reorder(., overallrank),
         State = reorder(State, overallrank)) %>% 
  select(State, C = cashrank, L = Lrrank, B = budgetrank, S = servicelvlrank, T = trustrank, overallrank, State_with_rank)

rankings_with_mad <- rankings %>% 
  left_join(rankings %>% 
              select(State:T) %>% 
              gather(key, value, -State) %>% 
              group_by(State) %>% 
              summarize(mean_abs_dev = mean(abs(value - mean(value)))),
            by = "State") %>% 
  arrange(overallrank)

# Range of ranking by 5 dimensions.
rankings_with_mad %>% 
  # filter(overallrank %in% c(1:5, 46:50)) %>% 
  mutate(State = reorder(State, -mean_abs_dev)) %>% 
  ggplot(aes(x = State, y = mean_abs_dev, fill = case_when(overallrank %in% 1:10 ~ "Top Ten",
                                                    overallrank %in% 41:50 ~ "Bottom Ten",
                                                    TRUE ~ "Average"))) +
  geom_col() +
  scale_y_continuous(breaks = seq(0, 20, by = 10), expand = c(0, 0, 0, 20)) +
  scale_fill_manual(limits = names(COLOURS_OVERALL), values = COLOURS_OVERALL) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "top") +
  theme(panel.grid.major.x = element_line(colour = "grey90", linetype = "dashed"), 
        axis.ticks.y = element_blank()) +
  labs(fill = "Overall fiscal ranking of the states:", 
       y = "Mean absolute deviation in rankings by five dimensions of solvency", 
       x = NULL) +
  annotate(y = 25, x = 48, geom = "text", label = "Uniform fiscal condition", hjust = 0, size = 7, colour = "grey30") +
  annotate(y = 25, x = 3, geom = "text", label = "Volatile fiscal condition", hjust = 0, size = 7, colour = "grey30")

ggsave(filename = "bars_dimensions_3_groups.png", height = 30, width = 25, units = "cm", dpi = 150)

# Radar plot.
plot_radar <- function(group) {
  
  overallrating_vec <- case_when(group == "Top Ten" ~ list(1:10),
                                 group == "Bottom Ten" ~ list(41:50),
                                 TRUE ~ list(11:40))[[1]]
    
  rankings %>% 
    filter(overallrank %in% overallrating_vec) %>% 
    select(State_with_rank, C:T) %>% 
    gather(key, value, -State_with_rank, factor_key = TRUE) %>% 
    arrange(key) %>% 
    ggplot(aes(x = key, y = value, group = State_with_rank, label = value)) +
    geom_polygon(fill = COLOURS_OVERALL[group]) +
    scale_y_continuous(breaks = NULL) +
    coord_radar() +
    geom_text_repel(size = 3, color = "grey30") +
    facet_wrap(~State_with_rank, ncol = 5) +
    theme_light() +
    theme(axis.ticks.y = element_blank(), 
          axis.text.y  = element_blank(), 
          axis.text.x = element_text(face = "bold", size = 11),
          panel.grid.major.x = element_line(linetype = "dashed"),
          strip.background = element_rect(fill = "white"), 
          panel.border = element_blank(),
          panel.spacing.x = unit(2,"line"), 
          panel.spacing.y = unit(2,"line"), 
          strip.text = element_text(face = "bold", size = 12, color = "grey30")) +
    labs(title = "Five dimensions of solvency",
         subtitle = "Trust fund (T), Cash (C), Long-run (L), Budget (B), Service-level (S), \n",
         x = NULL, 
         y = NULL)
}

plot_radar("Top Ten")
ggsave(filename = "radar_top_ten.png", height = 12, width = 25, units = "cm", dpi = 150)

plot_radar("Bottom Ten")
ggsave(filename = "radar_bottom_ten.png", height = 12, width = 25, units = "cm", dpi = 150)



gridExtra::grid.arrange(plot_radar("Top Five") + labs(title = "sa"), plot_radar("Bottom Five"), nrow = 2)

  
  
