
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

COLOURS_OVERALL <- c("Top Five" = "#00787E", 
                     "Above Average" = "#1DA7AA",
                     "Average" = "grey90", # originally "#91C6BE"
                     "Below Average" = "#EC9247",
                     "Bottom Five" = "#E3643B")

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

rankings_with_range <- bind_cols(
  rankings,
  rankings %>% 
    rowwise() %>% 
    summarize(min = min(C, L, B, S, T),
              max = max(C, L, B, S, T),
              range = max - min) %>% 
    ungroup()) %>% 
  arrange(overallrank)

# Range of ranking by 5 dimensions.
rankings_with_range %>% 
  mutate(State = reorder(State, -range)) %>% 
  ggplot(aes(x = State, y = range, fill = case_when(overallrank %in% 1:5 ~ "Top Five",
                                                    overallrank %in% 6:17 ~ "Above Average",
                                                    overallrank %in% 18:32 ~ "Average",
                                                    overallrank %in% 33:45 ~ "Below Average",
                                                    TRUE ~ "Bottom Five"))) +
  geom_col() +
  scale_y_continuous(breaks = seq(0, 50, by = 10), labels = c("0\n(Best)", "10", "20", "30", "40", "50\n(Worst)"), expand = c(0, 0, 0, 30)) +
  scale_fill_manual(limits = names(COLOURS_OVERALL), values = COLOURS_OVERALL) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "top") +
  theme(panel.grid.major.x = element_line(colour = "grey90", linetype = "dashed"), 
        axis.ticks.y = element_blank()) +
  labs(fill = "Overall fiscal ranking of the states:", y = "Range in rankings among five dimensions of solvency", x = NULL) +
  annotate(y = 52, x = 48, geom = "text", label = "Uniform fiscal condition", hjust = 0, size = 7, colour = "grey30") +
  annotate(y = 52, x = 3, geom = "text", label = "Volatile fiscal condition", hjust = 0, size = 7, colour = "grey30")

ggsave(filename = "bars_five_dimensions.png", height = 30, width = 25, units = "cm", dpi = 150)

# Radar plot.
plot_radar <- function(group) {
  
  overallrating_vec <- case_when(group == "Top Five" ~ list(1:5),
                                 group == "Above Average" ~ list(6:17),
                                 group == "Average" ~ list(18:32),
                                 group == "Below Average" ~ list(33:45),
                                 TRUE ~ list(46:50))[[1]]
    
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
    facet_wrap(~State_with_rank, nrow = 1) +
    theme_light() +
    theme(axis.ticks.y = element_blank(), 
          axis.text.y  = element_blank(), 
          axis.text.x = element_text(face = "bold", size = 11),
          panel.grid.major.x = element_line(linetype = "dashed"),
          strip.background = element_rect(fill = "white"), 
          panel.border = element_blank(),
          panel.spacing.x = unit(2,"line"),
          strip.text = element_text(face = "bold", size = 12, color = "grey30")) +
    labs(title = "Five dimensions of solvency",
         subtitle = "Trust fund (T), Cash (C), Long-run (L), Budget (B), Service-level (S), \n",
         x = NULL, 
         y = NULL)
}

plot_radar("Top Five")
ggsave(filename = "radar_top_five.png", height = 8, width = 25, units = "cm", dpi = 150)

plot_radar("Bottom Five")
ggsave(filename = "radar_bottom_five.png", height = 8, width = 25, units = "cm", dpi = 150)



gridExtra::grid.arrange(plot_radar("Top Five") + labs(title = "sa"), plot_radar("Bottom Five"), nrow = 2)

  
  
