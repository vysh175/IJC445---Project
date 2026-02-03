#===========================================================
# Loading required packages
#===========================================================
install.packages(c("ggridges","ggrepel","patchwork"))

library(dplyr)      
library(tidyr)      
library(janitor)    
library(stringr)  
library(ggplot2)    
library(readr)
library(ggridges)
library(ggrepel)
library(patchwork)

#===========================================================
#Loading the data
#===========================================================

# Load Public Performance Measure (PPM) data
ppm_raw <- read.csv(
  "Table 3114b - Public Performance Measure by operator and sector (moving annual average).csv")

# Load Cancelled and Significantly Late (CaSL) data
casl_raw <- read.csv(
  "Table 3194b - Cancelled and Significantly Late by operator and sector (moving annual average).csv")

#===========================================================
# Cleaning the data
#===========================================================

#Removing spaces and fully empty columns

ppm_clean <- ppm_raw %>%
  clean_names() %>%
  filter(!if_all(everything(), is.na))          


casl_clean <- casl_raw %>%
  clean_names() %>%
  filter(!if_all(everything(), is.na))


#Converting to numeric and wide to long format
ppm_clean_fixed <- ppm_clean %>% mutate(across(-time_period, as.numeric))

ppm_long <- ppm_clean_fixed %>%
  pivot_longer(                          
    cols = -time_period,
    names_to = "operator",
    values_to = "ppm" )


casl_clean_fixed <- casl_clean %>% mutate(across(-time_period, as.numeric))

casl_long <- casl_clean_fixed %>%
  pivot_longer(
    cols = -time_period,
    names_to = "operator",
    values_to = "casl")


#Combining ppm and casl 
rail_performance <- ppm_long %>%
  left_join(
    casl_long,
    by = c("time_period", "operator"))


#Extracting starting year 
rail_performance <- rail_performance %>%
  mutate(
    year = as.numeric(str_extract(time_period, "\\d{4}")))

#Filter to post 2019
rail_post_2019 <- rail_performance %>%
  filter(year >= 2019)

rail_post_2019 <- rail_post_2019 %>%
  filter(!str_detect(operator, "^national_|^sector_"))


#Selecting 8 operators 
top_operators <- rail_post_2019 %>%
  group_by(operator) %>%
  summarise(n_obs = n()) %>%
  arrange(desc(n_obs)) %>%
  slice_head(n = 8) %>%
  pull(operator)

rail_focus <- rail_post_2019 %>%
  filter(operator %in% top_operators)

# Cleaning operator names 
rail_focus <- rail_focus %>%
  mutate(
    operator_pretty = operator %>%
      str_remove_all("franchised_|open_access_|non_franchised_") %>%
      str_replace_all("_", " ") %>%
      str_to_title() %>%
      str_squish())

#Order operators by average reliability 
operator_order <- rail_focus %>%
  group_by(operator_pretty) %>%
  summarise(mean_ppm = mean(ppm, na.rm = TRUE)) %>%
  arrange(mean_ppm) %>%
  pull(operator_pretty)

rail_focus <- rail_focus %>%
  mutate(
    operator_pretty = factor(operator_pretty, levels = operator_order))

#============================================================
# Heatmap - PPM for selected UK operators
#============================================================

p1 <- ggplot(
  rail_focus,
  aes(
    x = year,
    y = operator_pretty,
    fill = ppm)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    option = "C",
    name = "PPM (%)") +
  labs(
    title = "Post-2019 Rail Service Reliability Across Selected UK Operators",
    x = "Year",
    y = "Rail Operator") +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(
      hjust = 0.7,        # Centres the title
      face = "bold",      # Makes title stand out
      size = 10),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    panel.grid = element_blank())

p1

#=====================================================================
#CaSL for selected UK operators
#=====================================================================

p2 <- ggplot(
  rail_focus,
  aes(
    x = casl,
    y = operator_pretty,
    fill = operator_pretty)) +
  ggridges::geom_density_ridges(
    alpha = 0.75,
    show.legend = FALSE,
    scale = 1.1) +
  labs(
    title = "Service Disruption Across Selected UK Rail Operators",
    subtitle = "Distribution of cancelled and significantly late services (CaSL), post-2019",
    x = "Cancelled and Significantly Late Services (CaSL, %)",
    y = "Rail Operator") +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.001,
      face = "bold",
      size = 12),
    plot.subtitle = element_text(
      hjust = 0.001,
      size = 9),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank())
p2

#================================================================
##Linking CaSL to PPM across selected UK operators
#================================================================

p3 <- ggplot(
  rail_focus,
  aes(
    x = casl,
    y = ppm,
    color = operator_pretty)) +
  geom_point(
    aes(size = year),
    alpha = 0.7) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = FALSE,
    linewidth = 0.8,
    color = "black") +
  scale_size_continuous(name = "Year") +
  labs(
    title = "PPM vs CaSL Relationship",
    subtitle = "Bubble size indicates year (post-2019)",
    x = "CaSL (%)",
    y = "PPM (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    panel.grid.minor = element_blank())
p3

#==================================================================
#Performance Map of Selected UK Rail Operators
#==================================================================

p4 <- ggplot(
  operator_summary,
  aes(
    x = avg_casl,
    y = avg_ppm)) +
  geom_point(
    aes(size = n_periods),
    colour = "#2C3E50",
    alpha = 0.85) +
  ggrepel::geom_text_repel(
    aes(label = operator_pretty),
    size = 3.5,
    max.overlaps = 20) +
  geom_vline(
    xintercept = mean(operator_summary$avg_casl),
    linetype = "dashed",
    colour = "grey40") +
  geom_hline(
    yintercept = mean(operator_summary$avg_ppm),
    linetype = "dashed",
    colour = "grey40") +
  scale_size_continuous(
    name = "Number of reporting periods",
    range = c(3, 7)) +
  scale_x_continuous(
    limits = c(1.5, 11.5),
    expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(
    limits = c(74, 94),
    expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    title = "Performance Map of Selected UK Rail Operators",
    subtitle = "Average punctuality (PPM) versus average service disruption (CaSL), post-2019",
    x = "Average Cancelled and Significantly Late Services (CaSL, %)",
    y = "Average Public Performance Measure (PPM, %)",
    caption = "Dashed lines indicate overall post-2019 averages. Higher PPM and lower CaSL indicate better performance.") +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 13),
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 10),
    plot.caption = element_text(
      hjust = 0.2,
      size = 8,
      colour = "grey40"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.minor = element_blank())
p4


