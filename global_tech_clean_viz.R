# Data cleaning numeric data in R

# Create and reshape data for analyzing tags ########################
# Install and load packages
library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(paletteer)
library(GGally)
library(hrbrthemes)

# Read data file
global_tech <- read.csv("global_tech.csv", stringsAsFactors = FALSE)

# Examine data set
glimpse(global_tech)
summary(global_tech)

# Convert date column
global_tech$date <- mdy(global_tech$date)
class(global_tech$date)

# Clean up tags column
global_tech$tags = str_replace_all(global_tech$tags, '\\[|\\]', '')
global_tech$tags = str_replace_all(global_tech$tags, "'", "")
global_tech$tags = strsplit(global_tech$tags, ',')
class(global_tech$tags)

# Dummify variables
dummy_vars_global <- global_tech %>% 
  unnest(tags) %>% 
  group_by(title) %>% 
  count(title, tags) %>% 
  spread(tags, n, fill = 0)

# Create df with measures of interest
interest_ind_global <- global_tech %>% 
  select(title, num_views, num_translations, num_comments)

# Merge data frames to create wide-form data
tags_pop_global <- inner_join(interest_ind_global, dummy_vars_global, by = "title")

# Add colSums to df and save as new df
tags_pop_global2 <- tags_pop_global %>% adorn_totals("row")

# Create long-form data frames #####################################
# Create function to filter out redundant tags
filter_tags <- c('Global Issues', 'Technology', 'TEDx', 'Science', 'TED Books', 'TED Fellows')
'%notin%' = function(x, y) {
  !(x %in% y)
}

# Create and clean dfs
global_tech_long <- global_tech %>% 
  select(title, date, num_views, num_translations, num_comments, tags) %>% 
  filter(tags %notin% filter_tags) %>% 
  unnest(tags) %>% 
  group_by(title) %>% 
  mutate(tags = str_trim(tags, side = 'both'))

# Factorize tags col
global_tech_long$tags <- factor(global_tech_long$tags)
is.factor(global_tech_long$tags)

# Summarise data ####################################################
# Tag frequency in descending order table
global_tag_count <- tbl_df(global_tech_long) %>% 
  group_by(tags) %>% 
  summarise(frequency = n()) %>% 
  arrange(desc(frequency)) %>% 
  filter(tags %notin% filter_tags)

# Num_views in descending order table
tags_num_views <- tbl_df(global_tech_long) %>% 
  group_by(tags) %>% 
  filter(tags %notin% filter_tags) %>% 
  tally(num_views)

# Num_comments in descending order table
tags_num_comments <- tbl_df(global_tech_long) %>% 
  group_by(tags) %>% 
  filter(tags %notin% filter_tags) %>% 
  tally(num_comments)

# Num_translations in descending order table
tags_num_translations <- tbl_df(global_tech_long) %>% 
  group_by(tags) %>% 
  filter(tags %notin% filter_tags) %>% 
  tally(num_translations)

# Measures of popularity per tag count tables
tags_num_views_per_freq <- inner_join(tags_num_views, global_tag_count, by = "tags")
tags_num_views_per_freq <- tags_num_views_per_freq %>% 
  mutate(num_views_per_tag_count = round(n / frequency))

tags_num_comments_per_freq <- inner_join(tags_num_comments, global_tag_count, by = "tags")
tags_num_comments_per_freq <- tags_num_comments_per_freq %>% 
  mutate(num_comments_per_tag_count = round(n / frequency))

tags_num_translations_per_freq <- inner_join(tags_num_translations, global_tag_count, by = "tags")
tags_num_translations_per_freq <- tags_num_translations_per_freq %>% 
  mutate(num_translations_per_tag_count = round(n / frequency))

# Create data visualizations ########################################
### Num_views ####
# Top 20 tags by num_views
tags_num_views %>% 
  arrange(desc(n)) %>% 
  top_n(20) %>% 
  mutate(tags = fct_reorder(tags, n)) %>%
  ggplot(aes(x = tags, y = n)) +
  geom_bar(stat = "identity", aes(fill = n), alpha = 0.6, width = 0.7) +
  coord_flip() +
  ggtitle('Top 20 Tags in Global Issues and Technology by View Count') +
  xlab("Tags") +
  ylab("Number of views") +
  scale_y_continuous(labels = label_comma()) + 
  scale_fill_paletteer_c("viridis::plasma") +
  theme_minimal() +
  theme(legend.position = "None")
ggsave("top_20_num_views.png", dpi = 900)

# Top 20 tags by num_views_per_tag_count
tags_num_views_per_freq %>% 
  arrange(desc(num_views_per_tag_count)) %>% 
  top_n(20) %>% 
  mutate(tags = fct_reorder(tags, num_views_per_tag_count)) %>%
  ggplot(aes(x = tags, y = num_views_per_tag_count)) +
  geom_bar(stat = "identity", aes(fill = num_views_per_tag_count), alpha = 0.6, width = 0.7) +
  coord_flip() +
  ggtitle('Top 20 Tags in Global Issues and Technology by Views Per Tag Count') +
  xlab("Tags") +
  ylab("Number of views per tag count") +
  scale_y_continuous(labels = label_comma()) + 
  scale_fill_paletteer_c("viridis::plasma") +
  theme_minimal() +
  theme(legend.position = "None")
ggsave("top_20_num_views_per.png", dpi = 900)

### Num_comments ####
# Top 20 tags by num_comments
tags_num_comments %>% 
  arrange(desc(n)) %>% 
  top_n(20) %>% 
  mutate(tags = fct_reorder(tags, n)) %>%
  ggplot(aes(x = tags, y = n)) +
  geom_bar(stat = "identity", aes(fill = n), alpha = 0.6, width = 0.7) +
  coord_flip() +
  ggtitle('Top 20 Tags in Global Issues and Technology by Number of Comments') +
  xlab("Tags") +
  ylab("Number of comments") +
  scale_y_continuous(labels = label_comma()) + 
  scale_fill_paletteer_c("viridis::plasma") +
  theme_minimal() +
  theme(legend.position = "None")
ggsave("top_20_num_comments.png", dpi = 900)

# Top 20 tags by num_comments_per_tag_count
tags_num_comments_per_freq %>% 
  arrange(desc(num_comments_per_tag_count)) %>% 
  top_n(20) %>% 
  mutate(tags = fct_reorder(tags, num_comments_per_tag_count)) %>%
  ggplot(aes(x = tags, y = num_comments_per_tag_count)) +
  geom_bar(stat = "identity", aes(fill = num_comments_per_tag_count), alpha = 0.6, width = 0.7) +
  coord_flip() +
  ggtitle('Top 20 Tags in Global Issues and Technology by Number of Comments Per Tag Count') +
  xlab("Tags") +
  ylab("Number of comments per tag count") +
  scale_y_continuous(labels = label_comma()) + 
  scale_fill_paletteer_c("viridis::plasma") +
  theme_minimal() +
  theme(legend.position = "None")
ggsave("top_20_num_comments_per.png", dpi = 900)

### Num_translations ####
# Top 20 tags by num_translations
tags_num_translations %>% 
  arrange(desc(n)) %>% 
  top_n(20) %>% 
  mutate(tags = fct_reorder(tags, n)) %>%
  ggplot(aes(x = tags, y = n)) +
  geom_bar(stat = "identity", aes(fill = n), alpha = 0.6, width = 0.7) +
  coord_flip() +
  ggtitle('Top 20 Tags in Global Issues and Technology by Number of Translations') +
  xlab("Tags") +
  ylab("Number of translations") +
  scale_y_continuous(labels = label_comma()) + 
  scale_fill_paletteer_c("viridis::plasma") +
  theme_minimal() +
  theme(legend.position = "None")
ggsave("top_20_num_translations.png", dpi = 900)

# Top 20 tags by num_translations_per_tag_count
tags_num_translations_per_freq %>%
  arrange(desc(num_translations_per_tag_count)) %>% 
  top_n(20) %>% 
  mutate(tags = fct_reorder(tags, num_translations_per_tag_count)) %>%
  ggplot(aes(x = tags, y = num_translations_per_tag_count)) +
  geom_bar(stat = "identity", aes(fill = num_translations_per_tag_count), alpha = 0.6, width = 0.7) +
  coord_flip() +
  ggtitle('Top 20 Tags in Global Issues and Technology by Number of Translations Per Tag Count') +
  xlab("Tags") +
  ylab("Number of translations per tag count") +
  scale_y_continuous(labels = label_comma()) + 
  scale_fill_paletteer_c("viridis::plasma") +
  theme_minimal() +
  theme(legend.position = "None")
ggsave("top_20_num_translations_per.png", dpi = 900)

### Composite index ###
# Rescale measurements of popularity
## Create function for normalization (min-max scaling)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

## Create df with rescaled/normalized n
resc_tags_num_views <- tags_num_views
resc_tags_num_views$n <- normalize(resc_tags_num_views$n)

resc_tags_num_comments <- tags_num_comments
resc_tags_num_comments$n <- normalize(resc_tags_num_comments$n)

resc_tags_num_translations <- tags_num_translations
resc_tags_num_translations$n <- normalize(resc_tags_num_translations$n)

## Merge dfs with normalized n
p1_merge <- inner_join(resc_tags_num_views, resc_tags_num_comments, by = 'tags')
global_composite <- inner_join(p1_merge, resc_tags_num_translations, by = 'tags')
global_composite <- global_composite %>% 
  rename('num_views' = n.x, 'num_comments' = n.y, 'num_translations' = n) %>% 
  mutate(composite = rowMeans(global_composite[,2:4], na.rm = TRUE))

### Baseline chart ###
global_composite_lower <- global_composite %>% 
  arrange(composite) %>% 
  top_n(-10)

global_composite_upper <- global_composite %>% 
  arrange(desc(composite)) %>% 
  top_n(10)

global_composite_baseline <- rbind(global_composite_lower,global_composite_upper)

tbl_df(global_composite_baseline) %>% 
  arrange(desc(composite)) %>% 
  top_n(20) %>% 
  mutate(tags = fct_reorder(tags, composite)) %>%
  ggplot(aes(x = reorder(tags, composite), y = composite)) + 
  geom_segment(aes(x = tags, xend = tags, y = 0.13585, yend = composite), color = "grey") +
  geom_point(color="orange", size = 4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
    ) +
  coord_flip() +
  ggtitle("Global Issues/Technology Tags vs Average Composite Measure of Trend") +
  xlab("Top 10 and bottom 10 tags") +
  ylab("Composite measure of trend")
ggsave("composite_top_ten.png", dpi = 900)

### Correlograms ###
global_composite.vars <- global_composite %>% select(num_views, num_comments, num_translations)
ggpairs(global_composite.vars, title = "Measures of Trend in Global Issues and Technology Correlation")
ggsave("global_corr.png", dpi = 900)

### Timeline ###
global_tech_long %>%
  ggplot(aes(x = date, y = num_views)) +
  geom_line(color = "grey") +
  geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 3) +
  theme_ipsum() +
  scale_x_date(limits = as.Date(c("2007-01-01","2018-01-01"))) +
  scale_y_continuous(labels = label_comma()) + 
  ggtitle("Number of Views in Global Issue and Technology by Year") +
  xlab("Year") +
  ylab("Number of views")
ggsave("global_year_views.png", dpi = 900)

global_tech_long %>%
  ggplot(aes(x = date, y = num_comments)) +
  geom_line(color = "grey") +
  geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 3) +
  theme_ipsum() +
  scale_x_date(limits = as.Date(c("2007-01-01","2018-01-01"))) +
  scale_y_continuous(labels = label_comma()) + 
  ggtitle("Number of Comments in Global Issue and Technology by Year") +
  xlab("Year") +
  ylab("Number of comments")
ggsave("global_year_comments.png", dpi = 900)

global_tech_long %>%
  ggplot(aes(x = date, y = num_translations)) +
  geom_line(color = "grey") +
  geom_point(shape = 21, color = "black", fill = "#69b3a2", size = 3) +
  theme_ipsum() +
  scale_x_date(limits = as.Date(c("2007-01-01","2018-01-01"))) +
  scale_y_continuous(labels = label_comma()) + 
  ggtitle("Number of Translations in Global Issue and Technology by Year") +
  xlab("Year") +
  ylab("Number of translations")
ggsave("global_year_translations.png", dpi = 900)
