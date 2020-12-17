library(tidyverse)
library(tidytext)

sweaters <- read_csv(here::here("data", "use_this_data", "holiday_sweaters-2020-12-15-clean.csv")) 

num_colors <- read_csv(here::here("data", "use_this_data", "holiday_sweaters-2020-12-15-clean.csv")) %>% 
  separate_rows(colors, sep = c(", ")) %>% 
  group_by(sweater) %>% 
  summarize(
    num_colors = length(colors)
  )

num_description_words <- read_csv(here::here("data", "use_this_data", "holiday_sweaters-2020-12-15-clean.csv"))  %>% 
  unnest_tokens(output = word, input = image_desc, token = "words") %>% 
  group_by(sweater) %>% 
  summarize(
    num_desc_words = length(word)
  )

all_data <- full_join(num_colors, num_description_words)

x <- ggplot(all_data, aes(x = num_colors, y = num_desc_words)) +
  geom_point(size = 8, color = "maroon1") +
  geom_point(size = 6, color = "lightyellow3") +
  geom_smooth(method = "lm", color = "maroon1") +
  labs(x = "Num Cols", y = "Num Desc Words") +
  theme_classic() +
  theme(axis.title = element_text(size = 40),
        axis.text = element_text(size = 2))
