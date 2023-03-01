# source: https://www.kaggle.com/code/jonathanbouchet/dragon-ball

### importing data
library(tidyverse)


# reading dataset
dbz <- read_csv("/Users/oscaralonso/Documents/DS/data/Dragon_Ball_Data_Set.csv")


# cleaning names of columns
dbz <- dbz %>% 
  janitor::clean_names()


# separating names and other stuff in character column
# getting power_level in numeric form and getting rid of commas
# factoring sagas to keep chronological order
dbz_sep <- dbz %>% 
  separate(character, into = c('name', 'other_stuff'),
           sep = "['\\(]") %>% 
  mutate(power_level = as.numeric(gsub("\\,", "", power_level)),
         saga_or_movie = factor(saga_or_movie, levels = c(unique(saga_or_movie))))

# getting Goku without GT & movies
goku <- dbz_sep %>% 
  filter(dragon_ball_series != 'Dragon Ball GT',
         !grepl('GT|Movie|OVA|History|Friends|Bardock', saga_or_movie),
         grepl('Goku', name)) %>% 
  select(!other_stuff) %>% 
  rename(goku_power_level = power_level,
         goku = name)

# getting Vegeta without GT & movies
vegeta <- dbz_sep %>% 
  filter(dragon_ball_series != 'Dragon Ball GT',
         !grepl('GT|Movie|OVA|History|Friends|Bardock', saga_or_movie),
         grepl('Vegeta', name)) %>% 
  select(!other_stuff) %>% 
  rename(vegeta_power_level = power_level,
         vegeta = name)

# merge goku and vegeta
goku_and_vegeta <- merge(goku, vegeta, by = 'saga_or_movie', all = T) %>% 
  select(goku_power_level, vegeta_power_level, saga_or_movie)


g <- goku_and_vegeta %>% 
  group_by(saga_or_movie) %>% 
  summarise(goku_avg_power = mean(goku_power_level),
            vegeta_avg_power = mean(vegeta_power_level)) %>%
  ggplot(aes(x = saga_or_movie)) +
  geom_line(aes(y = goku_avg_power, group = 1, color = 'Goku'), size = 2) +
  geom_point(aes(y = goku_avg_power), size = 3) +
  geom_line(aes(y = vegeta_avg_power, group = 1, color = 'Vegeta'), size = 2,
            shape = 15) +
  geom_point(aes(y = vegeta_avg_power), size = 3) +
  scale_x_discrete(name = "",
                   guide = guide_axis(angle = 90)) +
  scale_y_log10(name = '',
                labels = scales::label_comma()) +
  scale_color_manual(name = "",
                     values = c('Goku' = 'orange', 'Vegeta' = 'blue')) +
  geom_vline(aes(xintercept = 8), lty=2) +
  ggplot2::annotate("text", x = 7, y = 1e10,
                    label = "Dragon Ball timeline", size=5, colour="gray30",
                    angle = 90) +
  ggplot2::annotate("text", x = 9, y = 1e10,
                    label = "Dragon Ball Z timeline", size=5, colour="gray30",
                    angle = 90) + 
  labs(title = 'Power Level in Dragon Ball: Goku vs. Vegeta') +
  ggthemes::theme_economist_white()

g

# ggsave('/Users/oscaralonso/Desktop/dbz_goku_vegeta_power_levels.png',
#       width = 19,
#       height = 13,
#       units = "in",
#       dpi = 400)









