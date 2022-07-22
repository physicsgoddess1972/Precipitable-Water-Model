<<<<<<< HEAD
library(gapminder)
library(tidyverse)
library(gganimate)
options(gganimate.nframes=20)

gapminder %>%
    select(country, pop, year, continent) %>%
    group_by(year) %>%
    arrange(year, -pop) %>%
    mutate(rank=1:n()) %>%
    filter(rank <= 10) ->
ranked_by_year

my_theme <- theme_classic(base_family = "Times") +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  theme(legend.background = element_rect(fill = "linen")) +
  theme(plot.background = element_rect(fill = "linen")) +
  theme(panel.background = element_rect(fill = "linen"))

ggplot(data = ranked_by_year) +
  aes(group = country, fill = continent) +
  aes(xmin = 0 ,
      xmax = pop / 1000000) +
  aes(ymin = rank - .45,
      ymax = rank + .45) +
  scale_y_reverse() +
  scale_x_continuous(
    limits = c(-300, 1400),
    breaks = c(0, 400, 800, 1200),
    labels = c(0, 400, 800, 1200)) +
  labs(fill = "") +
  geom_rect(alpha = .7) +
  labs(x = 'Population (millions)') +
  aes(label = country, y = rank) +
  geom_text(col = "gray13",
            hjust = "right",
            x = -50) +
  labs(y = "") +
  scale_fill_viridis_d(option = "magma",
                       direction = -1) +
  geom_text(x = 1000 , y = -10,
            family = "Times",
            aes(label = as.character(year)),
            size = 30, col = "grey18") +
    my_theme -> g
=======
library(gapminder)
library(tidyverse)
library(gganimate)
options(gganimate.nframes=20)

gapminder %>%
    select(country, pop, year, continent) %>%
    group_by(year) %>%
    arrange(year, -pop) %>%
    mutate(rank=1:n()) %>%
    filter(rank <= 10) ->
ranked_by_year

my_theme <- theme_classic(base_family = "Times") +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(axis.line.y = element_blank()) +
  theme(legend.background = element_rect(fill = "linen")) +
  theme(plot.background = element_rect(fill = "linen")) +
  theme(panel.background = element_rect(fill = "linen"))

ggplot(data = ranked_by_year) +
  aes(group = country, fill = continent) +
  aes(xmin = 0 ,
      xmax = pop / 1000000) +
  aes(ymin = rank - .45,
      ymax = rank + .45) +
  scale_y_reverse() +
  scale_x_continuous(
    limits = c(-300, 1400),
    breaks = c(0, 400, 800, 1200),
    labels = c(0, 400, 800, 1200)) +
  labs(fill = "") +
  geom_rect(alpha = .7) +
  labs(x = 'Population (millions)') +
  aes(label = country, y = rank) +
  geom_text(col = "gray13",
            hjust = "right",
            x = -50) +
  labs(y = "") +
  scale_fill_viridis_d(option = "magma",
                       direction = -1) +
  geom_text(x = 1000 , y = -10,
            family = "Times",
            aes(label = as.character(year)),
            size = 30, col = "grey18") +
    my_theme -> g
>>>>>>> 47addb531525c32ecd60a78ae867ca064c77857a
g + gganimate::transition_time(year)