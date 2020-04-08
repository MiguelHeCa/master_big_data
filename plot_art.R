library(tidyverse)

test <- map_dfr(1:10, ~ crossing(x = {
  x = seq(30) + 0.3 * .x
  
  ifelse(x > 30, x - 30, x)
},
nesting(
  y = seq(1, .x, length.out = 10) ^ 0.5,
  t = 1:10
)))

map_dfr(1:10, ~ crossing(x = {
  x = seq(30) + 0.3 * .x
  
  ifelse(x > 30, x - 30, x)
},
nesting(
  y = seq(1, .x, length.out = 10) ^ 0.5,
  t = 1:10
))) %>%
  ggplot(aes(x, y)) +
  geom_point(color = 'white') +
  coord_polar()

# source: https://www.r-craft.org/r-news/tweetable-mathematical-art-with-r/
seq(-3, 3, by = .01) %>%
  expand.grid(x = ., y = .) %>%
  ggplot(aes(x = (x ^ 3 - sin(y ^ 2)), y = (y ^ 3 - cos(x ^ 2)))) +
  geom_point(
    alpha = .1,
    shape = 20,
    size = 0,
    color = "white"
  ) +
  theme_void() +
  coord_fixed() +
  theme(panel.background = element_rect(fill = "black")) +
  coord_polar()
