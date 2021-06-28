library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()


Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>% mutate(runs_per_game = R / G, AB_per_game = AB / G) %>%
  ggplot(aes(AB_per_game, runs_per_game)) + geom_point()

Teams %>% filter(yearID %in% 1961:2001) %>% mutate(wins_per_game = W / G, E_per_game = E / G) %>%
  ggplot(aes(wins_per_game, E_per_game)) + geom_point()

Teams %>% filter(yearID %in% 1961:2001) %>% mutate(X3B_per_game = X3B / G, X2B_per_game = X2B / G) %>%
  ggplot(aes(X3B_per_game, X2B_per_game)) + geom_point()
