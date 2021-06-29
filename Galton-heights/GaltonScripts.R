library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

# rho <- mean(scale(x)*scale(y))
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

library(Lahman)
data("Teams")
View(Teams)
Teams %>% filter(yearID %in% 1961:2001) %>% summarise(r = cor(R / G, AB / G)) %>% pull(r)
Teams %>% filter(yearID %in% 1961:2001) %>% summarise(r = cor(W / G, E / G)) %>% pull(r)
Teams %>% filter(yearID %in% 1961:2001) %>% summarise(r = cor(X3B / G, X2B / G)) %>% pull(r)

galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)
