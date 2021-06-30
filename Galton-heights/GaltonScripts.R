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

# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mean_mother <- mean(female_heights$mother)
sd_mother <- sd(female_heights$mother)
mean_daughter <- mean(female_heights$daughter)
sd_daughter <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)
slope <- r * sd_daughter / sd_mother
intercept <- mean_daughter - slope * mean_mother

