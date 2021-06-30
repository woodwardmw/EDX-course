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


---------------------


#find regression line for predicting runs from BBs
library(tidyverse)
library(Lahman)
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))

# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 

#Question 3
library(Lahman)
Teams <- Teams %>% filter(yearID %in% 1961:2001) %>% mutate(R_per_game = R / G, BB_per_game = BB / G, HR_per_game = HR / G)
fit <- lm(R_per_game ~ BB_per_game + HR_per_game, data = Teams)
summary(fit)
fit$coefficients
Teams %>% ggplot(aes(BB_per_game + HR_per_game, R_per_game)) + geom_point() + geom_smooth(method = "lm")


#Question 9
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
head(bat_02)

bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>% group_by(playerID) %>% summarise(mean_singles = mean(singles), mean_bb = mean(bb))
head(bat_03)
bat_99_01 %>% filter(mean_singles > 0.2) %>% nrow()
bat_99_01 %>% filter(mean_bb > 0.2) %>% nrow()

bat_99_02 <- inner_join(bat_02, bat_99_01)
cor(bat_99_02$singles, bat_99_02$mean_singles)
cor(bat_99_02$bb, bat_99_02$mean_bb)

bat_99_02 %>% ggplot(aes(singles, mean_singles)) + geom_point()
bat_99_02 %>% ggplot(aes(bb, mean_bb)) + geom_point()

lm(singles ~ mean_singles, data = bat_99_02)
lm(bb ~ mean_bb, data = bat_99_02)
