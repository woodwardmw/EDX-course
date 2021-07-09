library(Lahman)
library(tidyverse)
library(dslabs)
head(Teams)

Teams %>% filter(yearID == 1971) %>% do(tidy(lm(R_per_game ~ BB_per_game + HR_per_game, data = .), conf.int = TRUE))

Teams %>% filter(yearID %in% 1961:2018) %>% group_by(yearID) %>% do(tidy(lm(R_per_game ~ BB_per_game + HR_per_game, data = .), conf.int = TRUE)) %>%
  filter(term == "BB_per_game") %>% ggplot(aes(yearID, estimate)) + geom_point() + geom_smooth(method = lm)

fit <- Teams %>% filter(yearID %in% 1961:2018) %>% group_by(yearID) %>% do(tidy(lm(R ~ BB + HR, data = .))) %>% ungroup()
fit %>% filter(term == "BB") %>% lm(estimate ~ yearID, data = .) %>% tidy()
