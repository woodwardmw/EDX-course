library(tidyverse)
load("rda/murders.rda")
# head(murders)
murders %>% mutate(abb = reorder(abb, rate)) %>% 
  ggplot(aes(abb, rate)) + 
  geom_bar(width = 0.5, stat = "identity", color = "black") + 
  coord_flip()
ggsave("figs/barplot.png")