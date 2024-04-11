library(tidyverse)
library(ggthemes)
library(ggfortify)
library(GGally)

dat <- read.csv("~/R Projects/three30/partyinst_data_330.csv") %>%
  mutate(COW = factor(COWcode.x), leg_elect = factor(v2eltype_0), exe_elect = factor(v2eltype_6), elect_system = factor(v2elcomvot))

dat_plot <- select(dat, !COW)
ggpairs(dat_plot)


