library(tidyverse)
library(ggthemes)
library(ggfortify)
library(GGally)



dat <- read.csv("~/R Projects/three30/partyinst_data_330.csv") %>%
  mutate(COW = factor(COWcode.x), leg_elect = factor(v2eltype_0), exe_elect = factor(v2eltype_6), elect_system = factor(v2elcomvot), reg_vs = (v2eltrnout/100)*(v2pavote/100)) %>%
  na.omit()


#### Here are the names of the variables we will be using:
#### COW - a numerical indicator given to each country
#### v2elvaptrn - Voting Age Turnout
#### v2elvaptrn - Registered turnout
#### leg_elect - legislative elections
#### exe_elect - execitive elections
#### v2elcomvot - Compulsory voting
#### v2xpa_antiplural - party's anti-pluralism measure
#### v2pavote - party vote share
#### v2paseatshare - party seat share
#### v2paelcont - party brand continuity
#### v2papariah - party's pariah measure
#### v2pariglef - Party left right scale
#### elect_system - electoral system
#### v2x_polyarchy - Democracy score
#### piendla - Party institutioanlization ****
#### psla - Party stregnth ****
#### vap_vs - Percent of the VAP turnout that a party got ****
#### reg_vs - Percent of the Registered turn out a party got ****


ggpairs(dat, columns = c("v2elvaptrn", "v2elvaptrn", "leg_elect", "exe_elect", "v2elcomvot", "v2xpa_antiplural", "v2paseatshare", "v2paelcont", "v2papariah", "v2pariglef", "elect_system", "v2x_polyarchy", "piendla", "psla", "vap_vs", "reg_vs"))

summary(dat)

#### The VAP vote share variable, seems suspect, so there may be some coding errors, so we will stick with registed vote share.

## Stepwise model selection
attach(dat)

base_model <- lm(reg_vs ~ 1)
full_model <- lm(reg_vs ~ elect_system + v2pariglef + leg_elect + psla + v2papariah + exe_elect + v2paelcont + v2xpa_antiplural + v2elcomvot + v2x_polyarchy)

forward <- step(base_model,
                direction = "forward",
                k = 2,
                scope = list(lower = base_model, upper = full_model))
