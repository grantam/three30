library(tidyverse)
library(ggthemes)
library(ggfortify)
library(GGally)
library(alr4)
library(car)


dat <- read.csv("~/R Projects/three30/partyinst_data_330.csv") %>%
  mutate(COW = factor(COWcode.x), leg_elect = factor(v2eltype_0), exe_elect = factor(v2eltype_6), elect_system = factor(v2elcomvot), reg_vs = (v2eltrnout/100)*(v2pavote/100), v2paelcont = as.factor(v2paelcont)) %>%
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


ggpairs(dat, columns = c("v2elvaptrn", "v2elvaptrn", "exe_elect", "v2elcomvot", "v2xpa_antiplural", "v2paseatshare", "v2paelcont", "v2papariah", "v2pariglef", "elect_system", "v2x_polyarchy", "piendla", "psla", "vap_vs", "reg_vs"))

summary(dat)

#### The VAP vote share variable, seems suspect, so there may be some coding errors, so we will stick with registed vote share.

## Stepwise model selection
attach(dat)

base_model <- lm(reg_vs ~ 1)
full_model <- lm(reg_vs ~ elect_system + v2pariglef + psla + v2papariah + exe_elect + v2paelcont + v2xpa_antiplural + v2elcomvot + v2x_polyarchy)

car::vif(full_model)

forward <- step(base_model,
                direction = "forward",
                k = 2,
                scope = list(lower = base_model, upper = full_model))

back <- step(full_model,
                direction = "backward",
                k = 2,
                scope = list(lower = base_model, upper = full_model))

both <- step(base_model,
             direction = "both",
             k = 2,
             scope = list(lower = base_model, upper = full_model))

forward
back
both

#### The best formula from all three stepwise selections is this:

f1 <- reg_vs ~ psla + elect_system + v2x_polyarchy + v2pariglef + 
  v2papariah + v2paelcont

#### Looking at the model and checking assumptions

m1 <- lm(f1)

autoplot(m1, which = 1, nrow = 1, ncol = 1)

#### Linearity looks suprisingly good here.

autoplot(m1, which = 2, nrow = 1, ncol = 1) 
shapiro.test(m1$residuals)

#### Errors are not normal!

autoplot(m1, which = 3, nrow = 1, ncol = 1) 

#### There might be some heterogeneity, but its pretty good here.

autoplot(m1, which = 4, nrow = 1, ncol = 1)

#### Cooks Distance looks pretty good

#### Lets look at the histogram for the dependent variable

hist(reg_vs) #### This looks really skewed. Whe might want to do a Box Cox transformation

f1
bc_results <- boxCox((reg_vs+.1) ~ psla + elect_system + v2x_polyarchy + v2pariglef + v2papariah + 
                       v2paelcont)

#### It looks like we should mutate the DV and make it reg_vs^-1

dat_new <- dat %>%
  mutate(reg_vs = (reg_vs + .1)^(-1))

attach(dat_new)  

m2 <- lm(f1)

autoplot(m2, which = 1, nrow = 1, ncol = 1)

#### Linearity looks suprisingly good here.

autoplot(m2, which = 2, nrow = 1, ncol = 1) 
shapiro.test(m1$residuals)

#### Errors are more normal!

autoplot(m2, which = 3, nrow = 1, ncol = 1) 

#### There might be some heterogeneity, but its pretty good here.

autoplot(m2, which = 4, nrow = 1, ncol = 1)

#### Cooks Distance looks pretty good

#### To ensure that we have the best model here, lets rerun this with the stepwise selection

base_model <- lm(reg_vs ~ 1)
full_model <- lm(reg_vs ~ elect_system + v2pariglef + psla + v2papariah + exe_elect + v2paelcont + v2xpa_antiplural + v2elcomvot + v2x_polyarchy)

forward <- step(base_model,
                direction = "forward",
                k = 2,
                scope = list(lower = base_model, upper = full_model))

back <- step(full_model,
             direction = "backward",
             k = 2,
             scope = list(lower = base_model, upper = full_model))

both <- step(base_model,
             direction = "both",
             k = 2,
             scope = list(lower = base_model, upper = full_model))

forward
back
both

#### All of the selection methods choose this model, so we will go with that:

f2 <- reg_vs ~ psla + elect_system + v2pariglef + v2x_polyarchy + 
  v2xpa_antiplural + v2papariah + v2paelcont

#### Checking assumptions once again.

m3 <- lm(f2)

autoplot(m3, which = 1, nrow = 1, ncol = 1)

#### Linearity looks good here.

autoplot(m3, which = 2, nrow = 1, ncol = 1) 
shapiro.test(m3$residuals)

#### Errors are more normal than the other models. Best we are probaly going to get.

autoplot(m3, which = 3, nrow = 1, ncol = 1) 

#### There might be some heterogeneity, but its pretty good here.

autoplot(m3, which = 4, nrow = 1, ncol = 1)

#### no outliers

car::vif(m3)

#### No multicolinearity!

summary(m3)
