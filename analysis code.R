#### By Grant and Alex and Ethan

library(tidyverse)
library(ggthemes)
library(ggfortify)
library(GGally)
library(alr4)
library(car)
library(rstanarm)
library(interflex)
library(interplot)

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

summary(dat)

#### The VAP vote share variable, seems suspect, so there may be some coding errors, so we will stick with registed vote share.

## Stepwise model selection
attach(dat)

base_model <- lm(reg_vs ~ 1)
full_model <- lm(reg_vs ~ elect_system + v2pariglef + psla + v2papariah + exe_elect + v2paelcont + v2xpa_antiplural + v2x_polyarchy)

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
hist(m1$residuals)

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
hist(m2$residuals)

#### Errors are more normal!

autoplot(m2, which = 3, nrow = 1, ncol = 1) 

#### There might be some heterogeneity, but its pretty good here.

autoplot(m2, which = 4, nrow = 1, ncol = 1)

#### Cooks Distance looks pretty good

#### To ensure that we have the best model here, lets rerun this with the stepwise selection

base_model <- lm(reg_vs ~ 1)
full_model <- lm(reg_vs ~ elect_system + v2pariglef + psla + v2papariah + exe_elect + v2paelcont + v2xpa_antiplural + v2x_polyarchy)

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
f2


### Exploratory Data Analysis

ggplot(data = dat, aes(x = psla, y = reg_vs)) +
  geom_point() +
  geom_smooth() + 
  labs(title = "Party Stregnth vs Vote Share (Pre Box-Cox Transformation)", x = "Party Strength", y = "Vote Share") + 
  theme_minimal()

ggplot(data = dat_new, aes(x = psla, y = reg_vs)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Party Stregnth vs Vote Share (Post Box-Cox Transformation)", x = "Party Strength", y = "Vote Share") + 
  theme_minimal()

#### for Question 2

ggplot(data = dat, aes(x = psla, y = reg_vs)) +
  geom_point(aes(color = v2x_polyarchy)) +
  geom_smooth(color = "red") + 
  labs(title = "Party Stregnth vs Vote Share (Pre Box-Cox Transformation)", x = "Party Strength", y = "Vote Share") + 
  theme_minimal()

ggplot(data = dat_new, aes(x = psla, y = reg_vs)) +
  geom_point(aes(color = v2x_polyarchy)) +
  geom_smooth(color = "red") + 
  labs(title = "Party Stregnth vs Vote Share (Post Box-Cox Transformation)", x = "Party Strength", y = "Vote Share") + 
  theme_minimal()

dat_interflex <- dat %>%
  rename(Democracy = v2x_polyarchy, Vote_Share = reg_vs, Party_Strength = psla)

interflex(estimator = "gam", Y = "Vote_Share", X = "Party_Strength", D = "Democracy", data = dat_interflex)


## Inference

#### center and standardize the data

dat_center <- dat_new %>%
  mutate(psla = (psla - mean(psla))/(2*sd(psla)), v2pariglef = (v2pariglef - mean(v2pariglef))/(2*sd(v2pariglef)), v2x_polyarchy = (v2x_polyarchy - mean(v2x_polyarchy))/(2*sd(v2x_polyarchy)), v2xpa_antiplural = (v2xpa_antiplural - mean(v2xpa_antiplural))/(2*sd(v2xpa_antiplural)), v2papariah = (v2papariah - mean(v2papariah))/(2*sd(v2papariah)))



stan_sub <- stan_glm(f2, data = dat_center, refresh = 0)

new_data <- data.frame(psla = c(0, 1, 0,0,0,0,0,0,0,0,0), elect_system = c("0", "0", "1", "2", "3", "0","0","0","0","0","0"), v2pariglef = c(0,0,0,0,0,1,0,0,0,0,0), v2x_polyarchy = c(0,0,0,0,0,0,1,0,0,0,0), v2xpa_antiplural = c(0,0,0,0,0,0,0,1, 0,0,0), v2papariah = c(0,0,0,0,0,0,0,0,1,0, 0), v2paelcont = c("0", "0", "0", "0", "0", "0","0","0","0","1","2"))

predictions <- posterior_linpred(stan_sub, newdata = new_data)

pred_mat <- matrix(NA, nrow = 10, ncol = 3)
for (i in 1:10) {
  sub <- predictions[,1+i] - predictions[,1]
  pred_mat[i,] <- quantile(sub, c(0.025, .5, .975))
  }

pred_mat <- as.data.frame(pred_mat)
colnames(pred_mat) <- c("lower", "median", "upper")
pred_mat$IV <- c("Party Stregnth", "Compulsory Vote (Mild)", "Compulsory Vote (Medium)", "Compulsory Vote (Strong)", "Left Wing", "Democracy", "Anti-Pluralist", "Pariah", "Reformed Party", "Same Party")

pred_mat <- pred_mat %>%
  mutate(IV = reorder(IV, -median))

#### Use this to show out results instead of a regression table.

ggplot(data = pred_mat) +
  geom_linerange(aes(y = IV, xmin = upper*-1, xmax = lower*-1)) +
  geom_point(aes(y = IV, x = median*-1)) +
  labs(title="Substantive Effects of Predictors On Vote Share", y = "", x = "Effect Size", caption = "All predictors are mean centered and standardized to a 2 standard deviation change") +
  theme_minimal() +
  geom_vline(xintercept = 0)

#### How important is the effect of party strength

m3 <- lm(reg_vs ~ psla + elect_system + v2pariglef + v2x_polyarchy + v2xpa_antiplural + v2papariah + v2paelcont, data = dat_new)

m4 <- lm(reg_vs ~ elect_system + v2pariglef + v2x_polyarchy + v2xpa_antiplural + v2papariah + v2paelcont, data = dat_new)

m5 <- lm(reg_vs ~ piendla + elect_system + v2pariglef + v2x_polyarchy + v2xpa_antiplural + v2papariah + v2paelcont, data = dat_new)

anova(m3, m4, m5)

stan_sub2 <- stan_glm(reg_vs ~ piendla + elect_system + v2pariglef + v2x_polyarchy + v2xpa_antiplural + v2papariah + v2paelcont, data = dat_new, refresh = 0)

loo_m1 <- loo(stan_sub)
loo_m2 <- loo(stan_sub2)
loo_compare(loo_m1, loo_m2)

#### robustness check

robust <- stan_glm(gov ~ psla + elect_system + v2pariglef + v2x_polyarchy + v2xpa_antiplural + v2papariah + v2paelcont, data = dat_new, family = binomial(link = "logit"), refresh = 0)

test_data <- data.frame(psla = seq(0,1,by=.1), elect_system = "0", v2pariglef = 0, v2x_polyarchy = 0, v2xpa_antiplural = 0, v2papariah = 0, v2paelcont = "0")

fv <- posterior_epred(robust, newdata = test_data)

fitted_values <- as.data.frame(apply(fv, 2, mean))

ps <- seq(0,1, by = .1)

test_data1 <- cbind(ps, fitted_values) %>%
  rename(fv =`apply(fv, 2, mean)`)

ggplot(data = test_data1, aes(x = ps, y = fv)) +
  geom_smooth(color = "black") +
  labs(title = "Probability of Controling Government", x = "Party Strength", y = "Pr(Winning Governemnt)") +
  theme_minimal()

#### model for question 2

attach(dat_center)

base_model <- lm(reg_vs ~ psla*v2x_polyarchy)
full_model <- lm(reg_vs ~ elect_system + v2pariglef + v2papariah + exe_elect + v2paelcont + v2xpa_antiplural + psla*v2x_polyarchy)

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

#### all the models give the same formula here, so we are going to go with:

f3 <- reg_vs ~ psla + v2x_polyarchy + elect_system + v2pariglef + 
  v2xpa_antiplural + v2papariah + v2paelcont + psla:v2x_polyarchy


#### check assumptions

inter1 <- lm(f3)

autoplot(inter1, which = 1, nrow = 1, ncol = 1)

#### linearity looks good

autoplot(inter1, which = 2, nrow = 1, ncol = 1)
hist(inter1$residuals)

#### looks pretty normal

autoplot(inter1, which = 3, nrow = 1, ncol = 1)

#### constant variance looks good

autoplot(inter1, which = 4, nrow = 1, ncol = 1)

#### no serious outliers.

car::vif(inter1, type = "predictor")

#### no multicolinearity

#### meets assumptions

#### look at the summary

summary(inter1)

#### make a plot to show it

stan_sub <- stan_glm(f3, data = dat_center, refresh = 0)

new_data <- data.frame(psla = c(0,0, 0, 1, 1, 1), elect_system = "0", v2pariglef = 0, v2x_polyarchy = c(-1,0,1,-1,0,1), v2xpa_antiplural = 0, v2papariah = 0, v2paelcont = "0")

predictions <- posterior_linpred(stan_sub, newdata = new_data)

pred_mat <- matrix(NA, nrow = 3, ncol = 3)
for (i in 1:3) {
  sub <- predictions[,3+i] - predictions[,i]
  pred_mat[i,] <- quantile(sub, c(0.025, .5, .975))
}

pred_mat <- as.data.frame(pred_mat)
colnames(pred_mat) <- c("lower", "median", "upper")
pred_mat$IV <- c("Party Strength (Democracy = 2.5%)", "Party Strength (Democracy = 50%)", "Party Strength (Democracy = 97.5%)")

pred_mat <- pred_mat %>%
  mutate(IV = reorder(IV, -median))

#### Use this to show out results instead of a regression table.

ggplot(data = pred_mat) +
  geom_linerange(aes(y = IV, xmin = upper*-1, xmax = lower*-1)) +
  geom_point(aes(y = IV, x = median*-1)) +
  labs(title="Substantive Effects of Party Strength", subtitle = "At Different Levels of Democracy", y = "", x = "Effect Size", caption = "All predictors are mean centered and standardized to a 2 standard deviation change") +
  theme_minimal()

inter2 <- lm(-1*reg_vs ~ psla + v2x_polyarchy + elect_system + v2pariglef + v2xpa_antiplural + 
               v2papariah + v2paelcont + psla:v2x_polyarchy, data = dat_new)

interplot(m = inter2, var1 = "psla", var2 = "v2x_polyarchy") +
  labs(title = "Marginal Effect of Party Strength", subtitle = "Conditional on Democratic Quality", x = "Quality of Democracy", y = "Effect (Beta) of Party Strength") +
  theme_minimal()
  

#### what model is better? Interaction for non-interaction?

anova(m3, inter1)

## Predicting votes share of famous parties

