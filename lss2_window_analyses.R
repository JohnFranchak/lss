library(multcomp)
library(lsmeans)
library(tidyverse)
library(effects)
library(lme4)
library(lmerTest)
library(gridExtra)
library(rcompanion)
library(psych)
library(sjPlot)

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
my_theme <-   theme(text = element_text(size = 20),
                    axis.text.x = element_text(size = 20, color = "black"), axis.title.x = element_text(size = 24),
                    axis.text.y = element_text(size = 20,  color = "black"), axis.title.y = element_text(size = 24), 
                    panel.background = element_blank(),panel.border = element_blank(), panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                    legend.key = element_rect(fill = "white")) 

ds <- read_csv("~/Dropbox/LSS/window_stats.csv", na = "NaN")
ds <- mutate(ds, id = factor(id))
ds$task <- factor(ds$task,levels = c(1,2),labels = c("walk","search"))
ds <- filter(ds, id != 212) #What about 210?

#X CORR
ggplot(ds, aes(x = win, y = eyexpos_corr, color = id)) + facet_wrap(~ task) + 
  geom_smooth(se = F) + my_theme

ds %>% filter(win <= 30) %>% 
  lmer(eyexpos_corr ~ task*win + (1|id),.) -> res
anova(res)

ds %>% filter(win <= 30) %>% split(.$task) %>%
  map(~ lmer(eyexpos_corr ~ win + (1|id),.)) %>% 
  map(anova)

ds %>% group_by(win, task) %>% filter(win <= 30) %>%
  summarise(corr = mean(eyexpos_corr), n = n(), se = sd(eyexpos_corr)/sqrt(n), ymin = corr - se, ymax = corr + se) %>% 
  ggplot(aes(y = corr, x = win, color = task, ymin = ymin, ymax = ymax)) +
  geom_pointrange() + geom_line() + my_theme + ylab("Eye-head correlation")

#XCORR WITH ADDITIONAL COVARIATES
ds %>% filter(win <= 30) %>% 
  lmer(eyexpos_corr ~ task*win + path_speed + straightness + (1|id),.) -> res
anova(res)

ds %>% filter(win <= 30) %>% split(.$task) %>%
  map(~ lmer(eyexpos_corr ~ win + path_speed + straightness + (1|id),.)) %>% 
  map(anova)


#YCORR
ggplot(ds, aes(x = win, y = eyeypos_corr, color = id)) + facet_wrap(~ task) + 
  geom_smooth(se = F) + my_theme

ds %>% filter(win <= 30) %>% 
  lmer(eyeypos_corr ~ task*win + (1|id),.) -> res
anova(res)

ds %>% filter(win <= 30) %>% split(.$task) %>%
  map(~ lmer(eyeypos_corr ~ win + (1|id),.)) %>% 
  map(anova)

ds %>% group_by(win, task) %>% filter(win <= 30) %>%
  summarise(corr = mean(eyeypos_corr), n = n(), se = sd(eyeypos_corr)/sqrt(n), ymin = corr - se, ymax = corr + se) %>% 
  ggplot(aes(y = corr, x = win, color = task, ymin = ymin, ymax = ymax)) +
  geom_pointrange() + geom_line() + my_theme + ylab("Eye-head correlation")

#Speed
ggplot(ds, aes(x = win, y = path_speed, color = id)) + facet_wrap(~ task) + 
  geom_smooth(se = F) + my_theme

ds %>% filter(win <= 30) %>% 
  lmer(path_speed ~ task*win + (1|id),.) -> res
anova(res)

ds %>% filter(win <= 30) %>% split(.$task) %>%
  map(~ lmer(path_speed ~ win + (1|id),.)) %>% 
  map(anova)

ds %>% group_by(win, task) %>% filter(win <= 30) %>% filter(!is.na(path_speed)) %>%
  summarise(corr = mean(path_speed), n = n(), se = sd(path_speed)/sqrt(n), ymin = corr - se, ymax = corr + se) %>% 
  ggplot(aes(y = corr, x = win, color = task, ymin = ymin, ymax = ymax)) +
  geom_pointrange() + geom_line() + my_theme + ylab("Speed")

#Straightness
ggplot(ds, aes(x = win, y = straightness, color = id)) + facet_wrap(~ task) + 
  geom_smooth(se = F) + my_theme

ds %>% filter(win <= 30) %>% 
  lmer(straightness ~ task*win + (1|id),.) -> res
anova(res)

ds %>% filter(win <= 30) %>% split(.$task) %>%
  map(~ lmer(straightness ~ win + (1|id),.)) %>% 
  map(anova)

ds %>% group_by(win, task) %>% filter(win <= 30) %>% filter(!is.na(straightness)) %>%
  summarise(corr = mean(straightness), n = n(), se = sd(straightness)/sqrt(n), ymin = corr - se, ymax = corr + se) %>% 
  ggplot(aes(y = corr, x = win, color = task, ymin = ymin, ymax = ymax)) +
  geom_pointrange() + geom_line() + my_theme + ylab("Straightness")

# LONG FORMAT STD --------------
dsl <-  gather(ds, key = "cond", value = "std", "posx_std","posy_std","eyex_std","eyey_std")
dsl$eyehead <- factor(ifelse(is.na(str_extract(dsl$cond,"eye")),"head","eye"))
dsl$dim <- factor(ifelse(is.na(str_extract(dsl$cond,"x")),"y","x"))

dsl %>% group_by(win, task, eyehead, dim) %>% filter(win <= 30) %>%
  summarise(var = mean(std), n = n(), se = sd(std)/sqrt(n), ymin = var - se, ymax = var + se) %>% 
  ggplot(aes(y = var, x = win, color = task, ymin = ymin, ymax = ymax, shape = eyehead)) + facet_wrap(~ dim) +
  geom_pointrange() + geom_line() + my_theme 

dsl %>% filter(win <= 30) %>% split(.$dim) %>%
  map(~ lmer(std ~ task*win*eyehead + (1|id),.)) %>% 
  map(anova)

dsl %>% filter(win <= 30) %>% split(.$task) %>%
  map(~ lmer(posx_std ~ win + (1|id),.)) %>% 
  map(anova)

#EYEX STD WITH ADDITIONAL COVARIATES
ds %>% filter(win <= 30) %>% 
  lmer(eyex_std ~ task*win + path_speed + straightness + (1|id),.) -> res
anova(res)

ds %>% filter(win <= 30) %>% split(.$task) %>%
  map(~ lmer(eyex_std ~ win + path_speed + straightness + (1|id),.)) %>% 
  map(anova)

#EYEX STD WITH ADDITIONAL COVARIATES
ds %>% filter(win <= 30) %>% 
  lmer(posx_std ~ task*win + path_speed + straightness + (1|id),.) -> res
anova(res)

ds %>% filter(win <= 30) %>% split(.$task) %>%
  map(~ lmer(posx_std ~ win + path_speed + straightness + (1|id),.)) %>% 
  map(anova)

# LONG FORMAT GPS COVARIATES --------------
dsl <-  gather(ds, key = "cond", value = "stdev", "posx_std","posy_std","eyex_std","eyey_std")
dsl$eyehead <- factor(ifelse(is.na(str_extract(dsl$cond,"eye")),"head","eye"))
dsl$dim <- factor(ifelse(is.na(str_extract(dsl$cond,"x")),"y","x"))
dsl <- filter(dsl, dim == "x")

dsl %>% filter(win <= 30) %>% 
  lmer(stdev ~ eyehead * task * path_speed + (id|win),.)  -> res
  anova(res)
  plot_model(res, type = "int")
