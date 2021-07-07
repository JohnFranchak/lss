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

ds <- read_csv("window_stats.csv", na = "NaN")
ds <- mutate(ds, id = factor(id))
ds$task <- factor(ds$task,levels = c(1,2),labels = c("walk","search"))
ds <- filter(ds, id != 212 & id != 203) 

# MIN AND MAX STRAIGHTNESS --------------
ds_minstr <- ds %>% group_by(id, task) %>% slice_min(straightness, n = 5) %>% mutate(straight = 1)
ds_maxstr <- ds %>% group_by(id, task) %>% slice_max(straightness, n = 5) %>% mutate(straight = 0)
ds_minmax <- bind_rows(ds_minstr, ds_maxstr) %>% mutate(straight = factor(straight, levels = c(0,1), labels = c("sinuous", "straight")))
dsl <-  gather(ds_minmax, key = "cond", value = "std", "posx_std","posy_std","eyex_std","eyey_std")
dsl$eyehead <- factor(ifelse(is.na(str_extract(dsl$cond,"eye")),"head","eye"))
dsl$dim <- factor(ifelse(is.na(str_extract(dsl$cond,"x")),"y","x"))
dsl <- filter(dsl, dim == "x")

dsl %>% group_by(task, eyehead, straight) %>%
  summarise(var = mean(std), n = n(), se = sd(std)/sqrt(n), ymin = var - se, ymax = var + se) %>% 
  ggplot(aes(y = var, x = eyehead, color = straight, ymin = ymin, ymax = ymax)) + facet_wrap(~task) +
  geom_pointrange() + my_theme 

dsl %>% group_by(task, eyehead, straight) %>%
  summarise(var = mean(std), n = n(), se = sd(std)/sqrt(n), ymin = var - se, ymax = var + se) %>% 
  ggplot(aes(y = var, x = eyehead, color = task, ymin = ymin, ymax = ymax)) + facet_wrap(~straight) +
  geom_pointrange() + my_theme 

anova(lmer(std ~ task*eyehead*straight + (1|id), data = dsl))
dsl %>% group_by(task) %T>% group_map(~ print(anova(lmer(std ~ straight*eyehead + (1|id), data = .x)))) %>% group_keys()
dsl %>% group_by(straight) %T>% group_map(~ print(anova(lmer(std ~ task*eyehead + (1|id), data = .x)))) %>% group_keys()

summary(lmer(straightness ~ task*straight + (1|id), data = dsl))

dsl %>% group_by(task, straight) %>%
  summarise(str = mean(straightness), n = n(), se = sd(straightness)/sqrt(n), ymin = str - se, ymax = str + se) %>% 
  ggplot(aes(y = str, x = task, color = straight, ymin = ymin, ymax = ymax)) + 
  geom_pointrange() + my_theme 

# MIN AND MAX SPEED --------------
ds_minstr <- ds %>% group_by(id, task) %>% slice(which.min(path_speed)) %>% mutate(fast = 0)
ds_maxstr <- ds %>% group_by(id, task) %>% slice(which.max(path_speed)) %>% mutate(fast = 1)
ds_minmax <- bind_rows(ds_minstr, ds_maxstr) %>% mutate(fast = factor(fast, levels = c(0,1), labels = c("slowest", "fastest")))
dsl <-  gather(ds_minmax, key = "cond", value = "std", "posx_std","posy_std","eyex_std","eyey_std")
dsl$eyehead <- factor(ifelse(is.na(str_extract(dsl$cond,"eye")),"head","eye"))
dsl$dim <- factor(ifelse(is.na(str_extract(dsl$cond,"x")),"y","x"))
dsl <- filter(dsl, dim == "x")

dsl %>% group_by(task, eyehead, fast) %>%
  summarise(var = mean(std), n = n(), se = sd(std)/sqrt(n), ymin = var - se, ymax = var + se) %>% 
  ggplot(aes(y = var, x = eyehead, color = fast, ymin = ymin, ymax = ymax)) + facet_wrap(~task) +
  geom_pointrange() + my_theme 

dsl %>% group_by(task, eyehead, fast) %>%
  summarise(var = mean(std), n = n(), se = sd(std)/sqrt(n), ymin = var - se, ymax = var + se) %>% 
  ggplot(aes(y = var, x = eyehead, color = task, ymin = ymin, ymax = ymax)) + facet_wrap(~fast) +
  geom_pointrange() + my_theme 

anova(lmer(std ~ task*eyehead*fast + (1|id), data = dsl))
dsl %>% group_by(task) %T>% group_map(~ print(anova(lmer(std ~ fast*eyehead + (1|id), data = .x)))) %>% group_keys()
dsl %>% group_by(fast) %T>% group_map(~ print(anova(lmer(std ~ task*eyehead + (1|id), data = .x)))) %>% group_keys()

anova(lmer(path_speed ~ task*fast + (1|id), data = dsl))

dsl %>% group_by(task, fast) %>%
  summarise(spd = mean(path_speed), n = n(), se = sd(path_speed)/sqrt(n), ymin = spd - se, ymax = spd + se) %>% 
  ggplot(aes(y = spd, x = task, color = fast, ymin = ymin, ymax = ymax)) + 
  geom_pointrange() + my_theme 













# COMPARE SPREAD ACCORDING TO STRAIGHTNESS ALL BINS --------------
dsl <-  gather(ds, key = "cond", value = "std", "posx_std","posy_std","eyex_std","eyey_std")
dsl$eyehead <- factor(ifelse(is.na(str_extract(dsl$cond,"eye")),"head","eye"))
dsl$dim <- factor(ifelse(is.na(str_extract(dsl$cond,"x")),"y","x"))
dsl <- filter(dsl, dim == "x")
dsl$win <- factor(dsl$win)

dsl %>% group_by(task, eyehead) %>%
  summarise(var = mean(std), n = n(), se = sd(std)/sqrt(n), ymin = var - se, ymax = var + se) %>% 
  ggplot(aes(y = var, x = eyehead, color = task, ymin = ymin, ymax = ymax)) +
  geom_pointrange() + my_theme 

res <- lmer(std ~ task*eyehead*straightness + (1|id) + (1|win) + (1|id:win), data = dsl)
summary(res)

res <- lmer(std ~ task*eyehead + straightness + (task + eyehead|win) + (task + eyehead|id), data = dsl)
summary(res)
