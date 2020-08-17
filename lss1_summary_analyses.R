library(multcomp)
library(lsmeans)
library(tidyverse)
library(effects)
library(lme4)
library(lmerTest)
library(rstatix)
library(rcompanion)
library(psych)
library(ggforce)
library(patchwork)

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
theme_update(text = element_text(size = 18),
      axis.text.x = element_text(size = 18, color = "black"), axis.title.x = element_text(size = 21, margin = margin(t = 0, r = 0, b = 10, l = 0)),
      axis.text.y = element_text(size = 18,  color = "black"), axis.title.y = element_text(size = 21, margin = margin(t = 0, r = 10, b = 0, l = 0)), 
      panel.background = element_blank(),panel.border = element_blank(), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), 
      axis.ticks.length = unit(.25, "cm"), axis.ticks = element_line(size = 1, lineend = "round"),
      legend.key = element_rect(fill = "white")) 

# CREATE DATA SET ---------------------------------------------------------------
ds <- read_csv("summary_stats_LSS1.csv", na = "NaN")
ds <- mutate(ds, id = factor(id))
#ds <- filter(ds, id != 212 & id != 203) 
ds[ds["id"] == 13, "search_eyex_speed"] <- NA

# POSITION SD MEANS --------------
dsl <-  gather(ds, key = "cond", value = "std", "walk_eyex_std","search_eyex_std","walk_eyey_std","search_eyey_std")
dsl$dim <- factor(ifelse(is.na(str_extract(dsl$cond,"x")),"y","x"))
dsl$task <- factor(ifelse(is.na(str_extract(dsl$cond,"walk")),"search","walk"),levels = c("walk","search"),labels = c("Walk","Search"))

dsl %>% filter(dim == "x") %>% group_by(task) %>% get_summary_stats(std, type = "mean_sd")
dsl %>% filter(dim == "x") %>% t_test(std ~ task, paired = TRUE, var.equal = T, detailed = T) %>% add_significance()
dsl %>% filter(dim == "x") %>% cohens_d(std ~ task, paired = TRUE) 

dsl %>% group_by(dim, task) %>% filter(dim == "x") %>% 
  summarise(stdev = mean(std, na.rm = T), n = n(), se = sd(std, na.rm = T)/sqrt(n), ymin = stdev - se, ymax = stdev + se) %>% 
  ggplot() + 
  labs(x = "", y = "Position SD (º)") + 
  geom_sina(data = filter(dsl, dim == "x"), aes(y = std, x = task, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3) +
  geom_errorbar(aes(x = task, group = task,ymin = ymin, ymax = ymax), size =1, width = .3, position = position_dodge(.6)) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + 
  scale_y_continuous(breaks = c(0,10,20,30,40), limits = c(0,40))
ggsave("figures/lss1_position_sd.pdf", units = "in", width = 4, height = 4)
  
#NO OUTLIERS
dsl %>% group_by(dim, task) %>% 
  summarise(ym = mean(std, na.rm = T), lower = ym - 3*sd(std, na.rm = T), upper = ym + 3*sd(std, na.rm = T), ymin = min(std, na.rm =T), ymax = max(std, na.rm = T), x = 1) %>% 
  ggplot(aes(x = interaction(dim, task))) + geom_boxplot(aes(ymin = ymin, lower = lower, middle = ym, upper = upper, ymax = ymax), stat = "identity") +
  geom_point(data = dsl, aes(y = std, group = interaction(dim, task)), na.rm = T)

# SPEED MEANS --------------
dsl <-  gather(ds, key = "cond", value = "speed", "walk_eyex_speed","search_eyex_speed","walk_eyey_speed","search_eyey_speed")
dsl$dim <- factor(ifelse(is.na(str_extract(dsl$cond,"x")),"y","x"))
dsl$task <- factor(ifelse(is.na(str_extract(dsl$cond,"walk")),"search","walk"),levels = c("walk","search"),labels = c("Walk","Search"))

shapiro_test(ds$walk_eyex_speed - ds$search_eyex_speed)
dsl %>% filter(dim == "x") %>% group_by(task) %>% get_summary_stats(speed, type = "mean_sd")
dsl %>% filter(dim == "x") %>% t_test(speed ~ task, paired = TRUE) %>% add_significance()
dsl %>% filter(dim == "x") %>% cohens_d(speed ~ task, paired = TRUE) 

dsl %>% group_by(dim, task) %>% filter(dim == "x") %>% 
  summarise(Speed = mean(speed, na.rm = T), n = n(), se = sd(speed, na.rm = T)/sqrt(n), ymin = Speed - se, ymax = Speed + se) %>% 
  ggplot() + 
  labs(x = "", y = "Speed (º/s)") + 
  geom_sina(data = filter(dsl, dim == "x"), aes(y = speed, x = task, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3) +
  geom_errorbar(aes(x = task, group = task,ymin = ymin, ymax = ymax), size =1, width = .3, position = position_dodge(.6)) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + 
  scale_y_continuous(breaks = 0:5, limits = c(0,5)) 
  ggsave("figures/lss1_speed.pdf", units = "in", width = 4, height = 4)

#FIND OUTLIERS (Done, excluded at file read above)
dsl %>% group_by(dim, task) %>% 
  summarise(ym = mean(speed, na.rm = T), lower = ym - 3*sd(speed, na.rm = T), upper = ym + 3*sd(speed, na.rm = T), ymin = min(speed, na.rm =T), ymax = max(speed, na.rm = T), x = 1) %>% 
  ggplot(aes(x = interaction(dim, task))) + geom_boxplot(aes(ymin = ymin, lower = lower, middle = ym, upper = upper, ymax = ymax), stat = "identity") +
  geom_point(data = dsl, aes(y = speed, group = interaction(dim, task)))

# GPS DATA  --------------
#SPEED BY TASK
cor.test(ds$walk_path_speed, ds$search_path_speed)

ds %>% gather(key = "task", value = "speed", "walk_path_speed", "search_path_speed") %>% 
mutate(task = factor(task, levels = c("walk_path_speed", "search_path_speed"), labels = c("Walk", "Search"))) -> dsl

dsl %>% group_by(task) %>% get_summary_stats(speed, type = "mean_sd")
dsl %>% t_test(speed ~ task, paired = TRUE) %>% add_significance()
dsl %>% cohens_d(speed ~ task, paired = TRUE) 
  
  dsl %>%  group_by(task) %>% 
  summarise(Speed = mean(speed, na.rm = T), n = n(), se = sd(speed, na.rm = T)/sqrt(n), ymin = Speed - se, ymax = Speed + se) %>%
  ggplot() + 
  geom_sina(data = dsl, aes(y = speed, x = task, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3, na.rm = T) +
  geom_errorbar(aes(x = task, ymin = ymin, ymax = ymax), color = "black", size =1, width = .3, position = position_dodge(.6)) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + 
  labs(x = "", y = "Walking speed (m/s)") + 
  scale_y_continuous(breaks = c(.25, .5, .75, 1, 1.25, 1.5), limits = c(.25, 1.5)) +
    theme(legend.position = "none") -> p1
  #ggsave("figures/lss1_walking_speed.pdf", units = "in", width = 5, height = 4)

#SPEED SD BY TASK
cor.test(ds$walk_path_speed_sd, ds$search_path_speed_sd)

ds %>% gather(key = "task", value = "speed", "walk_path_speed_sd", "search_path_speed_sd") %>% 
  mutate(task = factor(task, levels = c("walk_path_speed_sd", "search_path_speed_sd"), labels = c("Walk", "Search"))) -> dsl

dsl %>% group_by(task) %>% get_summary_stats(speed, type = "mean_sd")
dsl %>% t_test(speed ~ task, paired = TRUE) %>% add_significance()
dsl %>% cohens_d(speed ~ task, paired = TRUE) 

dsl %>%  group_by(task) %>% 
  summarise(Speed = mean(speed, na.rm = T), n = n(), se = sd(speed, na.rm = T)/sqrt(n), ymin = Speed - se, ymax = Speed + se) %>%
  ggplot() + 
  geom_sina(data = dsl, aes(y = speed, x = task, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3, na.rm = T) +
  geom_errorbar(aes(x = task, ymin = ymin, ymax = ymax), color = "black", size =1, width = .3, position = position_dodge(.6)) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + 
  labs(x = "", y = "Walking speed SD (m/s)") +
  scale_y_continuous(breaks = c(0, .25, .5, .75, 1), limits = c(0,1)) +
  theme(legend.position = "none") -> p2
  #ggsave("figures/lss1_walking_speed_sd.pdf", units = "in", width = 5, height = 4)

#STRAIGHTNESS BY TASK
cor.test(ds$walk_straightness, ds$search_straightness)

ds %>% gather(key = "task", value = "speed", "walk_straightness", "search_straightness") %>% 
  mutate(task = factor(task, levels = c("walk_straightness", "search_straightness"), labels = c("Walk", "Search"))) -> dsl

dsl %>% group_by(task) %>% get_summary_stats(speed, type = "mean_sd")
dsl %>% t_test(speed ~ task, paired = TRUE) %>% add_significance()
dsl %>% cohens_d(speed ~ task, paired = TRUE) 

dsl %>%  group_by(task) %>% 
  summarise(Speed = mean(speed, na.rm = T), n = n(), se = sd(speed, na.rm = T)/sqrt(n), ymin = Speed - se, ymax = Speed + se) %>%
  ggplot() + 
  geom_sina(data = dsl, aes(y = speed, x = task, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3, na.rm = T) +
  geom_errorbar(aes(x = task, ymin = ymin, ymax = ymax), color = "black", size =1, width = .3, position = position_dodge(.6)) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + 
  labs(x = "", y = "Straightness ratio") + 
  scale_y_continuous(breaks = c(0, 2.5, 5, 7.5), limits = c(0,7.5)) +
  theme(legend.position = "none") -> p3
  #ggsave("figures/lss1_walking_straightness.pdf", units = "in", width = 5, height = 4)

  p1 + p2 + p3
  ggsave("figures/lss1_walking_stats_composite.pdf", units = "in", width = 9, height = 4)
  