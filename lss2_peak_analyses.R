library(multcomp)
library(emmeans)
library(tidyverse)
library(effects)
library(lme4)
library(lmerTest)
library(gridExtra)
library(rcompanion)
library(psych)
library(ggforce)
library(interactions)

setwd("~/Documents/Github/lss")
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
ds <- read_csv("summary_stats_peaks.csv", na = "NaN")
ds <- filter(ds, ds$opp == 0)
ds <- mutate(ds, id = factor(id))
ds$task <- factor(ds$cond, levels = c("Walk","Search"))
ds$bin[ds$bin > 7] <- 7
#ds$bin <- factor(ds$bin, levels = c(0,1,2,3,4,5,6,7,8,9,10,11), labels = c("0-10","10-20","20-30","30-40","40-50","50-60","60-70","70-80","80-90","90-100","100-110","110+"))
ds$bin <- factor(ds$bin, levels = c(0,1,2,3,4,5,6,7), labels = c("10","20","30","40","50","60","70","80+"))
ds$eye_prop <- abs(ds$eye_prop)
ds$head_prop <- abs(ds$head_prop)
ds$eye <- abs(ds$eye)
ds$head <- abs(ds$head)

# Head contribution --------------

#AVERAGING BY SAMPLE, USING SHIFT BINS
ds %>% group_by(task,id, bin) %>% 
  summarise(head_prop = mean(head_prop), na.rm = T) %>% 
  lmer(head_prop ~ bin*task + (1|id), data = .) -> res
summary(res)
anova(res)
emmeans(res, pairwise~task|bin,adjust = "Holm")
contrast(emmeans(res, ~bin|task), "consec",adjust = "Holm")
contrast(emmeans(res, ~bin|task), "poly")
contrast(emmeans(res, ~bin*task), "poly") 


ds %>% group_by(task, bin) %>% 
  summarise(eye = mean(abs(eye_prop), na.rm = T), n = n(), se_eye = sd(abs(eye_prop), na.rm = T)/sqrt(n), ymin_eye = eye - se_eye, ymax_eye = eye + se_eye, head = mean(abs(head_prop), na.rm = T), se_head = sd(abs(head_prop), na.rm = T)/sqrt(n), ymin_head = head - se_head, ymax_head = head + se_head) %>% 
  ggplot(aes(x = bin, color = task, y = head, ymin = ymin_head, ymax = ymax_head)) + 
  labs(x = "Total gaze shift (º)", y = "Head contribution to gaze shift (prop.)") + #facet_wrap(~ task) + 
  #geom_pointrange(aes(x = bin, color = task, y = eye, ymin = ymin_eye, ymax = ymax_eye), size =1, position = position_dodge(.6)) +
  geom_pointrange(size =1, shape = 22, fill = "white", na.rm = T) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + #geom_smooth(method = "loess", na.rm = T) + 
  theme(legend.position = "bottom") +
  ylim(c(.4,.7))
ggsave("figures/gaze_prop.pdf", units = "in", width = 10, height = 4)

#plot individuals
ds %>% group_by(id, task, bin) %>% 
  summarise(eye = mean(abs(eye_prop), na.rm = T), n = n(), se_eye = sd(abs(eye_prop), na.rm = T)/sqrt(n), ymin_eye = eye - se_eye, ymax_eye = eye + se_eye, head = mean(abs(head_prop), na.rm = T), se_head = sd(abs(head_prop), na.rm = T)/sqrt(n), ymin_head = head - se_head, ymax_head = head + se_head) %>% 
  ggplot(aes(x = bin, color = id, y = head, ymin = ymin_head, ymax = ymax_head, group = id)) + 
  labs(x = "Total gaze shift(º)", y = "Head contribution to gaze shift (prop.)") + facet_wrap(~ task) + 
  geom_smooth(method = "loess", na.rm = T, se = F) + 
  theme(legend.position = "bottom")
  #ylim(c(.4,.7))




#Eye contribution in degrees
ds %>% group_by(task, bin) %>% 
  summarise(eye_m = mean(abs(eye), na.rm = T), n = n(), se_eye = sd(abs(eye), na.rm = T)/sqrt(n), ymin_eye = eye_m - se_eye, ymax_eye = eye_m + se_eye) %>% 
  ggplot(aes(x = bin, color = task, y = eye_m, ymin = ymin_eye, ymax = ymax_eye)) + 
  labs(x = "Total gaze shift(º)", y = "Eye contribution (º)") +
  geom_pointrange(size =1, shape = 22, fill = "white", na.rm = T) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + ylim(0,100) + 
  theme(legend.position = "bottom") 

lmer(head ~ bin*task + (task|id),data = ds,REML=F) -> res #less good fit with bin|id added
summary(res)
anova(res)
plot(allEffects(res))
contrast(emmeans(res, ~bin|task), "poly",adjust = "Holm")
contrast(emmeans(res, ~bin|task), "consec",adjust = "Holm")
contrast(emmeans(res, ~task|bin), "pairwise",adjust = "Holm")
eff_size(emmeans(res, ~bin|task), sigma(res), df.residual(res))

#Head contribution in degrees
ds %>% group_by(task, bin) %>%  
  summarise(head_m = mean(head, na.rm = T), n = n(), se_head = sd(head, na.rm = T)/sqrt(n), ymin_head = head_m - se_head, ymax_head = head_m + se_head) %>% 
  ggplot(aes(x = bin, color = task, y = head_m, ymin = ymin_head, ymax = ymax_head)) + 
  labs(x = "Total gaze shift(º)", y = "Head contribution (º)") + 
  geom_pointrange(size =1, shape = 22, fill = "white", na.rm = T) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + ylim(0,90) + 
  theme(legend.position = "bottom") 
  
  #All eye data
  ggplot(data = filter(ds, total > 10 & total < 100), aes(x = total, y = abs(eye), color = task)) + geom_smooth() +
    xlim(0,100) + ylim(0,100) + geom_abline(intercept = 0, slope = 1, color = "gray")
  #All head data
  ggplot(data = filter(ds, total > 10 & total < 100), aes(x = total, y = abs(head), color = task)) + geom_smooth() +
    xlim(0,100) + ylim(0,100) + geom_abline(intercept = 0, slope = 1, color = "gray")
  #Both
  ggplot(data = filter(ds, total > 5 & total < 100)) + 
    geom_smooth(aes(x = total, y = abs(eye), color = task)) +
    geom_smooth(aes(x = total, y = abs(head), color = task), linetype = "dashed") +
    xlim(0,100) + ylim(0,100) + geom_abline(intercept = 0, slope = 1, color = "gray")
  
  #Head as proportion
  ggplot(data = filter(ds, total > 5 & total < 100)) + 
    #geom_smooth(aes(x = total, y = eye_prop, color = task)) +
    geom_smooth(aes(x = total, y = head_prop, color = task), linetype = "dashed") +
    xlim(0,100) + ylim(0,1) + geom_hline(yintercept = .5, color = "gray")
  
  
  #NOT AVERAGED BY SAMPLE
  lmer(head ~ total*task + (task|id),data = ds) -> res
  summary(res)
  anova(res)
  plot(allEffects(res))
  interact_plot(res, pred = total, modx = task, plot.points = F, robust = T)
  
  lmer(head_prop ~ bin*task + (task|id),data = ds) -> res
  summary(res)
  anova(res)
  plot(allEffects(res))
  contrast(emmeans(res, ~bin|task), "poly",adjust = "Holm")
  contrast(emmeans(res, ~task|bin), "pairwise",adjust = "Holm")
  eff_size(emmeans(res, ~bin|task), sigma(res), df.residual(res),method = "poly")
  eff_size(emmeans(res, ~task|bin), sigma(res), df.residual(res),method = "pairwise")
  
  
#NO OUTLIERS
# dsl %>% group_by(dim, eyehead, task) %>% 
#   summarise(ym = mean(std, na.rm = T), lower = ym - 3*sd(std, na.rm = T), upper = ym + 3*sd(std, na.rm = T), ymin = min(std, na.rm =T), ymax = max(std, na.rm = T), x = 1) %>% 
#   ggplot(aes(x = interaction(dim, task, eyehead))) + geom_boxplot(aes(ymin = ymin, lower = lower, middle = ym, upper = upper, ymax = ymax), stat = "identity") +
#   geom_point(data = dsl, aes(y = std, group = interaction(dim, task, eyehead)), na.rm = T)