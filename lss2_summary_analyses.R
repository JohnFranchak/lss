pkgs <- c("multcomp", "emmeans", "tidyverse", "effects", "lme4", "lmerTest", "psych","ggforce","patchwork")
#lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)
lapply(pkgs, library, character.only = TRUE)

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7")
theme_update(text = element_text(size = 18),
             axis.text.x = element_text(size = 18, color = "black"), axis.title.x = element_text(size = 21, margin = margin(t = 0, r = 0, b = 10, l = 0)),
             axis.text.y = element_text(size = 18,  color = "black"), axis.title.y = element_text(size = 21, margin = margin(t = 0, r = 10, b = 0, l = 0)), 
             panel.background = element_blank(),panel.border = element_blank(), panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), 
             axis.ticks.length = unit(.25, "cm"), axis.ticks = element_line(size = 1, lineend = "round"),
             legend.key = element_rect(fill = "white")) 

# CREATE DATA SET ---------------------------------------------------------------
ds <- read_csv("summary_stats_mot_corrected.csv", na = "NaN")
ds <- mutate(ds, id = factor(id))
ds <- filter(ds, id != 212 & id != 203) 
ds[ds["id"] == 229, "walk_posy_speed"] <- NA

# POSITION SD MEANS --------------
dsl <-  gather(ds, key = "cond", value = "std", "walk_posx_std","search_posx_std","walk_posy_std","search_posy_std","walk_eyex_std","search_eyex_std","walk_eyey_std","search_eyey_std")
dsl$eyehead <- factor(ifelse(is.na(str_extract(dsl$cond,"eye")),"head","eye"), levels = c("eye","head"),labels = c("Eyes","Head"))
dsl$dim <- factor(ifelse(is.na(str_extract(dsl$cond,"x")),"y","x"))
dsl$task <- factor(ifelse(is.na(str_extract(dsl$cond,"walk")),"search","walk"), levels = c("walk","search"),labels = c("Walk","Search"))

dsl %>% split(.$dim) %>% 
  map(~ lmer(std ~ eyehead*task + (1|id),data = .)) -> res
res %>% map(anova)
res %>% map(~ emmeans(.,pairwise~eyehead|task, adjust = "Holm"))
res %>% map(~ emmeans(.,pairwise~task|eyehead, adjust = "Holm"))

dsl %>% group_by(dim, eyehead, task) %>% filter(dim == "x") %>% 
  summarise(stdev = mean(std, na.rm = T), n = n(), se = sd(std, na.rm = T)/sqrt(n), ymin = stdev - se, ymax = stdev + se) %>% 
  ggplot() + 
  labs(x = "", y = "Position SD (º)") + 
  geom_sina(data = filter(dsl, dim == "x"), aes(y = std, x = eyehead, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3) +
  geom_errorbar(aes(x = eyehead, group = task,ymin = ymin, ymax = ymax), size =1, width = .3, position = position_dodge(.6)) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + 
  scale_y_continuous(breaks = c(0,10,20,30,40), limits = c(0,40))
ggsave("figures/lss2_position_sd.pdf", units = "in", width = 4, height = 4)
  
#NO OUTLIERS
dsl %>% group_by(dim, eyehead, task) %>% 
  summarise(ym = mean(std, na.rm = T), lower = ym - 3*sd(std, na.rm = T), upper = ym + 3*sd(std, na.rm = T), ymin = min(std, na.rm =T), ymax = max(std, na.rm = T), x = 1) %>% 
  ggplot(aes(x = interaction(dim, task, eyehead))) + geom_boxplot(aes(ymin = ymin, lower = lower, middle = ym, upper = upper, ymax = ymax), stat = "identity") +
  geom_point(data = dsl, aes(y = std, group = interaction(dim, task, eyehead)), na.rm = T)

#POSITION MEANS WITH TOTAL GAZE INCLUDED --------
dsl <-  gather(ds, key = "cond", value = "std", "walk_posx_std","search_posx_std","walk_eyex_std","search_eyex_std","walk_gazex_std","search_gazex_std")
dsl$eye <- factor(ifelse(is.na(str_extract(dsl$cond,"eye")),"","Eye"))
dsl$head <- factor(ifelse(is.na(str_extract(dsl$cond,"pos")),"","Head"))
dsl$total <- factor(ifelse(is.na(str_extract(dsl$cond,"gaze")),"","Gaze"))
dsl$eyehead <- factor(str_c(dsl$eye,dsl$head,dsl$total,sep = ""))
dsl$eyehead <- factor(dsl$eyehead, levels = c("Eye", "Head","Gaze"))
dsl$task <- factor(ifelse(is.na(str_extract(dsl$cond,"walk")),"search","walk"), levels = c("walk","search"),labels = c("Walk","Search"))

dsl %>% group_by(eyehead, task) %>% 
  summarise(stdev = mean(std, na.rm = T), n = n(), se = sd(std, na.rm = T)/sqrt(n), ymin = stdev - se, ymax = stdev + se) %>% 
  ggplot() + 
  labs(x = "", y = "Position SD (º)") + 
  geom_sina(data = filter(dsl), aes(y = std, x = eyehead, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3) +
  geom_errorbar(aes(x = eyehead, group = task,ymin = ymin, ymax = ymax), size =1, width = .3, position = position_dodge(.6)) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + 
  scale_y_continuous(breaks = c(0,10,20,30,40), limits = c(0,40))
ggsave("figures/lss2_position_sd.pdf", units = "in", width = 5, height = 4)

# SPEED MEANS --------------
dsl <-  gather(ds, key = "cond", value = "speed", "walk_posx_speed","search_posx_speed","walk_posy_speed","search_posy_speed","walk_eyex_speed","search_eyex_speed","walk_eyey_speed","search_eyey_speed")
dsl$eyehead <- factor(ifelse(is.na(str_extract(dsl$cond,"eye")),"head","eye"), levels = c("eye","head"),labels = c("Eyes","Head"))
dsl$dim <- factor(ifelse(is.na(str_extract(dsl$cond,"x")),"y","x"))
dsl$task <- factor(ifelse(is.na(str_extract(dsl$cond,"walk")),"search","walk"),levels = c("walk","search"),labels = c("Walk","Search"))

dsl %>% split(.$dim) %>% 
  map(~ lmer(speed ~ eyehead*task + (1|id),data = .)) -> res
res %>% map(anova)
res %>% map(~ emmeans(.,pairwise~eyehead|task, adjust = "Holm"))
res %>% map(~ emmeans(.,pairwise~task|eyehead, adjust = "Holm"))

dsl %>% group_by(dim, eyehead, task) %>% filter(dim == "x") %>% 
  summarise(Speed = mean(speed, na.rm = T), n = n(), se = sd(speed, na.rm = T)/sqrt(n), ymin = Speed - se, ymax = Speed + se) %>% 
  ggplot() + 
  labs(x = "", y = "Speed (º/s)") + 
  geom_sina(data = filter(dsl, dim == "x"), aes(y = speed, x = eyehead, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3) +
  geom_errorbar(aes(x = eyehead, group = task,ymin = ymin, ymax = ymax), size =1, width = .3, position = position_dodge(.6)) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + 
  scale_y_continuous(breaks = 0:5, limits = c(0,5)) 
  ggsave("figures/lss2_speed.pdf", units = "in", width = 4, height = 4)

#FIND OUTLIERS (Done, excluded at file read above)
dsl %>% group_by(dim, eyehead, task) %>% filter(dim == "x") %>% 
  summarise(ym = mean(speed, na.rm = T), lower = ym - 3*sd(speed, na.rm = T), upper = ym + 3*sd(speed, na.rm = T), ymin = min(speed, na.rm =T), ymax = max(speed, na.rm = T), x = 1) %>% 
  ggplot(aes(x = interaction(dim, task, eyehead))) + geom_boxplot(aes(ymin = ymin, lower = lower, middle = ym, upper = upper, ymax = ymax), stat = "identity") +
  geom_point(data = filter(dsl, dim == "x"), aes(y = speed, group = interaction(dim, task, eyehead)))

# LONG FORMAT EYE-HEAD CORR --------------
dsl <-  gather(ds, key = "cond", value = "corr", "walk_eyexpos_corr","walk_eyeypos_corr","search_eyexpos_corr","search_eyeypos_corr")
dsl$dim <- factor(ifelse(is.na(str_extract(dsl$cond,"x")),"y","x"))
dsl$task <- factor(ifelse(is.na(str_extract(dsl$cond,"walk")),"search","walk"),levels = c("walk","search"),labels = c("Walk","Search"))

t.test(ds$walk_eyexpos_corr,ds$search_eyexpos_corr, paired = T)

dsl %>% group_by(dim, task) %>% filter(dim == "x") %>% 
  summarise(stdev = mean(corr), n = n(), se = sd(corr)/sqrt(n), ymin = stdev - se, ymax = stdev + se) %>% 
  ggplot() + 
  geom_sina(data = filter(dsl, dim == "x"), aes(y = corr, x = task, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3) +
  geom_errorbar(aes(x = task, ymin = ymin, ymax = ymax), color = "black", size =1, width = .3, position = position_dodge(.6)) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + 
  labs(x = "", y = "Eye-head correlation (r)") + 
  scale_y_continuous(breaks = c(0,.25, .5, .75, 1), limits = c(0,1)) + 
  ggsave("figures/lss2_eye_head_corr.pdf", units = "in", width = 4, height = 4)

#BOXPLOT SHOWING 3SD RANGE
dsl %>% group_by(dim, task) %>% 
  summarise(ym = mean(corr), lower = ym - 3*sd(corr), upper = ym + 3*sd(corr), ymin = min(corr), ymax = max(corr), x = 1) %>% 
  ggplot(aes(x = interaction(dim, task))) + geom_boxplot(aes(ymin = ymin, lower = lower, middle = ym, upper = upper, ymax = ymax), stat = "identity") +
  geom_point(data = dsl, aes(y = corr, group = interaction(dim, task)))

# LONG FORMAT POSITION TOTAL SD --------------
dsl <-  gather(ds, key = "cond", value = "corr", "walk_gazex_std","walk_gazey_std","search_gazex_std","search_gazey_std")
dsl$dim <- factor(ifelse(is.na(str_extract(dsl$cond,"x")),"y","x"))
dsl$task <- factor(ifelse(is.na(str_extract(dsl$cond,"walk")),"search","walk"),levels = c("walk","search"),labels = c("Walk","Search"))

t.test(ds$walk_gazex_std,ds$search_gazex_std, paired = T)

#Don't need this graph, put them all together
# dsl %>% group_by(dim, task) %>% filter(dim == "x") %>% 
#   summarise(stdev = mean(corr), n = n(), se = sd(corr)/sqrt(n), ymin = stdev - se, ymax = stdev + se) %>%
#   ggplot() + 
#   geom_sina(data = filter(dsl, dim == "x"), aes(y = corr, x = task, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3) +
#   geom_errorbar(aes(x = task, ymin = ymin, ymax = ymax), color = "black", size =1, width = .3, position = position_dodge(.6)) +
#   scale_color_manual(values = cbp1[c(7,6)], name = "Task") + 
#   labs(x = "", y = "Eye + head SD (º)") + 
#   scale_y_continuous(breaks = c(10,20,30,40), limits = c(10,40)) 
#   ggsave("figures/lss2_position_sd_total.pdf", units = "in", width = 4, height = 4)

# GPS DATA  --------------
#SPEED BY TASK
t.test(ds$walk_path_speed, ds$search_path_speed, paired = T)
cor.test(ds$walk_path_speed, ds$search_path_speed)
describe(ds[,c("walk_path_speed", "search_path_speed")], na.rm = T)

  ds %>% gather(key = "task", value = "speed", "walk_path_speed", "search_path_speed") %>% 
  mutate(task = factor(task, levels = c("walk_path_speed", "search_path_speed"), labels = c("Walk", "Search"))) -> dsl
  dsl %>%  group_by(task) %>% 
  summarise(Speed = mean(speed, na.rm = T), n = n(), se = sd(speed, na.rm = T)/sqrt(n), ymin = Speed - se, ymax = Speed + se) %>%
  ggplot() + 
  geom_sina(data = dsl, aes(y = speed, x = task, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3, na.rm = T) +
  geom_errorbar(aes(x = task, ymin = ymin, ymax = ymax), color = "black", size =1, width = .3, position = position_dodge(.6)) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + 
  labs(x = "", y = "Walking speed (m/s)") + 
  scale_y_continuous(breaks = c(.25, .5, .75, 1, 1.25, 1.5), limits = c(.25, 1.5)) + 
  theme(legend.position = "none") -> p1
  #ggsave("figures/lss2_walking_speed.pdf", units = "in", width = 5, height = 4)

#SPEED SD BY TASK
t.test(ds$walk_path_speed_sd, ds$search_path_speed_sd, paired = T)
cor.test(ds$walk_path_speed_sd, ds$search_path_speed_sd)
describe(ds[,c("walk_path_speed_sd", "search_path_speed_sd")], na.rm = T)

ds %>% gather(key = "task", value = "speed", "walk_path_speed_sd", "search_path_speed_sd") %>% 
  mutate(task = factor(task, levels = c("walk_path_speed_sd", "search_path_speed_sd"), labels = c("Walk", "Search"))) -> dsl
dsl %>%  group_by(task) %>% 
  summarise(Speed = mean(speed, na.rm = T), n = n(), se = sd(speed, na.rm = T)/sqrt(n), ymin = Speed - se, ymax = Speed + se) %>%
  ggplot() + 
  geom_sina(data = dsl, aes(y = speed, x = task, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3, na.rm = T) +
  geom_errorbar(aes(x = task, ymin = ymin, ymax = ymax), color = "black", size =1, width = .3, position = position_dodge(.6)) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + 
  labs(x = "", y = "Walking speed SD (m/s)") + 
  scale_y_continuous(breaks = c(0, .25, .5, .75, 1), limits = c(0,1)) + 
  theme(legend.position = "none") -> p2
  #ggsave("figures/lss2_walking_speed_sd.pdf", units = "in", width = 5, height = 4)

#STRAIGHTNESS BY TASK
t.test(ds$walk_straightness, ds$search_straightness, paired = T)
cor.test(ds$walk_straightness, ds$search_straightness)
describe(ds[,c("walk_straightness", "search_straightness")], na.rm = T)
ds %>% gather(key = "task", value = "speed", "walk_straightness", "search_straightness") %>% 
  mutate(task = factor(task, levels = c("walk_straightness", "search_straightness"), labels = c("Walk", "Search"))) -> dsl
dsl %>%  group_by(task) %>% 
  summarise(Speed = mean(speed, na.rm = T), n = n(), se = sd(speed, na.rm = T)/sqrt(n), ymin = Speed - se, ymax = Speed + se) %>%
  ggplot() + 
  geom_sina(data = dsl, aes(y = speed, x = task, color = task), maxwidth = .5, position = position_dodge(.6), alpha = .5, size = 3, na.rm = T) +
  geom_errorbar(aes(x = task, ymin = ymin, ymax = ymax), color = "black", size =1, width = .3, position = position_dodge(.6)) +
  scale_color_manual(values = cbp1[c(7,6)], name = "Task") + 
  labs(x = "", y = "Straightness ratio") + 
  scale_y_continuous(breaks = c(0, 2.5, 5, 7.5), limits = c(0,7.5)) + 
  theme(legend.position = "none") -> p3
  #ggsave("figures/lss2_walking_straightness.pdf", units = "in", width = 5, height = 4)

p1 + p2 + p3
ggsave("figures/lss2_walking_stats_composite.pdf", units = "in", width = 9, height = 4)











# CORRELATIONS WITH SPEED AND STRAIGHTNESS--------------
xvars <- c("walk_posx_std","walk_eyex_std","walk_straightness", "walk_path_speed","walk_path_speed_sd")
(xcorrs <- corr.test(ds[,xvars]))
pairs.panels(ds[,xvars])

xvars <- c("search_posx_std","search_eyex_std","search_straightness", "search_path_speed","search_path_speed_sd")
(xcorrs <- corr.test(ds[,xvars]))
pairs.panels(ds[,xvars])

# COMBINATIONS OF EYE AND HEAD ROTATIONS--------------
describe(ds[,c("walk_bothcent", "walk_eyecent","walk_headcent","walk_nocent_samedir","walk_nocent_oppdir")], na.rm = T)
describe(ds[,c("search_bothcent", "search_eyecent","search_headcent","search_nocent_samedir","search_nocent_oppdir")], na.rm = T)
t.test(ds$walk_bothcent, ds$search_bothcent, paired = T)
t.test(ds$walk_eyecent, ds$search_eyecent, paired = T)
t.test(ds$walk_headcent, ds$search_headcent, paired = T)
t.test(ds$walk_nocent_samedir, ds$search_nocent_samedir, paired = T)
t.test(ds$walk_nocent_oppdir, ds$search_nocent_oppdir, paired = T)

ds %>% gather(key = "cond", value = "prop", "walk_bothcent", "walk_eyecent","walk_headcent","walk_nocent_samedir","walk_nocent_oppdir","search_bothcent", "search_eyecent","search_headcent","search_nocent_samedir","search_nocent_oppdir") -> dsl_prop
  dsl_prop$task <- factor(ifelse(is.na(str_extract(dsl_prop$cond,"search")),"Walk","Search"),levels = c("Walk","Search"))
  dsl_prop$nocent_samedir <- ifelse(is.na(str_extract(dsl_prop$cond,"nocent_samedir")),0,1)
  dsl_prop$nocent_oppdir <- ifelse(is.na(str_extract(dsl_prop$cond,"nocent_oppdir")),0,2)
  dsl_prop$headcent <- ifelse(is.na(str_extract(dsl_prop$cond,"headcent")),0,3)
  dsl_prop$eyecent <- ifelse(is.na(str_extract(dsl_prop$cond,"eyecent")),0,4)
  dsl_prop$bothcent <- ifelse(is.na(str_extract(dsl_prop$cond,"bothcent")),0,5)
  dsl_prop$Movement <- dsl_prop$nocent_samedir + dsl_prop$nocent_oppdir + dsl_prop$headcent + dsl_prop$eyecent + dsl_prop$bothcent 
  dsl_prop$Movement <- factor(dsl_prop$Movement, levels = c(1,2,3,4,5), labels = c("Both moved together","Both moved opposite", "Only eyes", "Only head", "Neither moved"))
  
  dsl_prop %>% group_by(Movement, task) %>% summarise(prop = mean(prop)) %>% 
  ggplot(aes(y = prop, fill = Movement, x = task)) +
    geom_bar(stat = "identity") + 
    geom_label(aes(label = sprintf("%0.1f%%", round(prop*100, digits = 1))),size = 3, position = position_stack(vjust = 0.5), show.legend = F) +
    xlab("") + ylab("Prop. of time") + 
    my_theme 
  
  # LONG FORMAT 2D SPEED --------------
  dsl <-  gather(ds, key = "cond", value = "speed", "walk_head_speed","walk_eye_speed","search_head_speed","search_eye_speed")
  dsl$eyehead <- factor(ifelse(is.na(str_extract(dsl$cond,"eye")),"head","eye"))
  dsl$task <- factor(ifelse(is.na(str_extract(dsl$cond,"walk")),"search","walk"))
  
  res <- lmer(speed ~ eyehead*task + (1|id),data = dsl)
  anova(res)
  contrast(lsmeans(res, pairwise~task|eyehead, adjust="Holm"))
  
  dsl %>% group_by(eyehead, task) %>% 
    summarise(Speed = mean(speed), n = n(), se = sd(speed)/sqrt(n), ymin = Speed - se, ymax = Speed + se) %>% 
    ggplot() + geom_point(data = dsl, aes(y = speed, x = eyehead, color = task), position = position_dodge(.3), size = 2) +
    geom_crossbar(aes(y = Speed, x = eyehead, color = task, ymin = ymin, ymax = ymax), width = .25, position = position_dodge(.3)) + 
    my_theme + ylab("Average 2D speed") + theme(legend.position = "top")
  
  # CORRELATIONS --------------
  xvars <- c("walk_posx_std","walk_eyex_std","search_posx_std","search_eyex_std","walk_posx_speed","walk_eyex_speed","search_posx_speed","search_eyex_speed")
  (xcorrs <- corr.test(ds[,xvars]))
  pairs.panels(ds[,xvars])
  describe(ds[,xvars])
  boxplot(ds[,xvars])
  r.test(n = 29, r12 = xcorrs$r["walk_posx_std", "walk_eyex_std"], 
         r34 = xcorrs$r["search_posx_std", "search_eyex_std"], 
         r13 = xcorrs$r["walk_posx_std", "search_posx_std"],
         r23 = xcorrs$r["walk_eyex_std", "search_posx_std"],
         r14 = xcorrs$r["walk_posx_std", "search_eyex_std"],
         r24 = xcorrs$r["walk_eyex_std", "search_eyex_std"])
  
  dsl <-  gather(ds, key = "cond", value = "std", "walk_posx_std","search_posx_std","walk_posy_std","search_posy_std","walk_eyex_std","search_eyex_std","walk_eyey_std","search_eyey_std")
  dsl$eyehead <- factor(ifelse(is.na(str_extract(dsl$cond,"eye")),"head","eye"))
  dsl$dim <- factor(ifelse(is.na(str_extract(dsl$cond,"x")),"y","x"))
  dsl$task <- factor(ifelse(is.na(str_extract(dsl$cond,"walk")),"search","walk"))
  
  dsl %>% filter(dim == "x") %>% spread(eyehead, std) %>% 
    group_by(id, task) %>% summarise(eye = mean(eye, na.rm =T), head = mean(head, na.rm = T)) %>% 
    ggplot(aes(x = eye, y = head, color = task)) + 
    geom_point(size = 2) + geom_smooth(method = lm, formula = y ~ x, se = F) +
    xlab("Eye horizontal SD") + ylab("Head horizontal SD") +
    xlim(c(10, 21)) + 
    my_theme + theme(legend.position = "top")
  
  
  yvars <- c("walk_posy_std","walk_eyey_std","search_posy_std","search_eyey_std","walk_posy_speed","walk_eyey_speed","search_posy_speed","search_eyey_speed")
  (ycorrs <- corr.test(ds[,yvars]))
  pairs.panels(ds[,yvars])
  describe(ds[,yvars])
  boxplot(ds[,yvars]) #What about 210? No, y values are w/in 3SD
  r.test(n = 29, r12 = ycorrs$r["walk_posy_std", "walk_eyey_std"], 
         r34 = ycorrs$r["search_posy_std", "search_eyey_std"], 
         r13 = ycorrs$r["walk_posy_std", "search_posy_std"],
         r23 = ycorrs$r["walk_eyey_std", "search_posy_std"],
         r14 = ycorrs$r["walk_posy_std", "search_eyey_std"],
         r24 = ycorrs$r["walk_eyey_std", "search_eyey_std"])
  
  dsl <-  gather(ds, key = "cond", value = "std", "walk_posx_std","search_posx_std","walk_posy_std","search_posy_std","walk_eyex_std","search_eyex_std","walk_eyey_std","search_eyey_std")
  dsl$eyehead <- factor(ifelse(is.na(str_extract(dsl$cond,"eye")),"head","eye"))
  dsl$dim <- factor(ifelse(is.na(str_extract(dsl$cond,"x")),"y","x"))
  dsl$task <- factor(ifelse(is.na(str_extract(dsl$cond,"walk")),"search","walk"))
  dsl %>% filter(dim == "y") %>% spread(eyehead, std) %>% 
    group_by(id, task) %>% summarise(eye = mean(eye, na.rm =T), head = mean(head, na.rm = T)) %>% 
    ggplot(aes(x = eye, y = head, color = task)) + 
    geom_point(size = 2) + geom_smooth(method = lm, formula = y ~ x, se = F) +
    xlab("Eye vertical SD") + ylab("Head vertical SD") + 
    my_theme + theme(legend.position = "top")
  
  #Change in eye/head across tasks
  ds$head_change <- ds$search_posx_std - ds$walk_posx_std
  ds$eye_change <- ds$search_eyex_std - ds$walk_eyex_std
  
  describe(ds[,c("head_change", "eye_change")])
  
  ggplot(ds, aes(x = eye_change, y = head_change)) + geom_point(size = 2) +
    xlab("Eye change") + ylab("Head change") + 
    my_theme + theme(legend.position = "top")
  
  #GAZE SPEED CORR - NO IDEA WHAT'S GOING ON HERE
  dsl <-  gather(ds, key = "cond", value = "corr", "walk_gazexspeed_corr","search_gazexspeed_corr")
  dsl$task <- factor(ifelse(is.na(str_extract(dsl$cond,"walk")),"search","walk"))
  
  t.test(ds$walk_gazexspeed_corr,ds$search_gazexspeed_corr, paired = T)
  
  dsl %>% group_by(task) %>% 
    summarise(stdev = mean(corr), n = n(), se = sd(corr)/sqrt(n), ymin = stdev - se, ymax = stdev + se) %>% 
    ggplot() + geom_point(data = dsl, aes(y = corr, x = task), position = position_dodge(.3), size = 2) +
    geom_crossbar(aes(y = stdev, x = task, ymin = ymin, ymax = ymax), width = .25, position = position_dodge(.3)) + 
    my_theme + ylab("Gaze-walking correlation") + theme(legend.position = "top")
  
  # LONG FORMAT SPEED CORR --------------
  #THIS IS POSITION VARIABILITY, NOT SPEED. MISLABELLED
  dsl <-  gather(ds, key = "cond", value = "corr", "walk_eyexspeed_corr","search_eyexspeed_corr","walk_headxspeed_corr","search_headxspeed_corr")
  dsl$eyehead <- factor(ifelse(is.na(str_extract(dsl$cond,"eye")),"head","eye"))
  dsl$task <- factor(ifelse(is.na(str_extract(dsl$cond,"walk")),"search","walk"))
  
  dsl %>% group_by(eyehead, task) %>% 
    summarise(stdev = mean(corr, na.rm = T), n = n(), se = sd(corr, na.rm = T)/sqrt(n), ymin = stdev - se, ymax = stdev + se) %>% 
    ggplot() + geom_point(data = dsl, aes(y = corr, x = eyehead, color = task), position = position_dodge(.3), size = 2, na.rm = T) +
    geom_crossbar(aes(y = stdev, x = eyehead, color = task, ymin = ymin, ymax = ymax), width = .25, position = position_dodge(.3)) + 
    my_theme + ylab("Exploration-speed correlation") + theme(legend.position = "top") + xlab("")
  
  res <- lmer(corr ~ eyehead*task + (1|id),data = dsl)
  anova(res)
  
  t.test(ds$walk_eyexspeed_corr,ds$search_eyexspeed_corr, paired = T)
  t.test(ds$walk_headxspeed_corr,ds$search_headxspeed_corr, paired = T)