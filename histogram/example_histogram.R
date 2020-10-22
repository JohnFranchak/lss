pkgs <- c("tidyverse", "ggforce","patchwork")
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

setwd("~/Documents/GitHub/lss/histogram")

# CREATE DATA SET ---------------------------------------------------------------
search <- read_csv("221_search.csv", col_names = c("Eye","Head"),na = "NaN")
walk <- read_csv("221_walk.csv", col_names = c("Eye","Head"), na = "NaN")

search$Task <- "Search"
walk$Task <- "Walk"

ds <- rbind(search, walk)
ds$Task <- factor(ds$Task)
ds$Gaze <- ds$Eye + ds$Head

style <-   list(
  geom_density(),
  scale_color_manual(values = cbp1[c(6,7)], name = "Task", guide = NULL),
  labs(x = "Horizontal Position (º)", y = ""),
  scale_x_continuous(breaks = seq(-100, 100, 25), limits = c(-100, 100)),
  scale_y_continuous(breaks = NULL))

p1 <- ggplot(ds, aes(x = Eye, color = task)) + style
p2 <- ggplot(ds, aes(x = Head, color = task)) + style
p3 <- ggplot(ds, aes(x = Gaze, color = task)) + style

p1 / p2 / p3
ggsave("eye_head_density.pdf", units = "in", width = 5, height = 12)
  