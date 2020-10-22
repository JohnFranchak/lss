library(tidyverse)
library(rstatix)

#LSS 1

incl <- read_csv("summary_stats_LSS1.csv", na = "NaN")
incl <- filter(incl, id != 35) #Bad posttest eye tracking accuracy

ds <- read_csv("LSS Calibration Data - LSS1 Final.csv", na = "NaN")
ds <- mutate(ds, id = factor(Participant))

#Remove participants who weren't in the analysis
ds <- filter(ds, id %in% incl$id)

#Wide to long
dsl <-  gather(ds, key = "check", value = "error", "error1","error2","error3","error4","error5","error1_pst","error2_pst","error3_pst","error4_pst","error5_pst")
dsl$phase <- factor(ifelse(is.na(str_extract(dsl$check,"pst")),"search","post"))

#Error summary stats by participant
dsl  %>% group_by(id) %>% summarise(error = mean(error, na.rm = T), .groups = "drop_last") %>% get_summary_stats(error, type = "common")

#Check error by phase
dsl %>% group_by(phase,id) %>% summarise(error = mean(error, na.rm = T)) -> ds_sum
t.test(error ~ phase, data = ds_sum, paired = T)
dsl  %>% group_by(phase) %>% get_summary_stats(error, type = "mean_sd")

#LSS 2

incl <- read_csv("summary_stats_mot_corrected.csv", na = "NaN")
incl <- filter(incl, id != 212 & id != 203) 

ds <- read_csv("LSS Calibration Data - LSS2 Final.csv", na = "NaN")
ds <- mutate(ds, id = factor(Participant))

#Remove participants who weren't in the analysis
ds <- filter(ds, id %in% incl$id)

#Wide to long
dsl <-  gather(ds, key = "check", value = "error", "error1","error2","error3","error4","error5","error1_pst","error2_pst","error3_pst","error4_pst","error5_pst")
dsl$phase <- factor(ifelse(is.na(str_extract(dsl$check,"pst")),"search","post"))

#Error summary stats by participant
dsl  %>% group_by(id) %>% summarise(error = mean(error, na.rm = T)) %>% get_summary_stats(error, type = "common")

#Check error by phase
dsl %>% group_by(phase,id) %>% summarise(error = mean(error, na.rm = T)) %>% filter(id != 230) -> ds_sum
t.test(error ~ phase, data = ds_sum, paired = T)
dsl  %>% group_by(phase) %>% filter(id != 230) %>% get_summary_stats(error, type = "mean_sd")
  