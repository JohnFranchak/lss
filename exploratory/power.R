library(pwr)
library(WebPower)

#STUDY 1
cohen.ES(test = "t", size = "medium")
pwr.t.test(d = 0.5, power = 0.95, type = "paired", alternative = "two.sided")

#STUDY 2
#spread d = .54 = cohen's f of 0.270
#speed d = 1.3

wp.rmanova(ng = 2, nm = 2, f = .27, power = .8, type = 2)
