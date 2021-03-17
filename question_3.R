# question 3
library(tidyverse)
library(haven)

mroz <- read_dta("C:\\Users\\Lui Yu Sen\\Google Drive\\NTU_study materials\\Economics\\HE3021 Intermediate Econometrics\\Week 8\\HE3021-Week-8-Tutorial-6\\rawdata\\MROZ.dta")

model <- glm(inlf ~ nwifeinc + educ + kidslt6 + age + exper,
             family = binomial(link = "probit"),
             data = mroz)
summary(model)