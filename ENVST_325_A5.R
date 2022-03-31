install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("olsrr")
install.packages("PerformanceAnalytics")

library(dplyr)
library(ggplot2)
library(lubridate)
library(olsrr)
library(PerformanceAnalytics)

ghg <- read.csv("/cloud/project/activity05/Deemer_GHG_Data.csv")

ghg$log.ch4 <- log(ghg$ch4 +1)

ghg$log.DIP <- log(ghg$DIP +1)
ghg$log.age <- log(ghg$age)
ghg$log.precip <- log(ghg$precipitation)
ghg$log.SA <- log(ghg$surface.area)
ghg$BorealV <- ifelse(ghg$Region == "Boreal", 1, 0)

#dependent variable ~ independent variable
mod.full <- lm( log.ch4 ~ airTemp + log.age + mean.depth + log.DIP + 
                  log.precip + BorealV + log.SA + mean.depth, data = ghg)

summary(mod.full)

full.step <- ols_step_forward_aic(mod.full)
full.step
