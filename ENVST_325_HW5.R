install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("olsrr")
install.packages("PerformanceAnalytics")
install.packages("forecast")


library(dplyr)
library(ggplot2)
library(lubridate)
library(olsrr)
library(PerformanceAnalytics)
library(forecast)

ETdat <- read.csv("/cloud/project/activity06/ETdata.csv")

ghg <- read.csv("/cloud/project/activity05/Deemer_GHG_Data.csv")



#--Q1--
ghg$CO2_transformation <- 1/(ghg$co2 + 1000)

ghg$log.ch4 <- log(ghg$ch4 +1)
ghg$log.DIP <- log(ghg$DIP +1)
ghg$log.age <- log(ghg$age)
ghg$log.precip <- log(ghg$precipitation)
ghg$log.SA <- log(ghg$surface.area)

#dependent variable ~ independent variable
reg_co2_model <- lm( CO2_transformation ~ airTemp + log.age + mean.depth + log.DIP + 
                  log.precip + log.SA, data = ghg)
summary(reg_co2_model)
#get a regression table
table(reg_co2_model)
regTable <- summary(reg_co2_model)$coefficients
#Write to file then export
write.csv(regTable, "/cloud/project/reg_out.csv")

reg_co2.step <- ols_step_forward_aic(reg_co2_model)
reg_co2.step

#--Q2--
ETall <- ETdat %>%
  group_by(date, crop) %>%
  summarise(ET.in = mean(Ensemble.ET))

Pistachios <- ETall %>%
  filter(crop == "Pistachios")
Pistachio_ts <- na.omit(ts(Pistachios$ET.in,
                           start = c(2016, 1),
                           frequency = 12))

Pistachio_decompose <- decompose(Pistachio_ts)
plot(Pistachio_decompose)

Pistachio_decompose

Almonds <- ETall %>%
  filter(crop == "Almonds")
Almond_ts <- na.omit(ts(Almonds$ET.in,
                           start = c(2016, 1),
                           frequency = 12))

Almond_decompose <- decompose(Almond_ts)
plot(Almond_decompose)

Almond_decompose

Fallow <- ETall %>%
  filter(crop == "Fallow/Idle Cropland")
Fallow_ts <- na.omit(ts(Fallow$ET.in,
                           start = c(2016, 1),
                           frequency = 12))

Fallow_decompose <- decompose(Fallow_ts)
plot(Fallow_decompose)

Fallow_decompose

Corn <- ETall %>%
  filter(crop == "Corn")
Corn_ts <- na.omit(ts(Corn$ET.in,
                        start = c(2016, 1),
                        frequency = 12))

Corn_decompose <- decompose(Corn_ts)
plot(Corn_decompose)

Corn_decompose

Grapes <- ETall %>%
  filter(crop == "Grapes (Table/Raisin)")
Grapes_ts <- na.omit(ts(Grapes$ET.in,
                      start = c(2016, 1),
                      frequency = 12))

Grapes_decompose <- decompose(Grapes_ts)
plot(Grapes_decompose)

Grapes_decompose
#--Q3--
acf(Pistachio_ts, lag.max = 24)

pacf(Pistachio_ts, lag.max = 24)

Pistachio_model4 <- arima(Pistachio_ts , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format

newPistachio <- forecast(Pistachio_model4)

newPistachioF <- data.frame(newPistachio)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newPistachioF$dateF <- ymd(paste(years,"/",month,"/",1))
# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = Pistachios, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(Pistachios$date[1]),newPistachioF$dateF[24])+  # Plotting original data
  geom_line(data = newPistachioF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newPistachioF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")

#Corn AR model
Corn_model4 <- arima(Corn_ts , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
Corn_model4

newCorn <- forecast(Corn_model4)
newCorn

newCornF <- data.frame(newCorn)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newCornF$dateF <- ymd(paste(years,"/",month,"/",1))
# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = Corn, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(Corn$date[1]),newCornF$dateF[24])+  # Plotting original data
  geom_line(data = newCornF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newCornF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")


#Fallow AR model
Fallow_model4 <- arima(Fallow_ts , # data 
                order = c(4,0,0)) # first number is AR order all other numbers get a 0 to keep AR format
Fallow_model4
newFallow <- forecast(Fallow_model4)
newFallow

newFallowF <- data.frame(newFallow)

# set up dates
years <- c(rep(2021,4),rep(2022,12), rep(2023,8))
month <- c(seq(9,12),seq(1,12), seq(1,8))
newFallowF$dateF <- ymd(paste(years,"/",month,"/",1))
# make a plot with data and predictions including a prediction interval
ggplot() +
  geom_line(data = Fallow, aes(x = ymd(date), y = ET.in))+
  xlim(ymd(Fallow$date[1]),newFallowF$dateF[24])+  # Plotting original data
  geom_line(data = newFallowF, aes(x = dateF, y = Point.Forecast),
            col="red") +  # Plotting model forecasts
  geom_ribbon(data=newFallowF, 
              aes(x=dateF,ymin=Lo.95,
                  ymax=Hi.95), fill=rgb(0.5,0.5,0.5,0.5))+ # uncertainty interval
  theme_classic()+
  labs(x="year", y="Evapotranspiration (in)")
