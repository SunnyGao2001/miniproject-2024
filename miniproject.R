
rm(list=ls())
getwd()
require(psych)
require(sjPlot)
setwd("Users/sunny/Downloads")
library('tidyverse')
library('dplyr')
library(ggplot2)
install.packages('ggthemes')
library(ggthemes) 
library('lme4')
library('Matrix')
library('lmtest')
install.packages('car')
library(car)

# Read the data file
?read.csv
seal <- read.csv('Desktop/Alaska Pribilof Island northern fur seal pup weight 1957-present.csv', 
                 header=T)
seal <- subset(seal, seal$sex =='M'|seal$sex =='F')
seal <- seal[!is.na(seal$length), ] #remove missing values
seal <- subset(seal, seal$plen >= 0 ) #remove data error

#data illustration/check outliners
boxplot(seal$wgt)
stats <- boxplot.stats(seal$wgt)
boxplot(seal$plen)
stats <- boxplot.stats(seal$plen)

seal <-  subset(seal,seal$wgt >= 6.0 ) #remove newborns 
seal <-  subset(seal,seal$wgt <= 15 ) #remove older juveniles
seal <-  subset(seal,seal$length >= 60 ) #same for length
names(seal)[names(seal) == "plen"] <- "length"

#data visulisation
ggplot(seal, aes(x = wgt, fill = sex)) +
  geom_histogram( alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_minimal()

ggplot(seal, aes(x = length, fill = sex)) +
  geom_histogram( alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_minimal()

#change the names of column
names(seal)[names(seal) == "yr"] <- "Year"

temp <- read.csv('Desktop/ala.temp.csv')
rain <- read.csv('Desktop/ala.rain.csv')
# add temperature and rainfall data '
view
#merge the climate data on to seal data 
seal <- merge(seal, temp, by = 'Year' , all.x = TRUE)
seal <- merge(seal, rain, by = 'Year' , all.x = TRUE) 
names(seal)[names(seal) == "Annual.Average.Temperature..degF."] <- "mean.temp"
names(seal)[names(seal) == "Annual.Total.Precipitation..in."] <- "precipitation"
seal$temp<- scale(seal$mean.temp)
seal$pre<- scale(seal$precipitation)
#standardize the temperature and rain, so model prediction can be carried

#find the correlation between weight and size, decide which size indicator to use
pairs.panels(seal[, c(10,16)] ) #0.67: high correlation. Pick weight
pairs.panels(seal[, c(21,22)] ) #sllight correlation, both are kept

#how temperature and rainfall affect mass moderated by sex with all interactions
sexall <- lm(wgt ~ sex * temp * pre , data = seal)
summary(sexall)

#model selection: remove the factor has no significance
one <- lm(wgt~ sex+ temp + pre + temp:sex  + sex:pre, data = seal)
summary (one)
anova(sexall, one)#compare whether removing the interactive factor has an effect:no effect

#repeat
two <- lm ( wgt~ sex+ temp + pre + sex:temp, data = seal) 
summary(two)
anova(one, two)

summary(two)

par(mfrow = c(2, 2))
plot(two)
tab_model(two)

#plot the results 
plot_model(two, show.values = TRUE, show.intercept = TRUE)

library('sjPlot')
#visualize the relationship between x and y
plot_model(two, show.values = TRUE, show.intercept = TRUE)
plot_model(two, type="eff",terms = c('temp', 'sex'))
plot_model(two, type="eff",terms = c('pre', 'sex'))

