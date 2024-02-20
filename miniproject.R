# Install and load the 'sf' package if you haven't already
rm(list=ls())

getwd()
# Set the path to your 
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

# Read the file
?read.csv
seal <- read.csv('Desktop/Alaska Pribilof Island northern fur seal pup weight 1957-present.csv', 
                 header=T)
seal <- subset(seal, seal$sex =='M'|seal$sex =='F')
seal <- seal[!is.na(seal$length), ]
seal <- subset(seal, seal$plen >= 0 )

boxplot(seal$wgt)
stats <- boxplot.stats(seal$wgt)
boxplot(seal$plen)
stats <- boxplot.stats(seal$plen)
stats

seal <-  subset(seal,seal$wgt >= 6.0 ) #remove newborns 
seal <-  subset(seal,seal$wgt <= 15 ) #remove juveniles
seal <-  subset(seal,seal$length >= 60 ) #same for length
names(seal)[names(seal) == "length"] <- "length"

ggplot(seal, aes(x = wgt, fill = sex)) +
  geom_histogram( alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_minimal()

ggplot(seal, aes(x = length, fill = sex)) +
  geom_histogram( alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_minimal()

par(mfrow = c(2, 2))


names(seal)[names(seal) == "yr"] <- "Year"

temp <- read.csv('Desktop/ala.temp.csv')
rain <- read.csv('Desktop/ala.rain.csv')
# add temperature and rainfall data '
view
#merge the data
seal <- merge(seal, temp, by = 'Year' , all.x = TRUE)
seal <- merge(seal, rain, by = 'Year' , all.x = TRUE) 
names(seal)[names(seal) == "Annual.Average.Temperature..degF."] <- "mean.temp"
names(seal)[names(seal) == "Annual.Total.Precipitation..in."] <- "precipitation"
seal$temp<- scale(seal$mean.temp)
seal$pre<- scale(seal$precipitation)

#find the correlation bewteen weight and size, decide which size indicator to use
pairs.panels(seal[, c(10,16,21,22)] ) #0.71: high correlation. Pick length

#how temperature and rainfall affect mass moderated by sex
sexall <- lm(wgt ~ sex * temp * pre , data = seal)
summary(sexall)
#model selection: remove the factor has no significance
one <- lm(wgt~ sex+ temp + pre + temp:sex  + sex:pre, data = seal)
summary (one)

anova(sexall, one)#compare whether removing the interactive factor has an effect:

two <- lm ( wgt~ sex+ temp + pre + sex:temp, data = seal) 
summary(two)
anova(one, two)

summary(three)
#sex's variation is captured by interaction, althohugh it does not appears 
#significant. it need to be remained in the model.
#the interaction of sex and temp is significant, therefore sex has an effect
#so sex is kept 

plot(three)
tab_model(two)

interaction.plot(seal$wgt, seal$temp,
                 trace.factor = seal$sex,
                xlab = "Mean Temperature", ylab = "Weight", type = "b", 
                 legend = TRUE, col = c("#69b3a2", "#404080") )

plot_model(three, show.values = TRUE, show.intercept = TRUE)

library('sjPlot')
?plot_model
plot_model(three, show.values = TRUE, show.intercept = TRUE)
plot_model(three,type="pred",terms = c('mean.temp', 'sex'))


