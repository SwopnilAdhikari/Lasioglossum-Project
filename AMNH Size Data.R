install.packages("tidyverse")
install.packages("here")
library(readxl)
library(tidyverse)
library(here)
amnhdata <- read_excel(here("data", "amnhsize.xlsx"))
############################################################
massvsmmat <- ggplot(data = amnhdata, aes(x = meanmat, y = mass)) 

massvsmmat + geom_point() + geom_smooth(method = "lm") + 
  annotate("text", x = 3.4, y = .08, label = "P-Value = 0.0024") + labs(
  x = "Mean Annual Temperature(C)",
  y = "Mass(g)",
  title = "Mass(g) vs Mean Annual Temperature"
)
lmMassvsmmat <- lm(mass ~ meanmat, data = amnhdata) 
summary(lmMassvsmmat)
###########################################################
massvsyear <- ggplot(data = amnhdata, aes(x = year, y = mass))
massvsyear + geom_point() + geom_smooth(method = "lm") +
annotate("text", x = 1918, y = 0.08, label = "P-Value = 3.358e-05") + labs(
  x = "Year",
  y = "Mass(g)",
  title = "Mass vs Year Collected"
)
lmMassvsyear <- lm(mass ~ year, data = amnhdata)
summary(lmMassvsyear)
###########################################################
massvsmaxt <- ggplot(data = amnhdata, aes(x = maxt, y = mass))
massvsmaxt + geom_point() + geom_smooth(method = "lm") + 
  annotate("text", x = 15, y = 0.09, label = "P-Value = 3.05e-04") + labs(
  x = "Temperature(C)",
  y = "Mass(g)",
  title = "Mass vs Max Temperature in Month of Collection"
)
lmMassvsmaxt <- lm(mass ~ maxt, data = amnhdata)
summary(lmMassvsmaxt)
###########################################################
massvsmonth <- ggplot(data = amnhdata, aes(x = month, y = mass))
massvsmonth + geom_point() + geom_smooth(method = "lm") + 
  annotate("text", x = 3.2, y = 0.09, label = "P-Value = 0.21") + labs(
  x = "Month",
  y = "Mass(g)",
  title = "Correlation of Mass to Month of Collection"
)
lmMassvsmonth <- lm(mass ~ month, data = amnhdata)
summary(lmMassvsmonth)
###########################################################
itxthoraxvsmmat <- ggplot(data = amnhdata, aes(x = meanmat, y = itxthorax))
itxthoraxvsmmat + geom_point() + geom_smooth(method = "lm") + 
  annotate("text", x = 3, y = 0.036, label = "P-Value = 0.1585") + labs(
  x = "Temperature(C)",
  y = "IT x Thorax(cm)",
  title = "IT x Thorax vs Mean Annual Temperature"
)
lmItxthoraxvsmmat <- lm(itxthorax ~ meanmat, data = amnhdata)
summary(lmItxthoraxvsmmat)
##########################################################
massvsmatybc <- ggplot(data = amnhdata, aes(x = matybc, y = mass))
massvsmatybc + geom_point() + geom_smooth(method = "lm") + 
  annotate("text", x = 3.5, y = 0.09, label = "P-Value = 2.8e-02") + labs(
  x = "Temperature(C)",
  y = "Mass(g)",
  title = "Mass vs Mean Annual Temperature(Year of Birth)"
)
lmMassvsmatybc <- lm(mass ~ matybc, data = amnhdata)
summary(lmMassvsmatybc)
##########################################################
itxthoraxvsmass <- ggplot(data = amnhdata, aes(x = itxthorax, y = mass))
itxthoraxvsmass + geom_point() + geom_smooth(method = "lm") + 
  annotate("text", x = 0.018, y = .09, label = "P-Value = 0.008181")+ labs(
  x = "IT x Thorax(cm)",
  y = "Mass(g)",
  title = "IT x Thorax vs Mass"
)
lmitxthoraxvsmass <- lm(itxthorax ~ mass, data = amnhdata)
summary(lmitxthoraxvsmass) 
############################################################
head(amnhdata, 5)
hist(amnhdata$mass)
lines(density(amnhdata$mass), col = 'green', lwd = 1)
############################################################
qqnorm(amnhdata$mass, pch = 1, frame = FALSE) 
qqline(amnhdata$mass, col = "green", lwd = 2)
###########################################################
logData <- log10(amnhdata$mass)
hist(logData)
lines(density(logData), col = "green", lwd = 2)
############################################################
mod1 <- lm(amnhdata$mass ~ amnhdata$meanmat)
plot(mod1)

