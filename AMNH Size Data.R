install.packages("tidyverse")
install.packages("here")
library(readxl)
library(tidyverse)
library(here)
amnhdata <- read_excel(here("data", "amnhsize.xlsx"))
############################################################
massvsmmat <- ggplot(data = amnhdata, aes(x = meanmat, y = mass)) 

massvsmmat + geom_point() + geom_smooth(method = "lm") + labs(
  x = "Mean Annual Temperature(C)",
  y = "Mass(g)",
  title = "Mass(g) vs Mean Annual Temperature"
)
lmMassvsmmat <- lm(mass ~ meanmat, data = amnhdata) 
summary(lmMassvsmmat)
###########################################################
massvsyear <- ggplot(data = amnhdata, aes(x = year, y = mass))
massvsyear + geom_point() + geom_smooth(method = "lm") + labs(
  x = "Year",
  y = "Mass(g)",
  title = "Mass vs Year Collected"
)
lmMassvsyear <- lm(mass ~ year, data = amnhdata)
summary(lmMassvsyear)
###########################################################
massvsmaxt <- ggplot(data = amnhdata, aes(x = maxt, y = mass))
massvsmaxt + geom_point() + geom_smooth(method = "lm") + labs(
  x = "Temperature(C)",
  y = "Mass(g)",
  title = "Mass vs Max Temperature in Month of Collection"
)
lmMassvsmaxt <- lm(mass ~ maxt, data = amnhdata)
summary(lmMassvsmaxt)
###########################################################
massvsmonth <- ggplot(data = amnhdata, aes(x = month, y = mass))
massvsmonth + geom_point() + geom_smooth(method = "lm") + labs(
  x = "Month",
  y = "Mass(g)",
  title = "Correlation of Mass to Month of Collection"
)
lmMassvsmonth <- lm(mass ~ month, data = amnhdata)
summary(lmMassvsmonth)
###########################################################
itxthoraxvsmmat <- ggplot(data = amnhdata, aes(x = meanmat, y = itxthorax))
itxthoraxvsmmat + geom_point() + geom_smooth(method = "lm") + labs(
  x = "Temperature(C)",
  y = "IT x Thorax(cm)",
  title = "IT x Thorax vs Mean Annual Temperature"
)
lmItxthoraxvsmmat <- lm(itxthorax ~ meanmat, data = amnhdata)
summary(lmItxthoraxvsmmat)
##########################################################
massvsmatybc <- ggplot(data = amnhdata, aes(x = matybc, y = mass))
massvsmatybc + geom_point() + geom_smooth(method = "lm") + labs(
  x = "Temperature(C)",
  y = "Mass(g)",
  title = "Mass vs Mean Annual Temperature(Year of Birth)"
)
lmMassvsmatybc <- lm(mass ~ matybc, data = amnhdata)
summary(lmMassvsmatybc)
##########################################################
itxthoraxvsmatybc <- ggplot(data = amnhdata, aes(x = itxthorax, y = matybc))
itxthoraxvsmatybc + geom_point() + geom_smooth(method = "lm") + labs(
  x = "IT x Thorax(cm)",
  y = "Temperature(C)",
  title = "IT x Thorax vs Mean Annual Temperature(Year of Birth)"
)
lmitxthoraxvsmatybc <- lm(itxthorax ~ matybc, data = amnhdata)
summary(lmitxthoraxvsmatybc) 

##GITHUB COMMIT###
############################################################

