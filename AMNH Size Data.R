install.packages("tidyverse")
install.packages("here")
library(readxl)
library(tidyverse)
library(here)
amnhdata <- read_excel(here("data", "amnhsize.xlsx"))
############################################################
massvsmmat <- ggplot(data = amnhdata, aes(x = mass, y = meanmat)) 

massvsmmat + geom_point() + labs(
  x = "Mass(g)",
  y = "Mean Annual Temperature(C)",
  title = "Mass(g) vs Mean Annual Temperature"
)
###########################################################
massvsyear <- ggplot(data = amnhdata, aes(x = year, y = mass))
massvsyear + geom_point() + labs(
  x = "Year",
  y = "Mass(g)",
  title = "Mass vs Year Collected"
)
###########################################################
massvsmaxt <- ggplot(data = amnhdata, aes(x = mass, y = maxt))
massvsmaxt + geom_point() + labs(
  x = "Mass(g)",
  y = "Temperature(C)",
  title = "Mass vs Max Temperature in Month of Collection"
)
###########################################################
massvsmonth <- ggplot(data = amnhdata, aes(x = month, y = mass))
massvsmonth + geom_point() + labs(
  x = "Month",
  y = "Mass(g)",
  title = "Correlation of Mass to Month of Collection"
)
###########################################################
itxthoraxvsmmat <- ggplot(data = amnhdata, aes(x = meanmat, y = itxthorax))
itxthoraxvsmmat + geom_point() + labs(
  x = "Temperature(C)",
  y = "IT x Thorax(cm)",
  title = "IT x Thorax vs Mean Annual Temperature"
)
##########################################################
massvsmatybc <- ggplot(data = amnhdata, aes(x = matybc, y = mass))
massvsmatybc + geom_point() + labs(
  x = "Temperature(C)",
  y = "Mass(g)",
  title = "Mass vs Mean Annual Temperature(Year of Birth)"
)
##########################################################
itxthoraxvsmatybc <- ggplot(data = amnhdata, aes(x = itxthorax, y = matybc))
itxthoraxvsmatybc + geom_point() + labs(
  x = "IT x Thorax(cm)",
  y = "Temperature(C)",
  title = "IT x Thorax vs Mean Annual Temperature(Year of Birth)"
)
 ##GITHUB COMMIT###
