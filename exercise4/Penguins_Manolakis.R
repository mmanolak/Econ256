#Penguin Graph
#Michael Manolakis - Econ 256 Data Vis - CRN 86179
#31 Jan 2025

#install.packages("palmerpenguins")
#install.packages("ggthemes")

library("palmerpenguins")
library("ggthemes")
setwd("~/Desktop/R Files/Data Vis/R Studio Files/exercise4")

#glimpse(penguins)

ggplot(data=penguins)

#creation of plot setup
ggplot(
  data=penguins,
  mapping=aes(x=flipper_length_mm, y=body_mass_g))+
#adding in color and shape differences for a given species
geom_point(mapping=aes(color=species,shape=species))+
#puts into place the linear regression, lm, and confidence interval, se
geom_smooth(method='lm',se=TRUE)+
#changing/assigning lables and titles
labs(
  title="Body mass and flipper length",
  subtitle="Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
  x="Flipper length (mm)", y = "Body mass (g)",
  color="Species", shape= "Species"
)+
#changing the color scale to work with those who suffer from colorblindness
scale_color_colorblind()+
annotate('text',x=220,y=2800,label=paste0('R²=',round(summary(lm(
  flipper_length_mm~body_mass_g,data=penguins))$r.squared,4)),size=4,hjust=0)