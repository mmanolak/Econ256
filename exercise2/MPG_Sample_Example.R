#Load necessary libraries
library(tidyverse,ggplot2)

myobject<-mpg

#Load the mpg dataset
data('mpg')

#Create a new column for the average of city (cty) and highway (hwy) MPG
mpg<-mpg %>%
  mutate(average_mpg=(cty+hwy)/2)

#View the updated dataset
head(mpg)

#city mpg vs engine displacement
ggplot(data=myobject,mapping=aes(x=cty,y=displ,color=as.factor(cyl)))+
  geom_point()+
  #adding regrssion line with 95% confidence interval
  geom_smooth(method='lm',se=TRUE,color='black', fullrange=FALSE)+  
  xlab('City MPG')+
  ylab('Engine Displacement')+
  ggtitle('City MPG vs Displacement')+
  theme_linedraw()+
  #limit y axis to start at 1.5 liters
  scale_y_continuous(limits = c(1.5, NA))+
  #adding in r^2 line
  annotate('text',x=27, y=6,label=paste0('R² = ',
    round(summary(lm(displ~cty,data=myobject))$r.squared,4)),size=5,hjust=0)


#highway mpg vs engine displacement alternative
ggplot(myobject,aes(x=hwy,y=displ,color=as.factor(cyl)))+
  geom_point()+
  geom_smooth(method='lm',se=TRUE,color='black',fullrange=FALSE)+
  xlab('Highway MPG')+
  ylab('Engine Displacement')+
  ggtitle('Highway MPG vs Displacement')+
  theme_linedraw()+
  scale_y_continuous(limits = c(1.5, NA))+
  annotate('text',x=35,y=6,label=paste0('R² = ',
  round(summary(lm(displ~cty,data=myobject))$r.squared,4)),size=5,hjust=0)

#mutating myobject into mpg to find average mpg
mpg <- mpg %>%
  mutate(avmpg=rowMeans(select(.,cty,hwy),na.rm=TRUE))

#graphing the average mpg with comparison to displacement
ggplot(mpg,aes(x=avmpg,y=displ,color=as.factor(cyl)))+
  geom_point()+
  geom_smooth(method='lm',se=TRUE,color='maroon', fullrange=FALSE)+
  xlab('Average MPG')+
  ylab('Engine Displacement')+
  ggtitle('Average MPG vs Displacement')+
  theme_linedraw()+
  scale_y_continuous(limits = c(1.5, NA))+
  annotate('text',x=30, y=6,label=paste0('R² = ',
  round(summary(lm(displ~cty,data=myobject))$r.squared,4)),size=5,hjust=0)
