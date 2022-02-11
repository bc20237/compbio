#bring in the data table downloaded from QIIME viewer (taxonomy.qzv)
taxlevel5<-read.csv("level-5.csv")
#install some stuff I didnt have bc I havent used R in like 4 years
#install.packages("ggplot2")
#install.packages("devtools")
library(devtools)
library(githubinstall)
#githubinstall("vqv/ggbiplot")
library(tidyverse)
library(ggplot2)
library(ggbiplot)
#changed the name to something shorter albeit less descriptive
tl5<-taxlevel5
require(plyr)

print(names(tl5[,c(2:53)]))
tl5.pca<-prcomp(tl5[,c(2:53)], center=TRUE, scale.=TRUE)
#look at the table output
summary(tl5.pca)
str(tl5.pca)
#plot the PCAs, by health status site and site/health status just to check it out
#health status
pcaPlot<-(ggbiplot(tl5.pca, obs.scale = 1, var.scale = 1, 
         groups=tl5[,321],
   ellipse = TRUE) + 
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top'))
pcaPlot

