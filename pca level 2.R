#bring in the data table downloaded from QIIME viewer (taxonomy.qzv)
taxlevel2<-read.csv("level-2.csv")
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
tl2<-taxlevel2
require(plyr)
#compute total abundance and add to dataset
#check out that it worked and also get an idea of what the spread of the data is
summary(tl2)
#made separate samples of sick v healthy
healthysamp<-tl2[1:67,]
naivesamp<-tl2[1:47,]
exposedsamp<-tl2[48:67,]
symptsamp<-tl2[68:85,]
#summary for all data
summary(tl2)
#descriptive stats for healthy
summary(healthysamp)
#find prop abundance:

x<-rep(sum(tl2[,2:42]))

#pca for the proportional abundance of top 10 groups

tl2.pca<-prcomp(tl2[,c(2:12)], center=TRUE, scale.=TRUE)
#look at the table output
summary(tl2.pca)
str(tl2.pca)
#plot the PCAs, by health status site and site/health status just to check it out
#health status
pcaPlot<-(ggbiplot(tl2.pca, obs.scale = 1, var.scale = 1, 
                   groups=tl2[,42],
                   ellipse = TRUE) +
            scale_color_discrete(name = '') +
            theme(legend.direction = 'horizontal', legend.position = 'top'))
pcaPlot

