#bring in the data table downloaded from QIIME viewer (taxonomy.qzv)
taxlevel4<-read.csv("level-4.csv")
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
tl4<-taxlevel4
require(plyr)
#compute total abundance and add to dataset
#check out that it worked and also get an idea of what the spread of the data is
summary(tl4)
#made separate samples of sick v healthy
healthysamp<-tl4[1:67,]
naivesamp<-tl4[1:47,]
exposedsamp<-tl4[48:67,]
symptsamp<-tl4[68:85,]
#summary for all data
summary(tl4)
#descriptive stats for healthy
summary(healthysamp)
#pca for the proportional abundance of top 10 groups

tl4.pca<-prcomp(tl4[,c(2:40)], center=TRUE, scale.=TRUE)
#look at the table output
summary(tl4.pca)
str(tl4.pca)
#plot the PCAs, by health status site and site/health status just to check it out
#health status
pcaPlot<-(ggbiplot(tl4.pca, obs.scale = 1, var.scale = 1, 
                   groups=tl4[,184],
                   ellipse = TRUE) +
            scale_color_discrete(name = '') +
            theme(legend.direction = 'horizontal', legend.position = 'top'))
pcaPlot

