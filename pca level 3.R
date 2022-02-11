#bring in the data table downloaded from QIIME viewer (taxonomy.qzv)
taxlevel3<-read.csv("level-3.csv")
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
tl3<-taxlevel3
require(plyr)
#compute total abundance and add to dataset
#check out that it worked and also get an idea of what the spread of the data is
summary(tl3)
#made separate samples of sick v healthy
healthysamp<-tl3[1:67,]
naivesamp<-tl3[1:47,]
exposedsamp<-tl3[48:67,]
symptsamp<-tl3[68:85,]
#summary for all data
summary(tl3)
#descriptive stats for healthy
summary(healthysamp)
#pca for the proportional abundance of top 10 groups
print(tl3[,c(2:86)])
tl3.pca<-prcomp(tl3[,c(2:30)], center=TRUE, scale.=TRUE)
#look at the table output
summary(tl3.pca)
str(tl3.pca)
#plot the PCAs, by health status site and site/health status just to check it out
#health status
pcaPlot<-(ggbiplot(tl3.pca, obs.scale = 1, var.scale = 1, 
                   groups=tl3[,93],
                   ellipse = TRUE) +
            scale_color_discrete(name = '') +
            theme(legend.direction = 'horizontal', legend.position = 'top'))
pcaPlot

