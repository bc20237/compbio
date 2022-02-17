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

tl5<-taxlevel5
require(plyr)

print(names(tl5[,c(2:38)]))
tl5.pca<-prcomp(tl5[,c(2:38)], center=TRUE, scale.=TRUE)
#look at the table output
summary(tl5.pca)
str(tl5.pca)
colrs <- adjustcolor( c("green","blue","red"), alpha=.6)

pcaPlot<-(ggbiplot(tl5.pca, obs.scale = 2, var.scale = 2, 
         groups=tl5[,321],
   ellipse = TRUE) + 
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')) + ggtitle('C') + theme(plot.title = element_text(size=20))+ theme(axis.title.x = element_text(size=20))+ theme(legend.text = element_text(size=30)) + theme(axis.title.y = element_text(size=20))+ scale_color_manual(values = c("#FFA500","#0000FF","#FF00FF"))+ #your colors here
  theme_classic()
pcaPlot


