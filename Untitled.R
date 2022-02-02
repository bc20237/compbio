install.packages(tinytex)
library(ggplot2)
setwd("/Users/Desktop")
test<-read.csv("testigraph.csv")
#SSfilt<-filter(SS, r>=0.35 | r<=-0.35)
library(psych)
library(ggplot2)
library(stringr)
# trying to make this table
SS2<-data.matrix(SS)
SS3<-SS
SS3$microbe1<-sub(".*f__", "", SS3$feature1) 
SS3$microbe2<-sub(".*f__", "", SS3$feature2)
SS3[SS3 == ""] <- NA 
SSda<-SS3[((SS3$microbe1=="Vibrionaceae" | SS3$microbe2=="Vibrionaceae" | SS3$microbe1=="Spirochaetaceae" | SS3$microbe2=="Spirochaetaceae" |SS3$microbe1=="Flammeovirgaceae"|SS3$microbe2=="Flammeovirgaceae" |SS3$microbe1=="Fusobacteriaceae"|SS3$microbe2=="Fusobacteriaceae"|SS3$microbe1=="Pseudoalteromonadaceae"|SS3$microbe2=="Pseudoalteromonadaceae"|SS3$microbe1=="JTB215"|SS3$microbe2=="JTB215"|SS3$microbe1=="Moritellaceae"|SS3$microbe2=="Moritellaceae"|SS3$microbe1=="Oceanospirillaceae"|SS3$microbe2=="Oceanospirillaceae"|SS3$microbe1=="[Acidaminobacteraceae]"|SS3$microbe2=="[Acidaminobacteraceae]")),]
#using ss3 to try to clean up feat1 and feat2

p2<- ggplot(data=SSda,mapping=aes(microbe1, microbe2, fill=r, label=round(r,2))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Pearson's\nCorrelation", title="Correlations overall",
       subtitle="") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446")
p2
