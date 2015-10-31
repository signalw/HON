data <- read.csv("comparatio rawdata.csv")
data$g <- cut(data$Comparatio,breaks=c(c(0),seq(0.5,2,0.05),c(20)))
library(ggplot2)
data2 <- data[!is.na(data$Comparatio),] ##remove empty comparatio?
###total pop
ggplot(data2,aes(g,Status))+
    stat_summary(fun.y="mean",geom="bar",fill="yellow")+
    stat_summary(aes(label=round(..y..,3)*100), fun.y=mean, geom="text", size=6)

data2 <- data[data$bandgroup=="0",]

###group base for regrettable
table(data[!is.na(data$Statusr),]$g)

###band pop
ggplot(data,aes(g,Status))+
    stat_summary(fun.y="mean",geom="bar",fill="yellow")+
    stat_summary(aes(label=round(..y..,3)*100), fun.y=mean, geom="text", size=6)+
    facet_grid(.~bandgroup)
