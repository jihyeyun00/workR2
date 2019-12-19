library(dplyr)
library(ggplot2)
library(tidyverse)

setwd("D:/TeamProject-R/teamproject2차/data/지혜자료 2-2자료")
transfer<-read.csv("외국인들 교통수단.csv",header=T)
View(transfer)
str(transfer)
transfer<-transfer[2,4:11]

names(transfer)<-c('렌터카','버스','택시','전세버스','시티투어버스','자전거 or 오토바이','친구 및 지인차량','기타')
rownames(transfer)<-'이용률'
newdata<-transfer[1:8];newdata

str(newdata)
head(newdata)
class(newdata)

#data 만들기
data <- data.frame(
    group=names(newdata)[1:8],
    value=c(newdata[,1],newdata[,2],newdata[,3],newdata[,4],newdata[,5],newdata[,6],newdata[,7],newdata[,8]), 
    stringsAsFactors = FALSE
)
data

#position of labels
data <- data %>% 
    arrange(desc(group)) %>%
    mutate(prop = value / sum(data$value) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )

str(data)
label <- paste(round(data$prop,0))
label <- paste(label,'%',sep = '')
data

#pie chart 
ggplot(data, aes(x="", y=prop, fill=group)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0)+
    theme_void()+
    theme(legend.position='right')+
    geom_text(aes(x=c(1,1,1,1.15,1.3,1,1,1),y = ypos, label = label), color = "black", size=7) +
    scale_fill_brewer(palette="Set1")







