library(ggplot2)
library(dplyr)
setwd('c:/workR')
foreign.sm<-read.csv('크루즈 3년간 국내 지역별 입항 관광객 나라별 분석.csv',header=T)
View(foreign.sm)
str(foreign.sm)

tr<-foreign.sm%>%filter(X=='제주');tr
korea<-tr%>%filter(X.2=='한국')%>%select('X2016년','X.3', 'X2017년','X.4','X2018년')
korea
japan<-tr%>%filter(X.2=='일본')%>%select('X2016년','X.3', 'X2017년','X.4','X2018년')
japan
kor<- t(korea)
jap<- t(japan)

kor<- as.numeric(kor)
jap<- as.numeric(jap)
tr1<- data.frame(kor, jap)
tr1
row.names(tr1)<-c(2016, 2016.5, 2017, 2017.5, 2018)
tr1
str(tr1)


plot(rownames(tr1), tr1$kor, 
     main = "국적별 방문객수 그래프", type ="l", lwd= 2, col='blue',ylim=c(0, 6000),
     xlab='연도', ylab="성장률")
lines(rownames(tr1), tr1$jap,type = "l", lwd= 2, col="red")
