#2013~2018년 국내관광객 입도 현황


setwd("D:/TeamProject-R/teamproject1차/data")
visit_past <- read.csv("제주특별자치도_연도별관광객입도현황_2013~2018.csv",header=T)


#2019년 국내관광객 입도 현황(1~10월)
setwd("D:/TeamProject-R/teamproject2차/data/은민 조사 자료")

visit_now <- read.csv("2019년 10월입도현황(확정본).csv",as.is=T,header=T)
visit_now[6,7]                          #2019년1~10월까지 국내관광객 입도 누계(문자형)
library(stringr)
visit_10 <- str_replace(visit_now[6,7],",","")
visit_10 <- str_replace(visit_10,",","")
visit_10 <- as.integer(visit_10)
visit_10                               #2019년1~10월까지 국내관광객 입도 누계(숫자형)


#2019년 11월 국내관광객 입도 (제주특별자치도관광협회)
visit_11 <- 1175213

#2019년 12월 18(수)까지 12월 국내 관광객 입도
visit_12 <- 636942

636942/18      #12월 일별 국내관광객 입도 평균 (하루 국내관광객 35000명씩 들어온다)

now <- visit_10+visit_11+visit_12  #2019년 1월부터 ~ 12월 18일까지 국내 관광객 입도 : 13050619


##데이터셋 만들기(2013~2019년 방문객)
past <- visit_past[1,1:7]
colnames(past) <- c("국적","2013년","2014년","2015년","2016년","2017년","2018년")

now <- data.frame(now)
colnames(now) <- "2019년"


visit <- cbind(past,now)
visit <- visit[,-1]
visit <- t(visit)
visit <- as.data.frame(visit)
library(tibble)
visit <- rownames_to_column(visit,var="year")
colnames(visit) <- c("year","number")
visit$number <- visit$number/1000000
visit

#그래프그리기

library(ggplot2)
ggplot(visit,aes(x=year,y=number))+
  geom_bar(stat="identity",width=0.7,fill="steelblue")+
  ggtitle("년도별 국내 관광객 입도 현황")+
  theme(plot.title=element_text(size=30,face="bold",colour="steelblue"))+
  labs(x="연도",y="관광객 수 (단위: 백만)")+
  geom_text(aes(label=number,vjust=-1,hjust=0,),colour="red")
