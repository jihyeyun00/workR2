setwd('D:/home/2차 팀프로젝트R')
airplane<-read.csv("제주특별자치도_제주국제공항항공수송실적현황_2013~2018.csv",header=T)
View(airplane)
airplane<-data.frame(airplane)
airplane<-airplane%>%filter(구분=='도착');airplane

ggplot(airplane,aes(x=연도별, y=국제선.운항..편.))+
  geom_bar(stat = 'identity',
           position = 'dodge',
           width = 0.7,
           fill='steelblue')+
  ggtitle('연도별 국제선 항공편')
        

airplane<-airplane%>%filter(구분=='도착');airplane
ggplot(airplane,aes(x=연도별,y=국내선.운항..편.))+
  geom_line(stat='identity',
            group=1,
            color='red')+
  geom_point(shape=2,
             color='black')+
   ggtitle('연도별 국제선 항공편')

#----------------------------------------------------------------------------------------------------------
airplane<-airplane$국내선.여객..명.    

airplane<-airplane[c(1,3,5,7,9,11),]   ##왜?incorrect dimension 이 뜨지?
airplane<-airplane[,c(1,3,5,7,9,11)]
airplane

names(airplane)<-colnames('국내방문객')
airplane
names(airplane)<-rownames(c('2013,2014,2015,2016'))


#----------------------------------------------------------------------------------------

setwd('D:/home/2차 팀프로젝트/새 폴더')
f.bm<-read.csv('외국인설문조사불만족.csv',header=T)
View(f.bm)
head(f.bm)
f.bm<-data.frame(f.bm)
f.bm<-f.bm[5,]
f.bm<-f.bm[,c(3:18)]
f.bm<-f.bm[,-1]
f.bm
library(ggplot)

names(f.bm)<-c('쇼핑', '강요', '언어소통이 불편', '기타','없다','음식', '물가비쌈', '대중교통 이용 불편', '택시기사 서비스가 불편', '관광가이드의 서비스가 불편', '여행정보를 얻기 어려움', '관광정보가 부정확', '관광종사원 불친절','안내표지판 부정확','식당과 음식이 불결')
f.bm

ggplot(f.bm,aes(x=names,y='빈도수'))   ## 왜?
  geom_bar(stat = 'identity',
    width=0.7,
           fill='steelblue')


#=-----------------------------------------------------------------------------------------------------------  
  
  
colnames(f.bm)<-c('쇼핑', '강요', '언어소통이 불편', '기타','없다','음식', '물가비쌈', '대중교통 이용 불편', '택시기사 서비스가 불편', '관광가이드의 서비스가 불편', '여행정보를 얻기 어려움', '관광정보가 부정확', '관광종사원 불친절','안내표지판 부정확','식당과 음식이 불결')
reason(f.bm)<-c('쇼핑', '강요', '언어소통이 불편', '기타','없다','음식', '물가비쌈', '대중교통 이용 불편', '택시기사 서비스가 불편', '관광가이드의 서비스가 불편', '여행정보를 얻기 어려움', '관광정보가 부정확', '관광종사원 불친절','안내표지판 부정확','식당과 음식이 불결')

ggplot(f.bm,aes(x=reason,y='빈도수'))+
  geom_bar(width=0.7,
       fill='steelblue')
       


#----------------------------------------------------------------------

library(ggplot2)
setwd('D:\workR\2차 팀프로젝트R')
foreign.sm<-read.csv('크루즈 3년간 국내 지역별 입항 관광객 나라별 분석.csv',header=T)
View(foreign.sm)
str(foreign.sm)

tr<-foreign.sm%>%filter(X=='제주');tr
korea<-tr%>%filter(X.2=='한국')%>%select('X2016년','X.3', 'X2017년','X.4','X2018년','X.5');korea
japan<-tr%>%filter(X.2=='일본')%>%select('X2016년','X.3', 'X2017년','X.4','X2018년','X.5');japan



year<-c('X2016년','X.3', 'X2017년','X.4','X2018년','X.5');year


#데이터 혁식을 바꾼후 이렇게 해야함.
# ggplot(tr,aes(x=year, y = korea))+
#   geom_line(col='red', lty = 1, lwd = 1)+
#   geom_line(aes(x =year ,y = japan),col='red', lty = 1, lwd = 1)

  

