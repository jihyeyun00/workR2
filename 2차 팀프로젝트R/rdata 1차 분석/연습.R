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


setwd('D:/home/2차 팀프로젝트R')
foreign.sm<-read.csv('크루즈 3년간 국내 지역별 입항 관광객 나라별 분석.csv',header=T)
View(foreign.sm)
str(foreign.sm)

str
tr<-foreign.sm%>%filter(X=='제주');tr
korea<-tr%>%filter(X.2=='한국')%>%select('X2016년','X.3', 'X2017년','X.4','X2018년');korea
japan<-tr%>%filter(X.2=='일본')%>%select('X2016년','X.3', 'X2017년','X.4','X2018년');japan
edit(japan)

year<-c("2016년상반기","2016년하반기","2017년상반기","2017년하반기", "2018년상반기" )
points( korea, main = "국적별 방문객수 그래프", type ="l", col=3,lwd= 2)
ggplot(tr)
 geom_line(aes(x=year,y=japan))



#복수의 선 그래프
late1<-korea
late2<-japan

plot(month, late1, main='지각생통계',
     type="b", lty=1, col="red",
     xlab="Month", ylab="late cnt")
lines(month,late2,type = "b",lty=1,   
      col="blue")



tr<-t(tr);tr
tr1<-data.frame(korea,japan);tr1

ggplot(tr,aes())


tr<-as.matrix(tr)
str(tr)
tr<-as.numeric(tr)
str(tr)

tr1<-data.frame(year,korea);tr1
ggplot(tr1,aes(x=year,y=korea))+
  geom_line(col='red')


tr<-data.frame(tr)
tr<-as_tibble(tr)
str(tr)
str(japan)
str(korea)
korea<-as.vector(korea)

tr1<-data.frame(korea,year);tr1



ggplot(tr1,aes(x=year,y=korea))+
  geom_line(stat ='identity',
            col='red')

  
 
year<-as.vector(year)
year<-as.factor(year)
travle<-subset(tr,X=='제주',select='X.2');travle


lines(year,korea,col=4,lwd=2)
lines(japan,year, col=4,lwd= 2)

ggplot(korea,mapping=aes(x='X.2',y=year))+
  geom_line(stat = 'identity',
            color='steelblue')




