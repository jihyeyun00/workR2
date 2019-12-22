
# ylim = y축 범위 지정하기. 원하는 값 지정할수 있어서 좋음
#par(new=TRUE) #내가 만든 그래프위에 추가로 선그래프 올려놓게 - 누적시키는 기능

y1<-c(0.8,0.5,0.4,0.4,0.5,0.7)
y2<-c(0.8,1.3,1.0,1.3,0.9,1.2)
x<-c(1:6)
plot(x,y1,ylab="소비자 물가 동향",type="o",col="red",ylim=c(0.3,1.5))
par(new=TRUE)
plot(x,y2,lty="dotted",ylab="소비자 물가 동향",type="l",col="blue",ylim=c(0.3,1.5))
legend(locator(1),legend=c('2015년','2016년'),lty=1,bg="yellow",col=c('red','blue'))

#legent(locaror(1) 이걸 적으면 그래프 만들고 나서 마우스 커서로 그래프 위에 눌러 범례 위치 지정)




#응용 #barplot x축 이름 - 직접 넣기
plot.new()
x<-c(100,200,300,350,500)
barplot(x,name=c('a','b','c','d','e'),las=1,col='yellow',ylim=c(0,600))




# x축 변수에 이름 지정 gu라는 벡터 만들기
data<-read.csv("C:/Users/SJY/Desktop/빅데이터전문가/seoulpopulation.csv",sep=",",header=TRUE)
data
data1<-subset(data,남자>=230000)
data1
gu<-data1$자치구

barplot(data1$남자,names=gu,las=1,col="darkgreen",horiz=TRUE,
main="서울시 남자 23만명 이상 자치구")





#bar 누적막대그래프 만들기
plot.new()
data<-read.csv("C:/Users/SJY/Desktop/빅데이터전문가/seoulpopulation.csv",sep=",",header=TRUE)
data
tot<-rowSums(data[,c('남자','여자')],na.rm=TRUE)
data<-cbind(data,tot)
data                                          #새로운 변수 만들어서 cbind 시킴

data1<-subset(data,tot>=500000)               #부분추출!! subset함수!!
data1
barplot(as.matrix(data1[1:6,2:3]),legend=c('남자','여자'),las=1,col=c("darkgreen","pink"),
main="서울시 인구 50만명 이상 자치구",ylim=c(0,400000))
# 둘의 차이는 beside=TRUE 누적막대에서 옆에 같이 배열시키는 거

barplot(as.matrix(data1[1:6,2:3]),legend=c('남자','여자'),beside=TRUE,las=1,col=c("darkgreen","pink"),
        main="서울시 인구 50만며 이상 자치구",ylim=c(0,400000))





# 히스토그램
plot.new()
x<-c(23,33,32,45,37,28,15,35,43,27,46,33,38,46,50,25)
hist(x,main="설문조사 나이별 분포",xlim=c(15,50),col="pink")

#상자수염
plot.new()
data<-read.csv("C:/Users/SJY/Desktop/빅데이터전문가/birthdie.csv",sep=",",header=TRUE)
data
boxplot(data$출생,data$사망,names=c('출생','사망'),col=c("pink","darkgreen"),
main="2013년 ~2014년 출생과 사망 집계")

#파이그래프
x<-c(27,43,15)
pie(x,radius=1,labels=c("일반택시","대중교통","자가용"))
