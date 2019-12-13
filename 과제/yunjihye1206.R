# 개인 프로젝트 #1


#윤지혜 2019.12.06

#   문제 2 )
# 제주 대중교통 현황을 분석한다. - 과연 만족도 1위가 맞는가? 1인 가구당 제주도 승용차 보유대수 연령대별 운전가능한 사람, 트래픽 잼 일어남   현재 제주도 버스 한시간에 한 대, 서울시와 제주도 버스 환승시간대비, 장점 :도로에 차가 별로 없음 단점 :도착시 까지 오래걸림
#   

#   
#   * 각 조 해당 번호의 조원이 해당 문제에 대하여 분석한다.	
# * 문제에 대한 정의를 구체화 시켜서 분석한다.
# 현재의 현황, 국내 타 지역과의  비교, 장/단점등의 분석 목적을 설정하여 분석한다.
# 
# * 자료 수집 참고 site
# 공공 데이터 포털 : https://www.data.go.kr
# 제주 데이터 허브 : https://www.jejudatahub.net
# 기상 자료 개방 포털 : https://data.kma.go.kr/cmmn/main.do
# SKT 데이터 허브 : https://www.bigdatahub.co.kr/index.do
# 네이버 데이터랩 : https://datalab.naver.com
# 
# * 2019년 12월 6일 오후 12시까지 제출		
# * 분석 결과 보고서는 PPT로 작성한다.
# * 분석 결과 보고서는 문제 정의, 분석 과정, 분석 결과, 참고 자료 및 도구순으로 작성한다.
# * R Script file의 처음에 주석으로 본인 이름과 작성일/제출일 기록
# * 분석 결과 보고서와 R script file, 수집한 자료를 "영문본인이름_조_제출일날짜.zip" 이름으로 
# 압축하여 제출한다.


#한국 안전 공단 대중교통 이용현황

setwd("D:/workR2/proje")
A<-read.csv( "한국교통안전공단_대중교통 이용인원 현황 (2018년) (1).csv",header=T)
A
View(A)

setwd("D:/workR2/proje")
B<-read.csv( "시도별 대중교통 접근수단.csv",header=T)
B
View(B)

setwd("D:\workR2\proje")
C<-read.csv( "시도별 대중교통 환승실태.csv",header=T)
C
View(C)


setwd("D:/workR2/proje")
D<-read.csv( "2015년운전면허 소지자.csv",header=T)
D
View(D)

setwd("D:\workR2")
E<-read.csv( "2019운전면허 소지자.csv",header=T)
E
View(E)

setwd("D:\workR2\proje")
G<-read.csv( "2015자동차 등록현황.csv",header=T)
G
View(G)

setwd("D:D:\workR2\proje")
H<-read.csv( ".csv",header=T)
H
View(H)

setwd("D:D:\workR2\proje")
I<-read.csv( "도시계획현황.csv",header=T)
I
View(I)


#1.한국교통안전공단_대중교통 이용인원 현황 (2018년)
#
#2.시도별 대중교통 접근 수단
#3.시도별 대중교통 환승실태
#4.2015년6월-인구통계
#5.2019년 10월 인구통계
#6.운전면허소지현황(사람별)-2016년06월
#7.2019자동차 운전면허 소지자
#8.세대당 자동차 등록대수 분석: https://www.jeju.go.kr/traffic/car/car.htm?act=view&seq=1182780
#전국 제1의 1가구당 자동차 보유 (2019년 6월 자료 필요함)

#대중교통 이용현황에서 서울과 제주만 뽑아내 $사용하고 col 되지 않을까,
#시도별 대중교통 접근수단에서 서울과 제주만 뽑아내 (그래프) $,col (승용차월등히 높음)
#시도별 대중교통 환승 실태에서 서울과 제주만 뽑아내 (그래프)
#(운전면허 소지자 현황에서 (2016과 2019비교 해서 그래프 ))
#세대당 자동차 등록 대수 분석 에서 (2016과 2019비교 해서 그래프) 
#할 수 있으면 도시계획현황에서 서울주차장 개소랑 주차장 면적 나누고 제주도도 나눠 보고 비교 그래프


#1.한국 교통 안전 공단 대중교통 이용현황
ko<-data.frame(A)
str(ko)
ko[2,c(2:4)]

ko[17,c(2:4)]
kov<-c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원","충북","충남","전북","전남","경북","경남","제주")
kov

kov2<-c("평일","토요일","일요일")


kov
class(kov)


pie(ds,main='대중교통이용현황') 
pie(ds.new,main = 'favorite season')

boxplot(평일~일요일, main="서울")  

plot(wt,mpg,main = "중량-연비 그래프",
     xlab="중량", ylab="연비",
     col="red",pch=19)  #0~25 까지 쓸수있,음의 상관관계, 양의 상관관계

class(kov)

par(mfrow=c(1,2))
for(i in 1:3){
  hist(ko[,i],
       main=colnames(ko)[i],
       col = "yellow")
}
par(mfrow=c(1,1)) #가상화면 복귀


#Factor형 (범주형)
bt <- c('A','B','B','O','AB','A')
bt.new <- factor(bt)  #팩터형 벡터 :범위 :levels
bt
bt.new
bt[5]
bt.new[5]
levels(bt.new) 
as.integer(bt.new)  #번호 기준이 레벨a,ab,b,o 
bt.new[7] <- 'B' #7번째에 공간을 만들어서 b를 넣는다
bt.new[8] <- 'C' #8번째에 공간을 만들어서 c를 넣어야 하는데 팩터 함수는 범위를 정해준 것(a,b,ab,o 형 밖에 없음)
#에 포함되지 않았으므로 na가 출력된 것
bt.new 



wt<-ko$
mpg<-mtcars$mpg

plot(wt,mpg,main = "중량-연비 그래프",
     xlab="중량", ylab="연비",
     col="red",pch=19)  #0~25 까지 쓸수있,음의 상관관계, 양의 상관관계

#여러변수들 간의 산점도 
vars<-c("mpg","disp","drat","wt")
target<-mtcars[,vars]
head(target)
pairs(target,main="multi plots") #다변량일때 산점도 그리는 방법



boxplot(myds$"평일"~myds$"일요일", main="서울")    ##? 범주형은 x축 ,물결 뒤로 

boxplot(myds$rm~myds$grp,main="방의 개수")          

myds<-ko[,c("평일","토요일","일요일")] ;myds

par(mfrow=c(1,2))
for(i in 1:3){
  hist(ko[,i],
       main=colnames(ko)[i],
       col = "yellow")
}
par(mfrow=c(1,1)) #가상화면 복귀



par(mfrow=c(2,3))
for(i in 1:5){
  hist(myds[,i],
       main=colnames(myds)[i],
       col = "yellow")
}
par(mfrow=c(1,1)) #가상화면 복귀

#시도별 대중교통 접근 시간
setwd("D:/workR2/proje")
A<-read.csv( "시도별 대중교통 접근수단.csv",header=T)
A
View(A)

ko<-data.frame(A)
str(ko)

ko<-c("X") ;ko
str(ko)

sum(is.na(ko))  

seoul<-c(93.7,	3.4,	1.7,	0.8) 
names(seoul)<-c("도보","승용차","자전거","택시")
seoul

jeju<-c(96.4,	2.1,0.5,0.7)
names(jeju)<-c("도보","승용차","자전거","택시")
jeju

summary(seoul)   #최소, 최대, 중앙 값등을 알려줘서 한번에 알 수 있다.
summary(jeju)

barplot(seoul,main = '서울대중교통 근접수단',col='yellow')  #팩터(숫자)가 아니지만 성격을 보면 
barplot(jeju,main = '제주대중교통 근접수단',col='blue') 

summary(seoul)
summary(jeju)



범주형이라는 것
#그래서 도수 분포표를 쓸 수 있다. 


boxplot(seoul,main='서울') #박스플롯:데이터가 분포가 어떤지있지 알수 
boxplot(jeju,main='제주')


서울<-1:4
<-c(93.7,	3.4,	1.7,	0.8)

plot(서울, 시간, main="대중교통근접수단",  #lwd = 선의 두께 ,#lty= 선의 모양
     type="l",  lty=1,  lwd=1,        #month :행  late :열
     xlab="서울",  ylab="시간")
plot(month, late, main='지각생통계',
     type="b", lty=1, lwd=1,
     xlab="Month", ylab="late cnt")

plot(month, late, main='지각생통계',
     type="o", lty=1, lwd=1,
     xlab="Month", ylab="late cnt")

plot(month, late, main='지각생통계',
     type="s", lty=1, lwd=1,
     xlab="Month", ylab="late cnt")


#복수의 선 그래프


'100'<-1:4
seoul<-c(93.7,	3.4,	1.7,	0.8)
jeju<-c(96.4,	2.1,  0.5,  0.7)

plot('근접시간', late1, main='서울 제주',
     type="b", lty=1, col="red",
     xlab="근접시간", ylab="100",
     ylim=c(1,4))
lines('근접시간',jeju,type = "b",lty=1,   
      col="blue")

seoul<-c(93.7,	3.4,	1.7,	0.8) 
names(seoul)<-c("도보","승용차","자전거","택시")
seoul

jeju<-c(96.4,	2.1,0.5,0.7)
names(jeju)<-c("도보","승용차","자전거","택시")
jeju

summary(seoul)   #최소, 최대, 중앙 값등을 알려줘서 한번에 알 수 있다.
summary(jeju)

barplot(seoul,main = '서울대중교통 근접수단',col='yellow')  #팩터(숫자)가 아니지만 성격을 보면 #범주형이라는 것
#그래서 도수 분포표를 쓸 수 있다. 
barplot(jeju,main = '제주대중교통 근접수단',col='blue') 

summary(seoul)
summary(jeju)

attributes(A)

A$도보
A$승용차
bicy<-A$자전거
plot(A$도보)

A['도보']

plot(A$도보)
A[2]
A[3,]
A[1,]
plot(A[3,])
A[,3]
A[c('도보','자전거')]
plot(A$도보,A$자전거)
A[1,c('도보','자전거')]
sum(A$오토바이)
sum(A$오토바이,na.rm=T)

apply(A,2,function(y) sum(is.na(y)))   #A라는 데이터에는 관측치에서 16개의 NA 가 있다.
#                          익명함수

auto.na<-A$오토바이 ;auto.na #벡터생성
auto.na2<-na.omit(A$오토바이)
A$auto.na2=ifelse(!is.na(A),A,0)
ifelse(!is.na(),A,0)
A[,'도보']
A['서울',]
edit(A)
A[2,'도보']  #이렇게해야나옴!!!
A['부산',c('도보','자전거')]
subset( A, 도보 >= 94, select = c( '도보','자전거' ) ) 
stem(A$도보)
summary(A)
length(A$오토바이)
A2<-subset(A,오토바이>0)
length(A2$오토바이)

A$도보2[A$도보==1]<-'1.'

boxplot(seoul,main='서울') #박스플롯:데이터가 분포가 어떤지있지 알수 
boxplot(jeju,main='제주')


