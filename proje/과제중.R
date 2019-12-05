# 개인 프로젝트 #1


#   문제 2 )
# 제주 대중교통 현황을 분석한다. - 과연 만족도 1위가 맞는가? 1인 가구당 제주도 승용차 보유대수 연령대별 운전가능한 사람, 트래픽 잼 일어남   현재 제주도 버스 한시간에 한 대, 서울시와 제주도 버스 환승시간대비, 장점 :도로에 차가 별로 없음 단점 :도착시 까지 오래걸림
#   
1.나는 불편한데, 왜? 1위 ? 혹시 제주시만? 모두 자가용을 이용하는게 아닐까? 제주도 인구대비 자가용 소유1위?
  
  제주특별자치도 청사 타당성조사 및 기본계획 수립,이동 공유 서비스 (MaaS)

출처 : 제주매일(http://www.jejumaeil.net)
-서울시 대중교통이용 현황이랑 비교하기 
2.자가용 이용 횟수,어느시간대에 막히나?
3.주 이용 대상
4.버스 기다리는 시간
5.서울시 대중교통 만족도

버스 환승 시간 대비, 환승 정보 
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

setwd("D:/WorkR2/proje")
A<-read.csv( "한국교통안전공단_대중교통 이용인원 현황 (2018년).csv",header=T)
A
View(A)


#자동차 운전면허 소지자


setwd("D:/WorkR2/proje")
D<-read.csv( "자동차 운전면허 소지자.csv",header=T)
D
View(D)

#주차장 현황
setwd("D:/WorkR2/proje")
re<-read.csv("주차장 현황.csv",header=T)
re
View(re)

#교통관광정보통신 
setwd("D:/WorkR2/proje")
E<-read.csv("교통 관광 정보 통신_최종.csv",header=T)
E
View(E)

#자동차 및 이륜차 등록대수
setwd("D:/WorkR2/proje")
G<-read.csv("자동차 및 이륜차 등록대수.csv",header=T)
G
View(G)

#제주2016년주민등록인구 통계
setwd("D:/WorkR2/proje")
Z<-read.csv("2016년12월-인구통계.csv",header=T)
Z
View(Z)
#제주2006년 주민등록인구 통계
setwd("D:/WorkR2/proje")
Y<-read.csv("2006년제주도인구.csv",header=T)
Y
View(Y)

#2019년 자동차 운전면허 소지자 
#2016년 자동차 운전면허 소지자
#2015년 자동차 운전면허 소지자

#시도별 대중교통 환승실태2017년

#제주세대및 인구2019년 것
#제주세대및 인구2015년 것
https://www.jeju.go.kr/open/stats/list/population.htm?year=2015&month=6

http://kosis.kr/index/index.do

http://jjpolice.go.kr/jjpolice/open/publish/open.htm?category=71&act=view&page=1&seq=52807
#제주도승용차 보유대수?


#대중교통 만족도 1위가 웬말? 세대당 (제주세대현황)제주도 승용차 보유대수(자동차 등록현황 ) 운전가능한 사람(운전면허증)  개선방안이 필요하다!!

g1<-data.frame(G);g1   #g1 자동차 등록대수

str(g1)

rer<-data.frame(re);rer #주차장 현황
str(rer)

d1<-data.frame(D);d1  #자동차 운전면허 소지자 현황
str(d1)


e1<-data.frame(E);e1 #교통관광정보통신
str(e1)

z16<-data.frame(z);z16 #2016년 인구통계
str(z16)
z06<-data.frame(Y);Y #2006년 인구통계
str(z06)


#1인당 자동차 등록대수 분석


p06


