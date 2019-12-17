#1.

#16년도 월별 국내 관광객 입도현황  (공공데이터포털)
##1. 파일 불러오기, 정제
setwd("C:/jeju")
visit2016 <- read.csv("제주특별자치도_내국인관광객현황_2016.csv",header=T)

visit2016 <- visit2016[1:3,]
visit2016
visit2016 <- as.data.frame(t(visit2016))
visit2016

visit2016 <- visit2016[4:15,]
colnames(visit2016) <- c("개별여행","부분패키지","패키지")
visit2016
str(visit2016)

for(i in 1:ncol(visit2016)){                              #팩터 -> 벡터로 바꾼다.(문자로 됨)
  visit2016[,i] <- as.vector(visit2016[,i])
}
str(visit2016)

for(i in 1:ncol(visit2016)){                             #문자 -> 숫자로 바꾼다.
  visit2016[,i] <- as.numeric(visit2016[,i])
}
str(visit2016)


visit2016 <- data.frame(visit2016,sum=rowSums(visit2016))  #월별 입도객 합계
visit2016


#17년도 월별 국내 관광객 입도현황
##1. 파일 불러오기, 정제
visit2017 <- read.csv("제주특별자치도_내국인관광객현황_20171231.csv",header=T)
visit2017 <- visit2017[1:3,-15]
visit2017
visit2017 <- as.data.frame(t(visit2017))
visit2017

visit2017 <- visit2017[3:14,]
colnames(visit2017) <- c("개별여행","부분패키지","패키지")
visit2017
str(visit2017)

for(i in 1:ncol(visit2017)){                              #팩터 -> 벡터로 바꾼다.(문자로 됨)
  visit2017[,i] <- as.vector(visit2017[,i])
}
str(visit2017)

for(i in 1:ncol(visit2017)){                             #문자 -> 숫자로 바꾼다.
  visit2017[,i] <- as.numeric(visit2017[,i])
}
str(visit2017)

visit2017 <- data.frame(visit2017,sum=rowSums(visit2017))  #월별 입도객 합계
visit2017


#18년도 월별 국내 관광객 입도현황
##1. 파일 불러오기, 정제
visit2018 <- read.csv("제주특별자치도_내국인관광객현황_20181231.csv",header=T)
visit2018 <- visit2018[1:3,-15]
visit2018
visit2018 <- as.data.frame(t(visit2018))
visit2018

visit2018 <- visit2018[3:14,]
colnames(visit2018) <- c("개별여행","부분패키지","패키지")
visit2018
str(visit2016)

for(i in 1:ncol(visit2018)){                              #팩터 -> 벡터로 바꾼다.(문자로 됨)
  visit2018[,i] <- as.vector(visit2018[,i])
}
str(visit2018)

for(i in 1:ncol(visit2018)){                             #문자 -> 숫자로 바꾼다.
  visit2018[,i] <- as.numeric(visit2018[,i])
}
str(visit2018)

visit2018 <- data.frame(visit2018,sum=rowSums(visit2018))  #월별 입도객 합계
visit2018


month <- 1:12
late1 <- visit2016$sum
late2 <- visit2017$sum
late3 <- visit2018$sum

plot(month,late2,type="l",main="월별 국내 관광객 입도 현황")                #17년도 월별 국내 관광객그래프
lines(month,late1,type="l",col="blue")                                      #16년도 월별 국내 관광객그래프
lines(month,late3,type="l",col="red")                                      #18년도 월별 국내 관광객그래프


#------------------------------------------------------------------------------------------------------


# 년도별 국내 관광객 입도 현황  (공공데이터포털)
visit <- read.csv("제주특별자치도_연도별관광객입도현황_2013~2018.csv",header=F);visit
visit <- visit[2,2:7];visit
visit <- t(visit);visit
visit <- as.numeric(visit)
visit <- visit/1000

year <- 2013:2018
visit <- data.frame(year,visit)
visit

    
library(ggplot2)                                                                          #년도별 국내 관광객 입도 현황 변화 그래프
ggplot(visit,aes(x=year,y=visit))+
  geom_bar(stat="identity",width=0.75,fill="steelblue")+
  ggtitle("년도별 국내 관광객 입도 현황") +
  theme(plot.title=element_text(size=30,face="bold",colour="steelblue"))+
  labs(x="연도",y="관광객 수 (단위: 백만)")+
  geom_text(aes(label=visit,vjust=-1,hjust=0,),colour="red")


#------------------------------------------------------------------------------------------------------

#국내방문객 제주도 여행 목적/ 제주여행 고려 요인 / 제주에서의 활동-> 자연경관



##제주여행 불만족 사항 (제주관광공사)
# 외국인보다는 내국인이 불편을 덜 느낀다고 나타남. (18년도 설문 조사)
# 제주도 내국인 관광객의 63.6% 는 제주여행에 불편을 느끼지 않았다.
# 불편을 느낀 부분은 비싼 물가라고 응답한 비율이 높았다. 



dislike1 <- read.csv("국내여행객불만족1.csv")
head(dislike1)
dislike1 <- dislike1[1,]
dislike1 <- dislike1[,-c(1:3)]
rownames(dislike1) <- "percentage"
colnames(dislike1) <- c("여행정보습득 어려움","관광정보 부정확","서비스 불친절","안내표지판 부정확","식당 불청결","입에 맞지 않는 음식","비싼 물가")
dislike1

dislike2 <- read.csv("국내여행객불만족2.csv")
head(dislike2)
dislike2 <- dislike2[1,]
dislike2 <- dislike2[,-c(1:3)]
dislike2
rownames(dislike2) <- "percetage"
colnames(dislike2) <- c("대중교통 불편성","택시 서비스 불편","관광 가이드 서비스","다양하지 못한 쇼핑","상품 구입 강요","기타","만족")
dislike2 


dislike <- data.frame(dislike1,dislike2);dislike
dislike <- t(dislike);dislike

names <- rownames(dislike)
dislike <- data.frame(reason=names,dislike)
dislike

order(dislike$percentage)
dislike <- dislike[order(dislike$percentage,decreasing=T),]
dislike <- head(dislike,6)
 
library(ggplot2)                                                                        #국내 제주여행객 불만사항 그래프
 
ggplot(dislike,aes(x=reason,y=percentage))+
  geom_bar(stat="identity",width=0.51,fill="steelblue") +
  ggtitle("제주 국내방문객 불만족 사항")+
  theme(plot.title=element_text(size=30,face="bold",colour="steelblue"))+
  labs(x="연도",y="퍼센트")


#우리나라 관광객수 변화 미미 -> 외국인 관광객수 줄었으나 대부분의 영향은 중국의 사드 때문 -> 우리가 해결할 수 있는것은 없다.
#-> 다른 나라 사례


# http://www.consumerdata.co.kr/news/articleView.html?idxno=3842
#국내방문객의 국내여행 중 제주도 는 몇위인가?


#푸켓 : 계속 증가, 작년부터는 중국관광객 수의 감소로 하락 
# 선박 전복 사고로 중국인 관광객 급감
# 중국인 관광객 회복을 위해 사증신청수수료+면제제도 재시행 정부에 요청 중
#1. 경전철 개발 ( 2023년 완공 ) - > 이동 시간 단축
#2. 신공항 건설 (2023년 완공 )