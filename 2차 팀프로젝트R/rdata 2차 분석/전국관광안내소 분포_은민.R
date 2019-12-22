###전국관광안내소 분포포


setwd("D:/TeamProject-R/teamproject2차/data/은민 조사 자료")
data <- read.csv("전국관광안내소표준데이터.csv",header=T,as.is=T)
View(data)

sum(data$시도명=="서울특별시") #48
sum(data$시도명=="부산광역시") #67
sum(data$시도명=="대구광역시") #21
sum(data$시도명=="인천광역시") #27
sum(data$시도명=="광주광역시") #13
sum(data$시도명=="대전광역시") #5
sum(data$시도명=="울산광역시") #10
sum(data$시도명=="세종특별자치도") #0
sum(data$시도명=="경기도") #54
sum(data$시도명=="강원도") #40
sum(data$시도명=="충청북도") #25
sum(data$시도명=="충청남도") #42
sum(data$시도명=="전라북도") #87
sum(data$시도명=="전라남도") #74
sum(data$시도명=="경상남도") #74
sum(data$시도명=="경상북도") #47
sum(data$시도명=="제주특별자치도") #10

location <- c("서울특별시","부산광역시","대구광역시","인천광역시","광주광역시",
              "대전광역시","울산광역시","세종특별자치도","경기도","강원도",
              "충청북도","충청남도","전라북도","전라남도","경상남도","경상북도",
              "제주특별자치도")
number <- c(48,67,21,27,13,5,10,0,54,40,25,42,87,74,74,47,10)
data2 <- data.frame(location=location,number=number)
data2


library(readxl)
library(sp)
library(RColorBrewer)
setwd("D:/TeamProject-R/teamproject2차/data/은민 조사 자료")
gadm <- readRDS("gadm36_KOR_1_sp.rds")
plot(gadm)
print(data2)


code <- c(16,1,4,11,7,5,17,15,8,6,2,3,13,14,10,9,12)

data3 <- cbind(data2,code)
data3

data4 <- data3[order(data3$code),]
standard=c(0,10,20,30,40,50,60,70,90)
sort=cut(data4$number,breaks=standard)
gadm$factor=as.factor(sort)
col <- brewer.pal(8,"Greens")
spplot(gadm,'factor',col.regions=col,main="전국관광안내소 분포")



# http://www.lgblog.co.kr/life/167616   참고
