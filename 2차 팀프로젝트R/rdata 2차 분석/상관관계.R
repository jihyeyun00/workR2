#제주도 국제선 직항 입도 외국인 나라별 방문통계

setwd('C:/TeamProject-R/teamproject1차/data')
airplain <- read.csv( "제주특별자치도_제주국제공항항공수송실적현황_2013~2018.csv", as.is = T, header= T)
to.korea <- read.csv("제주특별자치도_연도별관광객입도현황_2013~2018.csv", as.is = T, header=T)
View(airplain)
library(dplyr)
library(ggplot2)

str(airplain)
str(to.korea)

head(airplain)

# 국제선 도착 년도별
arrive <- airplain %>% filter(구분 == '도착') %>% select(국제선.운항..편.)
names(arrive) <- '국제선'
arrive

# 연도별 관광객 입도 현황
head(to.korea)
str(to.korea)

to.korea <- to.korea[,-8]
to.korea <- t(to.korea)
korea <- data.frame(to.korea)
names(korea) <- c('한국', '일본', '미국', '대만', '중국', '홍콩', '싱가포르', '말레이시아', '기타')
korea <- korea[-1,]
str(korea)
korea


for (i in 1:ncol(korea)) {
  korea[,i] <- as.character(korea[,i])
}
for (i in 1:ncol(korea)) {
  korea[,i] <- as.integer(korea[,i])
}

korea

# 상관관계 중국 포함
str(korea)
new.data <- korea[,-1]
new.data

new.data2 <- new.data %>% mutate(total = rowSums(new.data))
new.data2

new.data2 <- data.frame(new.data2, arrive)
new.data3 <- new.data2[, 9:10]
new.data3

plot(new.data3$total~new.data3$국제선)

cor(new.data3)
# 국제선 0.9578411 1.0000000
# 국제선 0.89031 1.00000

# 상관관계 중국 미포함
str(korea)
new.data <- korea[,-c(1,5)]
new.data


new.data2 <- new.data %>% mutate(total = rowSums(new.data))
new.data2

new.data2 <- data.frame(new.data2, arrive)
new.data3 <- new.data2[4:6, 8:9]
new.data3

plot(new.data3$total~new.data3$국제선)

cor(new.data3)
