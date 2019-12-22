library(rJava)
library(xlsx)
library(dplyr)
library(tidyverse)

setwd('D:/TeamProject-R/teamproject2차/data/제주 교통편 현황')
busstop <- read.xlsx(file = '버스정류장 다국어안내기 설치현황.xlsx', sheetIndex = 1, encoding = 'UTF-8')

View(busstop)
str(busstop)

# 전체버스 중에, 현재 안내판 있는 버스정류장 수 , 터치스크린 안내판 있는 버스정류장 수

bus <- busstop[1:2,1:2]
names(bus) <- c("종류","수")
bus

sum <- busstop[1,2];sum
value = c(busstop[1,2], busstop[2,2]);value

bus.v <-  value / sum *100
bus.v    
bus.v <- data.frame(bus.v)

bus <- data.frame(bus , bus.v)
bus

label <- paste(round(bus$bus.v,0))
label <- paste(label,'%',sep = '')
label
# Basic piechart
# https://leedakyeong.tistory.com/entry/R-%ED%8C%8C%EC%9D%B4%EC%B0%A8%ED%8A%B8%EB%9E%80-Pie-Chart-in-R

#도내 버스정류장 수 x BIT 버스정보안내기 있는 정류장 수
ggplot(bus, aes(x="", y=bus.v, fill=종류)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    ggtitle("도내 모든 버스정류장수 x BIT 버스정보안내기 설치정류장 비율") +
    geom_text(aes(label=label),
              position = position_stack(vjust =0.5))
   

# BIT 있는 버스정류장수 중에, 터치 스크린이 있어서 4개국어 가능한 버스정류장 수

bus.BIT <- busstop[2:3,1:2]
bus.BIT
names(bus.BIT) <- c("종류","수")
bus.BIT
# bus.BIT 라는 데이터프레임 에다가 퍼센트 데이터 추가하기


sum.BIT <- busstop[2,2];sum.BIT
sum <- busstop[1,2];sum
value.4 = c(busstop[3,2]);value.4
value.BIT= c(busstop[2,2]); value.BIT

bus.4 <-  value.4 / sum.BIT *100
bus.4    

bus.val <-  value.BIT / sum *100
bus.val

v <- data.frame(bus.4, bus.val)
v
v <- t(v)
v <- data.frame(v);v

bus <- data.frame(bus.BIT , v)
bus

# BIT전체 중에서 4개국어 있는 정류소 데이터프레임 완성 


label <- paste(round(bus$v,0))
label <- paste(label,'%',sep = '')
label


# Basic piechart
#도내 버스정류장 수 x BIT 버스정보안내기 있는 정류장 수
ggplot(bus, aes(x="", y=v, fill=종류)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    ggtitle("BIT 설치된 버스정류장 ")+
    geom_text(aes(label=label),
              position = position_stack(vjust =0.5))

   




