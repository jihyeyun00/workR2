# 제주도내 관광 교통수단 현황
#

setwd('D:/TeamProject-R/teamproject2차/data')
jeju.trans<- read.csv('18년 교통관광산업 현황.csv', header=T)
str(jeju.trans)
#'data.frame':	6 obs. of  6 variables:
#  $ 등록대수: Factor w/ 6 levels "렌터카","버스(시내,외,농어촌)",..: 4 2 6 5 3 1
#  $ X2014   : num  29156 473 1471 3930 2089 ...
#  $ X2015   : num  34974 489 1471 3926 2262 ...
#  $ X2016   : num  38274 513 1471 3919 2275 ...
#  $ X2017   : num  40052 742 1214 3899 2144 ...
#  $ X2018   : num  41170 865 1455 3890 2088 ...
# list형태로 내가 원하는 factor형 등록대수 6개의 레벨
head(jeju.trans) #원하는 데이터를 가져왔는데 년도별 col 이라서 #행으로 년도별 치환


#t() 함수 이용
jeju.t <- t(jeju.trans)
jeju.t
str(jeju.t) # list 함수로 바뀜 <- 데이터프레임으로 바꿔서 시각화 하기

jeju.df <- data.frame(jeju.t);jeju.df
names(jeju.df) <- c("전체", "버스(시내,외,농어촌)",
                   "업체택시 ", "개인택시", "전세버스" ,"렌터카")
jeju.df
jeju.df <- jeju.df[2:6, ]



# 이 과정이 너무 긴데...
# 처음에 데이터를 일단 행렬 변환 시켜준뒤 해보자.

jeju.tr <- t(jeju.trans[, 2:6]);jeju.tr
str(jeju.tr)
jeju.tr<- data.frame(jeju.tr);jeju.tr
str(jeju.tr)
rownames(jeju.tr)       
names(jeju.tr) <- c("전체", "버스",
                   "업체택시 ", "개인택시", "전세버스" ,"렌터카")

jeju.tr

library(ggplot2)
x <- 2014:2018
ggplot(jeju.tr, aes(x, 전체))+
  geom_bar(stat="identity", fill=rainbow(5)) +
  ggtitle( "제주의 관광 교통수단별 차량 등록대수") +
  theme( plot.title = element_text( size = 25, face = "bold")) +
  labs( x= "년도", y ="차량 등록대수")

#제주 관광교통수단 총 대수 그래프 
# 각 수단별 

jeju.bus <- jeju.tr$버스
jeju.bus

jeju.taxi <- jeju.tr$업체택시  +jeju.tr$개인택시
jeju.taxi

jeju.trbus <- jeju.tr$전세버스
jeju.trbus

jeju.rentcar <- jeju.tr$렌터카
jeju.rentcar

x <- 2014:2018

#plot 함수
par(mfrow=c(2,2))
plot(x, jeju.bus, type="b", cex=2, pch=15, fg="blue", lty="dashed", ylim=c(0,2000), 
     main="버스(시내,외,농어촌)")

plot(x, y=jeju.taxi, type="b", cex=2,  pch=15, fg="red", lty="dashed", ylim=c(5000,7000), 
     main=" 개인택시 + 업체택시")
plot(x ,jeju.trbus,  type="b", cex=2,  pch=15,  fg="darkgreen", lty="dashed", ylim=c(2000,4000),
     main="관광전세버스")
plot(x ,jeju.rentcar,  type="b", cex=2, pch=15, fg="pink", lty="dashed", ylim=c(20000,40000),
     main="렌트카의 수")




par(mfrow=c(1,1))

barplot(x, jeju.bus, xlab = "년도", ylab ="시내,외 +농어촌 버스 대수", 
        col="red" )
barplot(x, jeju.taxi, xlab = "년도", ylab ="개인택시 + 업체택시 수", 
        col="red")
barplot(x ,jeju.trbus, xlab = "년도", ylab ="전세버스 대수",  
        col="red")
barplot(x ,jeju.rentcar, xlab = "년도", ylab ="렌트카 수", 
        col="red")


y <- c("버스", "업체택시 ", "개인택시", "전세버스" ,"렌터카")