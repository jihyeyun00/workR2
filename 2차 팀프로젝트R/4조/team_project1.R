#
# 4조 팀 프로젝트
#


.libPaths("C:/R/library")
setwd("C:/Users/user/Desktop/4조")
install.packages("rJava")
install.packages("xlsx")
install.packages("tidyverse")
install.packages("ggmap")
library( rJava )
library( xlsx )
library(tidyverse)
## 제주 입도 관광객 수 연도별 데이터 
jry_2013 <- read.xlsx2("2014년 제주특별자치도 관광객 입도현황.xlsx",
                       sheetIndex = 12)
View(jry_2013)
tmp.total <- jry_2013[4,8]
tmp.na <- jry_2013[6,8]
tmp.for <- jry_2013[14,8]
tmp <- data.frame(전체 = tmp.total, 내국인 = tmp.na, 외국인 = tmp.for)
row.names(tmp) = 2013
tmp
tra2013 <-tmp

jry_2014 <- read.xlsx2("2014년 제주특별자치도 관광객 입도현황.xlsx",
                       sheetIndex = 12)
View(jry_2014)
head(jry_2014)
tmp2014.total <- jry_2014[4,7]
tmp2014.total
tmp2014.for <- jry_2014[14,7]
tmp2014.for
tmp2014.na <- jry_2014[6,7]
tmp2014.na
tmp2014 <- data.frame(전체 = tmp2014.total,내국인 = tmp2014.na,외국인 = tmp2014.na)
row.names(tmp2014) = 2014
tmp2014

cal <- function(x,y) {
  tmp.total <- x[4,7]
  tmp.na <- x[6,7]
  tmp.for <- x[14,7]
  tmp <- data.frame(전체 = tmp.total, 내국인 = tmp.na, 외국인 = tmp.for)
  row.names(tmp) = y
  tmp
}

tra2014 <-cal(jry_2014,2014)

jry_2015 <- read.xlsx2("2015년 제주특별자치도 관광객 입도현황.xlsx",
                       sheetIndex = 12)
tra2015 <- cal(jry_2015,2015)

rbind(cal(jry_2014,2014),cal(jry_2015,2015))


jry_2016 <- read.xlsx2("2016년 제주특별자치도 관광객 입도현황.xlsx",
                       sheetIndex = 12)
tra2016 <- cal(jry_2016,2016)


jry_2017 <- read.xlsx2("2017년_12월_관광객입도현황.xlsx",
                       sheetIndex = 1)
tra2017 <- cal(jry_2017,2017)

jry_2018 <- read.xlsx2("2018년_12월_관광객입도현황.xlsx",
                       sheetIndex = 1)
tra2018 <- cal(jry_2018,2018)



tourist <- rbind(tra2013, tra2014,tra2015,tra2016,tra2017,tra2018)
tourist

  library(tidyverse)
str(tourist)

ggplot(tourist, aes(x = 2013:2018,y = 전체))+
  geom_line()


a <- as.vector(tourist$전체)
a
b <- as.numeric(a)
b
c <- as.vector(tourist$내국인)
d <- as.numeric(c)
d
e <- as.vector(tourist$외국인)
f <- as.numeric(e)
f
tourist <- data.frame(전체=b,내국인 = d,외국인 =f)
str(tourist)


#그래프 그리기


plot(2013:2018,tourist$전체/10000, type = "o", ylim = c(800,1500),
     xlab = "연도", ylab = "방문자 수(만명)", main = "연도별 제주 방문자 수",
     col = rgb(0.2,0.4,0.1,0.7), lwd = 3, bty="l",pch=17)
lines(2013:2018,tourist$내국인/10000,type = "o",col = rgb(0.8,0.4,0.1,0.7),lwd = 3)
par(new = T)
plot(2013:2018,tourist$외국인/10000, type = "o", axes = F , ylab = '', xlab = "",
     col =3,lwd = 3, ylim = c(100,500))
axis(side = 4, ylab = "외국인 관광객", ylim = c(100,500),
     col = 3)
legend("topleft", colnames(tourist), cex = 0.8,
       pch = c(17,1,1), col = c(rgb(0.2,0.4,0.1,0.7),
                        rgb(0.8,0.4,0.1,0.7),3), lty = 1,
       text.col = "darkgray")
ggplot(tourist, aes(x = 2013:2018, y= 전체/10000)) +
  geom_point() +
  geom_line() + geom_line(aes(x = 2013:2018, y = 내국인/10000),
                            col = "red") +
  geom_point(aes(x = 2013:2018, y = 내국인/10000),
             col = "red") +
  ggtitle("연도별 제주 방문자 수") +
  theme(legend.position = "right") +
  theme(plot.title = element_text(size = 25, #title 서식 지정
                                  face = "bold",
                                  colour = "steelblue")) +
  labs(x = "연도", y = "방문자 수(만명)", size = 10)



###  연도별 제주 grdp


tmp_jgrdp <- read.xlsx2("제주지역grdp.xlsx",
                        sheetIndex = 1, startRow = 2)
tmp_jgrdp
jgrdp <- tmp_jgrdp[18,2:9]
jgrdp
colnames(jgrdp) = c(2010,2011,2012,2013,2014,2015
                    ,2016,2017)
row.names(jgrdp) = "제주"
jgrdp[4:8]
t(jgrdp)

str(jgrdp)
jgrdp <- as.vector(jgrdp)
jgrdp <- as.numeric(t(jgrdp))
plot(2013:2017,jgrdp[4:8], type = "o")
tour_grdp <- data.frame(tourist[-6,],grdp = jgrdp[4:8])

cor(tour_grdp)
plot(tour_grdp)
grdp_rate <- c(4.9,6.9,7.4,8.0,4.6)
grdp_rate
plot(2013:2017,grdp_rate, type = "l")

cor(data.frame(tourist[-6],grdp_rate))

plot(tour_grdp$전체/10000, tour_grdp$grdp, xlab = "관광객 수(만 명)",
     ylab = "제주 grdp", main = "관광객수와 Grdp 비교")
res <- lm(전체~grdp, data = tour_grdp)
res
abline(res)

ggplot(data = tour_grdp, aes(x = 전체/10000, y = grdp)) +
  geom_point(size = 4,col = "red") +
  ggtitle("관광객수와 Grdp 비교") +
  theme(plot.title = element_text(size = 25, #title 서식 지정
                                  face = "bold",
                                  colour = "steelblue")) +
  stat_smooth(method = "lm") +
  labs(x ="관광객 수(만 명)", y = "제주 Grdp()")


### 구글맵에 매출액 비교 

library(ggmap)
card <- read.xlsx2("카드매출.xlsx", sheetIndex = 3, startRow = 4)
card

register_google( key = 'AIzaSyAkl_0mai7HdkePLL2H6ldWSbIXBeEGNkk')

card1 <- card[9:14,]
row.names(card1) = c("제주도심", "서귀포도심", "서귀포동부",
                     "제주동부", "제주서부", "서귀포서부")
colnames(card1) = 2012:2017

card1.2017 <- as.character(card1$`2017`)
card1.2017 <- as.numeric(card1.2017)
card1.2017


card1
card1.2017


name <- c("제주도남동","제주동홍동","제주하천리","제주북촌리","제주상가리","제주덕수리")
gc <- geocode(enc2utf8(name))
gc
cen <- c(mean(gc$lon),mean(gc$lat))
cen
df <- data.frame(names = name, lon = gc$lon, lat = gc$lat)
df


map <- get_googlemap(center = cen,
                     zoom = 10,
                     maptype = "terrain")
ggmap(map) + 
  geom_point(data = df, aes(x = lon, y = lat),
             size = card1.2017, alpha = 0.5,
             col = "red") +
  scale_size_continuous( range = c(5, 7))



### 연평균 기온 비교

tem_j <- read.xlsx2("제주평균기온.xlsx", sheetIndex = 1, startRow = 1)
head(tem_j)
tem_j <- tem_j[,11][1:12]
tem_j <- as.character(tem_j)
tem_j <- as.numeric(tem_j)
tem_j


tem_s <- read.xlsx2("서울평균기온.xlsx", sheetIndex = 1, startRow = 1)
head(tem_j)
tem_s <- tem_s[,11][1:12]
tem_s <- as.character(tem_s)
tem_s <- as.numeric(tem_s)
tem_s

temp <- data.frame(tem_j,tem_s)
colnames(temp) <- c("제주평균기온","서울평균기온")
row.names(temp) <- c("1월","2월","3월","4월","5월","6월",
                     "7월","8월","9월","10월","11월","12월")
temp


plot(temp$제주평균기온, type = "b")
ggplot(temp , aes(x = 1:12, y = 제주평균기온)) +
  geom_line() + geom_point() +
  ggtitle("제주 월평균 기온(2018)") + 
  theme(plot.title = element_text(size = 25, #title 서식 지정
                                  face = "bold",
                                  colour = "steelblue")) +
  labs(x = "월(Month)", y = "평균기온")

par(mfrow = c(1,2))

month <- c("1월","2월","3월","4월","5월","6월",
           "7월","8월","9월","10월","11월","12월")
ggplot(temp, aes(x = 1:12, y= 제주평균기온)) +
  geom_line()

plot(1:12,temp$제주평균기온, type = "b")
lines(1:12, temp$서울평균기온, type = "b")
  

### 월별 입도 관광객 수

traveler <- read.xlsx2("2018 월별 입도 관광객 수.xlsx", sheetIndex = 1)
traveler
traveler <- traveler[-13,]
traveler <- as.character(traveler$내국인.관광객.수)
traveler <- as.numeric(traveler)
traveler
barplot(traveler)
par(new=T)
#plot(1:12,temp$제주평균기온, type = "b")
traveler
tem_traveler <- data.frame(temp, "관광객 수"=traveler)
ggplot(tem_traveler_stay, aes(x = 1:12, y = 관광객.수/10000)) +
  geom_bar(stat = "identity") + 
  ggtitle("월별 제주 방문 관광객 수") +
  theme(plot.title = element_text(size = 25, #title 서식 지정
                                  face = "bold",
                                  colour = "steelblue")) +
  labs( x = "월(Month)" , y = "관광객 수(만 명)")


### 월별 제주 방문객의 체류 기간(%)
st <- read.xlsx2("월별_체류기간.xlsx", sheetIndex=1, startRow = 3)
st
st1 <- as.character(st[,13])
st1 <- st1[-1]
st1 <- as.numeric(st1)
st1
tem_traveler_stay <- data.frame(tem_traveler, 체류기간=st1)
tem_traveler_stay

cor(tem_traveler_stay)


ggplot(data = tem_traveler_stay, aes(x = 제주평균기온, y = 관광객.수)) +
  geom_point(size = 4,col = "red") +
  ggtitle("관광객수와 평균기온 비교") +
  theme(plot.title = element_text(size = 25, #title 서식 지정
                                  face = "bold",
                                  colour = "steelblue")) +
  stat_smooth(method = "lm") +
  labs(y ="월 관광객 수(만 명)", x = "제주 월평균기온")



