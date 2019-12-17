#2019 12.13 송지영
#제주도 국제선 직항 입도 외국인 나라별 방문통계

setwd('D:/TeamProject-R/teamproject1차/jejudo')
airplain <- read.csv( "제주특별자치도_제주국제공항항공수송실적현황_2013~2018.csv", header= T)
View(airplain)
library(dplyr)
library(ggplot2)

treveler <-airplain$국제선.운항..편. ;treveler
treveler[1]

t.arrive <- airplain %>% filter(구분 == "도착") 
t.arrive

ggplot( t.arrive , aes( x =연도별, y=국제선.운항..편.) )+
  geom_line(linetype ="dotted", size =3, colour ="black")+
  geom_point(size=10, shape=19, colour="red")+
  ggtitle("제주국제공항 년도별 국제선 운항편 수") +
  theme(plot.title = element_text( size = 26, face = "bold", colour = "red"))

ggplot( t.arrive , aes( x =연도별, y=국내선.여객..명.) )+
  geom_line(linetype ="dotted", size =3, colour ="black")+
  geom_point(size=10, shape=19, colour="steelblue")+
  ggtitle("제주국제공항 년도별 국내선 도착 여객명 수") +
  theme(plot.title = element_text( size = 26, face = "bold", colour = "steelblue"))



#국제선 확인했으니
#국제선 크루즈 확인 : 크루즈 3년간 국내 지역별 입항 관광객 나라별 분석


setwd('D:/TeamProject-R/teamproject1차/jejudo')
cruze <- read.csv( "크루즈 3년간 국내 지역별 입항 관광객 나라별 분석.csv", header= T)
View(cruze)
str(cruze)

treveler <-filter(cruze, X=='제주')  ;treveler
str(treveler)
head(treveler)


korea <- filter(treveler, X.2 =="한국") %>% select(X2016년, X.4, X2017년, X.5, X2018년);korea
china <- filter(treveler, X.2 =="중국1)") %>% select(X2016년, X.4, X2017년, X.5, X2018년);china
japan <- filter(treveler, X.2 =="일본") %>% select(X2016년, X.4, X2017년, X.5, X2018년);japan
taiwan <- filter(treveler, X.2 =="대만") %>% select(X2016년, X.4, X2017년, X.5, X2018년);taiwan



year <- c("2016년상반기","2016년하반기","2017년상반기","2017년하반기", "2018년상반기" )
year <- c( 2016, 2016.6, 2017, 2017.8, 2018 )
plot( korea, main = "국적별 방문객수 그래프", type ="l", col=3,lwd= 2)


lines( year, japan , col=4,lwd= 2)
lines( year, china, col=2,lwd= 2) 
lines( year, taiwan, col =5 ,lwd= 2)

legend( x = 2017, y = 40, 
        legend= c("한국", "일본", "중국", "대만"), 
        cex= 1, col = c("green","blue", "red", "skyblue"),
        pch=3, lty=1:6)

# https://rfriend.tistory.com/73
# line type, symbol shape 참고














# 제주도 연도별 한국인, 외국인 관광객 방문통계         
to.korea <- read.csv("제주특별자치도_연도별관광객입도현황_2013~2018.csv", header=T)
View(to.korea)
str(to.korea)

#벡터로 봤을때
korea <- to.korea %>% filter(국적 =="한국") %>% select(X2013년, X2014년, X2015년, X2016년, X2017년, X2018년);korea
korea <- as.numeric(korea)
korea

#https://rfriend.tistory.com/73 데이터프레임에서 transform() 함수 이용해서 변수 합쳐서 새로운 변수 생성:mutate

japan <- to.korea %>% filter(국적 =="일본") %>% 
  select(X2013년, X2014년, X2015년, X2016년, X2017년, X2018년);
japan <- as.numeric(japan)/1000 ;japan

america <- to.korea %>% filter(국적 =="미국") %>% 
  select(X2013년, X2014년, X2015년, X2016년, X2017년, X2018년);
america <- as.numeric(america)/1000   ; america

taiwan <- to.korea %>% filter(국적 =="대만") %>% 
  select(X2013년, X2014년, X2015년, X2016년, X2017년, X2018년);
taiwan <- as.numeric(taiwan)/1000 ; taiwan

china <- to.korea %>% filter(국적 =="중국") %>% 
  select(X2013년, X2014년, X2015년, X2016년, X2017년, X2018년);
china <- as.numeric(china)/1000 ; china

hongkong <- to.korea %>% filter(국적 =="홍콩") %>% 
  select(X2013년, X2014년, X2015년, X2016년, X2017년, X2018년);
hongkong <- as.numeric(hongkong)/1000; hongkong

singapore <- to.korea %>% filter(국적 =="싱가포르") %>% 
  select(X2013년, X2014년, X2015년, X2016년, X2017년, X2018년);
singapore <- as.numeric(singapore)/1000; singapore

malaysia <- to.korea %>% filter(국적 =="말레이시아") %>% 
  select(X2013년, X2014년, X2015년, X2016년, X2017년, X2018년);
malaysia <- as.numeric(malaysia)/1000; malaysia

sum <- japan + america +taiwan +china +hongkong + singapore +malaysia +etc



etc <- to.korea %>% filter(국적 =="기타") %>% 
  select(X2013년, X2014년, X2015년, X2016년, X2017년, X2018년);
etc <- as.numeric(etc)/1000; etc


year <-2013:2018; year


#그래프 배경 이쁘게 만들기
plot( year, japan, xlab= "년도", ylab= "방문객수",
      main="제주도 년도별 외국인 관광객 방문통계 (중국 제외)",
      ylim=c(1,150), type= 'n')
loc <- par("usr")
rect(loc[1], loc[3], loc[2], loc[4], col = "lemonchiffon")
abline( h = seq(0, 150, 50), v = 2013:2018, col = "black", lty=3) 
abline( v = 1, lty = 1); abline( h = 0, lty = 1)


출처: https://issactoast.com/56 [Anyone can learn anything.]

points( year, japan, type= "l", col=2,  lty=1, las=1, lwd= 2) #일본 - 레드

points( year, america, type= "l", col=3 , lty=2, lwd= 2) # 미국- 초록
points( year, taiwan, type= "l", col=1 , lty=3, lwd= 2) # 대만 - 블랙
points( year, china, type= "l", col=4 , lwd= 2) #수치가 너무 커서 데이터 밖으로 벗어남
points( year, hongkong, type= "l", col=6,  lty=4 , lwd= 2) # 홍콩- 핑크
points( year, singapore, type= "l", col=8,  lty=5 , lwd= 2) # 싱가포르 - 회색
points( year, malaysia, type= "l", col= 4,  lty=6 , lwd= 2) # 말레이시아 - 블루
points( year, etc, type= "l", col=10 , lwd= 2) # 수치가 커서 데이터 밖
legend( x = 2016, y = 150, 
        legend= c("일본", "미국", "대만", "홍콩", "싱가포르", "말레이시아"), 
        cex= 2, col = c("red","green","black", "pink", "grey", "blue"),
        pch=3, lty=1:6)

#las(축 라벨 방향) 옵션의 종류는 {0,1,2,3} 네가지가 있다.
# 0: 축과 언제나 평행[기본 옵션], 1: 언제나 가로 방향, 2: 언제나 축과 직각 방향, 3: 언제나 세로 방향.

#출처: https://issactoast.com/56 [Anyone can learn anything.]





#그래프 배경 이쁘게 만들기
plot( year, japan, xlab= "년도", ylab= "방문객수",
      main="제주도 년도별 외국인 관광객 방문통계(총 합계 포함)",
      ylim=c(1,3500), type= 'n')
loc <- par("usr")
rect(loc[1], loc[3], loc[2], loc[4], col = "lemonchiffon")
abline( h = seq(0, 3500, 500), v = 2013:2018, col = "black", lty=3) 
abline( v = 1, lty = 1); abline( h = 0, lty = 1)


출처: https://issactoast.com/56 [Anyone can learn anything.]

points( year, japan, type= "l", col=1,  lty=1, las=1, lwd= 2) #일본  
points( year, america, type= "l", col=1 , lty=2, lwd= 2) # 미국
points( year, taiwan, type= "l", col=1 , lty=3, lwd= 2) # 대만 
points( year, china, type= "l", col=4 , lwd= 2) #중국
points( year, hongkong, type= "l", col=1,  lty=4 , lwd= 2) # 홍콩
points( year, singapore, type= "l", col=1,  lty=5 , lwd= 2) # 싱가포르 
points( year, malaysia, type= "l", col= 1,  lty=6 , lwd= 2) # 말레이시아
points( year, etc, type= "l", col=2 , lwd= 2) # #기타 국외관광객 
points( year, sum, type= "l", col=6, lwd= 4) #모든 외국 관광객
legend( x = 2013, y = 1800, 
        legend= c("중국인", "일본인", "미국인", "대만", "홍콩", "싱가포르", "말레이시아", "그외 외국인","외국인 합계"), 
        cex= 1.5, col = c("blue", "black","black", "black", "black", "black", "black", "red", "pink"),
        pch=3, lty=1:6)








#사례분석
#하와이섬 관광객 수 증가 그래프 그리기



setwd('D:/jejudo')
hawaii <- read.csv( "hawaii visitors.csv", header= T)
View(hawaii)
class(hawaii)
str(hawaii)

year <-hawaii$Year ;year
year <- year[ 80:97]

visitor <- hawaii$Total
visitor <- as.vector(visitor)[80:97]
visitor
str(visitor)

visitor<- as.numeric(visitor)
visitor

h.d <- data.frame(year,visitor); h.d

ggplot( h.d , aes( x =year, y=visitor) )+
  geom_line( arrow= arrow(), size =3, colour ="steelblue")+
  geom_point(size=3, shape=19, colour="blue")+
  ggtitle("하와이 년도별 관광객 수") +
  theme(plot.title = element_text( size = 26, face = "bold", colour = "red"),
        panel.background = element_blank())
