# 문1)
# R에서 제공하는 state.x77 데이터셋을 차원 축소하여 2차원 산점도와 3
# 차원 산점도를 작성하시오. (state.x77은 매트릭스 타입이기 때문에 데이터프레임
#                 으로 변환하여 실습한다.)
library(Rtsne)
library(ggplot2)
st<-data.frame(state.x77)
st

std=which(duplicated(st))
std
st<-st[-std,]
st
str(st)
dim(st)
head(st)

#2차원
tsne<-Rtsne(st, dim=2,  perplexity=5)
tsne

stf.tsne<-data.frame(tsne$Y)
head(stf.tsne)

ggplot(stf.tsne,aes(x=X1,y=X2))+  
  geom_point(size=2)


#3차원---안그려진다...
library(car)
library(rgl)
library(mgcv)
tsne<-Rtsne(st,dim=3,perplexity=5)
stf.tsne<-data.frame(tsne$Y)
head(stf.tsne)

scatter3d(x=stf.tsne$X1,y=stf.tsne$X2,
        z=stf.tsne$X3)


# 문2)
# R에서 제공하는 swiss 데이터셋을 차원 축소하여 2차원 산점도와 3차원
# 산점도를 작성하시오.
#2차원 
str(swiss)
head(swiss)
sw<-data.frame(swiss)
dim(sw)
swp=which(duplicated(sw))
swp

swf.tsne<-data.frame(tsne$Y)
head(swf.tsne)

ggplot(swf.tsne,aes(x=X1, y=X2))+
  geom_point(size=2)

#3차원
tsne<-Rtsne(sw,dim=3,perplexity=10)
swf.tsne<-data.frame(tsne$Y)
head(swf.tsne)
scatter3d(x=swf.tsne$X1,y=swf.tsne$X2,
          z=swf.tsne$X3)

# 문3) 
# R을 이용하여 지도를 출력하시오.
# (1) 서울시청을 중심으로 지도 크기는 600x600, 지도 유형은 roadmap인 지도를 출력
# 하시오.

library(ggmap) 

register_google(key = 'AIzaSyBdvaqxtk8AUDQE8fSZly76zSmzu6vYEP4')
gc<-geocode(enc2utf8("서울시청"))   
gc

cen<-as.numeric(gc) 
cen

map<-get_googlemap(center=cen,
                   zoom=10,
                   size=c(600,600),
                   maptype = "roadmap") 

ggmap(map)

# (2) 금강산 지역을 근방으로 지도 크기는 500x500, 지도 유형은 hybrid, zoom은 8
# 인 지도를 출력하시오.

gg<-geocode(enc2utf8("금강산"))
gg

cen<-as.numeric(gg) 
cen

map<-get_googlemap(center=cen,
                   zoom=8,
                   size=c(500,500),
                   maptype = "hybrid") 

ggmap(map)

# (3) 강남역 근방으로 지도 크기는 640x640, 지도 유형은 roadmap, zoom은 16인 지
# 도를 출력하시오.

kk<-geocode(enc2utf8("강남역"))
kk

cen<-as.numeric(kk) 
cen

map<-get_googlemap(center=cen,
                   zoom=16,
                   size=c(640,640),
                   maptype = "roadmap") 

ggmap(map)
# (4) 지도 유형은 roadmap, zoom은 9인 경도 127.397692, 위도 36.337058 지역의 지
# 도를 출력하시오.
 
cen<-c(127.397692,36.337058)   
map<-get_googlemap(center = cen,
                   zoom = 9,
                   maptype = "roadmap")
                 
ggmap(map)

# (5) 지도 유형은 roadmap, zoom은 10인 경도 135.502330, 위도 34.693594 지역의
# 지도를 출력하시오.
 
cen<-c(135.502330,34.693594)   
map<-get_googlemap(center = cen,
                   zoom = 9,
                   maptype = "roadmap")

ggmap(map)

# 문4)
# R을 이용하여 서울시 한강 이남의 구청들의 위치에 마커와 구청 이름을
# 지도 위에 표시하시오.
addr<-c("강서구청","서초구청","강동구청","양천구청","구로구청","영등포구청","동작구청","금천구청",
      "관악구청","강남구청","송파구청")

gc<-geocode(enc2utf8(addr))
gc

df<-data.frame(addr, lon=gc$lon,
               lat=gc$lat)
df

cen<-c(mean(df$lon),mean(df$lat))
map<-get_googlemap(center=cen,
                   maptype = "roadmap",
                   zoom=10,
                   size = c(640,640),
                   marker=gc)
ggmap(map)

gmap<-ggmap(map)
gmap+
  geom_text(data=df,            #데이터 셋
            aes(x=lon,y=lat),   #텍스트 위치 (경도, 위도값)
            size=5,             #텍스트 크기
            label=addr)      #텍스트 이름

#문5)
# R을 이용하여 대한민국의 광역시를 지도 위에 출력하시오. 단, 마커와 광
# 역시 이름을 함께 표시하시오.
# 부산, 대구, 인천, 광주, 대전, 울산

addr1<-c("부산광역시","대구광역시","인천광역시","광주광역시","대전광역시","울산광역시")
gc1<-geocode(enc2utf8(addr1))
gc1

df1<-data.frame(addr1, lon=gc1$lon,
               lat=gc1$lat)
df1

cen1<-c(mean(df1$lon),mean(df1$lat))
map1<-get_googlemap(center=cen1,
                   zoom=7,
                   size=c(640,640),
                   marker=gc1) 
ggmap(map1)

gmap<-ggmap(map1)
gmap+
  geom_text(data=df1,            
            aes(x=lon,y=lat),   
            size=5,            
            label=addr1) 


# 문6)
# R을 이용하여 서울, 경기, 강원 지역의 국립공원 위치를 지도 상에 마커로
# 시하되 국립공원의 이름을 함께 표시하시오.
addr2<-c("북한산","설악산","오대산",
         "치악산","태백산")
gc2<-geocode(enc2utf8(addr2))
gc2

df2<-data.frame(addr2, lon=gc2$lon,
               lat=gc2$lat)
df2

cen2<-c(mean(df2$lon),mean(df2$lat)) 
map2<-get_googlemap(center=cen2,
                   maptype = "roadmap",
                   zoom=8,
                   size = c(640,640),
                   marker=gc2)
ggmap(map2)

gmap<-ggmap(map2)
gmap+
  geom_text(data=df2,            
            aes(x=lon,y=lat),   
            size=5,            
            label=addr2)      


# 문7) 
# ‘2018년도 시군구별 월별 교통사고 자료’로부터 서울시의 각 구별 1년 교
# 통사고 발생건수를 지도상에 원의 크기로 나타내시오.
#
setwd("D:\workR2")
acc<-read.csv("2018년도_시군구별_월별_교통사고_자료 (2).csv",header=T)
acc
str(acc)                    #'시군수' 자료가 펙터타입임.
ac<- as.character(acc$시군구)   #'시군구' Factor타입을 문자타입으로 전환
ac

traffic <- geocode(enc2utf8(ac))
traffic

df.traffic <- data.frame(acc,lon= traffic$lon,
                         lat=traffic$lat)
df.traffic

cen9 <- c(126.983191,37.528566)        #서울을 센터로 잡음

map9 <- get_googlemap(center = cen9,
                      maptype = "roadmap",
                      zoom=11)
gmap2<- ggmap(map9)
gmap2+
  geom_point(data=df.traffic,
             aes(x=lon,y=lat,size=X2018),
             alpha=0.5, col="blue")+
  scale_size_continuous(range = c(1,14))


# 문8)
# 7번과 동일한 자료를 이용하여 제주시 1년 교통사고 발생건수를 지도상에 원의 크기로 나타내시오.
cen10 <- c(126.540893,33.389378)       
map10<- get_googlemap(center = cen10,
                      maptype = "roadmap",
                      zoom=11)
gmap3<- ggmap(map10)
gmap3+
  geom_point(data=df.traffic,
             aes(x=lon,y=lat,size=X2018),
             alpha=0.5, col="blue")+
  scale_size_continuous(range = c(4,14))
