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
names<-c("강서구청","서초구청","강동구청")
addr<-c("서울특별시 강서구 화곡6동 화곡로 302",
        "서울특별시 서초구 서초2동 남부순환로 2584",
        "서울특별시 성내동")
        
gc<-geocode(enc2utf8(addr))
gc

df<-data.frame(name=names, lon=gc$lon,
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
            label=df$name)      #텍스트 이름

#문5)
# R을 이용하여 대한민국의 광역시를 지도 위에 출력하시오. 단, 마커와 광
# 역시 이름을 함께 표시하시오.
# 부산, 대구, 인천, 광주, 대전, 울산

names<-c("부산광역시","대구광역시","인천광역시","광주광역시","대전광역시","울산광역시")
addr<-c("부산광역시","대구광역시","인천광역시","광주광역시","대전광역시","울산광역시")
gc<-geocode(enc2utf8(addr))
gc

df<-data.frame(name=names, lon=gc$lon,
               lat=gc$lat)
df
cen<-c(mean(df$lon),mean(df$lat))
map<-get_googlemap(center=cen,
                   zoom=10,
                   size=c(640,640),
                   marker=gc) 

ggmap(map)

gmap<-ggmap(map)
gmap+
  geom_text(data=df,            
            aes(x=lon,y=lat),   
            size=5,            
            label=df$name) 




# 문6)
# R을 이용하여 서울, 경기, 강원 지역의 국립공원 위치를 지도 상에 마커로
# 시하되 국립공원의 이름을 함께 표시하시오.
names<-c("북한산","설악산","오대산",
         "치악산","태백산")
addr<-c("경기 고양시 덕양구 대서문길 375",
        "강원 인제군 북면 한계리",
        "강원 홍천군 내면 오대산로 2",
        "강원 원주시 소초면 무쇠점2길 26",
        "강원 태백시 혈동")


gc<-geocode(enc2utf8(addr))
gc

df<-data.frame(name=names, lon=gc$lon,
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
  geom_text(data=df,            
            aes(x=lon,y=lat),   
            size=5,            
            label=df$name)      


# 문7) 
# ‘2018년도 시군구별 월별 교통사고 자료’로부터 서울시의 각 구별 1년 교
# 통사고 발생건수를 지도상에 원의 크기로 나타내시오.
#
setwd("D:/workR2")
tr<-read.csv("2018년도 시군구별 교통사고.csv",header=T)
tr
View(tr)


df.sl<-data.frame(tr)
df.sl

sl<-df.sl[2:14,]
# 문8)
# 7번과 동일한 자료를 이용하여 제주시 1년 교통사고 발생건수를 지도상에 원의 크기로 나타내시오.