library(ggplot2)
library(ggmap)
register_google(key = 'AIzaSyBdvaqxtk8AUDQE8fSZly76zSmzu6vYEP4')

name<-c("1코스 공식안내소","4코스 공식안내소","5코스 공식안내소","7코스 제주올레 여행자센터","7-1코스 서귀포시외버스터미널","10코스 공식안내소","11코스 공식안내소","18코스 간세라운지")
address<-c("서귀포시 성산읍 시흥리 2665-1","서귀포시 표선면 표선리 40","서귀포시 남원읍 남태해안로 140","서귀포시 중정로 22","서귀포시 일주동로 9217","서귀포시 안덕면 화순리 813-6","서귀포시 대정읍 하모리 2150","제주시 관덕로8길 7-9")

cs1<-geocode(enc2utf8(address))
cs1

df<-data.frame(name=name, lon=cs1$lon,
               lat=cs1$lat)
df

cen<-c(mean(df$lon),mean(df$lat)) #중심점 경도와 위도의 평균
map<-get_googlemap(center=cen,
                   maptype = "roadmap",
                   zoom=10,
                   size = c(640,640),
                   marker=cs1)
ggmap(map)

gmap<-ggmap(map)
gmap+
    geom_text(data=df,            #데이터 셋
              aes(x=lon,y=lat),   #텍스트 위치 (경도, 위도값)
              size=3,             #텍스트 크기
              label=df$name) 
