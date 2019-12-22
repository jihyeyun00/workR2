# 제주도에서 운영하는 실제 관광안내소의 위치

setwd("D:/TeamProject-R/teamproject2차/data/은민 조사 자료")
data <- read.csv("제주특별자치도_서귀포시_관광안내소_20190630.csv",as.is=TRUE,header=T)
data2 <- read.csv("제주특별자치도_제주시_관광안내소_20190516.csv",as.is=TRUE,header=T)

library(ggmap)
register_google( key= 'AIzaSyDlmljbgzrqBC-ug1Mr1Q1Y4gvEOkOcR_g')

seo <- data[,c(1,18)]
jeju <- data2[,c(1,18)]

center <- rbind(seo,jeju)   

name <- data.frame("성산항여객터미널","제주 서귀포시 성산읍 성산등용로 112-7")
colnames(name) <- c("관광안내소명","소재지도로명주소")

center <- rbind(center,name)
center                                #제주도에서 운영하는 관광안내소

center_gc<- geocode(enc2utf8 (center$소재지도로명주소))
center_gc

center_df <- data.frame(name=center$관광안내소명,lon=center_gc$lon,lat=center_gc$lat)
center_df

center_cen <- c(mean(center_df$lon),mean(center_df$lat))

center_map <- get_googlemap(center=center_cen,maptype="roadmap",zoom=10,size=c(800,800),marker=center_gc)
ggmap(center_map)


# 외국인 입장객이 많은 관광지(top10)
setwd("D:/TeamProject-R/teamproject2차/data/은민 조사 자료")
data3 <- read.csv("주요관광지점 입장객(2004.07 이후)_191217055308.csv", as.is=TRUE, header=T)
data4 <- subset(data3,내.외국인=="외국인")

library(stringr)
data4$총계 <- str_replace(data4$총계, ",", "")    #콤마제거
data4$총계 <- as.integer(data4$총계)
View(data4)
popular <- data4[order(data4$총계,decreasing=T),][,c(3,7)]

top10 <- head(popular,10)
top10[2,1] <-  "대포주상절리"
top10

top10_gc <- geocode(enc2utf8 (top10$관광지))
top10_gc

top10_df <- data.frame(name=top10$관광지,lon=top10_gc$lon,lat=top10_gc$lat)
top10_df

park <- geocode(enc2utf8 ("한라산"))
park <- as.numeric(park)


top10_map <- get_googlemap(center=park,maptype="roadmap",zoom=10,size=c(800,800),marker=top10_gc)
ggmap(top10_map)


# 외국인 입장객이 많은 관광지 (전부)
all <- popular
all[2,1] <- "대포주상절리"
all_gc <- geocode(enc2utf8(all$관광지))

all_df <- data.frame(name=all$관광지,lon=all_gc$lon,lat=all_gc$lat)
all_df


all_map <- get_googlemap(center=park,maptype="roadmap",zoom=10,size=c(800,800))    ###1. 점으로표시
ggmap(all_map)+geom_point(data=all_df,aes(x=lon,y=lat,size=1),alpha=1,col="red")

all_map2 <- get_googlemap(center=park,maptype="roadmap",zoom=10,size=c(800,800),marker=all_gc)   ###2. 마커로 표시
ggmap(all_map2) 



##정리 
# 1) 실제 관광안내소 위치
ggmap(center_map)

# 2) 외국인 방문객 많은 10곳의 위치
ggmap(top10_map)

# 3) 실제 관광안내소와 외국인 방문객 많은 곳(top10) 의 위치 차이 (2개 동시에 보여준다)
ggmap(center_map) +
  geom_point(data=top10_df,aes(x=lon,y=lat,size=1),alpha=0.7,col="blue")


# 4) 실제 관광안내소와, 외국인 방문객 많은 곳 전부 분포
ggmap(center_map) +
  geom_point(data=all_df,aes(x=lon,y=lat,size=1),alpha=0.5,col="black")


