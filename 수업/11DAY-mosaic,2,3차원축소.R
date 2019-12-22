#https://www.rdocumentation.org/packages/vcd/versions/1.4-4/topics/mosaic
#open source :주의점 1.bug가 존재 가능 2.저작권 확인(GPL,LGPL:완전무료-SOURCE공개 의무)(APL:아파치,BSD,MPL:공개의무없음)
#모자익은 범주형이면서 다중변수 일때 쓴다.
#차원축소란?
#ML:분류 :예)스팸문자
#   회귀 :예)제주 집을 살때, 보증금 얼마,,,예측
#   군집분류: 예)정치면, 사회면
#
#11일차


#mosaic plot():두개이상의 변수의 비율을 보고 싶을 때
# 다중변수 범주형 데이터에 대한 각 변수의 그룹별 비율을 면적으로 표시
# 정확한 값이라기 보다는 전체적인 비율을 확인할때
 
str(mtcars)
head(mtcars)
mosaicplot(~gear+vs,      #대상변수:~x, +y 축
           data=mtcars,   #데이터셋
           color=TRUE,    #y 축 변수의 그룹별 음영 다르게 표시
           main = "Gear and Vs") #제목
mosaicplot(~gear+vs,data=mtcars,
           color=c("green","blue"), #색상지정
           main = "Gear and Vs") 

tb1<-table(mtcars$gear,mtcars$vs) #(도수분포표)교차표를 만듦, 
tb1
mosaicplot(tb1,color = T,main = "Gear and Vs")


#(산점도는 상관관계 이므로...분포만...시각화) 
#차원축소(dimension reduction):변수개수에 따라 ()차원
#https://skyeong.net/186
#t-sne 기법
install.packages("Rtsne")
library(Rtsne)
library(ggplot2)

ds<-iris[,-5] #4차원이니까 
ds
dim(ds)
#차원축소
#중복 데이터 제거

dup = which(duplicated(ds))
dup
ds <- ds[-dup,]
ds

ds.y<-iris$Species[-dup]
ds.y

#차원축소 수행: t-sne 실행
tsne<-Rtsne(ds, dim=2,  perplexity=10) #차원축소대상 데이터셋
                                      #축소할 차원 2/3 차원
                                       #차원 축소 과정에서 
                            #데이터 샘플링을 수행하는데
                            #샘플의 개수
                            #(대상데이터수)/3 보다 작게 지정

tsne

#차원축소결과 시각화 
df.tsne<-data.frame(tsne$Y)
head(df.tsne)

ggplot(df.tsne,aes(x=X1,y=X2,color=ds.y))+
  geom_point(size=2)

#
install.packages(c("rgl","car"))
library(car)
library(rgl)
library(mgcv)

tsne<-Rtsne(ds,dim=3,perplexity=10)
df.tsne<-data.frame(tsne$Y)
head(df.tsne)

scatter3d(x=df.tsne$X1,y=df.tsne$X2,
          z=df.tsne$X3)

point<-as.integer(ds.y)
color<-c('red','green','blue')
scatter3d(x=df.tsne$X1,y=df.tsne$X2,
          z=df.tsne$X3,
          point.col=color[point],
          surface=FALSE)



#공간 시각화
#
#google map 사용

#절차
#1.R최신버전 설치
#2.ggpolt2최신버전 설치
#3.ggmap 설치
#4.구글맵을 사용하기 위한API key 획득
#5.구글맵을 이용한 공간 시각화 수행
library(ggmap) 

register_google(key = 'AIzaSyBdvaqxtk8AUDQE8fSZly76zSmzu6vYEP4')

gc<-geocode(enc2utf8("제주"))   #원하는 지점 위도, 경도를 자동으로 알아서 알려줌 (tibble함수=데이터프레임같은거)
gc

cen<-as.numeric(gc) #경도/ 위도를 "숫자"로 변환
cen

#지도표시 #직접(경도위도를 )알아오거나, geocode 를 쓰면된다.
map<-get_googlemap(center=cen)  #직접 지정할 수 있으면 위도 경도를 넣으면 됌
ggmap(map) #실제로 그려지는 것


gc<-geocode(enc2utf8("한라산"))
cen<-as.numeric(gc)
map<-get_googlemap(center = cen,    #지도 중심점 좌표
                   zoom = 10,        #지도확대 정도
                   size = c(640,640), #지도 크기
                   maptype = "roadmap") #지도 유형 (hybrid,terrain 도 있음)
ggmap(map)

#꺄오내가만든 지도 !!!! 
#"직접 경도와 위도를 지정할때는 geo, numeric 코드를 쓸 필요 없다.!!!!"

#gc1<-data.frame(lon=126.560164, lat=33.247416) #해야하나???선생님은 안함,경도, 위도 데이터셋 만들기

cen<-c(126.560164,33.247416)   #경도, 위도/구글맵에서는 위도/경도
map<-get_googlemap(center = cen,
                   zoom = 15,
                   maptype = "roadmap",
                   marker=gc1)
ggmap(map)
gmap<-ggmap(map)
gmap+
  geom_text(data=gc1,            #데이터 셋
            aes(x=lon,y=lat),   #텍스트 위치 (경도, 위도값)
            size=5,             #텍스트 크기
            label="긍정동커피")      #텍스트 이름




#내가 원하는 지점에 마커표시하는 법
gc<-geocode(enc2utf8("제주"))
cen<-as.numeric(gc)
map<-get_googlemap(center=cen,
                   maptype = "roadmap",
                   marker=gc)
ggmap(map)


#제주 관관지를 지도위에 표시
names<-c("용두암","성산일출봉","정방폭포",
         "중문관광단지","한라산1100고지","차귀도")
addr<-c("제주시 용두암길15",
        "서귀포시 성산읍 성산리",
        "서귀포시 동홍동 299-3",
        "서귀포시 중문동 2624-1",
        "서귀포시 색달동 산 1-2",
        "제주시 한경면 고산리 125")
gc<-geocode(enc2utf8(addr))
gc


#관광지 명칭과 좌표값으로 data frame 생성
df<-data.frame(name=names, lon=gc$lon,
               lat=gc$lat)
df

cen<-c(mean(df$lon),mean(df$lat)) #중심점 경도와 위도의 평균
map<-get_googlemap(center=cen,
                   maptype = "roadmap",
                   zoom=10,
                   size = c(640,640),
                   marker=gc)
ggmap(map)

#지도에 관광지 이름 추가
gmap<-ggmap(map)
gmap+
  geom_text(data=df,            #데이터 셋
            aes(x=lon,y=lat),   #텍스트 위치 (경도, 위도값)
            size=5,             #텍스트 크기
            label=df$name)      #텍스트 이름


#지도에 데이터 표시
dim(wind)
str(wind)

sp<-sample(1:nrow(wind), 50)
df<-wind[sp,]
head(df)

cen<-c(mean(df$lon),mean(df$lat))
gc<-data.frame(lon=df$lon, lat=df$lat)
head(gc)

#지도에 마커표시
map<-get_googlemap(center=cen,
                   maptype = "roadmap",
                   zoom=6,
                   marker=gc)
ggmap(map)

#지도에 풍속을 원의 크기로 표시
map<-get_googlemap(center=cen,
                   maptype = "roadmap",
                   zoom=6)
                   
gmap<-ggmap(map)


gmap+
  geom_point(data=df,                 #point 함수로 
             aes(x=lon,y=lat,size=spd),
             alpha=0.5, col="blue")+
 scale_size_continuous(range = c(1,14)) #원 크기 조절 range


#단계 구분도
install.packages("ggiraphExtra")
library(ggiraphExtra)


dim(USArrests)   #데이터셋 분석하기 전에 보면 좋은것
str(USArrests)
head(USArrests)

library(tibble)
crime<-rownames_to_column(USArrests,var="state")
crime$state<-tolower(crime$state)
str(crime)

library(ggplot2)
install.packages("mapproj")
library(mapproj)

state_map<-map_data("state")
str(state_map)

ggChoropleth(data=crime,
             aes(fill=Murder,
                 map_id=state),
             map=state_map)

#http://rpubs.com/cardiomoon/222145 우리나라지도를 색으로 구분할수 있는 곳
install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")  #이게 있어야 지도를 그릴수 있음,R패키지에서 제공하는게 아님.
devtools::install_github("cardiomoon/moonBook2")    #깃허브에 접근하기 위해서 devtools 를 쓰는 것임

library(kormaps2014)   
library(moonBook2)

areacode
str(kormap1)

#아래주소로 가야하는..데...
library(ggplot2)
theme_set(theme_gray(base_family="NanumGothic"))

ggplot(korpop1,aes(map_id=code,fill=총인구_명))+
  geom_map(map=kormap1,colour="black",size=0.1)+
  expand_limits(x=kormap1$long,y=kormap1$lat)+
  scale_fill_gradientn(colours=c('white','orange','red'))+
  ggtitle("2015년도 시도별 인구분포도")+
  coord_map()
