#10일차
##document -user's guide:사용자설명서 ,reference:명령 ,함수  
#markdown 형식->보고서 작성용 ,(크기를 다양하게 활용할 수 있는)
install.packages("tidyverse")
library(ggplot2)
#히스토그램같이 연속형 은 테이블 함수를 (도수분포표를)만들수 없다.구간간격을 지정해야
#상자수염그래프는 x 축이 의미가 없다.
#factor( mtcars$cyl ) ) ) + #그룹으로 묶는 것
#기어가 범주형이므로 컬러에 쓸수있다
#트리맵은 한눈에 인구분포(매출규모)를 파악하기 위해
#rownames:왼쪽 행의 이름들,그래서 names 함수로 이름을 정해주는 것,
#colnames:위쪽 열의 이름들

#https://www.tidyverse.org/packages/
#https://ggplot2.tidyverse.org/
#http://rpubs.com/brandonkopp/creating-a-treemap-in-r

library(tidyverse)

dim(mpg)
str(mpg)
head(mpg)
view(mpg)

ggplot(data=mpg)+                           #ggplot, +, geom_point:산점도그리는 함수 /mapping할때aes 함수를 쓴것(x,y지정)
  geom_point(mapping = aes(x=displ,y=hwy)) 


month<-c(1,2,3,4,5,6)
rain<-c(55,50,45,50,60,70)
df<-data.frame(month,rain)
df

ggplot(df,aes(x=month,y=rain))+            
  geom_bar(stat = "identity",              
           width = 0.7,
           fill="steelblue")

#막대순서 바꾸기:https://rfriend.tistory.com/93

ggplot(df,aes(x=month,y=rain))+            #ggplot,+,(mapping 생략도 가능)
  geom_bar(stat = "identity",              #stat :y값이있어야함,y축값으로 막대높이를 정하라 
           width = 0.7,                    #width: 폭, fill :막대색깔
           fill="steelblue")+
ggtitle("월별강수량")+
theme(plot.title = element_text(size = 25,   #'월별강수량'의 꾸미기
                                face = "bold",
                                colour="steelblue"))+
labs(x="월",y="강수량")+coord_flip()   #가로형막대로하라




ggplot(iris,aes(x=Petal.Length))+        #범주형이라면 카운트 해야하지만 연속형은 카운트 할 수 없음
  geom_histogram(binwidth = 0.6)        #연속형binwidth : 빈도 계산,꽃잎의 길이를 0.5단위로 카운트 해라

   
fillPalette<-c("red","yellow","green")

ggplot(iris,aes(x=Sepal.Width, fill=Species,  #fill에는 범주형을,species 는 범주형 문자형이지만 숫자로 자동 변환 1,2,3...
                color=Species))+              
  geom_histogram(binwidth = 0.5,position = "dodge")+  #dodge품종별로 각각그리기
  theme(legend.position = "top") +  #top 범례의 위치
  scale_fill_manual(values=fillPalette)


#ggplot:layer 방식,(계층방식:graphic 처리할때 쓰는) 각 함수를 호츨할때마다 바닥에 ggplot , 두번째 판 geom, histogram,  그 위에theme




#ggplot2 Scatter chart

ggplot(data=iris,mapping=aes(x=Petal.Length,       #ggpolt쓸때 data이름이랑, aes (x,y)값은 정해줘야 최소
                             y=Petal.Width))+      #각각의 판마다 역할을 나눠줌
  geom_point()    

ggplot(data=iris)+
 geom_point(mapping=aes(x=Petal.Length,
                         y=Petal.Width)) 




ggplot(data=iris,mapping=aes(x=Petal.Length,       
                             y=Petal.Width,
                             color=Species,
                             shape=Species))+      
  geom_point(size=3)+
  ggtitle("꽃잎의 길이와 폭")+
  theme(plot.title = element_text(size=25,
                                  face="bold",
                                  colour = 'red'))

#컴퓨터 색상 지정 방식:RGB, ARGB
#red, green, blue/ alpha + rgb
#rgb->16진수 :#000000~#FFFFFF
#16진수 한자리 =2진수(4자리,4bit)=nibble ->2bite 사용
#argb=#00000000~#FFFFFFFF 



#ggplot box plot
ggplot(data = iris,mapping = aes(y=Petal.Length))+   #박스플롯은 y 값을 정해주면 됌 x값은 (폭)은 의미가 없음
  geom_boxplot(fill="yellow")

ggplot(data = iris,mapping = aes(y=Petal.Length,
                                 fill=Species))+  
  geom_boxplot()


#ggplot Line chart 시계열 데이터를 쓸때 선그래프
#선그래프를 여러개 그릴때는 geom_line 에 aes 좌표를 넣어준다.

year<-1937:1960
cnt<-as.vector(airmiles)
df<-data.frame(year,cnt)
head(df)

ggplot(df,aes(x=year,y=cnt))+     
  geom_line(col="red")
  geom_line(aes(x=year,y=))



#ggplot 작성 graph 꾸미기(공통)
str(economics)

#사선
ggplot(economics,aes(x=date,y=psavert))+
  geom_line()+
  geom_abline(intercept = 12.18671, #회귀선
              slope = -0.0005444)
#intercept :y 절편값 (회귀식에서 나온값, 함수 나옴)
#slope : 기울기

  
#평행선
ggplot(economics,aes(x=date,y=psavert))+
  geom_line()+      
  geom_hline(yintercept = mean(economics$psavert)) #평균값이상과 이하로 넣은것


#수직선:특정한 날을 표현하고 싶을때
x_inter<-filter(economics,
                psavert==min(economics$psavert))$date
ggplot(economics,aes(x=date,y=psavert))+
  geom_line()+
  geom_vline(xintercept = x_inter)


#텍스트 추가
ggplot(airquality,aes(x=Day, y=Temp))+   
  geom_point()+
  geom_text(aes(label=Temp,    #0을 주면 점 바로 오른쪽 위
                vjust=-1,      #+1 하단 좌측 모서리
                hjust=-1))     #-1 위 오른쪽

#영역지정 및 화살표 표시
ggplot(mtcars,aes(x=wt,y=mpg))+
  geom_point()+
  annotate("rect",      #사각형모양으로 강조 하겠다
           xmin = 3,
           xmax = 4,
           ymin = 12,
           ymax = 21,
           alpha= 0.5, #알파 값은 투명도 :1에 가까울수록 불투명
           fill="skyblue")+
  annotate("segment",x=2.5, xend=3.7,   #segment는 화살표 , xend =길이
           y=10, yend = 17, color="red",
           arrow=arrow())+
  annotate("text",x=2.5, y=10,          #x,y 는 글자가 시작되는 위치
           label="point")



#treemap
install.packages("treemap")
library(treemap)
  

data(GNI2014)
dim(GNI2014)
str(GNI2014)
head(GNI2014)
View(GNI2014)

treemap(GNI2014,
        index = c("continent","iso3"),   #계층구조 콤마로 여러개해도 돼?
        vSize = "population",            #타일크기
        vColor="GNI",                     #타일컬러
        type="value",                   #타일컬러링 방법(값이 높은것은 진하게)
        bg.labels="yellow",             #레이블배경색
        title="World's GNI")            #제목


st<-data.frame(state.x77)
st<-data.frame(st,stname=rownames(st)) #data.frame(컬럼1=자료, 컬럼2=자료...)
View(st)
treemap(st,
        index=c("stname"),
        vSize = "Area",
        vColor = "Income",
        type="value",
        title="미국주별수입")


#산점도에 Bubble 추가 (bubble chart) 1)버블차트
symbols(st$Illiteracy, st$Murder,    #원의 x, y 좌표
        circles = st$Population,     #원의 반지를
        inches = 0.3,               #원 크기 조절값
        fg="white",                     #원 테두리 색
        bg="lightgray",            #원 바탕색
        lwd=1.5,                    #원 테두리선 두께
        xlab='rate of Illiteracy',
        ylab = "crime(murder) rate",
        main= "Illiteracy and Crime")
text(st$Illiteracy, st$Murder,      #텍스트 출력 x,y 좌표
     rownames(st),                  #출력할 text
     cex=0.6,                        #폰트크기
     col ="brown")                    #폰트컬러


#ggplot 이용 2)버블차트 The R Graph Gallery :http://r-graph-gallery.com/index.html
# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)

# The dataset is provided in the gapminder library
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

# Interactive version
p <- data %>%
  mutate(gdpPercap=round(gdpPercap,0)) %>%
  mutate(pop=round(pop/1000000,2)) %>%
  mutate(lifeExp=round(lifeExp,1)) %>%
  
  # Reorder countries to having big bubbles on top
  arrange(desc(pop)) %>%
  mutate(country = factor(country, country)) %>%
  
  # prepare text for tooltip
  mutate(text = paste("Country: ", country, "\nPopulation (M): ", pop, "\nLife Expectancy: ", lifeExp, "\nGdp per capita: ", gdpPercap, sep="")) %>%
  
  # Classic ggplot
  ggplot( aes(x=gdpPercap, y=lifeExp, size = pop, color = continent, text=text)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="none")

# turn ggplot interactive with plotly
pp <- ggplotly(p, tooltip="text")
pp