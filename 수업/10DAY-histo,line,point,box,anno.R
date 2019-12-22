#연속형 데이터-히스토그램
#누적히스토그램이 됌
ggplot(iris,aes(x=Sepal.Width, fill=Species,  #fill에는 범주형을,species 는 범주형 문자형이지만 숫자로 자동 변환 1,2,3...
                color=Species))+              
  geom_histogram(binwidth = 0.3,)+  #dodge품종별로 각각그리기: 즉, 색깔별로 막대그래프를 그려라
  theme(legend.position = "top") +  #top 범례의 위치
  scale_fill_manual(values=fillPalette)

#색깔별로 막대그래프가 됌
ggplot(iris,aes(x=Sepal.Width, fill=Species,  
                color=Species))+              
  geom_histogram(binwidth = 0.3,position = "dodge")+  
  theme(legend.position = "top") +  
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



