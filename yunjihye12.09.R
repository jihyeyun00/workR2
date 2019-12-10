# * 실습 결과를 R Script file로 제출
# * R Script file 이름은 "영문본인이름_제출일날짜.R" 부여하여 제출
# * R Script file의 처음에 주석으로 본인 이름과 작성일/제출일 기록
# 
# 문1)
# R에서 제공하는 mtcars 데이터셋에서 gear(기어의 수)에 대해 ggplot으로
# 막대그래프를 작성하시오. 단, 제목과 x축 레이블은 ‘기어의 수’, y축 레이블
# 은 ‘빈도수’로 나타내시오.
#
str(mtcars)
head(mtcars)
ggplot(mtcars,aes(x=gear))+            #ggplot,+,mapping 생략도 가능
geom_bar(              #stat :y, width: 폭, fill :막대색깔
         width = 0.7,
         fill="steelblue")+
ggtitle("기어수와 빈도")+
  labs(x="기어의 수",y="빈도수")

# 문2)
# R에서 제공하는 mtcars 데이터셋에서 cyl(실린더의 수)에 대해 막대 색이
# 초록색인 막대그래프를 ggplot으로 작성하시오.
ggplot(mtcars,aes(x=cyl))+            #ggplot,+,mapping 생략도 가능
  geom_bar(width = 0.7,
           fill="green")+
  ggtitle("실린더")+
  theme(plot.title = element_text(size = 25,   
                                  face = "bold",
                                  colour="steelblue"))
  labs(x="실린더의 수")
# 
# 문3) 
# R에서 제공하는 mtcars 데이터셋에서 mpg(연비)에 대해 구간 간격이 5.0
# 인 히스토그램을 ggplot으로 작성하시오. 
# 
ggplot(mtcars,aes(x=mpg))+
  
  geom_histogram(binwidth = 5.0)        #답 잘보자


# 문4)??
# R에서 제공하는 trees 데이터셋의 Girth(나무 둘레)에 대해 ggplot으로
# 히스토그램을 작성하시오. 여기에서는 히스토그램의 제목, x축 레이블, y축
# 레이블을 한글로 표시하시오. (구간 간격은 3.0, 막대의 색은 steelblue로 한다.)
str(trees)
head(trees)
ggplot(trees,aes(x=as.factor(Girth)))+       
  geom_histogram(binwidth = 3.0)+        
  ggtitle("나무둘레")+
  theme(plot.title = element_text(size=25,
                                  face="bold",
                                  colour = "s"))  
  
# 문5)
# R에서 제공하는 mtcars 데이터셋에서 mpg(연비)를 x축으로 하고, wt(중
#                                             량)를 y축으로 하는 산점도를 ggplot으로 작성하시오. (단, 점의 색은 gear의
#                                                                                수에 따라 다르게 표시한다.)

  ggplot(data=mtcars,mapping=aes(x=mpg,       
                              y=wt))+     
  geom_point()      
  
# 문6)
# R에서 제공하는 mtcars 데이터셋에서 mpg(연비)에 대해 ggplot으로 상
# 자그림을 작성하되, cyl(실린더 수)에 따라 그룹을 나누어 작성하시오.
  ggplot(data = mtcars,mapping = aes(x=as.factor(cyl),y=mpg))+   
    geom_boxplot(fill="green")
  
 
# 문7) 
# 다음은 2015년부터 2026년도까지의 예상 인구수 추계 자료이다. 연도를
# x축으로 하여 ggplot으로 선그래프를 작성하시오.
# 
# 연도		총인구 (천명)		연도		총인구 (천명)
# 2015		51014				2021		52123
# 2016		51245				2022		52261
# 2017		51446				2023		52388
# 2018		51635				2024		52504
# 2019		51811				2025		52609
# 2020		51973				2026		52704	
# 
year<-2015:2026
popu<-c(51014,51245,51446,51635,51811,51973,52123,52261,52388,52504,52609,52704)

yp<-data.frame(year,popu)
head(yp)
ggplot(yp,aes(x=year,y=popu))+
  geom_line(col="red")

# 문8)
# 다음과 같이 데이터셋 us를 생성한 후 물음에 답하시오. 여기서 state.x77
# 은 미국 50개 주의 통계정보가, state.division은 미국 50개 주의 지역 구분
# (예: 북부, 중부, 남부……) 정보가 저장된 데이터셋이다.
# 
# us <- data.frame(state.x77, state.division)
st77 <- data.frame(state.x77)
stdi<-data.frame(state.division)
# (1) 미국 50개 주에 대해 각각의 주들이 지역구분별로 묶인 트리맵을 작성하시오.
# 또한, 타일의 면적은 Population(인구수), 타일의 색은 Income(소득)으로 나타내고,
# 각각의 타일에는 주의 이름을 표시하시오. 마지막으로 이 트리맵에서 관찰할 수 있
# 는 것이 무엇인지 설명하시오


data(us)
head(us)
str(us)
View(us)
treemap(us,
        index = c("state.division",""),   #계층구조 콤마로 여러개해도 돼?
        vSize = "population",            #타일크기
        vColor="Income",                     #타일컬러
        type="value",                   #타일컬러링 방법(값이 높은것은 진하게)
        bg.labels="yellow",             #레이블배경색
        title="us")           

st<-data.frame(state.x77)
st2<-data.frame(st,stname=rownames(stdi)) #data.frame(컬럼1=자료, 컬럼2=자료...)
us<-data.frame(st,st2)
treemap(us,
        index=c("state.division",),
        vSize = "Area",
        vColor = "Income",
        type="value",
        title="미국주별수입")


# (2) 미국 50개 주에 대해 각각의 주들이 지역구분별로 묶인 트리맵을 작성하시오.
# 또한, 타일의 면적은 HS.Grad(고등학교 졸업률), 타일의 색은 Murder(범죄률)로 나타
# 내고, 각각의 타일에는 주의 이름을 표시하시오. 마지막으로 이 트리맵에서 관찰할
# 수 있는 것이 무엇인지 설명하시오.
# 
# (3) us 데이터셋에 대해 x축은 Income(소득), y축은 Illiteracy(문맹률), 원의 크기는
# Population(인구수), 원의 색은 green(초록색), 원 내부에는 주의 이름을 표시한 버
# 블차트를 작성하시오. 또한 이 버블차트에서 관찰할 수 있는 것이 무엇인지 설명하
# 시오.
# 
# (4) us 데이터셋에 대해 x축은 Illiteracy(문맹률), y축은 Murder(범죄률), 원의 크기
# 는 Area(면적), 원의 색은 green(초록색), 원 내부에는 주의 이름을 표시한 버블차트
# 를 작성하시오. 또한 이 버블차트에서 관찰할 수 있는 것이 무엇인지 설명하시오.
