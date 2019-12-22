#filter():행추출     df를 class==1 조건에 만족하는 것으로 
df%>%filter(class==1)
df%>%filter(class==2)    
df%>%filter(class!=1)
df%>%filter(class!=2)


df%>%filter(science>70)
df%>%filter(math<50)

df%>%filter(class==1 & math>=50)
df%>%filter(math>=50 | english>=90)
df%>%filter(class==1 %in% c(1,2,5))

class1<-df %>% filter(class==1)    #split 함수는 list 형식으로 추출,파이프는 따로 종류도 
class2<-df %>% filter(class==2)    
class1
class2


#select():변수추출
df %>% select(math)
df %>% select(science)

df %>% select(class,math,science)
df %>% select(-math)


#dplyr 함수 조합
df%>%filter(class==1)%>%select(science)

df%>%select(id,science)%>%head

df%>%select(id,science)%>%sum
df%>%select(science)%>%sum

df%>%select(id,science)%>%max

#arrange():정렬
df%>%arrange(science)
df%>%arrange(desc(science))


#mutate():파생변수추가
df%>%
  mutate(total=math+english+science)%>%head


df%>%
  mutate(total=math+english+science,average=(math+english+science)/3)%>%head

df%>%
  mutate(grade=ifelse(science>=60,'pass','fail'))%>%head

df%>%
  mutate(total=math+english+science,
         average=(math+english+science)/3 )%>%
  mutate(grade=ifelse(science>60,'pass',
                      ifesle(average<60, 'fail',
                             'normal')))%>%head  

df%>%
  mutate(total=math+english+science,
         average=(math+english+science)/3 )%>%arrange(desc(average))%>%head

df.sort<-df%>%          #필요하다면 변수에 담고
  mutate(total=math+english+science,
         average=(math+english+science)/3 )%>%arrange(desc(average))%>%head
df.sort




#summary():집단별 요약
#group_by:집단별 나누기
df%>%summarise(mean_math=mean(math))

df%>%group_by(class)%>%
  summarise(mean_math=mean(math),
            mean_english=mean(english),
            mean_science=mean(science),
            n=n())                       #n()은 빈도수를 계산해 table이랑 비슷, tibble은 data.frame이랑 비슷
#db= double, 숫자


install.packages("ggplot2")

str(ggplot2::mpg)     # :: 의 의미ggplot2 라는 애가 가지고 있는 mpg 
mpg<-data.frame(ggplot2::mpg)
dim(mpg)
str(mpg)
head(mpg)
View(mpg)


mpg%>%
  group_by(manufacturer,drv)%>%
  summarise(mean_cty=mean(cty))%>%
  head(10)

mpg%>%
  group_by(manufacturer,drv)%>%
  filter(class=='suv')%>%
  mutate(tot=(cty+hwy)/2)%>%
  summarise(mean_tot=mean(tot))%>%
  arrange(desc(mean_tot))%>%
  head(5)


#data 합치기 -공통변수가 있다는 가정하에   ##전체질문
# left_join(): 가로로합치기(변수추가)
# inner_join(): 가로로합치기(변수추가)
# full_join(): 가로로합치기(변수추가)
# bind_rows():세로로 합치기(데이터추가)

df1<-data.frame(id=c(1,2,3,4,5),         #??
                midterm=c(60,80,70,90,85))

df1
df2<-data.frame(id=c(1,2,3,4,5),
                final=c(60,80,70,90,85))
df2
total<-left_join(df1,df2,by="id")
total

df1<-data.frame(id=c(1,2,3),
                address=c("서울","부산","제주"),
                stirngsFactors=F)   #팩터로 하지마
df2<-data.frame(id=c(1,2,4),
                gender=c("남","여","남"))


df_left<-left_join(df1,df2,by='id')
df_left
df_inner<-inner_join(df1,df2,by='id')
df_inner
df_full<-full_join(df1,df2,by='id')
df_full
df_bind<-bind_rows(df1,df2)
df_bind





df1<-data.frame(id=c(1,2,3,4,5),
                test=c(60,80,70,90,85))
df2<-data.frame(id=c(1,2,3,4,5),
                test=c(60,80,70,90,85))

df_all<-bind_rows(df1,df2)
df_all


install.packages("psych")
library(psych)

summary(mtcars)         #요약할때 쓰는 함수
describe(mtcars)

install.packages("descr")
require(descr)

df<-data.frame(id=c(1,2,4),
               gender=c("남","여","남"))
table(df$gender)

freq(df$gender)   #그래프를 자동으로  
freq(df$gender,plot=F)