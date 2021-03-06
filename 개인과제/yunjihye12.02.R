#문1
#1-1 
# 문1) 
# 어떤 학급의 성별이 다음과 같을 때 주어진 문제를 해결하기 위한 R 코드를 작성하시오.
# 
# F F F M M F F F M M
# 
# 1. 위의 자료를 gender 벡터에 저장하시오.
gender <- c('f','f','f','m','m','f','f','f','m','m'); gender
# 2. gender에 있는 값들에 대해 도수분포표를 작성하여 출력하시오.
table(gender)
# 3. gender에 있는 값들에 대해 막대그래프를 작성하여 출력하시오.
G<- table(gender)

barplot(G,main='gender')
# 4. gender에 있는 값들에 대해 원그래프를 작성하여 출력하시오.

pie(G,main='gender') 



#문2
# 문2)
# 좋아하는 계절에 대한 조사 결과가 다음과 같을 때 주어진 문제를 해결하기 위한 R 코드를 작성하시오.

# 여름 겨울 봄 가을 여름 가을 겨울 여름 여름 가을

# 1. 위의 자료를 season 벡터에 저장하시오.
season <- c('여름','겨울','봄','가을','여름','가을','겨울','여름','여름','가을'); season
# 2. season에 있는 값들에 대해 도수분포표를 작성하여 출력하시오.
table(season)
# 3. season에 있는 값들에 대해 막대그래프를 작성하여 출력하시오.
Ta1 <- table(season)

barplot(Ta1,main = 'season')
# 4. season에 있는 값들에 대해 원그래프를 작성하여 출력하시오.

pie(Ta1,main = 'season')



#문3

# 문3)
# 학생 A의 과목별 성적이 다음과 같을 때 각 문제를 해결하기 위한 R 코드를 작성하시오.
# 
# KOR ENG ATH HIST SOC MUSIC BIO EARTH PHY ART
# 90 	85 	73 	80 	 85  65    78  50    68  96
# 
# 1. 위 데이터를 score 벡터에 저장하시오(과목명은 데이터 이름으로 저장).
score <- c(90,85,73,80,85,65,78,50,68,96)
names(score) <- c('KOR','ENG','ATH','HIST','SOC','MUSIC','BIO','EARTH','PHY','ART')
# 2. score 벡터의 내용을 출력하시오.
score
# 3. 전체 성적의 평균과 중앙값을 각각 구하시오.
mean(score);mean

median(score)

# 4. 전체 성적의 표준편차를 출력하시오.
sd(score)
# 5. 가장 성적이 높은 과목의 이름을 출력하시오.
which.max(score) #선생님 답 :names( score[ score == max( score ) ] )

# 6. 성적에 대한 상자그림을 작성하고, 이상치에 해당하는 과목이 있으면 출력하시오.
boxplot(score,main='학생A') #박스플롯:데이터가 분포가 어떤지있지 알수 
boxplot.stats(score)$out  

#이상치 없음

# 7. 다음 조건을 만족하는 위 성적에 대한 히스토그램을 작성하시오.
# (그래프 제목: 학생 성적, 막대의 색: 보라색)
str(score)

hist(score, main='학생성적',col='purple') 



# 문4
# R에서 제공하는 mtcars 데이터셋에 대해 다음 문제를 해결하기 위한 R코드를 작성하시오.
#  #선생님은 먼저 확인 :
#dim( mtcars )
#str( mtcars ) :,data frame 인지,num 은 연속형, 도수분포해보고 
#head( mtcars )
#tail( mtcars )

# 1. 중량(wt)의 평균값, 중앙값, 절사평균값(절사범위: 15%), 표준편차를 각각 구하시오.
mtcars
m.w <- mtcars$wt  ; mtcars[,6]
mean(m.w)
median(m.w)
sd(m.w)
mean(m.w= 0.15)
# 2. 중량(wt)에 대해 summary( ) 함수의 적용 결과를 출력하시오.
summary(m.w)   #최소, 최대, 중앙 값등을 알려줘서 한번에 알 수 있다.

# 3. 실린더수(cyl)에 대해 도수분포표를 출력하시오.
c.l <- mtcars$cyl; c.l
table(c.l)

# 4. 앞에서 구한 도수분포표를 막대그래프로 출력하시오.
barplot(c.l,main = '실린더 수')  #팩터(숫자)가 아니지만 성격을 보면 범주형이라는 것
                                    #그래서 도수 분포표를 쓸 수 있다. 
# 5. 중량(wt)의 히스토그램을 출력하시오.
hist(m.w)

# 6. 중량(wt)에 대해 상자그림을 출력하시오.(단, 상자그림으로부터 관찰할 수 있는정보를 함께 출력하시오.)
boxplot(m.w)
boxplot.stats(m.w)
boxplot.stats(m.w)$stats  #정상범위 사분위수
boxplot.stats(m.w)$n      #관측치 개수
boxplot.stats(m.w)$conf   #중앙값 신뢰구간
boxplot.stats(m.w)$out    #이상치 특이값 목록
# 7. 배기량(disp)에 대한 상자그림을 출력하시오.(단, 상자그림으로부터 관찰할 수 있는 정보를 함께 출력하시오.)
d.i <- mtcars$disp ;d.i

boxplot(d.i)
boxplot.stats(d.i)
boxplot.stats(d.i)$stats  #정상범위 사분위수
boxplot.stats(d.i)$n      #관측치 개수
boxplot.stats(d.i)$conf   #중앙값 신뢰구간
boxplot.stats(d.i)$out    #이상치 특이값
# 8. 기어수(gear)를 그룹 정보로 하여 연비(mpg) 자료에 대해 상자그림을 작성작성하고,
m.g <- mtcars$gear

boxplot(m.g)
boxplot.stats(m.g)
boxplot.stats(m.g)$stats  #정상범위 사분위수
boxplot.stats(m.g)$n      #관측치 개수
boxplot.stats(m.g)$conf   #중앙값 신뢰구간
boxplot.stats(m.g)$out    #이상치 특이값

# 각 그룹의 상자그림을 비교하여 관찰할 수 있는 것이 무엇인지 나타내시오. 연비가 나쁘다, 적당하다, 좋다

#일변량중 그룹으로 구성된 자료의 상자그림
boxplot(mpg~gear, #그룹으로 묶일것 물결 다음
        data=mtcars, 
        main='연비')
boxplot(mtcars$mpg~mtcars$gear,
        main='연비') 


