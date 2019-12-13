#제주시특별자치도에 제안,실질적인 결과 
#data Mining :의사 결정을 위해서 DB로 부터 규칙과 패턴을 발견하는 기법:DB-정형화 Data
#Text Mining: Text data 로부터 규칙과 패턴을 발견하는 기법, 차료처리 과정과 자료 분석 과정:Text :비정형 데이타 
#Text data 를 분석하는 데 쓰는 Word cloud :자연어 처리



#워드클라우드(Word Cloud)
#한글 워드 클라우드 절차
#1.Java 실행환경 구축
#2.자료수집(Text 자료)
  # 2.1 text file 형태로 수집 (메모장으로 열리는 파일)
  # 2.2 web scraping을 이용하여 수집

#3.명사추출
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_231')
#필요시 설치
install.packages("wordcloud")   #word cloud
install.packages("wordcloud2")  #word cloud 
install.packages("KoNLP")       #한국어처리,사전정보:꼭 설치되어야함
install.packages("RColorBrewer") #색상선택

library(wordcloud)
library(wordcloud2)
library(KoNLP)
library(RColorBrewer)       #사전설정해야한다.

library(dplyr)
library(ggplot2)

#텍스트 파일에는 (저장후)아무것도 기록되지 않은 공백 줄 마지막에 한줄이상
setwd("D:/workR2")
text<-readLines("mis_document.txt",encoding = "UTF-8")
text

#'우리말씀' 한글 사전 로딩
buildDictionary(ext_dic = 'woorimalsam')
pal2<-brewer.pal(8,'Dark2') #색상 팔레트 생성:8번 다크계열의 색을 쓰겠다
noun<-sapply(text, extractNoun,USE.NAMES = F) #명사추출 Use.names :
noun
class(noun)
#4.추출된 단어(주로 명사)에 대한 빈도수 계산 및 시각화 

noun2<-unlist(noun)   #list->vector 로 변환 
wordcount<-table(noun2)
sort.noun<-sort(wordcount,decreasing = T)[1:10]
sort.noun
sort.noun<-sort.noun[-1]   #의미없는 공백 제거
barplot(sort.noun,names.arg = names(sort.noun),
        col='steelblue',main = '빈도수 높은 단어',
        ylab='단어 빈도수')

df<-as.data.frame(sort.noun)
df
ggplot(df,aes(x=df$noun2, y=df$Freq))+
  geom_bar(stat='identity', 
           width = 0.7,
           fill='steelblue')+
  ggtitle('빈도수 높은 단어')+
  theme(plot.title = element_text(size = 25,
                                  face='bold',
                                  colour = 'steelblue',
                                  hjust = 0,
                                  vjust = 1))+
  labs(x='명사',y='단어빈도수')+
  geom_text(aes(label=df$Freq),hjust=-0.3)+  #빈도표시
  coord_flip()

#5.word cloud 작성
pal2<-brewer.pal(9,'Blues')[5:9]   #파랑색 팔레트색이 최대 9가지인데 5~9까지의 색
wordcloud(names(wordcount),       #단어
          freq= wordcount,         #단어빈도
          scale = c(6,0.7),       #단어폰트크기(최대, 최소)
          min.freq = 3,           #단어최소빈도
          random.order =F,       #단어출력위치 T는 무작위로 나옴
          rot.per = .1,           #90도 회전 단어 비율, 10%만큼 단어 들을 회전해라
          colors = pal2)          #단어 색 (팔레트 색 지정)
                                  #사전에 등재되지 않은 것들은 안 나타날 수도 있고, 불필요한 것들이 있을 수도 있다.
           

#6.전처리 과정 수행
#  
#  6.2생략된 단어를 사전에 등재

buildDictionary(ext_dic = 'woorimalsam',
                user_dic = data.frame('정치','ncn'), #ncn: 명사
                replace_usr_dic=T)
noun<-sapply(text,extractNoun,USE.NAMES = F)
noun2<-unlist(noun)

# 6.1불필요한 단어 삭제
noun2<-noun2[nchar(noun2)>1]
noun2<-gsub('하지','',noun2)    #'하지' 를 ''로 바꿔라
noun2<-gsub('때문','',noun2)
wordcount<-table(noun2)


#7.word cloud 작성
pal3<-brewer.pal(9,'Blues')[5:9]
wordcloud(names(wordcount),       #단어
          freq=wordcount,         #단어빈도
          scale = c(6,0.7),       #단어폰트크기(최대, 최소)
          min.freq = 3,           #단어최소빈도
          random.order =F,       #단어출력위치 T는 무작위로 나옴
          rot.per = .1,           #90도 회전 단어 비율, 10%만큼 단어 들을 회전해라
          colors = pal2)          #단어 색 (팔레트 색 지정)
#사전에 등재되지 않은 것들은 안 나타날 수도 있고, 불필요한 것들이 있을 수도 있다.



#애국가 형태소 분석
library(KoNLP)
useSystemDic()
useSejongDic()
useNIADic()

#애국가 가사 :
#https://mois.go.kr/frt/sub/a06/b08/nationalIcon_3/screen.do

#1.사전 설정
useSejongDic()

#2.텍스트 데이터 가져오기
word_data<-readLines('애국가(가사).txt')
word_data

#3.명사추출
word_data2<-sapply(word_data,extractNoun,USE.NAMES = F)    #'남산위에'라고 뽑힘, '남산'이라는 단어가 사전에 없을 수도 있음
word_data2                                                 #결과 확인을 해야함.

#3.1 제대로 추출되지 않은 단어를 사용자 사전에 등록
add_words<-c('백두산','남산','철갑','가을','하늘','달')
buildDictionary(user_dic = 
         data.frame(add_words, rep('ncn',length(add_words))),
         replace_usr_dic=T)
get_dictionary('user_dic')

#3.2 단어 추가 후 다시 명사 추출
word_data2<-sapply(word_data,extractNoun,USE.NAMES = F)
word_data2

#4.행렬을 벡터로 변환
undata<-unlist(word_data2)
undata

#5.사용 빈도 확인
word_table<-table(undata)
word_table

#6.필터링:두 글자 이상 단어만 선별, 공백이나 한 자리 문자를 걸러냄

undata2<-undata[nchar(undata)>=2]
undata2
word_table2<-table(undata2)
word_table2

#7.데이터 정렬
sort(word_table2,decreasing = T)

#애국가 형태 분석 완료
#가장 기본적인 전처리만 수행, 100%정확한 데이터라 볼 수 없음

#8.word cloud 작성 후 분석-!!!이거 하는게 낫지
library(wordcloud2)
wordcloud2(word_table2)

wordcloud2(word_table2,
           color='random-light',
           backgroundColor = 'black')

#8.2 모양변경
wordcloud2(word_table2,
           fontFamily = '맑은 고딕',
           size=1.2,color='random-light',
           backgroundColor='black',
           shape='star')

#8.3 선택색상반복
wordcloud2(word_table2,size=1.6,
           color = rep_len(c('red','blue'),
                          nrow(word_table2)))
wordcloud2(demoFreq,size = 1.6,
           color = rep_len(c('red','blue'),
                           nrow(word_table2)))

#8.4일정 방향 정렬
wordcloud2(word_table2,              #-는 시계방향
           minRotation = -pi/6,
           maxRotation = -pi/6,
           rotateRatio = 1)
wordcloud2(demoFreq,
           minRotation = -pi/6,
           maxRotation = -pi/6,
           rotateRatio = 1)

print(-pi/6)


