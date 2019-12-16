# 문1)
# 20대 국회 개원 여·야 3당 대표 국회연설문에 대해 각각 워드클라우드를
# 작성하시오.
# 예제소스 파일은 ‘ex_10-1.txt’, ‘ex_10-2.txt’, ‘ex_10-3.txt’이다.
#
library(wordcloud)
library(wordcloud2)
library(KoNLP)
library(RColorBrewer)       #사전설정해야한다.

library(dplyr)
library(ggplot2)


setwd("D:/workR2")
text2<-readLines("ex_10-1.txt",encoding = "UTF-8")
text2

jh<-sapply(text2, extractNoun,USE.NAMES = F) #명사추출 Use.names :
jh

jh.n<-unlist(jh)   #list->vector 로 변환 
jh.count<-table(jh.n)
jh.count

ch.n2<-ch.n1[nchar(ch.n1)>1]
ch.n2

sort.jh<-sort(jh.count,decreasing = T)[1:10]
sort.jh

sort.jh.n<-sort.jh[-1]
sort.jh.n

sort.jh.n<-gsub('한','',sort.jh.n)    #??'하지' 를 ''로 바꿔라
sort.jh.n<-gsub('들','',sort.jh.n)
sort.jh.n<-gsub('것','',sort.jh.n)
wordcount<-table(sort.jh.n)

pal3<-brewer.pal(9,'Blues')[7:9]   #파랑색 팔레트색이 최대 9가지인데 5~9까지의 색
wordcloud(names(sort.jh.n),       #단어
          freq= sort.jh.n,         #단어빈도
          scale = c(6,0.7),       #단어폰트크기(최대, 최소)
          min.freq = 3,           #단어최소빈도
          random.order =F,       #단어출력위치 T는 무작위로 나옴
          rot.per = .1,           #90도 회전 단어 비율, 10%만큼 단어 들을 회전해라
          colors = pal3)  

#
setwd("D:/workR2")
text1<-readLines("ex_10-2.txt",encoding = "UTF-8")
text1


mn<-sapply(text1, extractNoun,USE.NAMES = F)
mn

mn1<-unlist(mn)
w.count<-table(mn1)
sort.mn<-sort(w.count,decreasing = T)[1:10]
sort.mn
sort.mnM<-sort.mn[-1] 
mn2<-mn1[nchar(mn1)>1][1:10]
mn2
w.count2<-table(mn2)
w.count2
pal3<-brewer.pal(9,'Blues')[5:9]
wordcloud(names(w.count2),       #단어
          freq= w.count2,         #단어빈도
          scale = c(6,0.7),       #단어폰트크기(최대, 최소)
          min.freq = 3,           #단어최소빈도
          random.order =T,       #단어출력위치 T는 무작위로 나옴
          rot.per = .1,           #90도 회전 단어 비율, 10%만큼 단어 들을 회전해라
          colors = pal3) 

#
setwd("D:/workR2")
text2<-readLines("ex_10-3.txt",encoding = "UTF-8")
text2

mn3<-sapply(text2, extractNoun,USE.NAMES = F)
mn3
mn4<-unlist(mn3)   #list->vector 로 변환 
w.count3<-table(mn4)
sort.mn3<-sort(w.count3,decreasing = T)[1:10]
sort.mn3
sort.mn4<-sort.mn[-1]
sort.mn4

mn5<-mn4[nchar(mn4)>1][1:10]
mn5

w.count3<-table(mn5)
w.count3

pal4<-brewer.pal(11,'BrBG')[5:8]
wordcloud(names(w.count3),       #단어
          freq= w.count3,         #단어빈도
          scale = c(6,0.7),       #단어폰트크기(최대, 최소)
          min.freq = 3,           #단어최소빈도
          random.order =T,       #단어출력위치 T는 무작위로 나옴
          rot.per = .1,           #90도 회전 단어 비율, 10%만큼 단어 들을 회전해라
          colors = pal4) 
brewer.pal.info
# 문2)
# 스티브 잡스의 스탠포드 대학 졸업식 연설문에 대해 워드클라우드를 작성
# 하시오.
# Tip. 예제소스 파일은 ‘ex_10-4.txt’이다.
# 
# 문3) 
# 오바마 대통령의 데통령 당선 연설문에 대해 워드클라우드를 작성하시오
# Tip. 예제소스 파일은 ‘ex_10-5.txt’이다.
