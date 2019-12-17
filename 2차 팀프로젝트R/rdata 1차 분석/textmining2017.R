#
# text mining
#

Sys.setenv(JAVA_HOME = 'C:/Program Files/Java/jre1.8.0_231')

#library
library(wordcloud)
library(wordcloud2)
library(KoNLP)
library(RColorBrewer)

library(dplyr)
library(ggplot2)

# 파일 불러오기
setwd('C:/WorkR/teamproject')
word_data <- readLines('2017외국인방문.txt', encoding = 'UTF-8')
word_data

# 사전 로딩
useSejongDic()

# 명사추출
word_data2 <- sapply(word_data, extractNoun, USE.NAMES = F)
head(word_data2)
tail(word_data2)

# 사전등록
add_words <- c(1:100, '.', '맛집여행','(',')', '\'', '\'.')
buildDictionary(user_dic = data.frame(add_words,
                                      rep('ncn', length(add_words))),
                replace_usr_dic = T)
get_dictionary('user_dic')

# 추가후 명사추출
word_data2 <- sapply(word_data, extractNoun, USE.NAMES = F)
head(word_data2)
tail(word_data2)

# 백터
undata <- unlist(word_data2)
head(undata)
tail(undata)

# 빈도
word_table <- table(undata)
word_table
head(word_table)
tail(word_table)

# 필터링
undata2 <- undata[nchar(undata) >= 2]
head(undata2)
tail(undata2)

num <- c(1:100)
num_n <- as.character( num[1:100] )

for (i in 1:length(num)) {
    undata2 <- gsub(num_n[i], '', undata2)
}
undata2 <- gsub('.0', '', undata2)
undata2 <- gsub('비율', '', undata2)
undata2 <- gsub('월별', '', undata2)
undata2 <- gsub('단위', '', undata2)
undata2 <- gsub('연령', '', undata2)

word_table2 <- table(undata2)
head(word_table2)
tail(word_table2)

word_table2 <- sort(word_table2, decreasing = T)
head(word_table2)
word_table2 <- word_table2[-c(1,2,3,4,7,11,25)]

word_table2

# wordcloud
pal3 <- brewer.pal(9, 'Blues')[5:9]
wordcloud2(word_table2, color = pal3, size = 2)

