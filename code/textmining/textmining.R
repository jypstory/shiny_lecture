#install.packages(c('rvest','httr','KoNLP','stringr','tm','qgraph','xml2','dplyr','networkD3'))
library(rvest)
library(httr)
library(KoNLP)
library(stringr)
library(tm)
library(qgraph)
library('xml2')
library(dplyr)
library(networkD3)

#par(family="Apple SD Gothic Neo")   ## for mac

url_base <- 'https://movie.daum.net/moviedb/grade?movieId=99611&type=netizen&page='   # 크롤링 대상 URL
all.reviews <- c() 

for(page in 1:300){                            ## 300페이지 까지만 수집 (본인이 나름대로 설정) 
  url <- paste(url_base, page, sep='')         ## url_base의 뒤에 페이지를 1~n 까지 늘리면서 접근
  htxt <- read_html(url)                       ## html 코드 불러오기
  comments <- html_nodes(htxt, 'div') %>% html_nodes('p')  ## comment 가 있는 위치 찾아 들어가기 
  reviews <- html_text(comments)               ## 실제 리뷰의 text 파일만 추출
  reviews <- repair_encoding(reviews, from = 'utf-8')  ## 인코딩 변경
  if( length(reviews) == 0 ){ break }          ## 리뷰가 없는 내용은 제거
  reviews <- str_trim(reviews)                 ## 앞뒤 공백문자 제거
  all.reviews <- c(all.reviews, reviews)       #결과값 저장
}

## 데이터 확인
all.reviews

##불필요 내용 필터링
all.reviews <- all.reviews[!str_detect(all.reviews,"평점")]   # 수집에 불필요한 단어가 포함된 내용 제거, stopWord로도 가능
Encoding(all.reviews)
options(encoding="utf-8")

## 명사/형용사 추출 함수 생성
ko.words <- function(doc){
  d <- as.character(doc)
  pos <- paste(SimplePos22(d))   ##SimplePos09
  extracted <- str_match(pos, '([가-힣]+)/[NC]')
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
}

options(mc.cores=1)    ## 단일 Core 만 활용하도록 변경 (옵션)
cps <- Corpus(VectorSource(all.reviews))  

stopWord <- c("텍스트", "분석")

tdm <- TermDocumentMatrix(cps,
                          control=list(tokenize=ko.words,    ## token 분류시 활용할 함수명 지정
                                       removePunctuation=T,  ## 구두점 제거
                                       stopwords=stopWord,   ## 불필요 단어 제거
                                       removeNumbers=T,      ## 숫자 제거
                                       wordLengths=c(4, 10), ## 단어 길이 조정, 한글은 2자
                                       weighting=weightBin   ## 단어가 텍스트에 출현하면 1, 아니면 0을 반환
                          ))  

#최종결과 확인
dim(tdm)
tdm.matrix <- as.matrix(tdm)
Encoding(rownames(tdm.matrix)) <- "UTF-8"
word.count <- rowSums(tdm.matrix)  ##각 단어별 합계를 구함
word.order <- order(word.count, decreasing=T)  #다음으로 단어들을 쓰인 횟수에 따라 내림차순으로 정렬
freq.words <- tdm.matrix[word.order[1:20], ] #Term Document Matrix에서 자주 쓰인 단어 상위 20개에 해당하는 것만 추출
co.matrix <- freq.words %*% t(freq.words)  #행렬의 곱셈을 이용해 Term Document Matrix를 Co-occurence Matrix로 변경

## 1. qgraph 시각화
qgraph(co.matrix,
       labels=rownames(co.matrix),   ##label 추가
       diag=F,                       ## 자신의 관계는 제거함
       layout='spring',              ##노드들의 위치를 spring으로 연결된 것 처럼 관련이 강하면 같이 붙어 있고 없으면 멀리 떨어지도록 표시됨
       edge.color='blue',
       vsize=log(diag(co.matrix))*2) ## diag는 matrix에서 대각선만 추출. 해당 단어가 얼마나 나왔는지를 알 수 있음. 
                                     ## vsize는 원의 크기를 결정. 단어의 빈도수를 인자로 넘김. log를 적용하여 차이를 줄여줌. 


## 2. networkD3 시각화, 출처 : https://junhewk.github.io/text/2017/08/08/cooccurence-matrix-with-Naver-blog/
node_df <- data_frame(node=rownames(co.matrix), value=as.numeric(diag(co.matrix))) %>%
mutate(idx=row_number()-1)
link_df <- as_data_frame(as.table(co.matrix)) %>%
filter(n > 4) %>%
rename(source=`Terms`, target=`Terms.1`) %>%
left_join(node_df %>% rename(source_idx=idx) %>% select(-value), by=c('source'='node')) %>%
left_join(node_df %>% rename(target_idx=idx) %>% select(-value), by=c('target'='node'))

forceNetwork(Links=as.data.frame(link_df), Nodes=as.data.frame(node_df),
             Source='source_idx', Target='target_idx',
             NodeID='node', Group='node')

forceNetwork(Links=as.data.frame(link_df), Nodes=as.data.frame(node_df),
             Source='source_idx', Target='target_idx',
             NodeID='node', Group='node', Nodesize='value', Value='n',
             radiusCalculation=JS("Math.sqrt(d.nodesize) * 3"),
             opacityNoHover=TRUE, linkDistance=100,
             zoom=TRUE, opacity=0.8, fontSize=15,
             fontFamily="Apple SD Gothic Neo")
