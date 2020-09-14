#공부용

# 1. 라이브러리 로딩 ----
library(tidyverse)

#tidyverse 란?
# 기본 R이 재설계한 향상된 도구로 바꾸는 방법
# 일반적인 데이터 표현과 API 디자인을 공유하기 때문에 조화롭게 작동하는 패키지 세트

library(RNOmni)

#모르겠음

# 2. 정규화 함수 정의 ----
data_normalization <- function(file_path){ 
  print('head')
  head(file_path)
  # 정규화 함수 z-정규화
  normalization <- function(x) { 
    norm_x <- (x - mean(x)) / sd(x)
    return(norm_x)
  }
  
  # Inverse Normal Transformation(int)
  inv_normal_trans <- function(u, k=3/8){
    n <- length(u) #dataframe의 열의 개수를 셀 때 사용하는 함수
    r <- rank(u)
    #순위를 매김. 순위대로 정렬하는 것이 아니라 그 해당 값이 몇번째 순위인지 써줌
    return(qnorm((r-k)/(n-2*k+1))) #return ()계산한 값이 정규분포확률에 해당하는 값을 알려줌

  }
  
  # 데이터 로딩
  loaded_data <- read.csv(file_path,
                          fileEncoding = "EUC-KR",
                          stringsAsFactor = FALSE)
  # 데이터 정규화
  norm_data <- loaded_data %>% # %>% = chain() 함수 or %>% =출력하라?
    select(lot_no, timestamp, high_velocity:clamping_force, quality) %>%
    #() <- 선택한 열들을 추출
    mutate_at(vars(-lot_no, -timestamp, -quality), funs(inv_normal_trans))
    #mutate_at <- 열을 추가하는 여러 변수를 사용하는 경우에 사용하는 함수임
    #vars(-lot,no,-timestatmp,-quality) <- 이 세 변수 빼고 분산을 구하라
    #funs(inv_normal_trans)
    #즉, loaded_data에서 열데이터(?)별로 int정규화를 하고 싶을때!

  #norm_data <- loaded_data
  #norm_data <- select(lot_no, timestamp, high_velocity:clamping_force, quality)
  #norm_data <- mutate_at(vars(-lot_no, -timestamp, -quality), funs(inv_normal_trans))



  return(norm_data)
}

# 3. 데이터 정규화 후 저장----
in_folder_path <- "./data/test_data/"
out_folder_path <- "./data/norm_data/"
file_paths <- list.files(in_folder_path)
#폴더 내 파일들 이름을 list-up하여 객체로 만들기
file_paths <- file_paths[grep("P.csv", file_paths)]
#grep <- 찾아서 행 번호 출력
#file_paths중에서 'filtered'라는 텍스트 검색

for (file_path in file_paths){
#file_paths에서 찾은 것을 하나씩 file_path로 꺼냄
  temp_path <- paste0(in_folder_path, file_path)
  #in_folder_path과, file_path를 공백 없이 출력하여 temp_path에 대입
  #print(temp_path) #ex) "./data/0701P.csv"
  temp_data <- data_normalization(temp_path)
  #temp_data를 정규화함수에 넣어서 return 값을 temp_data에 대입

  #계산된 정규분포확률 값들이 temp_data에 들어가게 됨.
  
  temp_path <- paste0(out_folder_path, file_path)
  #out_folder_path와, file_path를 공백 없이 출력하여 temp_path에 대입
  print(temp_path)

  write.csv(temp_data, file = temp_path, row.names = FALSE, fileEncoding = "EUC-KR")
  #temp_data를 temp_path(이름,경로)로 행이름은 제외하고 "EUC-KR"인코딩으로 csv 파일 저장
}

# 메모리 정리
rm(list=ls())
gc() #가비지컬렉션 수행
