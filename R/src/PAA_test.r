#공부용

# 1. 라이브러리 로딩 ----
library(tidyverse)
library(TSclust)



# 2. PAA 함수 정의 ----
convert_to_PAA <- function(file_path, paa_ratio = 2){
  print('start')
  start <- proc.time()
  #현재 프로세스에 대해 총 경과 된 CPU시간을 start에 저장
  loaded_data <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "EUC-KR")
  #lot_info <- unique(loaded_data$lot_no) #중복된 인덱스 (reg_date) 제거해서 lot_info에 넣기
  #print(length(lot_info)) #lot_info의 행수를 출력
  
  paa_data <- data.frame() 
  paa_lotno <- vector() #vector()로 연결함수 생성(공벡터?)
  #-> 기본 디폴트 설정으로 '논리'자료형을 갖는 공벡터(즉, 길이 0) 생성
  time_index <- vector() #vector()로 연결함수 생성(공벡터?)
  #-> 기본 디폴트 설정으로 '논리'자료형을 갖는 공벡터(즉, 길이 0) 생성
  
  #for (lot in lot_info){ #중복 제거한 인덱스에서 하나씩 인덱스를 빼옴
    
  temp_data <- loaded_data %>%
    select(-reg_date, -Alarm) 
      #filter(lot_no == lot) %>% #loaded_data에서 중복 제거한 인덱스와 기존 인덱스가 일치하면 추출하라
      #select(-lot_no, -reg_date, -Alarm)
      #일치할 때, 인덱스와 시간, quality행을 빼고 추출한 데이터를 temp_data에 넣어라
    
  
  temp_class <- loaded_data %>%
    select(Alarm) %>%
      unique %>%
        as.character
  #filter(lot_no == lot) %>%  #loaded_data에서 중복 제거한 인덱스와 기존 인덱스가 일치하면 추출하라
  #select(RS40018) #%>% #quality를 선택해서 
      #unique %>% #중복된 걸 제거하고
      #as.character #문자형으로 자료형변환 한 데이터를 temp_class에 넣어라. 

  print(nrow(temp_data))
  w <- round(nrow(temp_data) / paa_ratio)
  print(w)
  
    #temp_data의 행수를 paa_ratio=2로 나눈값을 반올림해라!
  #paa_lotno <- c(paa_lotno, rep(lot, each = w))
    #paa_lotno rep(lot, eace=w) lot을 각각 원소마다 w만큼 반복해라
    
  print('paa_lotno')
  print(paa_lotno)

  time_index <- c(time_index, 1:w)
    #1부터 구한 w만큼 time_index의 공벡터 형태로 time_index에 넣기
    
  temp_paa <- tbl_df(sapply(temp_data, function(x) PAA(x, w)))
    #tbl_df <- 큰 데이터 프레임을 보기 쉽게 바꿔줌
    #sapply 벡터, 데이터 프레임 등에 함수를 적용하고 그 결과를 벡터 또는 행렬로 반환함
    #temp_data 이 데이터를 함수에 적용하고 그 함수에는 PAA(x, w)라는 인자를 전달함. 결과 값을 벡터로 반환함.
  temp_paa <- temp_paa %>% mutate(Alarm = temp_class)                
    
  paa_data <- bind_rows(paa_data, temp_paa)
  #}
  
  #paa_data <- data.frame(lot_no = as.character(paa_lotno),
  paa_data <- data.frame(time_index = time_index,
                         paa_data)
  time_elapsed <- proc.time() - start
  print(time_elapsed)
  
  return(paa_data)
}


# 3. 데이터 PAA 후 저장----
in_folder_path <- "./data/norm_data/"
out_folder_path <- "./data/paa_data/"
file_paths <- list.files(in_folder_path)

for (file_path in file_paths[2]){
  temp_path <- paste0(in_folder_path, file_path)
  print(temp_path)
  temp_data <- convert_to_PAA(temp_path, paa_ratio = 2)
  
  temp_path <- paste0(out_folder_path, file_path)
  print(temp_path)
  write.csv(temp_data, file = temp_path, row.names = FALSE, fileEncoding = "EUC-KR")
}

# 메모리 정리
rm(list=ls())
gc()