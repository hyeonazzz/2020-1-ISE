# 1. 라이브러리 로딩 ----
library(tidyverse)

# 2. 데이터 추출 함수 정의 ----
extract_paa_meta <- function(file_path){
  loaded_data <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "EUC-KR")
  temp_data <- loaded_data %>% select(reg_date, Alarm)
  return(temp_data)
}

extract_paa_data <- function(file_path){
  loaded_data <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "EUC-KR")
  temp_data <- loaded_data %>% select(-reg_date)
  return(temp_data)
}


# 3. 데이터 처리 후 저장----
in_folder_path <- "./data/test_data/"
out_folder_path <- "./data/paa_data/"
file_paths <- list.files(in_folder_path)

for (file_path in file_paths[2]){
  temp_path <- paste0(in_folder_path, file_path)
  print(temp_path)
  temp_data <- extract_paa_data(temp_path)
  temp_path <- paste0(out_folder_path, "data_", file_path)
  print(temp_path)
  write.csv(temp_data, file = temp_path, row.names = FALSE, fileEncoding = "EUC-KR")
  
  temp_path <- paste0(in_folder_path, file_path)
  print(temp_path)
  temp_data <- extract_paa_meta(temp_path)
  temp_path <- paste0(out_folder_path, "meta_", file_path)
  print(temp_path)
  write.csv(temp_data, file = temp_path, row.names = FALSE, fileEncoding = "EUC-KR")
}

# 메모리 정리
rm(list=ls())
gc()