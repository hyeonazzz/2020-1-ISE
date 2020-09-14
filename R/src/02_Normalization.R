# 1. 라이브러리 로딩 ----
library(tidyverse) #
library(RNOmni)

# 2. 정규화 함수 정의 ----
data_normalization <- function(file_path){
  
  # 정규화 함수
  normalization <- function(x) {
    norm_x <- (x - mean(x)) / sd(x)
    return(norm_x)
  }
  
  # Inverse Normal Transformation
  inv_normal_trans <- function(u, k=3/8){
    n <- length(u)
    r <- rank(u)
    return(qnorm((r-k)/(n-2*k+1)))
  }
  
  # 데이터 로딩
  loaded_data <- read.csv(file_path,
                          fileEncoding = "EUC-KR",
                          stringsAsFactor = FALSE)
  # 데이터 정규화
  norm_data <- loaded_data %>%
    select(lot_no, timestamp, high_velocity:clamping_force, quality) %>%
    mutate_at(vars(-lot_no, -timestamp, -quality), funs(inv_normal_trans))

  return(norm_data)
}

# 3. 데이터 정규화 후 저장----
in_folder_path <- "./data/test_data/"
out_folder_path <- "./data/norm_data/"
file_paths <- list.files(in_folder_path) 
file_paths <- file_paths[grep("filtered", file_paths)]


for (file_path in file_paths){
  temp_path <- paste0(in_folder_path, file_path)
  print(temp_path)
  temp_data <- data_normalization(temp_path)
  
  temp_path <- paste0(out_folder_path, file_path)
  print(temp_path)
  write.csv(temp_data, file = temp_path, row.names = FALSE, fileEncoding = "EUC-KR")
}

# 메모리 정리
rm(list=ls())
gc()
