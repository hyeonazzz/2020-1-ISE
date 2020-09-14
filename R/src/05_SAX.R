# 1. 라이브러리 로딩 ----
library(tidyverse)
library(TSclust)

# 2. SAX 함수 정의 ----
convert_to_SAX <- function(file_path, alpha = 10){
  start <- proc.time()
  loaded_data <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "EUC-KR")
  
  temp_data <- loaded_data %>% select(-quality)
  
  temp_sax <- tbl_df(sapply(temp_data, function(x) convert.to.SAX.symbol(x, alpha)))
  
  sax_data <- data.frame(temp_sax,
                         quality = loaded_data$quality)
  time_elapsed <- proc.time() - start
  print(time_elapsed)
  
  return(sax_data)
}

# 3. 데이터 SAX 후 저장----
in_folder_path <- "./data/paa_post_data/"
out_folder_path <- "./data/sax_data/"
file_paths <- list.files(in_folder_path)
file_paths <- file_paths[-grep("meta", file_paths)]

for (file_path in file_paths[2]){
  temp_path <- paste0(in_folder_path, file_path)
  print(temp_path)
  temp_data <- convert_to_SAX(temp_path, alpha = 10)
  
  temp_path <- paste0(out_folder_path, file_path)
  print(temp_path)
  write.csv(temp_data, file = temp_path, row.names = FALSE, fileEncoding = "EUC-KR")
}

# 메모리 정리
rm(list=ls())
gc()