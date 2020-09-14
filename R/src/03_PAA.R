# 1. 라이브러리 로딩 ----
library(tidyverse)
library(TSclust)

# 2. PAA 함수 정의 ----
convert_to_PAA <- function(file_path, paa_ratio = 2){
  
  start <- proc.time()
  loaded_data <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "EUC-KR")
  lot_info <- unique(loaded_data$lot_no)
  print(length(lot_info))
  
  paa_data <- data.frame()
  paa_lotno <- vector()
  time_index <- vector()
  
  for (lot in lot_info){
    temp_data <- loaded_data %>%
      filter(lot_no == lot) %>%
      select(-lot_no, -timestamp, -quality)
    
    temp_class <- loaded_data %>%
      filter(lot_no == lot) %>%
      select(quality) %>%
      unique %>%
      as.character
    
    w <- round(nrow(temp_data) / paa_ratio)
    paa_lotno <- c(paa_lotno, rep(lot, each = w))
    time_index <- c(time_index, 1:w)
    
    temp_paa <- tbl_df(sapply(temp_data, function(x) PAA(x, w)))
    temp_paa <- temp_paa %>% mutate(quality = temp_class)                 
    
    paa_data <- bind_rows(paa_data, temp_paa)
  }
  
  paa_data <- data.frame(lot_no = as.character(paa_lotno),
                         time_index = time_index,
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