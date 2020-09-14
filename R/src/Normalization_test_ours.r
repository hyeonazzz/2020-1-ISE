#?��리�?� ?��규화?�� 코드

# 1. ?��?��브러�? 로딩 ----
library(tidyverse)
library(RNOmni)

data_normalization <- function(file_path){ 
  
  
  # Inverse Normal Transformation(int)
  inv_normal_trans <- function(u, k=3/8){
    n <- length(u) #dataframe?�� ?��?�� 개수�? ??� ?�� ?��?��?��?�� ?��?��
    r <- rank(u)
    #?��?���? 매�?�. ?��?��??��? ?��?��?��?�� 것이 ?��?��?�� �? ?��?�� 값이 몇번�? ?��?��?��지 ?���?
    return(qnorm((r-k)/(n-2*k+1))) #return ()계산?�� 값이 ?��규분?��?��률에 ?��?��?��?�� 값을 ?��?���?

  }
  
  # ?��?��?�� 로딩
  loaded_data <- read.csv(file_path,
                          fileEncoding = "EUC-KR",
                          stringsAsFactor = FALSE)
  # ?��?��?�� ?��규화
  norm_data <- loaded_data %>% # %>% = chain() ?��?�� or %>% =출력?��?��?
    select(reg_date:Alarm) %>%
    #() <- ?��?��?�� ?��?��?�� 추출
    mutate_at(vars(-reg_date,-Alarm), funs(inv_normal_trans))
   


  return(norm_data)
}


in_folder_path <- "./data/test_data/"
out_folder_path <- "./data/norm_data/"
file_paths <- list.files(in_folder_path)
print(file_paths)
#?��?�� ?�� ?��?��?�� ?��름을 list-up?��?�� 객체�? 만들�?
file_paths <- file_paths[grep("W", file_paths)]
#grep <- 찾아?�� ?�� 번호 출력
#file_paths중에?�� 'p'?��?�� ?��?��?�� 검?��

for (file_path in file_paths[18]){
  
  temp_path <- paste0(in_folder_path, file_path)

  temp_data <- data_normalization(temp_path)

  temp_path <- paste0(out_folder_path, file_path)
  write.csv(temp_data, file = temp_path, row.names = FALSE, fileEncoding = "EUC-KR")

}

# 메모�? ?���?
rm(list=ls())
gc() #가비�?�컬렉?�� ?��?��