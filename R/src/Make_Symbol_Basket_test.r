# 1. 라이브러리 로딩 ----
library(tidyverse)
library(tictoc)

# 2. Symbol Basket 생성 ----
# from list To Dataframe transform by row
list_to_dataframe <- function(in_data) {
  nCol <- max(vapply(in_data, length, 0))
  data <- lapply(in_data, function(row) c(row, rep(NA, nCol-length(row))))
  data <- matrix(unlist(data), nrow=length(data), ncol=nCol, byrow=TRUE)
  df_data <- data.frame(data)
  return(df_data)
}

# Symbol Basket 계산 함수
make_symbol_basket <- function(sax_data, bot_value = 1, top_value = 10, var_name = FALSE){
  basketList <- list()
  rowLength <- nrow(sax_data)
  print("rowLength:")
  print(rowLength)
  colLength <- ncol(sax_data)
  print("colLength:")
  print(colLength)
  col_name <- colnames(sax_data)
  print("col_name:")
  print(col_name)
  
  for(i in 1:rowLength){
    tempBasket <- NULL
    tempEvent <- NULL
    for(j in 1:colLength){
      nowEvent <- sax_data[i,j]
      if(nowEvent == top_value){
        if(var_name){
          tempEvent <- paste(col_name[j], "top_10%", sep="_")  
        } else {
          tempEvent <- "top_10%"  
        }
      }
      else if(nowEvent == bot_value){
        if(var_name){
          tempEvent <- paste(col_name[j], "btm_10%", sep="_")
        } else {
          tempEvent <- "btm_10%"  
        }
      }
      else{
        tempEvent <- NA
      }
      tempBasket <- c(tempBasket, tempEvent)
    }
    basketList[[i]] <- tempBasket
  }
  
  basket_df <- list_to_dataframe(basketList)
  colnames(basket_df) <- col_name
  eID <- 1:rowLength
  basket_df <- cbind(eID, basket_df)
  
  return(basket_df)
}

# Symbol Basket 생성 함수
get_symbol_basket <- function(file_path, bot_value, top_value){
  loaded_data <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "EUC-KR")
  sax_data <- loaded_data %>% select(-Alarm)
 
  tic("Time for making the symbolbasket")
  symbol_basket <- make_symbol_basket(sax_data, bot_value, top_value)
  print("symbol_basket:")
  print(symbol_basket)
  toc()
  
  return(symbol_basket)
}

# 3. Symbol Sequence Basket 생성 ----
# sliding window
slidingWindow <- function(x, windowSize){
  vLen <- length(x)
  walker <- 1
  isEnd <- FALSE
  
  result <- NULL
  tempV <- NULL
  
  if(windowSize > vLen){
    stop("Window size should be smaller than length of vector")
  }
  
  while(!isEnd){
    for(i in 1:windowSize){
      
      tempV[i] <- x[walker]
      
      if(i < windowSize){
        walker <- walker + 1  
      }
    }
    if(walker == vLen){
      isEnd = TRUE
    }
    result <- c(result, tempV)
    walker <- walker - windowSize +2
  }
  return(result)
}

# counting non Na's length by row
nonNACols <- function(x){
  as.vector(apply(x, 1, function(x) length(which(!is.na(x)))))
}

# Symbol Sequence Basket 생성 함수
make_symbol_sequence_basket <- function(symbol_basket, window_size = 2){
  sb_basket <- symbol_basket %>% select(-eID)
  basketSq <- as.data.frame(apply(sb_basket, 2, FUN = function(x) slidingWindow(x,window_size)))
  sID <- rep(1:(nrow(sb_basket)+1-window_size), by=1, each=window_size)
  eID <- rep(1:window_size, times = (nrow(sb_basket)+1-window_size))
  basketSize <- nonNACols(basketSq)
  basketSq <- cbind(sID,eID,basketSize,basketSq)
  return(basketSq)
}

# Symbol Sequence Basket 생성 함수
get_symbol_sequence_basket <- function(file_path, window_size = 2, var_name = FALSE){
  loaded_data <- read.csv(file_path, stringsAsFactors = FALSE, fileEncoding = "EUC-KR")
  sax_data <- loaded_data %>% select(-Alarm)
  symbol_basket <- make_symbol_basket(sax_data, var_name = var_name)
  symbol_sq_basket <- make_symbol_sequence_basket(symbol_basket, window_size = window_size)
  
  return(symbol_sq_basket)
}

# 4. 데이터 처리 후 저장----
in_folder_path <- "./data/sax_data/"
out_folder_path <- "./data/symbol_basket/"
file_paths <- list.files(in_folder_path)

for (file_path in file_paths[3]){
  temp_path <- paste0(in_folder_path, file_path)
  print(temp_path)
  temp_data <- get_symbol_basket(temp_path, bot_value = 1, top_value = 10)
  temp_path <- paste0(out_folder_path, file_path)
  print(temp_path)
  write.csv(temp_data, file = temp_path, row.names = FALSE, fileEncoding = "EUC-KR")
}

out_folder_path <- "./data/symbol_sequence_basket/"

for (file_path in file_paths[3]){
  temp_path <- paste0(in_folder_path, file_path)
  print(temp_path)
  temp_data <- get_symbol_sequence_basket(temp_path, window_size = 10, var_name = TRUE)
  temp_data <- temp_data[rowSums(is.na(temp_data[,c(-1,-2,-3)]))!=ncol(temp_data[,c(-1,-2,-3)]),]
  
  temp_path <- paste0(out_folder_path, file_path)
  print(temp_path)
  write.csv(temp_data, file = temp_path,
            row.names = FALSE, fileEncoding = "EUC-KR")
  
  # Delete NAs from Sequences
  basketSq <- scan(temp_path, what = "character()", skip = 1)
  basketSq <- gsub(",NA", "", basketSq)
  write(basketSq, temp_path)
}

# 메모리 정리
rm(list=ls())
gc()
