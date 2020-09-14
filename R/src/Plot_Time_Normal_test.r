#공부용 + 우리 시각화한 코드 (RS40018번만)

# 1. 라이브러리 로딩----
library(tidyverse)
library(lattice)
library(ggplot2)
library(gridExtra)
library(zoo)
library(dplyr)

# 2. ----
plot_qq <- function(df, df_name){
  df %>%
    select(RS40018) %>% #0702P에서 RS40018열 선택
    gather("Variable", "Value") %>% #gather 로 합침
    qqmath(~Value | factor(Variable), data = ., #지수분포의 모수 추정
    #y변수~x변수 | 보여주기 원하는 변수명 기재 ~Value | Variable을 보여주고자 함.
           # main = df_name,
           #layout = c(5,2),
           # scales = list(y=list(relation="free")),
           ylim = c(-4, 4), xlim = c(-4, 4),
           prepanel = prepanel.qqmathline, #신뢰구간
           type = c("p", "g"), #p - 점으로 q?
           panel = function(x, ...){ #이론적 분포에 의해 결정된 점을 (일반적으로) 통과하는 선
             panel.qqmathline(x, ...)
             panel.qqmath(x, ...)
             panel.abline(h = c(-1.28, 1.28), v = c(-1.28, 1.28), #h 수평선 위치 v 수직선 위치 
                          lty = 'dashed', col = 'red')
           })
}

plot_qqq <- function(df, df_name){
  df %>%
    select(RS40018) %>% #0702P에서 RS40018열 선택
    gather("Variable", "Value") %>% #gather 로 합침
    qqmath(~Value | factor(Variable), data = ., #지수분포의 모수 추정
    #y변수~x변수 | 보여주기 원하는 변수명 기재 ~Value | Variable을 보여주고자 함.
           # main = df_name,
           #layout = c(5,2),
           # scales = list(y=list(relation="free")),
           ylim = c(70, 100), xlim = c(-4, 4),
           prepanel = prepanel.qqmathline, #신뢰구간
           type = c("p", "g"), #p - 점으로 q?
           panel = function(x, ...){ #이론적 분포에 의해 결정된 점을 (일반적으로) 통과하는 선
             panel.qqmathline(x, ...)
             panel.qqmath(x, ...)
             panel.abline(h = c(90, 95), v = c(-1.28, 1.28), #h 수평선 위치 v 수직선 위치 
                          lty = 'dashed', col = 'red')
           })
}

plot_dens <- function(df, df_name){
  df %>%
    select(RS40018) %>%
    gather("Variable", "Value") %>%
    # densityplot(~Value | factor(Variable), data = .)
    histogram(~Value | factor(Variable), data = .,
              type = "density",
              key = list(space="top", column=2,
                         lines=list(col=c("red", "blue"), lty=c("solid", "dashed")),
                         text=list(c("Distribution of time series", "Standard normal distribution"))),
              panel=function(x, ...) {
                panel.histogram(x,...)
                panel.densityplot(x, col="red",
                                  darg=list(kernel="gaussian"), ...)
                panel.mathdensity(dmath=dnorm, col="blue", lty = 'dashed',
                                  args=list(mean=0, sd=1))
              })
}

plot_denss <- function(df, df_name){

  df %>%
    select(RS40018) %>%
    gather("Variable", "Value") %>%
    # densityplot(~Value | factor(Variable), data = .)
    histogram(~Value | factor(Variable), data = .,
              type = "density",
              key = list(space="top", column=2, #space="top" legend의 위치 column=2 열 갯수
                         lines=list(col=c("red", "blue"), lty=c("solid", "dashed")), #lines의 색과 모양 설정
                         text=list(c("Distribution of time series", "Standard normal distribution"))), #line의 이름 설정
              panel=function(x, ...) {
                print('denss is x..')
                print(x,...)
                panel.histogram(x,...)
                # panel.densityplot(x, col="red",
                #                   darg=list(kernel="gaussian"), ...)
                panel.mathdensity(dmath=dnorm, col="blue", lty = 'dashed',
                                  args=list(mean=87.4, sd=2.4)) #args=list(mean=0, sd=1))
              })
}

plot_dens_paa <- function(df, df_name){
  df %>%
    select(RS40018) %>%
    gather("Variable", "Value") %>%
    # densityplot(~Value | factor(Variable), data = .)
    histogram(~Value | factor(Variable), data = .,
              type = "density",
              xlim = c(-4,4),
              key = list(space="top", column=2,
                         lines=list(col=c("red", "blue"), lty=c("solid", "dashed")),
                         text=list(c("Distribution of time series", "Standard normal distribution"))),
              panel=function(x, ...) {
                panel.histogram(x,...)
                panel.densityplot(x, col="red",
                                  darg=list(kernel="gaussian"), ...)
                panel.mathdensity(dmath=dnorm, col="blue", lty = 'dashed',
                                  args=list(mean=0, sd=1))
              })
}


plot_ts <- function(df, df_name){
  df %>%
    select(RS40018) %>%
    ts(.) %>%
    xyplot(.,
           ylim = c(-3.0, 3.0), grid = T,
           par.strip.text=list(cex=2.2),
           ylab=list(label="Value", cex=1.8), #y축 이름과 cex=1.8 글씨의 크기
           xlab=list(label="Index", cex=1.8), #x축 이름과 cex=1.8 글씨의 크기
           scales=list(cex=1.8), #축의 숫자들의 크기
           panel = function(x, y, ...){
             panel.abline(h = c(-1.28, 0, 1.28), lty = 'dashed', col = 'red') #abline은 일직선/ dashed모양으로 빨간색으로 그림
             panel.xyplot(x, y, ...)
           })
}

plot_tss <- function(df, df_name){ #0702P.csv파일, 0702 라는 이름
  df %>%
    select(RS40018) %>% #0702P에서 RS40018열 선택
    ts(.) %>% #시계열 데이터를 ts객체로 저장
    xyplot(.,  #xyplot() <- 스캐터플롯 작성하는데 사용
           ylim = c(-30.0, 100.0), grid = T, #y값의 범위 -30.0~100.0 grid는 표시
           par.strip.text=list(cex=2.2), 
           ylab=list(label="Value", cex=1.8),
           xlab=list(label="Index", cex=1.8),
           scales=list(cex=1.8),
           panel = function(x, y, ...){
             panel.abline(h = c(30, 87.4, 90), lty = 'dashed', col = 'red')
             panel.xyplot(x, y, ...)
           })
}

plot_paa <- function(df, df_name){
  df %>%
    select(RS40018) %>%
    ts(.) %>%
    xyplot(., main = paste0(df_name," (PAA)"),
           ylim = c(-3.0, 3.0), grid = T,
           par.strip.text=list(cex=1.5),
           ylab=list(label="Value", cex=1.2), xlab=list(label="Index", cex=1.2),
           scales=list(cex=1.2),
           panel = function(x, y, ...){
             panel.abline(h = c(-1.28, 0, 1.28), lty = 'dashed', col = 'red')
             panel.xyplot(x, y, ...)
           })
}




# 3. ----
print('260 lines ::: test_data에서 가져오겠습니다..')
in_folder_path <- "./data/test_data/"
out_folder_path <- "./result/time_series_plot/"
file_paths <- list.files(in_folder_path)
print('264 lines ::: file_paths입니다')
print(file_paths[2])
for (file_path in file_paths[2]){
  print('file_paths is what?')
  temp_path <- paste0(in_folder_path, file_path)
  print(temp_path)
  
  loaded_data <- read.csv(temp_path, stringsAsFactors = FALSE, fileEncoding = "EUC-KR")
  data_name <- gsub("P.csv", "", file_path)
  print('----------------')
  print('data_name')
  print(data_name)

  temp_path <- paste0(out_folder_path, paste0("raw_ts_",file_path))
  print(temp_path)
  png(gsub(".csv", ".png", temp_path), width = 1000, height = 450)
  #png파일로 저장하는 코드 gsub(temp_path에서 .csv를 .png로 바꿔라) 사이즈는 가로 1000, 세로 450
  print(plot_tss(loaded_data, data_name)) 
  dev.off()
  
  temp_path <- paste0(out_folder_path, paste0("raw_qq_",file_path))
  print(temp_path)
  png(gsub(".csv", ".png", temp_path), width = 1000, height = 450)
  print(plot_qqq(loaded_data, data_name))
  dev.off()
  
  temp_path <- paste0(out_folder_path, paste0("raw_dens_",file_path))
  print(temp_path)
  png(gsub(".csv", ".png", temp_path), width = 1000, height = 450)
  print(plot_denss(loaded_data, data_name))
  dev.off()
  
}

in_folder_path <- "./data/norm_data/"
file_paths <- list.files(in_folder_path)

for (file_path in file_paths[2]){
  temp_path <- paste0(in_folder_path, file_path)
  print(temp_path)
  
  loaded_data <- read.csv(temp_path, stringsAsFactors = FALSE, fileEncoding = "EUC-KR")
  data_name <- gsub("P.csv", "", file_path)
  
  temp_path <- paste0(out_folder_path, paste0("ts_",file_path))
  print(temp_path)
  png(gsub(".csv", ".png", temp_path), width = 1000, height = 450)
  print(plot_ts(loaded_data, data_name))
  dev.off()
  
  temp_path <- paste0(out_folder_path, paste0("qq_",file_path))
  print(temp_path)
  png(gsub(".csv", ".png", temp_path), width = 1000, height = 450)
  print(plot_qq(loaded_data, data_name))
  dev.off()
  
  temp_path <- paste0(out_folder_path, paste0("dens_",file_path))
  print(temp_path)
  png(gsub(".csv", ".png", temp_path), width = 1000, height = 450)
  print(plot_dens(loaded_data, data_name))
  dev.off()
  
}
