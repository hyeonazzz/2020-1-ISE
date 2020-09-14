# 1. 라이브러리 로딩----
library(tidyverse)
library(lattice)
library(ggplot2)
library(gridExtra)
library(zoo)

# 2. ----
plot_qq <- function(df, df_name){
  df %>%
    select(-lot_no, -timestamp, -quality) %>%
    gather("Variable", "Value") %>%
    qqmath(~Value | factor(Variable), data = .,
           # main = df_name,
           layout = c(5,2),
           # scales = list(y=list(relation="free")),
           ylim = c(-4, 4), xlim = c(-4, 4),
           prepanel = prepanel.qqmathline,
           type = c("p", "g"),
           panel = function(x, ...){
             panel.qqmathline(x, ...)
             panel.qqmath(x, ...)
             panel.abline(h = c(-1.28, 1.28), v = c(-1.28, 1.28),
                          lty = 'dashed', col = 'red')
           })
}

plot_dens <- function(df, df_name){
  df %>%
    select(-lot_no, -timestamp, -quality) %>%
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

plot_dens_paa <- function(df, df_name){
  df %>%
    select(-lot_no, -time_index, -quality) %>%
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
    select(-lot_no, -timestamp, -quality) %>%
    ts(.) %>%
    xyplot(.,
           ylim = c(-3.0, 3.0), grid = T,
           par.strip.text=list(cex=2.2),
           ylab=list(label="Value", cex=2.0),
           xlab=list(label="Index", cex=2.0),
           scales=list(cex=1.8),
           panel = function(x, y, ...){
             panel.abline(h = c(-1.28, 0, 1.28), lty = 'dashed', col = 'red')
             panel.xyplot(x, y, ...)
           })
}

plot_paa <- function(df, df_name){
  df %>%
    select(-lot_no, -time_index, -quality) %>%
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

plot_sax <- function(df, df_name){
  df %>%
    select(-quality) %>%
    ts(.) %>%
    xyplot(., main = paste0(df_name," (SAX)"), ylim = c(0, 11), grid = T,
           panel = function(x, y, ...){
             panel.abline(h = c(2, 5, 9), lty = 'dashed', col = 'red')
             panel.xyplot(x, y, ...)
           })
}

plot_scatter <- function(df, c1, c2){
  fmla <- formula(paste0(c1, "~", c2))
  sp <- xyplot(fmla, data = df, jitter.x = T, jitter.y = T,
         ylim = c(-3.0, 3.0), xlim = c(-3.0, 3.0),
          panel = function(x, y, ...){
            panel.abline(h = c(-1.28, 1.28), v = c(-1.28, 1.28),
                        lty = 'dashed', col = 'red')
            panel.rect(-3, -3, -1.28, -1.28, col = "red", alpha = 0.2)
            panel.rect(-3, 1.28, -1.28, 3, col = "red", alpha = 0.2)
            panel.rect(1.28, 1.28, 3, 3, col = "red", alpha = 0.2)
            panel.rect(1.28, -3, 3, -1.28, col = "red", alpha = 0.2)
            panel.rect(-3, -1.28, -1.28, 1.28, col = 'yellow', alpha = 0.2)
            panel.rect(-1.28, 1.28, 1.28, 3, col = 'yellow', alpha = 0.2)
            panel.rect(1.28, -1.28, 3, 1.28, col = 'yellow', alpha = 0.2)
            panel.rect(-1.28, -3, 1.28, -1.28, col = 'yellow', alpha = 0.2)
            panel.rect(-1.28, -1.28, 1.28, 1.28, col = 'green', alpha = 0.4)
            panel.xyplot(x, y, ...)
          })
  # Marginal density plot of x (top panel) and y (right panel)
  xplot <- ggplot(df, aes_string(x = c1)) +
    geom_density() + xlim(c(-3, 3)) + ylim(c(0, 0.5)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "grey"),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank())
  yplot <- ggplot(df, aes_string(x = c2)) +
    geom_density() + xlim(c(-3, 3)) + ylim(c(0, 0.5)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "grey"),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    coord_flip()
  blank_plot <- ggplot() + geom_blank(aes(1,1)) + theme_void()
  # Arranging the plot
  grid.arrange(xplot, blank_plot, sp, yplot,
               ncol=2, nrow=2, widths = c(4,1.4), heights=c(1.4, 4))
}


plot_scatter2 <- function(df, c1, c2){
  d <- data.frame(x1=c(-3, -3, 1.28, 1.28, -3, -1.28, 1.28, -1.28, -1.28),
                  y1=c(-3, 1.28, 1.28, -3, -1.28, 1.28, -1.28, -3, -1.28),
                  x2=c(-1.28, -1.28, 3, 3, -1.28, 1.28, 3, 1.28, 1.28),
                  y2=c(-1.28, 3, 3, -1.28, 1.28, 3, 1.28, -1.28, 1.28),
                  t=c("red",'red','red','red','yellow','yellow','yellow','yellow','green'))
  ggplot() +
  geom_rect(data = d, mapping = aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t),
            color="black", alpha=0.2)
  
  # Scatter plot colored by groups ("Species")
  sp <- ggplot(df, aes_string(x = c1, y = c2)) +
    geom_point(colour = "royalblue") + xlim(c(-3, 3)) + ylim(c(-3, 3)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "grey"),
          panel.grid.minor = element_line(colour = "grey"))
  # Marginal density plot of x (top panel) and y (right panel)
  xplot <- ggplot(df, aes_string(x = c1)) +
    geom_density() + xlim(c(-3, 3)) + ylim(c(0, 0.5)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "grey"),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank())
  yplot <- ggplot(df, aes_string(x = c2)) +
    geom_density() + xlim(c(-3, 3)) + ylim(c(0, 0.5)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "grey"),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    coord_flip()
  blank_plot <- ggplot() + geom_blank(aes(1,1)) + theme_void()
  # Arranging the plot
  grid.arrange(xplot, blank_plot, sp, yplot,
               ncol=2, nrow=2, widths = c(4,1.4), heights=c(1.4, 4))
}

plot_scatter3 <- function(df, c1, c2){
  # Scatter plot colored by groups ("Species")
  sp <- ggplot(df, aes_string(x = c1, y = c2)) +
    geom_point(colour = "royalblue") + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "grey"),
          panel.grid.minor = element_line(colour = "grey"))
  ggMarginal(sp, type = "histogram", fill = "royalblue")
}

normality_test <- function(df){
  library(tseries)
  library(moments)
  
  numeric_df <- select_if(df, is.numeric)
  jarque <- apply(numeric_df, 2, jarque.bera.test)
  ks <- apply(numeric_df, 2, ks.test, y="pnorm")
  shapiro <- apply(numeric_df, 2, shapiro.test)
  agostino <- apply(numeric_df, 2, agostino.test)
  
  test_list <- list()
  col_names <- colnames(numeric_df)
  for(i in length(col_names)){
    col_name <- col_names[i]
    test_list[[length(test_list)+1]] <- c(col_name, jarque$method, jarque$p.value)
    test_list[[length(test_list)+1]] <- c(col_name, ks$method, ks$p.value)
    test_list[[length(test_list)+1]] <- c(col_name, shapiro$method, shapiro$p.value)
    test_list[[length(test_list)+1]] <- c(col_name, agostino$method, agostino$data.name, agostino$p.value)  
  }
  print(test_list)
  test_df <- as.data.frame(test_list)
  
  return(test_df)
}

# 3. ----
in_folder_path <- "./data/clean_data/"
out_folder_path <- "./result/time_series_plot/"
file_paths <- list.files(in_folder_path)
print('file_paths:')
print(file_paths)

for (file_path in file_paths[4]){
  temp_path <- paste0(in_folder_path, file_path)
  print(temp_path)
  
  loaded_data <- read.csv(temp_path, stringsAsFactors = FALSE, fileEncoding = "EUC-KR")
  data_name <- gsub("_filtered.csv", "", file_path)
  
  temp_path <- paste0(out_folder_path, paste0("raw_ts_",file_path))
  print(temp_path)
  png(gsub(".csv", ".png", temp_path), width = 1200, height = 1600)
  print(plot_ts(loaded_data, data_name))
  dev.off()
  
  temp_path <- paste0(out_folder_path, paste0("raw_qq_",file_path))
  print(temp_path)
  png(gsub(".csv", ".png", temp_path), width = 1000, height = 450)
  print(plot_qq(loaded_data, data_name))
  dev.off()
  
  temp_path <- paste0(out_folder_path, paste0("raw_dens_",file_path))
  print(temp_path)
  png(gsub(".csv", ".png", temp_path), width = 1000, height = 450)
  print(plot_dens(loaded_data, data_name))
  dev.off()
  
}


in_folder_path <- "./data/norm_data/"
file_paths <- list.files(in_folder_path)

for (file_path in file_paths[2]){
  temp_path <- paste0(in_folder_path, file_path)
  print(temp_path)
  
  loaded_data <- read.csv(temp_path, stringsAsFactors = FALSE, fileEncoding = "EUC-KR")
  data_name <- gsub("_filtered.csv", "", file_path)
  
  temp_path <- paste0(out_folder_path, paste0("ts_",file_path))
  print(temp_path)
  png(gsub(".csv", ".png", temp_path), width = 1200, height = 1600)
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

in_folder_path <- "./data/paa_data/"
file_paths <- list.files(in_folder_path)

for (file_path in file_paths[2]){
  temp_path <- paste0(in_folder_path, file_path)
  print(temp_path)
  
  loaded_data <- read.csv(temp_path, stringsAsFactors = FALSE, fileEncoding = "EUC-KR")
  data_name <- gsub("_filtered.csv", "", file_path)
  
  print(sum(is.na(loaded_data)))
  
  temp_path <- paste0(out_folder_path, paste0("paa_",file_path))
  print(temp_path)
  png(gsub(".csv", ".png", temp_path), width = 1200, height = 1600)
  print(plot_paa(loaded_data, data_name))
  dev.off()
  
  temp_path <- paste0(out_folder_path, paste0("dens_paa_",file_path))
  print(temp_path)
  png(gsub(".csv", ".png", temp_path), width = 1000, height = 450)
  print(plot_dens_paa(loaded_data, data_name))
  dev.off()
  
  normality_df <- normality_test(loaded_data)
  output_path <- paste0(out_folder_path, paste0("normality_test_",file_path))
  write.csv(normality_df, "")
  
}

in_folder_path <- "./data/sax_data/"
file_paths <- list.files(in_folder_path)

for (file_path in file_paths[3]){
  temp_path <- paste0(in_folder_path, file_path)
  print(temp_path)
  
  loaded_data <- read.csv(temp_path, stringsAsFactors = FALSE, fileEncoding = "EUC-KR")
  data_name <- gsub("_filtered.csv", "", file_path)
  
  temp_path <- paste0(out_folder_path, paste0("sax_",file_path))
  print(temp_path)
  
  png(gsub(".csv", ".png", temp_path), width = 1200, height = 1600)
  print(plot_sax(loaded_data, data_name))
  dev.off()
}

# 4.----
rm(list=ls())
gc()