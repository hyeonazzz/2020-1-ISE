# 1. 라이브러리 로딩 ----
library(arules)
library(arulesViz)

# 2. ----
add_measures <- function(ruleset, trans){
  post_rule <- ruleset
  quality(post_rule)$coverage <- interestMeasure(post_rule, "coverage")
  quality(post_rule)$kulczynski <- interestMeasure(post_rule, "kulczynski", transactions = trans)
  quality(post_rule)$Imbalance <- interestMeasure(post_rule, "imbalance", transactions = trans)
  post_rule <- sort(post_rule, by = "kulczynski")
  return(post_rule)
}

post_processing <- function(ruleset){
  post_rule <- sort(ruleset, by = "confidence")
  subset_mat <- is.subset(post_rule, post_rule)
  subset_mat[lower.tri(subset_mat, diag=T)] <- FALSE
  redundant <- colSums(subset_mat, na.rm=T) >= 1
  post_rule <- post_rule[!redundant]

  post_rule <- sort(post_rule, by = "kulczynski")
  subset_mat <- is.subset(post_rule, post_rule)
  subset_mat[lower.tri(subset_mat, diag=T)] <- FALSE
  redundant <- colSums(subset_mat, na.rm=T) >= 1
  post_rule <- post_rule[!redundant]
  
  return(post_rule)
}

get_arules <- function(file_path){
  loaded_data <- read.csv(file_path, stringsAsFactors = TRUE, fileEncoding = "EUC-KR")
  print(loaded_data)
  # tmp_data <- loaded_data[rowSums(is.na(loaded_data[,-1]))!=ncol(loaded_data[,-1]),]
  tmp_data <- loaded_data
  
  tmp_trans <- as(tmp_data[,-1], "transactions")

  # min_sup <- round(quantile(itemFrequency(tmp_trans))[2], 3)
  # min_sup <- round(min(itemFrequency(tmp_trans)), 3)
  print(sort(itemFrequency(tmp_trans)))

  rules <- apriori(tmp_trans, parameter = list(supp=0.01, conf=0.5, minlen=2))
  measure_rules <- add_measures(rules, tmp_trans)
  pruned_rule <- post_processing(measure_rules)
  
  return(pruned_rule)
}

# 3. ----
in_folder_path <- "./data/symbol_basket/"
out_folder_path <- "./result/symbol_basket/"
file_paths <- list.files(in_folder_path)

for (file_path in file_paths){
  temp_path <- paste0(in_folder_path, file_path)
  print(temp_path)
  temp_rules <- get_arules(temp_path)

  df_rule <- as(temp_rules, "data.frame")
  
  temp_path <- paste0(out_folder_path, file_path)
  print(temp_path)
  write.csv(df_rule, file = temp_path, row.names = FALSE, fileEncoding = "EUC-KR")
  
  png(gsub(".csv", ".png", temp_path), width = 1600, height = 900)
  # plot(head(temp_rules, 10), method = "graph", engine = "graphviz",
  #      measure = "support", shading = "confidence",
  #      measureLabels=TRUE, alpha = 1)
  dev.off()
}

# 4. ----
rm(list=ls())
gc()
