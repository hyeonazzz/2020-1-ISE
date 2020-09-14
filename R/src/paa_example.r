#paa 예시 받은 코드랑은 관련 없이 그냥 paa공부 코드

series1 <-c (2.02, 2.33, 2.99, 6.85, 9.20, 8.80, 7.50, 6.00, 5.85, 3.85, 4.85, 3.85, 2.22, 1.45, 1.34)
in_folder_path <- "./data/norm_data/"
out_folder_path <- "./data/paa_data/"
file_paths <- list.files(in_folder_path)
temp_path <- paste0(in_folder_path, file_paths[1])
print(temp_path)
paa <- function(ts, paa_size){
  len = length(ts)
  if (len == paa_size) {
    ts
  }
  else {
    if (len %% paa_size == 0) {
      colMeans(matrix(ts, nrow=len %/% paa_size, byrow=F))
    }
    else {
      res = rep.int(0, paa_size)
      for (i in c(0:(len * paa_size - 1))) {
        idx = i %/% len + 1# the spot
        pos = i %/% paa_size + 1 # the col spot
        res[idx] = res[idx] + ts[pos]
      }
      for (i in c(1:paa_size)) {
        res[i] = res[i] / len
      }
      res
    }
  }
}

s1_paa = paa(series1,7)
print(s1_paa)

s1_paa = paa(series1,9)
print(s1_paa)