setwd("D:\\Personal Study\\R Notes & Codes\\R Test")
q1_dataset = read.csv("C:/Users/rshars/Downloads/train.csv", stringsAsFactors = FALSE, na.strings = "")

library(reshape)
install.packages("e1071")
library(e1071)
options(scipen = 999)

univariate_func =  function(q1_dataset, total_obs){

q1_dataset = head(q1_dataset,total_obs)

i=sapply(q1_dataset,is.numeric)
q1_numeric = q1_dataset[,i]
c= sapply(q1_dataset,is.character)
q1_character= q1_dataset[,c]
#q1_numeric$AD_AMS_SELLER_ID = as.numeric(q1_numeric$AD_AMS_SELLER_ID)

count= vector(mode = "numeric", length = 0)
nmiss = vector(mode = "numeric", length = 0)
missing_perc = vector(mode = "numeric", length = 0)
average = vector(mode = "numeric", length = 0)
minimum = vector(mode = "numeric", length = 0)
maximum = vector(mode = "numeric", length = 0)
add = vector(mode = "numeric", length = 0)
std_dev = vector(mode = "numeric", length = 0)
mid_val = vector(mode = "numeric", length = 0)
kurt = vector(mode = "numeric", length = 0)
skew = vector(mode = "numeric", length = 0)
quantile_25 = vector(mode = "numeric", length = 0)
quantile_75 = vector(mode = "numeric", length = 0)
quantile_01 = vector(mode = "numeric", length = 0)
quantile_99 = vector(mode = "numeric", length = 0)

for (i in 1:ncol(q1_numeric)) {
  count[i]= length(q1_numeric[,i])
  nmiss[i] = sum(is.na(q1_numeric[,i]))
  missing_perc[i]= ((sum(is.na(q1_numeric[,i]))/length(q1_numeric[,i]))*100)
  average[i]= mean((q1_numeric[,i]), na.rm = T)
  minimum[i] = min((q1_numeric[,i]), na.rm = T)
  maximum[i] =  max((q1_numeric[,i]), na.rm = T)
  add[i]= sum(q1_numeric[,i], na.rm = T)
  std_dev[i]= sd(q1_numeric[,i], na.rm = T)
  mid_val[i] = median(q1_numeric[,i], na.rm = T)
  kurt[i] = kurtosis(q1_numeric[,i], na.rm = T)
  skew[i] = skewness(q1_numeric[,i], na.rm =T)
  quantile_25[i] = quantile(q1_numeric[,i],0.25, na.rm = T)
  quantile_75[i] = quantile(q1_numeric[,i],0.75, na.rm = T)
  quantile_01[i] = quantile(q1_numeric[,i],0.01, na.rm = T)
  quantile_99[i] = quantile(q1_numeric[,i],0.99, na.rm = T)
}

x= colnames(q1_numeric)

numeric_output = as.data.frame(cbind(x,count,nmiss,missing_perc,average,minimum,maximum,add,std_dev,mid_val,kurt,skew,quantile_25,quantile_75,quantile_01,quantile_99))

numeric_output <<- rename(numeric_output, c(x= "varname",
                                          count_char = "n",
                                          missing_perc="%missing",
                                          average = "mean",
                                          minimum = "min",
                                          maximum = "max",
                                          add = "sum",
                                          std_dev = "std",
                                          mid_val = "median",
                                          kurt = "kurtosis",
                                          skew = "skewness",
                                          quantile_25 = "q1",
                                          quantile_75 = "q3",
                                          quantile_01="p1",
                                          quantile_99 = "p99"   
                                          ))

count_char= vector(mode = "numeric", length = 0)
distinct_char = vector(mode = "numeric", length = 0)
mode_char = vector(mode = "numeric", length = 0)
miss_char = vector(mode = "numeric", length = 0)
missing_perc_char = vector(mode = "numeric", length = 0)

mode_finding <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in 1:ncol(q1_character)) {
  count_char[i]= length(q1_character[,i])
  distinct_char[i] = length(unique(q1_character[,i]))
  mode_char[i]= mode_finding(q1_character[,i])
  miss_char[i] = sum(is.na(q1_character[,i]))
  missing_perc_char[i]= ((sum(is.na(q1_character[,i]))/length(q1_character[,i]))*100)
}

y= colnames(q1_character)

character_output = as.data.frame(cbind(y,count_char,distinct_char,mode_char,miss_char,missing_perc_char))

character_output <<- rename(character_output, c(y= "Attribute",
                                              count_char = "count",
                                              distinct_char = "distinct",
                                              mode_char = "mode",
                                              miss_char="missing",
                                              missing_perc_char = "missing%"))
}

univariate_func(q1_dataset,nrow(q1_dataset))
