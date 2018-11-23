# load librareis and data
library(ggplot2)
library(reshape2)
library(magrittr)
library(dplyr)
remove(list = ls())
load("clean_data_nov172018.RData")

# Get y
regress.df <- df.clean %>% select(cost)
regress.df$adjusted_cost <- df.clean$cost - df.clean$port_price_diff

# Get regressor
regress.df$ship_year <- as.numeric(format(as.Date(df.clean$ship_time), '%Y'))
regress.df$ship_month <- as.numeric(format(as.Date(df.clean$ship_time), '%m'))
regress.df$ship_month_factor <- as.factor(regress.df$ship_month)
regress.df$ship_month_square <- regress.df$ship_month^2
regress.df$trend <- (regress.df$ship_year - 2016) * 12 + regress.df$ship_month
regress.df$port <- df.clean$port
regress.df$total_weight <- df.clean$total_weight
regress.df$vsl_type <- factor(df.clean$vsl_type)
regress.df$contract_time <- df.clean$contract_time
regress.df$bid_time <- df.clean$bid_time
regress.df$ship_time <- df.clean$ship_time
regress.df$port[regress.df$port == 'JiaoJiang'] = 'JiaoJiangSSZD'



# Only look at major ports and major vsl type
regress.df <- filter(regress.df, port %in% c('NingBoZhenHai', 'NingBoHaiZhao', 'NingBoJiangYong', 'JiaoJiangSSZD', 'NingBoHuaPu'),
                     vsl_type %in% c('0.5W', '1.3-1.5W', '1-1.25W'))

df.clean <- filter(df.clean, port %in% c('NingBoZhenHai', 'NingBoHaiZhao', 'NingBoJiangYong', 'JiaoJiangSSZD', 'NingBoHuaPu'),
                   vsl_type %in% c('0.5W', '1.3-1.5W', '1-1.25W'))
df.clean$adjusted_cost <- df.clean$cost - df.clean$port_price_diff



mape_list <- c()
r_square_list <- c()
mae_list <- c()
for (bid in unique(regress.df$bid_time)){
  train.df <- filter(regress.df, bid_time != bid)
  test.df <- filter(regress.df, bid_time == bid)  
  
  model <- lm(cost ~ trend + ship_month_factor + vsl_type + port + total_weight, data = train.df)
  
  result_test_raw <- data.frame(true = test.df$cost, pred = predict(model, test.df))
  result_test_raw$error <- result_test_raw$true - result_test_raw$pred
  r_square <- summary(model)$r.squared
  r_square_list <- c(r_square_list, r_square)
  mae <- mean(abs(result_test_raw$error))
  mae_list <-c(mae_list, mae)
  mape <- mean(abs(result_test_raw$error) / result_test_raw$true)
  mape_list <- c(mape_list, mape)
}
res_1 <- c(mean(r_square_list), mean(mae_list), mean(mape_list))


mape_list <- c()
r_square_list <- c()
mae_list <- c()
for (bid in unique(regress.df$bid_time)){
  train.df <- filter(regress.df, bid_time != bid)
  test.df <- filter(regress.df, bid_time == bid)  
  
  model <- lm(cost ~ trend + ship_month + ship_month_square + vsl_type + port + total_weight, data = train.df)
  
  result_test_raw <- data.frame(true = test.df$cost, pred = predict(model, test.df))
  result_test_raw$error <- result_test_raw$true - result_test_raw$pred
  r_square <- summary(model)$r.squared
  r_square_list <- c(r_square_list, r_square)
  mae <- mean(abs(result_test_raw$error))
  mae_list <-c(mae_list, mae)
  mape <- mean(abs(result_test_raw$error) / result_test_raw$true)
  mape_list <- c(mape_list, mape)
}
res_2 <- c(mean(r_square_list), mean(mae_list), mean(mape_list))


comparison <- data.frame(FactorModel = res_1, QuadModel = res_2)
rownames(comparison) <- c('Avg R Square', 'Avg Mae', 'Avg MAPE')

# Conclusion: quardratic model wins

####### Add in the last 1 - 10 prices

# Initialize and add in the column "id" for reference
regress.df$id <- 1:nrow(regress.df)

# Create data frame "bid_time_ref" for reference
bid_time_list <- unique(regress.df$bid_time)
cost_mat <- c()
for (i in 1:length(bid_time_list)){
  regress.tmp <- regress.df %>% filter(ship_time<bid_time_list[i])
  print(nrow(regress.tmp))
  if (nrow(regress.tmp) == 0){ cost_mat = c(cost_mat, rep(NA,10)) }
  else { 
    tmp <- regress.tmp$cost[(length(regress.tmp$adjusted_cost)- 9): length(regress.tmp$adjusted_cost)]
    tmp <- rev(tmp)
    tmp <- cumsum(tmp)/(1:(length(tmp)) )
    cost_mat = c(cost_mat, tmp)  }
}
dim(cost_mat) <- c(10,length(cost_mat)/10)
tmp <- mean(cost_mat, na.rm = T)
cost_mat <- t(cost_mat)
cost_mat[is.na(cost_mat)] <-  tmp
bid_time_ref <- cbind(data.frame(bid_times = bid_time_list), data.frame(cost_mat))

# Add in the last ten prices
last_rec_mat <- c()
for (i in 1:nrow( regress.df)){
  last_rec_mat <- c(last_rec_mat, as.numeric(bid_time_ref[which(bid_time_ref$bid_times %in% regress.df$bid_time[i]), 2:11]) ) 
}
dim(last_rec_mat) = c(10,length(last_rec_mat)/10)
last_rec_mat <- t(last_rec_mat)
regress.df <- cbind(  regress.df, data.frame(last_rec_mat) )


# Re-run the regression 

result_mat <- c()
for (i in 1:10){
  mape_list <- c()
  r_square_list <- c()
  mae_list <- c()
  
  for (bid in unique(regress.df$bid_time)){
      train.df <- filter(regress.df, bid_time != bid)
      test.df <- filter(regress.df, bid_time == bid)
      str_tmp <- "cost ~ trend + ship_month + ship_month_square + vsl_type + port + total_weight"
      str_tmp2 <- paste("X", i, sep = "")
      str = paste(str_tmp, str_tmp2, sep = "+")
      model <- lm(as.formula(str), data = train.df)
      result_test_raw <- data.frame(true = test.df$cost, pred = predict(model, test.df))
      result_test_raw$error <- result_test_raw$true - result_test_raw$pred
      r_square <- summary(model)$r.squared
      r_square_list <- c(r_square_list, r_square)
      mae <- mean(abs(result_test_raw$error))
      mae_list <-c(mae_list, mae)
      mape <- mean(abs(result_test_raw$error) / result_test_raw$true)
      mape_list <- c(mape_list, mape)
  }
  result_mat <- c(result_mat, c(mean(r_square_list), mean(mae_list), mean(mape_list)))
}
dim(result_mat) <- c(3,length(result_mat)/3)

comparison <- cbind(comparison, data.frame(result_mat))
