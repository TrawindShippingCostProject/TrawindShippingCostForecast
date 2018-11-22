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
comparison
  
  
  



