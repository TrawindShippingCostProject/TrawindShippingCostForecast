# Preprocess the df data (531 rows)
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

regress.df$contract_time <- as.Date(regress.df$contract_time, "%Y-%m-%d")
regress.df$bid_time <- as.Date(regress.df$bid_time, "%Y-%m-%d")
regress.df$ship_time <- as.Date(regress.df$ship_time, "%Y-%m-%d")

sapply(regress.df, class)

save(regress.df, file = "Preprocessed_Ship_data_Dec2018.Rda")
