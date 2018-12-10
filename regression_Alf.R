# load librareis and data
library(ggplot2)
library(reshape2)
library(magrittr)
library(dplyr)
remove(list = ls())
load("clean_data_nov172018.RData")
df.base <- df.clean

# Get y, [Alf: this is equivalent to 
regress.df <- df.clean %>% select(cost)
regress.df$adjusted_cost <- df.clean$cost - df.clean$port_price_diff

# Add ship_year and ship_month
df.base$ship_year <- as.numeric(format(as.Date(df.clean$ship_time), '%Y'))
df.base$ship_month <- as.numeric(format(as.Date(df.clean$ship_time), '%m'))

# Add long-term fixed effect
regress.df$trend <- (df.base$ship_year - 2016) * 12 + df.base$ship_month

# Add month effect, vsl_company, total_weight
regress.df$ship_year <- df.base$ship_year
regress.df$ship_month_factor <- as.factor(df.base$ship_month)
regress.df$ship_month_value <- df.base$ship_month
regress.df$port <- df.base$port
regress.df$total_weight <- df.base$total_weight
regress.df$vsl_type <- factor(df.base$vsl_type)
regress.df$contract_time <- df.base$contract_time
regress.df$total_weight <- df.base$total_weight
regress.df$bid_time <- df.base$bid_time
regress.df$ship_time <- df.base$ship_time
regress.df$log_adjusted_cost <- log(regress.df$adjusted_cost)
regress.df$ship_month_square <- regress.df$ship_month_value ^ 2


# plot something to visualize the data
remove(p1, p2, p3, df.base, df.clean)

p1 <- ggplot(data = regress.df, mapping  = aes(x = ship_time, y = log_adjusted_cost)) + geom_point(aes(shape = vsl_type)) + 
  geom_vline(xintercept = regress.df$bid_time, color = 'green', linetype = 2)

p2 <- ggplot(data = regress.df, mapping = aes(x = ship_time, y = log_adjusted_cost)) + geom_point() + facet_wrap(~ vsl_type, nrow = 2) +
  geom_vline(xintercept = regress.df$bid_time, color = 'green', linetype = 2)

temp.df <- subset(regress.df, port %in% c('NingBoZhenHai', 'NingBoHaiZhao', 'NingBoJiangYong', 'JiaoJiangSSZD', 'NingBoHuaPu') & 
                                            vsl_type %in% c('0.5W', '1.3-1.5W', '1-1.25W'))

p_temp <- ggplot(data = temp.df, mapping = aes(x = ship_time, y = adjusted_cost)) + geom_point() +
  geom_vline(xintercept = regress.df$bid_time, color = 'green', linetype = 2) + facet_grid(port ~ vsl_type)

p3 <- ggplot(data = regress.df, mapping  = aes(x = ship_time, y = adjusted_cost)) + geom_point() + 
  facet_wrap(port ~ vsl_type) + geom_vline(xintercept = regress.df$bid_time, color = 'green', linetype = 2)



# Pivot table
remove(pivot_table)
pt <- tbl_df(regress.df)
pivot_table <- pt %>% group_by(port, ship_time, vsl_type) %>% summarise(PortTotalWeight = sum(total_weight), NumShipment = n(), AverageCost = mean(cost), 
                                               MinCost = min(cost), MaxCost = max(cost), StdCost = sd(cost))
temp_pt <- filter(pivot_table, NumShipment > 1, StdCost > 0) %>% arrange(desc(StdCost))

pivot_table <- pt %>% group_by(port) %>% summarise(PortTotalWeight = sum(total_weight), NumShipment = n(), AverageCost = mean(cost), MinCost = min(cost), 
                                                   MaxCost = max(cost), StdCost = sd(cost)) %>% arrange(desc(PortTotalWeight))

pivot_table <- pt %>% group_by(vsl_type) %>% summarise(VslTotalWeight = sum(total_weight), NumShipment = n()) %>% arrange(desc(VslTotalWeight))



pivot_table2 <- temp_pt %>% group_by(port, ship_time) %>% summarise(PortTotalWeight = sum(total_weight), NumShipment = n(), AverageCost = mean(adjusted_cost), 
                                                              MinCost = min(adjusted_cost), MaxCost = max(adjusted_cost), StdCost = sd(adjusted_cost)) %>% arrange(ship_time)


p4 <- ggplot(data = pivot_table, mapping = aes(x = StdCost)) + geom_freqpoly(binwidth = 0.2)
hist(pivot_table$StdCost)


pivot_table <- pt %>% group_by(port, ship_time) %>% summarise(PortTotalWeight = sum(total_weight), NumShipment = n(), AverageCost = mean(adjusted_cost), 
                                                              MinCost = min(adjusted_cost), MaxCost = max(adjusted_cost), StdCost = sd(adjusted_cost)) %>% arrange(ship_time)


 # Divide by training and testing
remove(regress.test, regress.train)
regress.train <- regress.df[1:409,]
regress.test <- regress.df[410:531,]


# Trying on some models

# This is ZH's model
model1 <- lm(cost ~ trend + ship_month_factor + vsl_type + port + total_weight, data = regress.train)
summary(model1)

# This is Alf's model
model2 <- lm(adjusted_cost ~ trend + ship_month_square + ship_month_value + vsl_type + port + total_weight, data = regress.train)
summary(model2)

tmp <- c(mean(abs(model2$residuals)), mean(abs(model2$residuals)/regress.train$adjusted_cost),summary(model2)$r.squared)
regress.test$port[regress.test$port == 'JiaoJiang'] = 'JiaoJiangSSZD'
result_test_raw <- data.frame(true = regress.test$adjusted_cost, pred = predict(model2, regress.test))
result_test_raw$error <- result_test_raw$true - result_test_raw$pred

tmp2 <- c(mean(abs(result_test_raw$error)), mean(abs(result_test_raw$error)/result_test_raw$true), NA)
summary_result_model2 <- data.frame(train = tmp, test = tmp2)
rownames(summary_result_model2) <- c("APE", "MAPE", "R-Squared")
summary_result_model2



regress.tmp <- regress.df
regress.tmp$port[regress.tmp$port == 'JiaoJiang'] = 'JiaoJiangSSZD'
result_tmp <- data.frame(true = regress.tmp$adjusted_cost, pred = predict(model2, regress.tmp), ship_time = regress.tmp$ship_time)
plot.df <- melt(result_tmp, id.vars = "ship_time")
model2.p <- ggplot(plot.df, aes(x = ship_time, y = value, group = variable, color = variable)) + geom_line() + geom_point() + 
  geom_vline(xintercept = df.base$bid_time[410])

model2.p




# what if we only look at the some of the most used (vsl_type, port)
pt <- tbl_df(regress.df)
vsl_count_table <- pt %>% group_by(vsl_type) %>% summarise(NumShipment = n()) %>% arrange(NumShipment)

major_shipment_reg.df <- filter(regress.df, port %in% c('NingBoZhenHai', 'NingBoHaiZhao', 'NingBoJiangYong', 'JiaoJiangSSZD'),
                             vsl_type %in% c('0.5W', '1.3-1.5W', '1-1.25W'))
p3 <- ggplot(data = major_shipment_reg.df, mapping  = aes(x = ship_time, y = adjusted_cost)) + geom_point() + 
  facet_wrap(port ~ vsl_type) + geom_vline(xintercept = major_shipment_reg.df$bid_time, color = 'green', linetype = 2)
p3




major_shipment_reg.train.df = major_shipment_reg.df[1:343,]
major_shipment_reg.test.df = major_shipment_reg.df[344:432,]

model2_major <- lm(adjusted_cost ~ trend + ship_month_square + ship_month_value + vsl_type + port + total_weight, data = major_shipment_reg.train.df)
summary(model2_major)

tmp <- c(mean(abs(model2_major$residuals)), mean(abs(model2_major$residuals)/major_shipment_reg.train.df$adjusted_cost),summary(model2)$r.squared)
major_shipment_reg.test.df$port[major_shipment_reg.test.df$port == 'JiaoJiang'] = 'JiaoJiangSSZD'
result_test_raw <- data.frame(true = major_shipment_reg.test.df$adjusted_cost, pred = predict(model2_major, major_shipment_reg.test.df))
result_test_raw$error <- result_test_raw$true - result_test_raw$pred

tmp2 <- c(mean(abs(result_test_raw$error)), mean(abs(result_test_raw$error)/result_test_raw$true), NA)
summary_result_model2_major <- data.frame(train = tmp, test = tmp2)
rownames(summary_result_model2) <- c("APE", "MAPE", "R-Squared")
summary_result_model2



regress.tmp <- major_shipment_reg.df
regress.tmp$port[major_shipment_reg.df$port == 'JiaoJiang'] = 'JiaoJiangSSZD'
result_tmp <- data.frame(true = major_shipment_reg.df$adjusted_cost, pred = predict(model2_major, major_shipment_reg.df), ship_time = major_shipment_reg.df$ship_time)
plot.df <- melt(result_tmp, id.vars = "ship_time")
model2_major.p <- ggplot(plot.df, aes(x = ship_time, y = value, group = variable, color = variable)) + geom_line() + geom_point() + 
  geom_vline(xintercept = major_shipment_reg.df$ship_time[344])
model2_major.p












## Sanity check
#tmp <- regress.df %>% select(-1)
#tmp2 <- unique(tmp)
## Passed! No repetition of rows



## playing around with R
remove(x, y, test_df)
x = runif(1000, 1, 10)
y = 0.5 * x ^ 2 - 5 * x + 10 + rnorm(length(x), 0, 1)
test_df <- tibble(x,  y)
test_df$x_2 <- x^2
test_m1 <- lm(y ~ x^2 + x, data = test_df)
ggplot(test_df) + geom_point(mapping = aes(x = x, y = y))
summary(test_m1)






