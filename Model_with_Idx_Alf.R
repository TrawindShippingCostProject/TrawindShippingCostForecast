sessionInfo()
remove(list = ls())
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(reshape)
library(zoo)
setwd("/Users/alf/Desktop/Trawind proj local/TrawindShippingCostForecast")

# Load two major dataset
load("Preprocessed_Ship_data_Dec2018.Rda")
load("cleaned_idx_data/Idx_df.Rda")

# Temporarily not using Fuel380, SHSP..
Idx_df <- subset(Idx_df, select = -c(Fuel380, SHSPCBCF_Idx))

# Fill na with previous value
Idx_df <- Idx_df %>% do(na.locf(.))

# merge into one 
test_df = merge(x = regress.df, y = Idx_df, by.x = "contract_time", by.y = "Date", all.x = TRUE)

# Testing filling up missing values
df1 <- Idx_df %>% select(Date, BDI, CoastalBulkCoalFreightIdx)
sapply(df1, class)
df2 <- df1 %>% do(na.locf(.))


df3 <- regress.df %>% select(bid_time, contract_time)
df3 %>% sapply(class)
df3$diff <- df3$contract_time - df3$bid_time


df4 <- data.frame(c1 = c(1,2,3,4,5,6,7,8,9,10))
mutate(df4, diff = c1 - lag(c1, 3))


# note
# 1. go back Consolidate Index Data and fill na first before merging table
# 2. get the trend column (1 week, 2 weeks, 1 month)
# 3. when merge with the 531 table, use the bid date
# 4. add a column of how many days between contract date and bid date

df1 <- data.frame(c1 = c(1,2,3,4,5,6,7,8,9,10))
df2 <- data.frame(c1 = c(1,2,3,4,5,6,7,8,9,10), c2 = c('a','b','c','d','e',NA, NA, NA, NA, NA))
df3 <- data.frame(c1 = c(1,2,3,4,5,6,7,8,9,10), c2 = c(NA, NA, NA, NA, NA, 'f','g','h','i', 'j'))
df_m <- merge(x = df1, y = df2, x.by = c1, y.by = c1, x.all = TRUE)
df_m <- merge(x = df_m, y = df3, x.by = c1, y.by = c1, x.all = TRUE)




