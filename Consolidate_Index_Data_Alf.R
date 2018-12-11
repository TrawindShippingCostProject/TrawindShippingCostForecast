sessionInfo()
remove(list = ls())
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(reshape)
setwd("/Users/alf/Desktop/Trawind proj local/TrawindShippingCostForecast Alf/All data")
save(, file = "cleaned_data/.Rda")

# All date would be named "Date" with the same format "YYYY-MM-DD"

# BDI data
x <- read_csv("BDI.csv")
x <- rename(x, c(date="Date"))
x$Date <- as.Date(x$Date, "%Y/%m/%d")
sapply(x, class)
c(min(x$Date), max(x$Date))
BDI <- x
save(BDI, file = "cleaned_data/BDI.Rda")
remove(BDI, x)


# Petroleum 380 ***************  ATT: Has to be updated, the length is too short, which only starts 2017-12-06
x <- read_excel("Petro380_HK.xlsx")
x <- rename(x, c("LQM Petroleum 380 Centistoke Bunker Fuel Spot Price/Hong Kong China" = "Fuel380"))
x$Date <- as.Date(x$Date, "%Y/%m/%d")
names(x)
sapply(x, class)
c(min(x$Date), max(x$Date))
Petro380_HK <- x
save(Petro380_HK, file = "cleaned_data/Petro380_HK.Rda")
remove(Petro380_HK, x)



# China Coastal Bulk Coal Freight Index *************** ATT:: This has "Coal"
x <- read_excel("CN_Coastal_Bulk_Coal_Freight_Idx.xlsx")
names(x)
sapply(x, class)
x$Date <- as.Date(x$Date, "%Y/%m/%d")
x <- rename(x, c("China Coastal Bulk Coal Freight Index" = "CoastalBulkCoalFreightIdx"))
sapply(x, class)
c(min(x$Date), max(x$Date))
CN_CoastalBulkCoalFreightIdx <- x 
save(CN_CoastalBulkCoalFreightIdx, file = "cleaned_data/CN_CoastalBulkCoalFreightIdx.Rda")
remove(CN_CoastalBulkCoalFreightIdx, x)



# China Coastal Bulk Freight Index
x <- read_excel("CN_Coastal_Bulk_Freight_Idx.xlsx")
names(x)
sapply(x, class)
x$Date <- as.Date(x$Date, "%Y/%m/%d")
x <- rename(x, c("China Coastal Bulk Freight Index" = "CoastalBulkFreightIdx"))
c(min(x$Date), max(x$Date))
CN_CoastalBulkFreightIdx <- x 
save(CN_CoastalBulkFreightIdx, file = "cleaned_data/CN_CoastalBulkFreightIdx.Rda")
remove(CN_CoastalBulkFreightIdx, x)


# SHSPCBCF *************** ATT: Should get longer data, should also get a description of the data
x <- read_excel("SHSPCBCF.xlsx")
names(x)
sapply(x, class)
x$Date <- as.Date(x$Date, "%Y/%m/%d")
x <- rename(x, c("SHSPCBCF Index" = "SHSPCBCF_Idx"))
c(min(x$Date), max(x$Date))
SHSPCBCF <- x 
save(SHSPCBCF, file = "cleaned_data/SHSPCBCF.Rda")
remove(SHSPCBCF, x)



# Shenhai Idx 2016
x <- read_excel("2016_ShenhaiIndex_Shi.xlsx")
names(x)
sapply(x, class)
x <- rename(x, c("date" = "Date",
                 "北-张家港（2-4万吨）" = "N_ZhangJiaGang_2_4wt",
                 "北-常熟（4-5万吨）" = "N_ChangShu_4_5_wt",
                 "北-乍浦（2-3万吨）" = "N_ZhaPu_2_3_wt",
                 "北-上海闸港（1.6-1.8万吨）" = "N_ShanghaiZhaGang_16_18_kt",
                 "北-广州（6-7万吨）" = "N_GuangZhou_6_7_wt",
                 "综合指数" = "Shenhai_Idx"))
x$Date <- as.Date(x$Date, "%Y/%m/%d")
c(min(x$Date), max(x$Date))
Shenhai_2016 <- x 
save(Shenhai_2016, file = "cleaned_data/Shenhai_2016.Rda")
remove(Shenhai_2016, x)


# Shenhai Idx 2017
x <- read_csv("2017_ShenhaiIndex_Shi.csv")
sapply(x, class)
x <- rename(x, c("date" = "Date",
                 "北-张家港（2-4万吨）" = "N_ZhangJiaGang_2_4wt",
                 "北-常熟（4-5万吨）" = "N_ChangShu_4_5_wt",
                 "北-乍浦（2-3万吨）" = "N_ZhaPu_2_3_wt",
                 "北-上海闸港（1.6-1.8万吨）" = "N_ShanghaiZhaGang_16_18_kt",
                 "北-广州（6-7万吨）" = "N_GuangZhou_6_7_wt",
                 "综合指数" = "Shenhai_Idx"))
# x$Date <- as.Date(x$Date, "%d/%m/%Y")
c(min(x$Date), max(x$Date))
sapply(x, class)
Shenhai_2017 <- x 
save(Shenhai_2017, file = "cleaned_data/Shenhai_2017.Rda")
remove(Shenhai_2017, x)


# Shenhai Idx 2018
x <- read_csv("2018_ShenhaiIndex_Shi.csv")
names(x)
sapply(x, class)
x <- rename(x, c("日期" = "Date",
                 "北-张家港（2-3万吨）" = "N_ZhangJiaGang_2_3wt",
                 "北方-常熟（4-5万吨）" = "N_ChangShu_4_5_wt",
                 "北方-乍浦（2-3万吨）" = "N_ZhaPu_2_3_wt",
                 "北-上海闸港（1.6-1.8万吨）" = "N_ShanghaiZhaGang_16_18_kt",
                 "北方-广州（6-7万吨）" = "N_GuangZhou_6_7_wt",
                 "综合指数" = "Shenhai_Idx"))
c(min(x$Date), max(x$Date))
sapply(x, class)
Shenhai_2018 <- x 
save(Shenhai_2018, file = "cleaned_data/Shenhai_2018.Rda")
remove(Shenhai_2018, x)


# Shanghang Index 
x <- read_excel("Shanghang_Idx_cleaned.xls")
names(x)
sapply(x, class)
x$Date <- as.Date(x$Date, "%Y-%m-%d")
sapply(x, class)
c(min(x$Date), max(x$Date))
Shanghang_Idx <- x
save(Shanghang_Idx, file = "cleaned_data/Shanghang_Idx.Rda")
remove(Shanghang_Idx, x)

# Shanghai Luowengang Price
x <- read_excel("shanghai_luowengang.xlsx")
x <- x %>% select(date, price)
x <- rename(x, c("date" = "Date", "price" = "SH_Luowengang_Price"))
x$Date <- as.Date(x$Date, "%Y-%m-%d")
c(min(x$Date), max(x$Date))
SH_Luowengang_Price <- x
save(SH_Luowengang_Price, file = "cleaned_data/SH_Luowengang_Price.Rda")
remove(SH_Luowengang_Price, x)


# Shanghai GangPi Price
x <- read_excel("shanghai_gangpi.xlsx")
x <- x %>% select(date, price)
x <- rename(x, c("date" = "Date", "price" = "SH_Gangpi_Price"))
x$Date <- as.Date(x$Date, "%Y-%m-%d")
sapply(x, class)
c(min(x$Date), max(x$Date))
SH_Gangpi_Price <- x
save(SH_Gangpi_Price, file = "cleaned_data/SH_Gangpi_Price.Rda")
remove(SH_Gangpi_Price, x)


# Guangzhou Luowengang Price
x <- read_excel("guangzhou_luowengang.xlsx")
x <- x %>% select(date, price)
x <- rename(x, c("date" = "Date", "price" = "GZ_Luowengang_Price"))
x$Date <- as.Date(x$Date, "%Y-%m-%d")
c(min(x$Date), max(x$Date))
GZ_Luowengang_Price <- x
save(GZ_Luowengang_Price, file = "cleaned_data/GZ_Luowengang_Price.Rda")
remove(GZ_Luowengang_Price, x)



# Guangzhou Luowengang Price
x <- read_excel("guangzhou_gangpi.xlsx")
x <- x %>% select(date, price)
x <- rename(x, c("date" = "Date", "price" = "GZ_Gangpi_Price"))
x$Date <- as.Date(x$Date, "%Y/%m/%d")
c(min(x$Date), max(x$Date))
GZ_Gangpi_Price <- x
save(GZ_Gangpi_Price, file = "cleaned_data/GZ_Gangpi_Price.Rda")
remove(GZ_Gangpi_Price, x)


# Consolidate Data
# Step 1) initiate an empty dataframe and create date column 
remove(list = ls())
date1 <- as.Date("2016-01-01")
date2 <- as.Date("2018-12-31")
Idx_df <- data.frame(Date = seq(date1, date2, by = "days"))
remove(date1, date2)

# Step 2) Load all cleanded index data
load("cleaned_data/BDI.Rda")
load("cleaned_data/Petro380_HK.Rda")
load("cleaned_data/CN_CoastalBulkCoalFreightIdx.Rda")
load("cleaned_data/CN_CoastalBulkFreightIdx.Rda")
load("cleaned_data/SHSPCBCF.Rda")
load("cleaned_data/Shenhai_2016.Rda")
load("cleaned_data/Shenhai_2017.Rda")
load("cleaned_data/Shenhai_2018.Rda")
load("cleaned_data/Shanghang_Idx.Rda")
load("cleaned_data/SH_Luowengang_Price.Rda")
load("cleaned_data/SH_Gangpi_Price.Rda")
load("cleaned_data/GZ_Luowengang_Price.Rda")
load("cleaned_data/GZ_Gangpi_Price.Rda")



# Step 3) Merging
Idx_df <- merge(x = Idx_df, y = BDI, by = "Date", all.x = TRUE)
Idx_df <- merge(x = Idx_df, y = Petro380_HK, by = "Date", all.x = TRUE)
Idx_df <- merge(x = Idx_df, y = CN_CoastalBulkCoalFreightIdx, by = "Date", all.x = TRUE)
Idx_df <- merge(x = Idx_df, y = CN_CoastalBulkFreightIdx, by = "Date", all.x = TRUE)
Idx_df <- merge(x = Idx_df, y = SHSPCBCF, by = "Date", all.x = TRUE)
Idx_df <- merge(x = Idx_df, y = Shenhai_2016, by = "Date", all.x = TRUE)
Idx_df <- merge(x = Idx_df, y = Shenhai_2017, by = "Date", all.x = TRUE)
Idx_df <- merge(x = Idx_df, y = Shenhai_2018, by = "Date", all.x = TRUE)
Idx_df <- merge(x = Idx_df, y = Shanghang_Idx, by = "Date", all.x = TRUE)
Idx_df <- merge(x = Idx_df, y = SH_Luowengang_Price, by = "Date", all.x = TRUE)
Idx_df <- merge(x = Idx_df, y = SH_Gangpi_Price, by = "Date", all.x = TRUE)
Idx_df <- merge(x = Idx_df, y = GZ_Luowengang_Price, by = "Date", all.x = TRUE)
Idx_df <- merge(x = Idx_df, y = GZ_Gangpi_Price, by = "Date", all.x = TRUE)

save(Idx_df, file = "cleaned_data/Idx_df.Rda")













