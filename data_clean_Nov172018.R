remove(list = ls())
library(ggplot2)
library(dplyr)
setwd("/Users/hengzhang/Dropbox/My Projects/Xinfeng Private/TrawindShippingCostForecast")
load("Cleaned_Trawind_Data_Eng_V0.RData")
my.df <- xinfeng_alf_cleaned_eng

######### Clean data: translate Chinese into English and convert to factors
df.clean <- my.df %>% select(1:4)

# Add column "vsl_company"
fun.convert <- function(x){
  if (x == "华德")
      return("HuaDe")
  else if (x == "中海北方")
      return("ZhongHaiBF")
  else if (x == "利信")
      return("LiXin")
  else if (x == "沪航")
      return("HuHang")
  else if (x == "中集")
      return("ZhongJi")
  }
df.clean$vsl_company <- as.factor(sapply(my.df$vsl_company, FUN = fun.convert))

### Add column "vsl_name"
vsl_name_list <- unique(my.df$vsl_name)
fun.convert <- function(x){which(vsl_name_list %in%  x)}
df.clean$vsl_name <- as.factor(sapply(my.df$vsl_name, FUN = fun.convert))

### Write it into csv for future reference
to.write.df <- data.frame(vsl_name_list, 1:length(vsl_name_list))
write.csv(to.write.df, file = "tmp.csv", fileEncoding = "UTF-8")

### Add column "vsl_type", "cargo_type"
df.clean$vsl_type <- as.factor(my.df$vsl_type)

### Note: the columns "customer", "start_port", "cargo_type" are removed since they only contain
# single values

### Add column "total_weight"
df.clean$total_weight <- my.df$total_weight

### Add column "port"
# Write the port names into csv for future reference
port_list <- unique(my.df$p)
write.csv(data.frame(port_list), file = "tmp.csv", fileEncoding = "UTF-8")

# Then add the column
fun.convert <- function(x){
    if (x == "宁波宝达") {return("NingBoBaoDa")}
    else if (x == "宁波海兆"){return("NingBoHaiZhao")}
    else if (x == "宁波华埠"){return("NingBoHuaPu")}
    else if (x == "宁波江甬"){return("NingBoJiangYong")}
    else if (x == "椒江三山再东"){return("JiaoJiangSSZD")}
    else if (x == "宁波镇海"){return("NingBoZhenHai")}
    else if (x == "宁波北仑永港"){return("NingBoBLYG")}
    else if (x == "宁波大榭"){return("NingBoDaXie")}
    else if (x == "象山"){return("XiangShan")}
    else if (x == "椒江八达"){return("JiaoJiangBaDa")}
    else if (x == "玉环"){return("YuHuan")}
    else if (x == "椒江大华"){return("JiaoJiangDaHua")}
    else if (x == "椒江再东"){return("JiaoJiangZaiDong")}
    else if (x == "椒江三山八达"){return("JiaoJiangSSBD")}
    else if (x == "椒江三山大华"){return("JiaoJiangSSDH")}
    else if (x == "椒江"){return("JiaoJiang")}
}
df.clean$port <- as.factor(sapply(my.df$p, FUN = fun.convert))

### Remove column "port_type" since it is repeat with "port_type_value"

### Add columns "port_type_value", "port_price_diff", "distance"
df.clean$port_type_value <- as.numeric(my.df$port_type_value)
df.clean$port_price_diff <- as.numeric(my.df$port_price_diff)
df.clean$distance <- as.numeric(my.df$distance)

### Remove column "customer_quote_time" since only NA values
### Add columns "income", "cost", "bang_dian_cost", "chao_qi_cost", "kau_gang_cost",
# "same_time_cost"
df.clean$income <- as.numeric(my.df$actual_income)
df.clean$cost <- as.numeric(my.df$actual_cost)
df.clean <- cbind(df.clean, my.df %>% select(22:24))
for (i in 15:17){df.clean[is.na(df.clean[,i]),i] = 0}
colnames(df.clean)[17] = "kua_gang_cost"

### Remove columns "same_time_competition" since they are all NAs. 
### Remove columns "other_factor" since they are all NAs
save(file = "clean_data_nov172018.RData", list = c("df.clean"))

## Testing area

