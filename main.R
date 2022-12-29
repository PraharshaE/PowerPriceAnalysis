## Kindly refer to the rmd file for elaborated comments and pseudo code

library(tidyverse)
library(data.table)
library(dplyr)
library(magrittr)
library(ggplot2)

## Task 1
file_names <- list.files(path = "historicalPriceData", pattern = "*.csv", full.names = TRUE) 
orig_df <- do.call(rbind, lapply(file_names, fread))
orig_df

## Task 2
average_settlementpoint_prices <- orig_df %>%
  mutate(Year = lubridate::year(Date), Month = lubridate::month(Date)) %>%
  group_by(SettlementPoint, Year, Month) %>%
  summarize(AveragePrice = mean(Price))
average_settlementpoint_prices

## Task 3
dir.create("Outputs")
write.csv(average_settlementpoint_prices, "Outputs/AveragePriceByMonth.csv", row.names=FALSE)

## Task 4
hourly_prices <- orig_df %>%
  filter(!grepl('^LZ_*', SettlementPoint)) %>%
  mutate(Year = lubridate::year(Date)) %>%
  group_by(Year, SettlementPoint) %>%
  summarize(HourlyVolatility = sd(log(Price)))
hourly_prices

## Task 5
write.csv(hourly_prices, "Outputs/HourlyVolatilityByYear.csv", row.names=FALSE)

## Task 6
max_volatility <- hourly_prices %>%
  group_by(Year) %>%
  slice_max(HourlyVolatility, n = 1)
max_volatility

write.csv(max_volatility, "Outputs/MaxVolatilityByYear.csv", row.names=FALSE)


## Task 7

##1. Ensuring that the date column is in expected datetime format which is mm/dd/yyyy hh:mm:ss
##2. Split theColumn "Date" into individual "Date" and "Hour of the day" columns
##3. Pivot/Unpivot the Hour column into Rows and assign rowheaders x1:x24 so the dataframe now has the columns Settlement point, Date,X1:X24
##3. Split the data frame into multiple data frames on the unique values present in SettlementPoint Column and write each dataframe to separate csv files named according to the format "spot_<SettlementPoint>" in the subdirectory "formattedSpotHistory"



df <- orig_df
df$Date <- as.Date(orig_df$Date)
df <- orig_df %>%  mutate_at(vars(SettlementPoint),as.factor)
df_split <- split(df, df$SettlementPoint) 
##lapply(df_dat, function(x) write.csv(x, paste0('df_', x$SettlementPoint[1], '.csv'), row.names = FALSE))


avg_monthly_prices <- average_settlementpoint_prices %>% mutate(Date = lubridate::make_date(year = Year, month = Month, day = 1))
avg_hub_price <- avg_monthly_prices %>% filter(!grepl('^LZ_*', SettlementPoint))
avg_lz_price <- avg_monthly_prices %>% filter(!grepl('^HB_*', SettlementPoint))
ggplot(avg_hub_price, aes(Date, AveragePrice, color = SettlementPoint)) + geom_line() + xlab("Date") + ylab("Average Price")
ggplot(avg_lz_price, aes(Date, AveragePrice, color = SettlementPoint)) + geom_line() + xlab("Date") + ylab("Average Price")
ggsave("Outputs/SettlementHubAveragePriceByMonth.png", avg_hub_price)
ggsave("Outputs/LoadZoneAveragePriceByMonth.png", avg_lz_price)