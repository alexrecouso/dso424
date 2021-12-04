data$Month = data %>% select(Month) %>% unlist %>%  
  as.numeric
#we need to assume 28 as the last day of each month for consistency
data_monthly = data_monthly %>% mutate(Date = str_c(Year, Month, '28', sep='-'))
data_monthly$Date = as.POSIXlt(data_monthly$Date, format="%Y-%m-%d")
data_monthly %>% head