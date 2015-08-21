library(dplyr)
library(reshape2)
library(ggplot2)
library(RSQLServer)
#########################################################################################################################

channel <- src_sqlserver(server="SQL", database="XY", user="libo.jin", password="123456")
data <- list()
data$ReturnDaily <- tbl(channel, "ReturnDaily") %>%
  filter(IfTradingDay == 1) %>%
  select(InnerCode, CompanyCode, SecuCode, SecuAbbr, TradingDay, DailyReturn,
         MarketCap, FloatMarketCap, IndustryCodeNew, IndustryNameNew, IfWeekEnd, IfMonthEnd,IfSuspended) %>%
  collect %>%
  mutate(TradingDay = as.Date(TradingDay))






startdate <- as.Date("2004-12-29")
enddate <- as.Date("2015-6-29")

trading_date <- data$TradingDay %>%
  filter(IfMonthEnd == 1, TradingDate >= startdate & TradingDate <= enddate) %>%
  select(TradingDate) %>%
  mutate(Start = lag(TradingDate)) %>%
  rename(End =  TradingDate) %>%
  select(Start, End) %>%
  na.omit()

nIndexCode <- 4982

stock_return_demean <- data$ReturnDaily %>%
  group_by(InnerCode) %>%
  arrange(TradingDay) %>%
  mutate(LagFloatMarketCap = lag(FloatMarketCap)) %>%
  filter(!is.na(LagFloatMarketCap)) %>%
  inner_join(data$IndexComponent %>% filter(IndexInnerCode == nIndexCode), by = c("InnerCode" = "SecuInnerCode")) %>%
  filter(TradingDay >= InDate & TradingDay < OutDate) %>% 
  group_by(TradingDay) %>% 
  mutate(DemeanReturn = DailyReturn - weighted.mean(DailyReturn, LagFloatMarketCap)) %>%
  ungroup()

##################   industry return first   monthly return second
monthly_return <- data.frame()
for(i in c(1:nrow(trading_date))){
  start <- trading_date[[i, 1]]
  end <- trading_date[[i, 2]]
  monthly_return_temp <- stock_return_demean %>% 
    filter(TradingDay > start & TradingDay <= end) %>%
    group_by(InnerCode) %>%
    summarise(MonthlyReturn = expm1(sum(log1p(DemeanReturn)))) %>%
    mutate(Start = start, End = end) %>%
    inner_join(data$ReturnDaily %>% filter(TradingDay == start) %>% mutate(LagShares = FloatMarketCap/ClosePrice) %>%
                 select(InnerCode, FloatMarketCap, LagShares), by = "InnerCode") %>%
    rename(LagFloatMarketCap = FloatMarketCap) %>%
    inner_join(data$ReturnDaily %>% filter(TradingDay == end) %>% mutate(Shares =  FloatMarketCap/ClosePrice) %>%
                 select(InnerCode, FloatMarketCap, Shares), by = "InnerCode")
  monthly_return <- rbind(monthly_return, monthly_return_temp)
}   


WeightingScheme <- function(x){
  x <- as.vector(x)
  weigth_data <- vector(length = length(x))
  weigth <- c(0.2, 0.4, 0.6, 0.8, rep(1, 16), 0.8, 0.6, 0.4, 0.2)
  for(i in c(1:length(x))){
    if(i < 24) weigth_data[i] <- NA else weigth_data[i] <- sum(x[(i-23):i]*weigth)
  }
  return(weigth_data)
}

code <- unique(monthly_return$InnerCode)
dilution <- data.frame()
for(i in code) {
  temp <- monthly_return %>% 
    filter(InnerCode == i) %>% 
    arrange(End) %>%
    mutate(Dilution = log(LagShares/Shares), Dilution2 = log((LagFloatMarketCap/FloatMarketCap)*(1+MonthlyReturn))) %>%
    mutate(SumDilution = WeightingScheme(Dilution), SumDilution2 = WeightingScheme(Dilution2)) %>%
    select(InnerCode, End, FloatMarketCap, Dilution, Dilution2, SumDilution, SumDilution2)
  
  dilution <- rbind(dilution, temp)
}

####################################################################################################
DeMean <- function(Data, half_life){
  Data <- as.vector(Data)
  ExponentialSmooth <- function(x, lambda){
    ans <- x
    oneml <- 1 - lambda
    for(i in 2:length(x)) {
      ans[i] <- lambda * ans[i-1] + oneml * x[i]
    }
    
    return(ans)
  }
  
  DeMean <- Data
  for(i in c(1:length(Data)))
    if(i > half_life){
      lambda <- 1 - log(2)/(i - half_life)
      temp <- ExponentialSmooth(Data[1:i], lambda)
      DeMean[i] <- Data[i] - temp[i]
    }else  DeMean[i] <- NA   
  
  return(DeMean)
}
 

Order <- function(data){
  temp <- data.frame(RawDate = data, Rank = percent_rank(desc(data)))
  order <- temp %>%
    mutate(Order = ifelse(Rank <= 1/5, "First",
                          ifelse(Rank > 1/5 & Rank <= 2/5, "Second",
                                 ifelse(Rank > 2/5 & Rank <= 3/5, "Third",
                                        ifelse(Rank > 3/5 & Rank <= 4/5, "Fourth", "Fifth"))))) %>%
    mutate(Order = factor(Order,order = TRUE, levels = c("First", "Second", "Third", "Fourth", "Fifth")))
  return(order$Order)
}


################################################################################################


nIndexCode <- 4982 # 确认市场指数， 全流通 4088，中证500 4978， 中证800 4982， 沪深300 3145  
industry_dilution_winsorise <- dilution %>%
  inner_join(data$IndexComponent %>% filter(IndexInnerCode == nIndexCode), by = c("InnerCode" = "SecuInnerCode")) %>%
  filter(End >= InDate & End < OutDate) %>%  # 挑出成分股
  inner_join(monthly_return %>% select(InnerCode, MonthlyReturn, Start, End), by = c("InnerCode", "End" = "Start")) %>%
  rename(Start = End, End = End.y) %>%
  filter(!is.na(SumDilution), !is.na(SumDilution2)) %>%
  group_by(InnerCode) %>%
  mutate(WinsoriseDilution = ifelse(SumDilution < quantile(SumDilution, 0.005), quantile(SumDilution, 0.005),
                                    ifelse(SumDilution > quantile(SumDilution, 0.995), quantile(SumDilution, 0.995), SumDilution)),
         WinsoriseDilution2 = ifelse(SumDilution2 < quantile(SumDilution2, 0.005), quantile(SumDilution2, 0.005),
                                    ifelse(SumDilution2 > quantile(SumDilution2, 0.995), quantile(SumDilution2, 0.995), SumDilution2)))  %>%
  filter(abs(WinsoriseDilution) <= 0.3, abs(WinsoriseDilution2) <= 0.3)  %>%
  ungroup() %>%
  inner_join(data$ReturnDaily %>% select(InnerCode, TradingDay, IndustryNameNew), by = c("InnerCode", "Start" = "TradingDay")) %>%
  group_by(IndustryNameNew, Start, End) %>%
  summarise(Dilution = weighted.mean(WinsoriseDilution, sqrt(FloatMarketCap)),
            Dilution2 = weighted.mean(WinsoriseDilution2, sqrt(FloatMarketCap)),
            Return = weighted.mean(MonthlyReturn, FloatMarketCap),
            FloatMarketCap = sum(FloatMarketCap)) %>%
  ungroup()


smooth_number <- 3
industry_dilution_smooth <- industry_dilution_winsorise %>%
  group_by(IndustryNameNew) %>%
  arrange(Start) %>%
#   mutate(SmoothDilution1 = EMA(Dilution, smooth_number),
#          SmoothDilution2 = EMA(Dilution2, smooth_number)) %>%
  mutate(SmoothDilution1 = Dilution,
         SmoothDilution2 = Dilution2) %>%
  filter(!is.na(SmoothDilution1), !is.na(SmoothDilution2)) %>%
  ungroup()

demean_number <- 18
industry_dilution_demean <- industry_dilution_smooth %>%
  group_by(IndustryNameNew) %>%
  arrange(Start) %>%
  # mutate(DeMean = SmoothDilution - EMA(Dilution, demean_number))  %>%
  mutate(DeMean = DeMean(SmoothDilution1, demean_number),
         DeMean2 = DeMean(SmoothDilution2, demean_number))  %>%
  ungroup() %>% 
  na.omit()




portfolio  <- industry_dilution_demean %>%
  group_by(End) %>%
  mutate(Order1 = Order(DeMean),
         Order2 = Order(DeMean2)) %>%
  select(Start, End, IndustryNameNew, Return, FloatMarketCap, DeMean, DeMean2, Order1, Order2) %>%
  melt(id = c("Start", "End", "IndustryNameNew", "Return", "FloatMarketCap", "DeMean", "DeMean2")) %>%
  group_by(variable, value, Start, End) %>%
  summarise(PortfolioReturn_equal = mean(Return),
            PortfolioReturn_weight = weighted.mean(Return, FloatMarketCap)) %>%
  group_by(variable, value) %>%
  arrange(End) %>%
  mutate(CumlateRetun_equal = expm1(cumsum(log1p(PortfolioReturn_equal))),
         CumlateRetun_weight = expm1(cumsum(log1p(PortfolioReturn_weight)))) %>%
  ungroup() %>% 
  select(variable, value, Start, End, CumlateRetun_equal, CumlateRetun_weight) %>%
  rename(Math = variable, Order = value) %>%
  melt(id = c("Math", "Order", "Start", "End")) %>% 
  mutate(Order = factor(Order,order = TRUE, levels = c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh")))


ggplot(portfolio, aes(x = End, y =  value, color = Order)) + geom_line() +
  ggtitle(paste("smooth ", smooth_number, " demean ", demean_number)) +
  xlab(NULL) + ylab(NULL) +
  facet_grid(Math ~ variable)



 













