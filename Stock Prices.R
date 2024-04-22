tuesdata <- tidytuesdayR::tt_load('2023-02-07')

library(tidyverse)

big_tech_stock_prices <- tuesdata$big_tech_stock_prices
big_tech_companies <- tuesdata$big_tech_companies


risk_free <- read.csv("TB1YR.csv")


colnames(risk_free)[1] <- "date"
colnames(risk_free)[2] <- "risk_free"

risk_free <- risk_free %>% mutate(date = as.Date(date))

market <- big_tech_stock_prices %>% group_by(date = lubridate::floor_date(date, "month"), stock_symbol) %>% 
  summarise(volume = sum(volume, na.rm=TRUE)) %>% 
  mutate(weight = volume / sum(volume))


company_monthly_return <- big_tech_stock_prices %>% 
  group_by(date = lubridate::floor_date(date, "month"), stock_symbol) %>%
  summarise(monthly_return = prod(1+(close-open)/open)) %>% 
  mutate(monthly_return = monthly_return - 1)

market <- cbind(market, return = company_monthly_return$monthly_return)

view(market)

market_return <- market %>% group_by(date) %>% summarise(SP_14 = sum(return*weight))

view(market_return)

SP_14_rf <- market_return %>% left_join(risk_free, by = "date")

AAPL <- company_monthly_return %>% filter(stock_symbol == "AAPL") %>% select(-stock_symbol)

AAPL_and_market <- SP_14_rf %>% left_join(AAPL, by = "date") %>% drop_na()

market_sd <- sd(SP_14_rf$SP_14)

AAPL_sd <- sd(AAPL$monthly_return)

AAPL_and_market_cor <- cor(AAPL_and_market$SP_14, AAPL_and_market$monthly_return)

AAPL_Beta <- AAPL_and_market_cor*AAPL_sd/market_sd

AAPL_and_market <- AAPL_and_market %>% mutate(AAPL_risk_premium = monthly_return - risk_free, 
                           Market_risk_premium = SP_14 - risk_free)


AAPL_and_market %>% ggplot(aes(Market_risk_premium, AAPL_risk_premium)) + geom_point() + 
  geom_smooth(method="lm")

CAPM <- lm(AAPL_risk_premium ~ Market_risk_premium, data = AAPL_and_market)

summary(CAPM)

