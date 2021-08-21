#################################################################
################## MAKRET PERFORMANCE TRACKER ###################
#################################################################

# Metrics: Day-over-Day (DoD), Week-over-Week (WoW), Month-over-Month (MoM), Year-over-Year (YoY)
# NOTE: Reporting period and trading days vary depending on timezone and asset class which is why asset classes need to be pulled in seperately
# End results should reflect 252 trading days

# Libraries ----

library(BatchGetSymbols)
library(rvest)
library(dplyr)
library(ggplot2)
library(quantmod)
library(tidyverse)
library(cowplot)
library(stargazer)
library(dplyr)
library(formattable)
library(tseries)
library(DT)
library(shiny)
library(webshot)
library(blastula)
library(keyring)

# Equities
df = BatchGetSymbols(
  tickers=c("^GSPC","^DJI","^IXIC","^RUT","^FTSE"),
  first.date = Sys.Date() - 366,
  last.date = Sys.Date()-1,
  thresh.bad.data = 0.75,
  bench.ticker = "^GSPC",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = TRUE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = FALSE,
  be.quiet = FALSE
)

equities <- df$df.tickers %>% 
  arrange(by_group=ref.date) %>% 
  select(c("ticker","price.close","ref.date"))%>%
  group_by(ticker) %>% 
  pivot_wider(id_cols = c("ref.date"),names_from = "ticker",values_from = "price.close") %>% 
  rename(`Dow Jones`="^DJI",`FTSE 100`="^FTSE",`S&P 500`="^GSPC",Nasdaq="^IXIC",`Russell 2000`="^RUT") %>% 
  pivot_longer(cols =c("Dow Jones","FTSE 100","S&P 500","Nasdaq","Russell 2000"), names_to = "ticker",values_to = "price.close") %>%
  group_by(ticker) %>%
  mutate(dod=percent((price.close/dplyr::lag(price.close,n=1))-1),
         wow=percent((price.close/dplyr::lag(price.close,n=5))-1),
         mom=percent((price.close/dplyr::lag(price.close,n=20))-1),
         yoy=percent((price.close/dplyr::lag(price.close,n=252))-1))%>% 
  filter(ref.date==Sys.Date()-1) %>% 
  rename(c("Last Price"=price.close)) %>%
  ungroup() %>% 
  select(-c("ref.date")) 
 
# U.S. 10 YR Treasury
df0 = BatchGetSymbols(
  tickers=c("^TNX"),
  first.date = Sys.Date() - 366,
  last.date = Sys.Date(),
  thresh.bad.data = 0.75,
  bench.ticker = "^GSPC",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = TRUE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = FALSE,
  be.quiet = FALSE
)

tres <- df0$df.tickers %>% 
  arrange(by_group=ref.date) %>% 
  select(c("ticker","price.close","ref.date"))%>%
  group_by(ticker)%>%
  pivot_wider(id_cols = c("ref.date"),names_from = "ticker",values_from = "price.close") %>% 
  rename(`10Yr Treasury*`="^TNX") %>% 
  pivot_longer(cols =c("10Yr Treasury*"), names_to = "ticker",values_to = "price.close") %>% 
  group_by(ticker) %>%
  mutate(dod=percent((price.close-dplyr::lag(price.close,n=1))/100),
         wow=percent((price.close-dplyr::lag(price.close,n=5))/100),
         mom=percent((price.close-dplyr::lag(price.close,n=20))/100),
         yoy=percent((price.close-dplyr::lag(price.close,n=248)))/100)%>% 
  filter(ref.date==Sys.Date()-1) %>% 
  rename(c("Last Price"=price.close)) %>%
  ungroup() %>% 
  select(-c("ref.date"))

# Commodities
df1 = BatchGetSymbols(
  tickers=c("CL=F","GC=F","SI=F"),
  first.date = Sys.Date() - 366,
  last.date = Sys.Date(),
  thresh.bad.data = 0.75,
  bench.ticker = "^GSPC",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = TRUE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = FALSE,
  be.quiet = FALSE
)

comms <- df1$df.tickers %>% 
  arrange(by_group=ref.date) %>% 
  select(c("ticker","price.close","ref.date"))%>%
  group_by(ticker)%>%
  pivot_wider(id_cols = c("ref.date"),names_from = "ticker",values_from = "price.close") %>% 
  rename(`Crude Oil`="CL=F",`Gold`="GC=F",`Silver`="SI=F") %>% 
  pivot_longer(cols =c("Crude Oil","Gold","Silver"), names_to = "ticker",values_to = "price.close") %>% 
  group_by(ticker) %>%
  mutate(dod=percent((price.close/dplyr::lag(price.close,n=1))-1),
         wow=percent((price.close/dplyr::lag(price.close,n=5))-1),
         mom=percent((price.close/dplyr::lag(price.close,n=20))-1),
         yoy=percent((price.close/dplyr::lag(price.close,n=250))-1))%>% 
  filter(ref.date==Sys.Date()-1) %>% 
  rename(c("Last Price"=price.close)) %>%
  ungroup() %>% 
  select(-c("ref.date"))

# Currencies
df2 = BatchGetSymbols(
  tickers=c("EURUSD=X","GBPUSD=X","JPY=X"),
  first.date = Sys.Date() - 366,
  last.date = Sys.Date()-1,
  thresh.bad.data = 0.75,
  bench.ticker = "^GSPC",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = FALSE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = FALSE,
  be.quiet = FALSE
)

curr <- df2$df.tickers %>% 
  arrange(by_group=ref.date) %>% 
  select(c("ticker","price.close","ref.date"))%>%
  group_by(ticker)%>%
  pivot_wider(id_cols = c("ref.date"),names_from = "ticker",values_from = "price.close") %>% 
  rename(`EUR/USD`="EURUSD=X",`GBP/USD`="GBPUSD=X",`USD/JPY`="JPY=X") %>% 
  pivot_longer(cols =c("EUR/USD","GBP/USD","USD/JPY"), names_to = "ticker",values_to = "price.close") %>% 
  group_by(ticker) %>%
  mutate(dod=percent((price.close/dplyr::lag(price.close,n=1))-1),
         wow=percent((price.close/dplyr::lag(price.close,n=5))-1),
         mom=percent((price.close/dplyr::lag(price.close,n=20))-1),
         yoy=percent((price.close/dplyr::lag(price.close,n=261))-1))%>% 
  filter(ref.date==Sys.Date()-1) %>% 
  rename(c("Last Price"=price.close)) %>%
  ungroup() %>% 
  select(-c("ref.date"))

# Crypto (e.g. of differnce due to asset class trading days) 
df3 = BatchGetSymbols(
  tickers=c("BTC-USD","ETH-USD","ADA-USD","LINK-USD","DOGE-USD"),
  first.date = Sys.Date() - 366,
  last.date = Sys.Date()-1,
  thresh.bad.data = 0.75,
  bench.ticker = "^GSPC",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = TRUE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = FALSE,
  be.quiet = FALSE
)

crypto <- df3$df.tickers %>% 
  arrange(by_group=ref.date) %>% 
  select(c("ticker","price.close","ref.date"))%>%
  group_by(ticker)%>%
  pivot_wider(id_cols = c("ref.date"),names_from = "ticker",values_from = "price.close") %>% 
  rename(Bitcoin="BTC-USD",Ethereum="ETH-USD",Cardano="ADA-USD",Chainlink="LINK-USD",Dogecoin="DOGE-USD") %>% 
  pivot_longer(cols =c("Bitcoin","Ethereum","Cardano","Chainlink","Dogecoin"), names_to = "ticker",values_to = "price.close") %>% 
  group_by(ticker) %>%
  mutate(dod=percent((price.close/dplyr::lag(price.close,n=1))-1),
         wow=percent((price.close/dplyr::lag(price.close,n=7))-1),
         mom=percent((price.close/dplyr::lag(price.close,n=30))-1),
         yoy=percent((price.close/dplyr::lag(price.close,n=362))-1))%>% 
  filter(ref.date==Sys.Date()-1) %>% 
  rename(c("Last Price"=price.close)) %>%
  ungroup() %>% 
  select(-c("ref.date"))

# Nikkie (e.g. of differnce due to timezone) 
df4 = BatchGetSymbols(
  tickers=c("^N225"),
  first.date = Sys.Date() - 366,
  last.date = Sys.Date()-1,
  thresh.bad.data = 0.75,
  bench.ticker = "^GSPC",
  type.return = "arit",
  freq.data = "daily",
  how.to.aggregate = "last",
  do.complete.data = TRUE,
  do.fill.missing.prices = TRUE,
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"),
  do.parallel = FALSE,
  be.quiet = FALSE
)

nik = df4$df.tickers %>%
  arrange(ref.date)%>%
  select(c("ticker","price.close","ref.date"))%>%
  group_by(ticker)%>%
  pivot_wider(id_cols = c("ref.date"),names_from = "ticker",values_from = "price.close") %>% 
  rename(`Nikkei 225`="^N225") %>% 
  pivot_longer(cols =c("Nikkei 225"), names_to = "ticker",values_to = "price.close") %>% 
  group_by(ticker) %>%
  mutate(dod=percent((price.close/dplyr::lag(price.close,n=1))-1),
         wow=percent((price.close/dplyr::lag(price.close,n=5))-1),
         mom=percent((price.close/dplyr::lag(price.close,n=20))-1),
         yoy=percent((price.close/dplyr::lag(price.close,n=244))-1))%>% 
  filter(ref.date==Sys.Date()-1) %>% 
  rename(c("Last Price"=price.close)) %>%
  ungroup() %>% 
  select(-c("ref.date")) 

################################################
################  COMBINE  ALL  ################
################################################

## ADD CUSTOM BASKET CODE HERE AND add "index" in to rbind 

table <- rbind(equities, nik, tres, comms, curr, crypto) %>% 
  rename(Price="Last Price",DoD="dod",WoW="wow",MoM="mom",YoY="yoy") 

## Control row order
final_table <-  table[c(1,3,5,4,19,2,6,7,11,12,13,9,10,8,15,17,14,18,16),] %>% 
  rename(" "=ticker)

################################################
###################  EXPORT  ###################
################################################

## Create function to control decimal points in table [for "Price" specifically]
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

## Create function to add dataframe into for formattable (formattble needs a function)
## Positive returns will be green and negative returns red 

table_format <- function(df){
  final.table = as.htmlwidget(formattable(df,list(
    `Price`= formatter("span", specify_decimal(df$Price,1)),
    `DoD` = formatter("span", style = x ~ style(color = ifelse(x < 0, "red", "green"))),
    `WoW` = formatter("span", style = x ~ style(color = ifelse(x < 0, "red", "green"))),
    `MoM` = formatter("span", style = x ~ style(color = ifelse(x < 0, "red", "green"))),
    `YoY` = formatter("span", style = x ~ style(color = ifelse(x < 0, "red", "green"))))))
  final.table$dependencies <- c(final.table$dependencies)
  final.table
}  

## Create function to save as a .png and .html file 

table_webshot <- function(input, path){
  dirname <- "c:/FOLDER"  ##REPLACE WITH YOUR OWN PATH
  saveWidget(input, file.path(dirname, paste(path, ".html", sep = "")), selfcontained = FALSE) 
  webshot::webshot(file.path(dirname, paste(path, ".html", sep = "")), file.path(dirname, paste(path, ".png", sep = "")))
}

## Run it...
Tracker_ <- table_format(final_table)

# KEEP IT FLEXIBLE
today=Sys.Date()
name = paste("Tracker_",today,sep="")

# NOTE: Adding "today" in to "name" will save the file with the date the code is run. It will be helpful if you decide to add this script to task scheduler to run once a day.

################################################
##################  DRUM ROLL  #################
################################################

table_webshot(Tracker_, name)
