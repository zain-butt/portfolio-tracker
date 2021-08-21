##Create Your Custom basket
#E.g. 75/25 stock/crypto portfolio 
#Note: Reporting of data varies based on asset classes
#All data is publically available from yahoo finance 

start_value = 100
crypto_weight = 0.25
equity_weight = 0.75

# Input ticker (check yahoo finance)

################################################
###################  CRYPTO  ###################
################################################

# Pulling data
crypto_basket = BatchGetSymbols(
  tickers=c("BTC-USD","ETH-USD","ADA-USD","LINK-USD"),
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
  be.quiet = FALSE)

################################################
##################  EQUITIES  ##################
################################################
 
 # Pulling data
equity_basket = BatchGetSymbols(
  tickers=c("PAWZ","BUG","BOTZ","PRNT","URA","SNSR","KOMP","ESPO","EDOC","GNOM","CLOU"),
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
  be.quiet = FALSE)


################################################
####################  INDEX  ###################
################################################

# Assigning indexdate for start_value

indexdate = "2021-01-01"

index_crypto <- crypto_basket$df.tickers %>% 
  arrange(by_group=ref.date) %>% 
  select(c("ticker","price.close","ref.date"))%>%
  group_by(ref.date) %>%
  mutate(tot=sum(price.close),
         tickers=length(unique(crypto_basket$df.tickers[["ticker"]])),
         invested=(start_value*crypto_weight)/tickers,
         units=invested/price.close)%>% 
  filter(ref.date==indexdate) %>%     
  rename(index_date="ref.date") %>% 
  select(c("ticker","index_date","units"))

index_equity <- equity_basket$df.tickers %>% 
  arrange(by_group=ref.date) %>% 
  select(c("ticker","price.close","ref.date"))%>%
  group_by(ref.date) %>%
  mutate(tot=sum(price.close),
         tickers=length(unique(equity_basket$df.tickers[["ticker"]])),
         invested=(start_value*equity_weight)/tickers,
         units=invested/price.close)%>% 
  filter(ref.date==indexdate) %>% 
  rename(index_date="ref.date") %>% 
  select(c("ticker","index_date","units"))

################################################
###############  Calculate Index  ##############
################################################

crypto <- crypto_basket$df.tickers %>% 
  arrange(by_group=ref.date) %>% 
  select(c("ticker","price.close","ref.date"))%>%
  group_by(ref.date) %>%
  group_by(ticker)%>%
  left_join(index_crypto,by =c("ticker"="ticker") ) %>%
  group_by(ticker)%>%
  filter(ref.date<=Sys.Date()-1) %>%
  mutate(ret=as.numeric(price.close*units)) %>%
  pivot_wider(id_cols = "ref.date",names_from = "ticker",values_from = "ret") %>% 
  mutate(crp_rt=`ADA-USD`+`BTC-USD`+`ETH-USD`+`LINK-USD`) %>% 
  select(c("ref.date","crp_rt")) %>% 
  pivot_longer(cols =c("crp_rt"), names_to = "sub",values_to = "return") 

equity <- equity_basket$df.tickers %>% 
  arrange(by_group=ref.date) %>% 
  select(c("ticker","price.close","ref.date"))%>%
  group_by(ref.date) %>%
  group_by(ticker)%>%
  left_join(index_equity,by =c("ticker"="ticker") ) %>%
  group_by(ticker)%>%
  filter(ref.date<=Sys.Date()-1) %>% 
  mutate(ret=as.numeric(price.close*units)) %>% 
  pivot_wider(id_cols = "ref.date",names_from = "ticker",values_from = "ret") %>% 
  mutate(eq_rt= BOTZ+BUG+CLOU+EDOC+ESPO+GNOM+KOMP+PAWZ+PRNT+SNSR+URA) %>% 
  select(c("ref.date","eq_rt")) %>% 
  pivot_longer(cols =c("eq_rt"), names_to = "sub",values_to = "return") 
  

################################################
###########  Calculated performance  ###########
################################################

index  <-  merge(crypto, equity, by="ref.date", all = T) %>% 
  filter(!is.na(return.x)) %>%
  filter(!is.na(return.y)) %>% 
  mutate(Price=return.x+return.y) %>% 
  select(-c("sub.x","sub.y","return.x","return.y")) %>% 
  mutate(ticker="Hello World Index",
         dod=percent((Price-dplyr::lag(Price,n=1))/100),
         wow=percent((Price-dplyr::lag(Price,n=5))/100),
         mom=percent((Price-dplyr::lag(Price,n=20))/100),
         yoy=percent((Price-dplyr::lag(Price,n=249)))/100)%>% 
  filter(ref.date==Sys.Date()-1) %>% 
  rename(c(`Last Price`=Price)) %>%
  ungroup() %>% 
  select(-c("ref.date")) %>% 
  relocate(ticker,.before = "Last Price")
