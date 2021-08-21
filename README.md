# portfolio-tracker

Create automated email to compare performance of custom index relative to various asset classes using publically available data from Yahoo finance.

# Libraries used:
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
library(mailR)

# Order of operations:
1. Custom basket.R 
2. tracker.R
3. Email.R 
4. Optional/not covered: Add to task scheduler and receive daily email

# What's next:
Working through the rebalancing component.
