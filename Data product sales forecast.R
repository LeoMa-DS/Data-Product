
# update forecast package for xgboost
install.packages("devtools")
library(devtools)
devtools::install_github("ellisp/forecastxgb-r-package/pkg")
library(forecastxgb)

# read excel 
library("readxl")

# data preparation 
library(dplyr)
library(tidyverse)
library(lubridate)
library(tseries)


# algorithm

library(smooth) 

data1 <- read_xlsx("online_retail_ll.xlsx",
                        sheet = 2,
                        col_types = c(rep("text",3), "numeric", "date", "numeric", rep("text", 2)))

data2 <- read_xlsx("online_retail_ll.xlsx",
                        sheet = 1,
                        col_types = c(rep("text",3), "numeric", "date", "numeric", rep("text", 2)))
Rawdata <- bind_rows(data1,data2, id=NULL)

Rawdata <- Rawdata %>% mutate (Amount= Rawdata$Quantity * Rawdata$Price) 
Rawdata <- Rawdata %>% mutate (Dates= as.Date(format(as.Date(Rawdata$InvoiceDate), "%Y-%m-%d") ) )

# extract 2 SKU data for forecasting 
dataDOT <- filter(Rawdata,StockCode == "DOT")

data85123A <- filter(Rawdata,StockCode == "85123A")

# Group by month and transform time series data

groupDOT <- dataDOT %>%
group_by(month = floor_date(Dates, 'month')) %>%
  summarize(Amount = sum(Amount))
tsDOT <- ts(groupDOT$Amount, groupDOT$month, frequency =12) 


group85123A <- data85123A %>%
  group_by(month = floor_date(Dates, 'month')) %>%
  summarize(Amount = sum(Amount))
ts85123A <- ts(group85123A$Amount, group85123A$month, frequency = 12)

# Decompose of time series 

decomDOT <- decompose(tsDOT, type='mult')
decom85123A <- decompose(ts85123A, type='mult')
plot(decom85123A)

decomreportDOT <- as.data.frame(decomDOT$seasonal)
decomreportDOT <- cbind(decomreportDOT,decomDOT$trend)
write.csv(decomreportDOT, 'decomDOT.csv')

decomreport85123A <- as.data.frame(decom85123A$seasonal)
decomreport85123A <- cbind(decomreport85123A,decom85123A$trend)
write.csv(decomreport85123A, 'decom85123A.csv')

# xgBoost 

xgbmodelDOT <- xgbar(tsDOT, seas_method = 'decompose')
xgbforecastDOT <- forecastxgb:::forecast.xgbar(xgbmodelDOT, 12)

xgbmodel85123A <- xgbar(ts85123A, seas_method = 'decompose')
xgbforecast85123A <- forecastxgb:::forecast.xgbar(xgbmodel85123A, 12)

# ARIMA 

arimamodelDOT <- auto.arima(tsDOT)
arimaforecastDOT <- forecast:::forecast.Arima(arimamodelDOT, h=12)

arimamodel85123A <- auto.arima(ts85123A)
arimaforecast85123A <- forecast:::forecast.Arima(arimamodel85123A, h=12)


# exponential smoothing Holtwinters

eshwmodelDOT <- HoltWinters(tsDOT, alpha=0.5, beta=FALSE, gamma=TRUE, seasonal =c("multiplicative"))
eshwforecastDOT <- forecast:::forecast.HoltWinters(eshwmodelDOT, h=12)

eshwmodel85123A <- HoltWinters(ts85123A, alpha=0.5, beta=FALSE, gamma=TRUE, seasonal =c("multiplicative"))
eshwforecast85123A <- forecast:::forecast.HoltWinters(eshwmodel85123A, h=12)

# Naive Method 
naiveDOT <- naive(tsDOT, h=12)

naive85123A <- naive(ts85123A, h=12)

# TBATS 

tbatsDOT <- tbats(tsDOT)
tbatsforecastDOT <- forecast:::forecast.bats(tbatsDOT, h=12)

tbats85123A <- tbats(ts85123A)
tbatsfor85123A <- forecast:::forecast.tbats(tbats85123A, h=12)

# MAPE report 
MAPEDOT <- as.data.frame (accuracy(xgbforecastDOT))
MAPEDOT <- rbind(MAPEDOT, accuracy(arimaforecastDOT))
MAPEDOT <- rbind(MAPEDOT,accuracy(eshwforecastDOT))
MAPEDOT <- rbind(MAPEDOT,accuracy(naiveDOT))
MAPEDOT <- rbind(MAPEDOT,accuracy(tbatsforecastDOT))
row.names(MAPEDOT) <- c ('XGoost','ARIMA','ES HoltWinters', 'Naive', ' TBATS')
write.csv(MAPEDOT, 'MAPEDOT.csv')

MAPE85123A <- as.data.frame (accuracy(xgbforecast85123A))
MAPE85123A <- rbind(MAPE85123A,accuracy(arimaforecast85123A))
MAPE85123A <- rbind(MAPE85123A,accuracy(eshwforecast85123A))
MAPE85123A <- rbind(MAPE85123A,accuracy(naive85123A))
MAPE85123A <- rbind(MAPE85123A,accuracy(tbatsfor85123A))
row.names(MAPE85123A) <- c ('XGBoost','ARIMA','ES HoltWinters', 'Naive', ' TBATS')
write.csv(MAPE85123A, 'MAPE85123A.csv')

# write report CSV of the best MAPE model 

predictDOT <- as.data.frame (xgbforecastDOT$mean)
fperiodDOT <- seq (max(groupDOT$month) + months(1), max(groupDOT$month) + months(12), by='months')
predictDOT <- predictDOT %>% mutate (Date = fperiodDOT) 

predict85123A <- as.data.frame (xgbforecast85123A$mean)
fperiod85123A <- seq (max(group85123A$month) + months(1), max(group85123A$month) + months(12), by='months')
predict85123A <- predict85123A %>% mutate (Date = fperiod85123A) 

write.csv(predictDOT, 'predictDOT.csv')
write.csv(predict85123A, 'predict85123A.csv')
write.csv(groupDOT, 'groupDOT.csv')
write.csv(group85123A, 'group85123A.csv')


