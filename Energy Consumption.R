#Minhaz
#v1



# Directory ---------------------------------------------------------------

setwd("Documents/Ubiqum/3.1 Electricity Consumption")
getwd()

# Library and Packages ----------------------------------------------------


library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(gridExtra)
library(scales)
library(ggplot2)
library(DMwR)
library(forecast)
library(stats)
library(graphics)

# Dataview ----------------------------------------------------------------

household_power_consumption <- read_delim("household_power_consumption.txt", ";")
View(household_power_consumption)


# Combining the date and time ---------------------------------------------

household_power_consumption <- cbind(household_power_consumption,
                                     paste(household_power_consumption$Date, 
                                           household_power_consumption$Time),
                                     stringsAsFactors=FALSE)
colnames(household_power_consumption)[10] <-"DateTime"
household_power_consumption <- household_power_consumption[,c(ncol(household_power_consumption), 
                                                              1:(ncol(household_power_consumption)-1))]
head(household_power_consumption)

# Conversion of Date and Time in correct format

household_power_consumption$DateTime <- strptime(household_power_consumption$DateTime,
                                                 "%d/%m/%Y %H:%M:%S", tz= "UTC")
household_power_consumption$DateTime <- as.POSIXct(household_power_consumption$DateTime,
                                                   "%d/%m/%Y %H:%M:%S", tz= "UTC")

household_power_consumption$Date <- as.Date(household_power_consumption$Date, "%d/%m/%Y")

str(household_power_consumption)


# Data Exploration --------------------------------------------------------

summary(household_power_consumption)

glimpse(household_power_consumption)


# Missing Values Analysis -------------------------------------------------


HPC_MissingValues1 <- household_power_consumption[rowSums(is.na(household_power_consumption)) > 0,]
View(HPC_MissingValues1)

# Replace NA with 0

hh_pwr_consmptn_MisValrep <- household_power_consumption

hh_pwr_consmptn_MisValrep[is.na(hh_pwr_consmptn_MisValrep)] <- 0

is.na(hh_pwr_consmptn_MisValrep)

# Adding New Variable Power(Votage/Intensity) -----------------------------------------------

hh_pwr_consmptn <- household_power_consumption %>%
  mutate(Voltage_Intensity = Voltage / Global_intensity)
head(hh_pwr_consmptn)
str(hh_pwr_consmptn)
View(hh_pwr_consmptn)


# Obtain time related variables -----------------------------------------------------

hh_pwr_consmptn$Day_of_Week <- weekdays(hh_pwr_consmptn$Date)

head(hh_pwr_consmptn)

hh_pwr_consmptn$Month <- months(hh_pwr_consmptn$Date)

hh_pwr_consmptn$Month_Year <- format(hh_pwr_consmptn$Date, "%Y-%m")

hh_pwr_consmptn$Month_No <- month(hh_pwr_consmptn$Date)

head(hh_pwr_consmptn)

hh_pwr_consmptn$Quarter <- quarter(hh_pwr_consmptn$Date)

head(hh_pwr_consmptn)

View(hh_pwr_consmptn)

str(hh_pwr_consmptn)

hh_pwr_consmptn$Year <- year(hh_pwr_consmptn$Date)

hh_pwr_consmptn_MisValrep <- hh_pwr_consmptn

hh_pwr_consmptn_MisValrep[is.na(hh_pwr_consmptn_MisValrep)] <- 0

# Data Grouping -----------------------------------------------------------

# Create a group_by object using the year column 
hh_pwr_consmptn_year <- hh_pwr_consmptn %>%  group_by(Year) 

# view class of the grouped object

class(hh_pwr_consmptn_year)

# Average usage per year?

tally(hh_pwr_consmptn_year)

Yearly_Summary <- hh_pwr_consmptn_year %>% 
  summarise(mean(Global_active_power, na.rm = TRUE))

Yearly_Summary

# What is the Quarterly average usage?

Quarterly_Summary <- hh_pwr_consmptn %>%
  group_by(Quarter) %>% 
  summarise(mean(Global_active_power, na.rm = TRUE))

Quarterly_Summary

# Check Each Quarter per year basis

Yearly_Quarterly_Summary <- hh_pwr_consmptn %>%
  group_by(Year, Quarter) %>%
  summarise(mean(Global_active_power, na.rm = TRUE))

Yearly_Quarterly_Summary

# Create a group_by object using the month column 

hh_pwr_consmptn_month <- hh_pwr_consmptn %>% group_by(Month)

# how many measurements were made each MONTH?
tally(hh_pwr_consmptn_month)

Monthly_Summary <- hh_pwr_consmptn_month %>% 
  summarise(mean(Global_active_power, na.rm = TRUE))

Monthly_Summary

# what was the daily average for each DAY OF THE WEEK?

Daily_Summary <- hh_pwr_consmptn %>% 
  group_by(Day_of_Week) %>%
  summarise(mean(Global_active_power, na.rm = TRUE))


# Data Slicing by Year ----------------------------------------------------

hh_pwr_consmptn_2007 <- hh_pwr_consmptn %>% filter(Year==2007)

hh_pwr_consmptn_2008 <- hh_pwr_consmptn %>% filter(Year==2008)

hh_pwr_consmptn_2009 <- hh_pwr_consmptn %>% filter(Year==2009)

hh_pwr_consmptn_2010 <- hh_pwr_consmptn %>% filter(Year==2010)

str(hh_pwr_consmptn_2007)

str(hh_pwr_consmptn_2008)

str(hh_pwr_consmptn_2009)

str(hh_pwr_consmptn_2010)
# Initial Visualization ---------------------------------------------------

Overall_Plot <- hh_pwr_consmptn %>% 
  ggplot(aes(x = DateTime, y = Global_active_power)) + geom_line()

Overall_Plot

TsPlot_2007 <- hh_pwr_consmptn_2007 %>%
  ggplot(aes(x = DateTime, y = Global_active_power)) + geom_line()

TsPlot_2007

TsPlot_2008 <- hh_pwr_consmptn_2008 %>%
  ggplot(aes(x = DateTime, y = Global_active_power)) + geom_line()

TsPlot_2008

TsPlot_2009 <- hh_pwr_consmptn_2009 %>%
  ggplot(aes(x = DateTime, y = Global_active_power)) + geom_line()

TsPlot_2009

TsPlot_2010 <- hh_pwr_consmptn_2010 %>%
  ggplot(aes(x = DateTime, y = Global_active_power)) + geom_line()

TsPlot_2010


# Daily, Monthly Sum data ------------------------------------------------

#DAYS

hh_pwr_consmptn_DailyAgg_2007 <- hh_pwr_consmptn_2007 %>%
  group_by(Year, Date) %>%
  summarise(Daily_global_Active = sum(Global_active_power))

head(hh_pwr_consmptn_DailyAgg_2007)

str(hh_pwr_consmptn_DailyAgg_2007)

hh_pwr_consmptn_DailyAgg_2008 <- hh_pwr_consmptn_2008 %>%
  group_by(Year, Date) %>%
  summarise(Daily_global_Active = sum(Global_active_power))

hh_pwr_consmptn_DailyAgg_2009 <- hh_pwr_consmptn_2009 %>%
  group_by(Year, Date) %>%
  summarise(Daily_global_Active = sum(Global_active_power))

hh_pwr_consmptn_DailyAgg_2010 <- hh_pwr_consmptn_2010 %>%
  group_by(Year, Date) %>%
  summarise(Daily_global_Active = sum(Global_active_power))

hh_pwr_consmptn_DailyAgg <- hh_pwr_consmptn %>%
  group_by(Year, Date) %>%
  summarise(Daily_global_Active = sum(Global_active_power))

#MONTHS

hh_pwr_consmptn_MonthlyAgg <- hh_pwr_consmptn %>% 
  group_by(Month_Year) %>% 
  summarise(Monthly_global_Active = sum(Global_active_power))

hh_pwr_consmptn_MonthlyAgg_MVR0 <- hh_pwr_consmptn_MisValrep %>%
  group_by(Year, Month_No) %>%
  summarise(Monthly_Global_Active = sum(Global_active_power))

hh_pwr_consmptn_MonthlyAgg_MVR0_Subm123 <- hh_pwr_consmptn_MisValrep %>%
  group_by(Year, Month, Month_No, Month_Year) %>%
  summarise(Monthly_Global_Active = sum(Global_active_power),
            Monthly_Sub_Meter_1 = sum(Sub_metering_1),
            Monthly_Sub_Meter_2 = sum(Sub_metering_2),
            Monthly_Sub_Meter_3 = sum(Sub_metering_3))


head(hh_pwr_consmptn_MonthlyAgg)

head(hh_pwr_consmptn_MonthlyAgg_MVR0)

str(hh_pwr_consmptn_MonthlyAgg_MVR0)

summary(hh_pwr_consmptn_MonthlyAgg_MVR0)

head(hh_pwr_consmptn_MonthlyAgg_MVR0_Subm123)

# 2nd stage visualisation -------------------------------------------------

DailySum_2007_Trend <- hh_pwr_consmptn_DailyAgg_2007 %>%
  ggplot(aes(y = Daily_global_Active, x = Date)) +
  geom_line()

DailySum_2007_Trend

DailySum_2008_Trend <- hh_pwr_consmptn_DailyAgg_2008 %>%
  ggplot(aes(y = Daily_global_Active, x = Date)) +
  geom_line()

DailySum_2008_Trend

DailySum_2009_Trend <- hh_pwr_consmptn_DailyAgg_2009 %>%
  ggplot(aes(y = Daily_global_Active, x = Date)) +
  geom_line()

DailySum_2009_Trend

DailySum_2010_Trend <- hh_pwr_consmptn_DailyAgg_2010 %>%
  ggplot(aes(y = Daily_global_Active, x = Date)) +
  geom_line()

DailySum_2010_Trend

DailySum_Trend <- hh_pwr_consmptn_DailyAgg %>%
  ggplot(aes(y = Daily_global_Active, x = Date)) +
  geom_line()

DailySum_Trend

MonthlySum_Trend <- hh_pwr_consmptn_MonthlyAgg_MVR0_Subm123 %>%
  ggplot(aes(x = Month_Year, y = Monthly_Global_Active, group = 1)) +
  geom_line()

MonthlySum_Trend

MonthlySum_Trend_SM <- hh_pwr_consmptn_MonthlyAgg_MVR0_Subm123 %>%
  ggplot() +
  geom_line(aes(x = as.factor(Month_Year), y = Monthly_Sub_Meter_1, group = 1), color = "blue") +
  geom_line(aes(x = as.factor(Month_Year), y = Monthly_Sub_Meter_2, group = 1), color = "red") +
  geom_line(aes(x = as.factor(Month_Year), y = Monthly_Sub_Meter_3, group = 1), color = "green") +
  xlab('Dates') +
  ylab('Submeter_Consumption')

MonthlySum_Trend_SM

# Preparation for Analysis ------------------------------------------------

# Global Active Power Only
hh_pwr_consmptn_MonthlyAgg_ForeC <- ts(hh_pwr_consmptn_MonthlyAgg_MVR0$Monthly_Global_Active, 
                                        frequency = 12, 
                                        start = c(2006, 12), end = c(2010, 11))



str(hh_pwr_consmptn_MonthlyAgg_ForeC)

head(hh_pwr_consmptn_MonthlyAgg_ForeC)

plot.ts(hh_pwr_consmptn_MonthlyAgg_ForeC)


# Submetering

hh_pwr_consmptn_MonthlyAgg_ForeC_SM1 <- ts(
  hh_pwr_consmptn_MonthlyAgg_MVR0_Subm123$Monthly_Sub_Meter_1, 
  frequency = 12, 
  start = c(2006, 12), end = c(2010, 11))

hh_pwr_consmptn_MonthlyAgg_ForeC_SM2 <- ts(
  hh_pwr_consmptn_MonthlyAgg_MVR0_Subm123$Monthly_Sub_Meter_2, 
  frequency = 12, 
  start = c(2006, 12), end = c(2010, 11))

hh_pwr_consmptn_MonthlyAgg_ForeC_SM3 <- ts(
  hh_pwr_consmptn_MonthlyAgg_MVR0_Subm123$Monthly_Sub_Meter_3, 
  frequency = 12, 
  start = c(2006, 12), end = c(2010, 11))


plot.ts(hh_pwr_consmptn_MonthlyAgg_ForeC_SM1)

plot.ts(hh_pwr_consmptn_MonthlyAgg_ForeC_SM2)

plot.ts(hh_pwr_consmptn_MonthlyAgg_ForeC_SM3)


# Forecasting  ------------------------------------------------------------

# Global Power Active only

HH_PWR_CONSMP_TSLM <- tslm(hh_pwr_consmptn_MonthlyAgg_ForeC ~ trend + season)

HH_PWR_CONSMP_Forecast <- forecast(HH_PWR_CONSMP_TSLM)

plot(HH_PWR_CONSMP_Forecast)

summary(HH_PWR_CONSMP_Forecast)

# Submetering 

HH_PWR_CONSMP_SM1_TSLM <- tslm(hh_pwr_consmptn_MonthlyAgg_ForeC_SM1 ~ trend + season)

HH_PWR_CONSMP_SM2_TSLM <- tslm(hh_pwr_consmptn_MonthlyAgg_ForeC_SM2 ~ trend + season)

HH_PWR_CONSMP_SM3_TSLM <- tslm(hh_pwr_consmptn_MonthlyAgg_ForeC_SM3 ~ trend + season)

HH_PWR_CONSMP_SM1_Forecast <- forecast(HH_PWR_CONSMP_SM1_TSLM)

HH_PWR_CONSMP_SM2_Forecast <- forecast(HH_PWR_CONSMP_SM2_TSLM)

HH_PWR_CONSMP_SM3_Forecast <- forecast(HH_PWR_CONSMP_SM3_TSLM)

summary(HH_PWR_CONSMP_SM1_Forecast)

summary(HH_PWR_CONSMP_SM2_Forecast)

summary(HH_PWR_CONSMP_SM3_Forecast)

# Smothing and Decomposition ----------------------------------------------

# Global Active Power only

HH_PWR_CONSMP_Decompose <- decompose(hh_pwr_consmptn_MonthlyAgg_ForeC)

summary(HH_PWR_CONSMP_Decompose)

plot(HH_PWR_CONSMP_Decompose)

# Submetering

HH_PWR_CONSMP_SM1_Decompose <- decompose(hh_pwr_consmptn_MonthlyAgg_ForeC_SM1)

summary(HH_PWR_CONSMP_SM1_Decompose)

plot(HH_PWR_CONSMP_SM1_Decompose)

HH_PWR_CONSMP_SM2_Decompose <- decompose(hh_pwr_consmptn_MonthlyAgg_ForeC_SM2)

summary(HH_PWR_CONSMP_SM2_Decompose)

plot(HH_PWR_CONSMP_SM2_Decompose)

HH_PWR_CONSMP_SM3_Decompose <- decompose(hh_pwr_consmptn_MonthlyAgg_ForeC_SM3)

summary(HH_PWR_CONSMP_SM3_Decompose)

plot(HH_PWR_CONSMP_SM3_Decompose)

# Holt Winters ------------------------------------------------------------

# Global Active Power

HH_PWR_CONSMP_HoltWint <- HoltWinters(hh_pwr_consmptn_MonthlyAgg_ForeC, 
                                      beta = FALSE, gamma = FALSE)

HH_PWR_CONSMP_HoltWint1 <- HoltWinters(hh_pwr_consmptn_MonthlyAgg_ForeC)

summary(HH_PWR_CONSMP_HoltWint)

summary(HH_PWR_CONSMP_HoltWint1)

plot(HH_PWR_CONSMP_HoltWint)

plot(HH_PWR_CONSMP_HoltWint1)

# Submetering

#SubMeter1

HH_PWR_CONSMP_SM1_HoltWint <- HoltWinters(hh_pwr_consmptn_MonthlyAgg_ForeC_SM1, 
                                      beta = FALSE, gamma = FALSE)

HH_PWR_CONSMP_SM1_HoltWint1 <- HoltWinters(hh_pwr_consmptn_MonthlyAgg_ForeC_SM1)

summary(HH_PWR_CONSMP_SM1_HoltWint)

summary(HH_PWR_CONSMP_SM1_HoltWint1)

plot(HH_PWR_CONSMP_SM1_HoltWint)

plot(HH_PWR_CONSMP_SM1_HoltWint1)

# Submeter2

HH_PWR_CONSMP_SM2_HoltWint <- HoltWinters(hh_pwr_consmptn_MonthlyAgg_ForeC_SM2, 
                                          beta = FALSE, gamma = FALSE)

HH_PWR_CONSMP_SM2_HoltWint1 <- HoltWinters(hh_pwr_consmptn_MonthlyAgg_ForeC_SM2)

summary(HH_PWR_CONSMP_SM2_HoltWint)

summary(HH_PWR_CONSMP_SM2_HoltWint1)

plot(HH_PWR_CONSMP_SM2_HoltWint)

plot(HH_PWR_CONSMP_SM2_HoltWint1)

# Submeter3

HH_PWR_CONSMP_SM3_HoltWint <- HoltWinters(hh_pwr_consmptn_MonthlyAgg_ForeC_SM3, 
                                          beta = FALSE, gamma = FALSE)

HH_PWR_CONSMP_SM3_HoltWint1 <- HoltWinters(hh_pwr_consmptn_MonthlyAgg_ForeC_SM3)

summary(HH_PWR_CONSMP_SM3_HoltWint)

summary(HH_PWR_CONSMP_SM3_HoltWint1)

plot(HH_PWR_CONSMP_SM3_HoltWint)

plot(HH_PWR_CONSMP_SM3_HoltWint1)


# Comparison for GAP to Submetering ---------------------------------------

hh_pwr_consmptn_MonthlyAgg_MVR0_Subm123_GAPinKWH <- 
  hh_pwr_consmptn_MonthlyAgg_MVR0_Subm123 %>% mutate(Monthly_GAP_KWH = 
                                                       Monthly_Global_Active*1000/60)
head(hh_pwr_consmptn_MonthlyAgg_MVR0_Subm123_GAPinKWH)

#hh_pwr_consmptn_MonthlyAgg_MVR0_Subm123_GAPinKWH$Year <- NULL 
#hh_pwr_consmptn_MonthlyAgg_MVR0_Subm123_GAPinKWH$Month_No <- NULL

hh_pwr_consmptn_MonthlyAgg_MVR0_Subm1234 <- 
  hh_pwr_consmptn_MonthlyAgg_MVR0_Subm123_GAPinKWH %>% 
  mutate(Monthly_Sub_Meter_4 = 
           Monthly_GAP_KWH - 
           (Monthly_Sub_Meter_1 + Monthly_Sub_Meter_2 + Monthly_Sub_Meter_3))

# Global Active Power in KWH
hh_pwr_consmptn_KWH_MonthlyAgg_ForeC <- ts(hh_pwr_consmptn_MonthlyAgg_MVR0_Subm1234$Monthly_GAP_KWH, 
                                       frequency = 12, 
                                       start = c(2006, 12), end = c(2010, 11))



str(hh_pwr_consmptn_KWH_MonthlyAgg_ForeC)

head(hh_pwr_consmptn_KWH_MonthlyAgg_ForeC)

plot.ts(hh_pwr_consmptn_KWH_MonthlyAgg_ForeC)

HH_PWR_CONSMP_KWH_TSLM <- tslm(hh_pwr_consmptn_KWH_MonthlyAgg_ForeC ~ trend + season)

HH_PWR_CONSMP_KWH_Forecast <- forecast(HH_PWR_CONSMP_KWH_TSLM)

plot(HH_PWR_CONSMP_KWH_Forecast)

summary(HH_PWR_CONSMP_KWH_Forecast)


HH_PWR_CONSMP_KWH_Decompose <- decompose(hh_pwr_consmptn_KWH_MonthlyAgg_ForeC)

summary(HH_PWR_CONSMP_KWH_Decompose)

plot(HH_PWR_CONSMP_KWH_Decompose)

# TimeSeries for Submeter_4 -----------------------------------------------

# VISUALIZATION

MonthlySum_Trend_SM <- hh_pwr_consmptn_MonthlyAgg_MVR0_Subm1234 %>%
  ggplot() +
    geom_line(aes(x = as.factor(Month_Year), y = Monthly_Sub_Meter_1, group = 1), 
              color = "blue") +
    geom_line(aes(x = as.factor(Month_Year), y = Monthly_Sub_Meter_2, group = 1), 
              color = "red") +
    geom_line(aes(x = as.factor(Month_Year), y = Monthly_Sub_Meter_3, group = 1), 
              color = "green") +
    geom_line(aes(x = as.factor(Month_Year), y = Monthly_Sub_Meter_4, group = 1), 
              color = "black") +
    xlab('Dates') +
    ylab('Submeter_Consumption')

MonthlySum_Trend_SM_leg <- hh_pwr_consmptn_MonthlyAgg_MVR0_Subm1234 %>%
  ggplot() + 
  geom_line(aes(x = as.factor(Month_Year), y = Monthly_Sub_Meter_1, group = 1, col = "Submeter1")) +
  geom_line(aes(x = as.factor(Month_Year), y = Monthly_Sub_Meter_2, group = 1, col = "Submeter2")) +
  geom_line(aes(x = as.factor(Month_Year), y = Monthly_Sub_Meter_3, group = 1, col = "Submeter3")) +
  geom_line(aes(x = as.factor(Month_Year), y = Monthly_Sub_Meter_4, group = 1, col = "Submeter4")) +
  xlab('Dates') +
  ylab('Submeter_Consumption')


MonthlySum_Trend_SM_GAP <- hh_pwr_expenditure_MonthlyAgg_MVR0_Subm1234 %>%
  ggplot(aes(x = as.factor(Month_No), y = Monthly_GAP_KWH, group = 1)) +
  geom_line() + facet_wrap(~ Year)
  xlab("Months") + 
  ylab ("Total Consumption")

YearlySum_Trend_SM <- hh_pwr_consmptn_MonthlyAgg_MVR0_Subm1234 %>%
  ggplot() +
  geom_smooth(aes(x = as.factor(Year), y = Monthly_Sub_Meter_1, group = 1), 
            color = "blue") +
  geom_smooth(aes(x = as.factor(Year), y = Monthly_Sub_Meter_2, group = 1), 
            color = "red") +
  geom_smooth(aes(x = as.factor(Year), y = Monthly_Sub_Meter_3, group = 1), 
            color = "green") +
  geom_smooth(aes(x = as.factor(Year), y = Monthly_Sub_Meter_4, group = 1), 
            color = "black") + 
  scale_colour_manual("", 
                      breaks = c("Submeter_1", "Submeter_2", "Submeter_3", "Submeter_4"),
                      values = c("blue", "red", "green", "black")) +
  xlab('Dates') +
  ylab('Submeter_Consumption')

# Prepare for Analysis

hh_pwr_consmptn_MonthlyAgg_ForeC_SM4 <- ts(
  hh_pwr_consmptn_MonthlyAgg_MVR0_Subm1234$Monthly_Sub_Meter_4, 
  frequency = 12, 
  start = c(2006, 12), end = c(2010, 11))

plot.ts(hh_pwr_consmptn_MonthlyAgg_ForeC_SM4)

# Forecast

HH_PWR_CONSMP_SM4_TSLM <- tslm(hh_pwr_consmptn_MonthlyAgg_ForeC_SM4 ~ trend + season)

HH_PWR_CONSMP_SM4_Forecast <- forecast(HH_PWR_CONSMP_SM4_TSLM)

summary(HH_PWR_CONSMP_SM4_Forecast)

# Smothing and Decomposing

HH_PWR_CONSMP_SM4_Decompose <- decompose(hh_pwr_consmptn_MonthlyAgg_ForeC_SM4)

summary(HH_PWR_CONSMP_SM4_Decompose)

plot(HH_PWR_CONSMP_SM4_Decompose)

# Winter-Holt

HH_PWR_CONSMP_SM4_HoltWint <- HoltWinters(hh_pwr_consmptn_MonthlyAgg_ForeC_SM4, 
                                          beta = FALSE, gamma = FALSE)

HH_PWR_CONSMP_SM4_HoltWint1 <- HoltWinters(hh_pwr_consmptn_MonthlyAgg_ForeC_SM4)

summary(HH_PWR_CONSMP_SM4_HoltWint)

summary(HH_PWR_CONSMP_SM4_HoltWint1)

plot(HH_PWR_CONSMP_SM4_HoltWint)

plot(HH_PWR_CONSMP_SM4_HoltWint1)


# Energy Price ------------------------------------------------------------
  
hh_pwr_consmptn_MonthlyAgg_MVR0_Subm1234$prices <- c(0.0924,
                                                     0.0924,
                                                     0.0924,
                                                     0.0924,
                                                     0.0924,
                                                     0.0924,
                                                     0.0924,
                                                     0.0914,
                                                     0.0914,
                                                     0.0914,
                                                     0.0914,
                                                     0.0914,
                                                     0.0914,
                                                     0.091,
                                                     0.091,
                                                     0.091,
                                                     0.091,
                                                     0.091,
                                                     0.091,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.0908,
                                                     0.094,
                                                     0.094,
                                                     0.094,
                                                     0.094,
                                                     0.094,
                                                     0.094,
                                                     0.0995,
                                                     0.0995,
                                                     0.0995,
                                                     0.0995,
                                                     0.0995)

hh_pwr_expenditure_MonthlyAgg_MVR0_Subm1234 <- 
  hh_pwr_consmptn_MonthlyAgg_MVR0_Subm1234 %>% mutate(Expenditure = Monthly_GAP_KWH*prices/100)

str(hh_pwr_expenditure_MonthlyAgg_MVR0_Subm1234)


# Expenditure Forecast ----------------------------------------------------

# Preparation

hh_pwr_expenditure_MonthlyAgg_ForeC <- ts(hh_pwr_expenditure_MonthlyAgg_MVR0_Subm1234$Expenditure, 
                                       frequency = 12, 
                                       start = c(2006, 12), end = c(2010, 11))

str(hh_pwr_expenditure_MonthlyAgg_ForeC)

head(hh_pwr_expenditure_MonthlyAgg_ForeC)

plot.ts(hh_pwr_expenditure_MonthlyAgg_ForeC)

# Forecasting

HH_PWR_EXP_TSLM <- tslm(hh_pwr_expenditure_MonthlyAgg_ForeC ~ trend + season)

HH_PWR_EXP_Forecast <- forecast(HH_PWR_EXP_TSLM)

plot(HH_PWR_EXP_Forecast)

summary(HH_PWR_EXP_Forecast)

# Decomposing

HH_PWR_EXP_Decompose <- decompose(hh_pwr_expenditure_MonthlyAgg_ForeC)

summary(HH_PWR_EXP_Decompose)

plot(HH_PWR_EXP_Decompose)

# Winter-Holt

HH_PWR_EXP_HoltWint <- HoltWinters(hh_pwr_expenditure_MonthlyAgg_ForeC, 
                                      beta = FALSE, gamma = FALSE)

HH_PWR_EXP_HoltWint1 <- HoltWinters(hh_pwr_expenditure_MonthlyAgg_ForeC)

summary(HH_PWR_EXP_HoltWint)

summary(HH_PWR_EXP_HoltWint1)

plot(HH_PWR_EXP_HoltWint1)

plot(HH_PWR_EXP_HoltWint1)


