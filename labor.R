
rm(list=ls())
### LABOR ECONOMICS
library(tidyverse)
library(fredr)
library(tsibble)
library(fpp3)
library(ggplot2)
library(fpp2)
library(dplyr)

### Set Key
fredr_set_key("")

### OFFICIAL Unemployment Rate ###
fed <- fredr(series_id = "FEDFUNDS") ###1954
jobless <- fredr(series_id = "UNRATE") ##1948
gdp <- fredr(series_id = "GDP")
inf <- fredr(series_id = "CPIAUCSL")
auto_prod <- fredr(series_id = "DAUPSA")
ten_year <- fredr(series_id="REAINTRATREARAT10Y")

### monthly master set

inf <- inf %>%
  as_tibble() %>%
  mutate(
    date = ymd(date)
  )
### merge employed and inf
monthly_macro <- merge(payroll, inf, by = "date")

### rename columns
monthly_macro$employed <- monthly_macro$value.x
monthly_macro$inf <- monthly_macro$value.y

### add domestic auto production

auto_prod <- auto_prod %>%
  as_tibble() %>%
  mutate(
    date = ymd(date)
  )

## add to master
monthly_macro <- merge(monthly_macro, auto_prod, by = "date")

monthly_macro$auto_production <- monthly_macro$value

### 10 year real interest rate

ten_year <- ten_year %>%
  as_tibble() %>%
  mutate(
    date = ymd(date)
  )

monthly_macro <- merge(monthly_macro, ten_year, by="date")
head(monthly_macro)

monthly_macro$ten_year <- monthly_macro$value.y

### clean monthly data set

monthly_macro$realtime_start.x <- NULL
monthly_macro$realtime_start.y <- NULL
monthly_macro$realtime_end.x <- NULL
monthly_macro$realtime_end.y <- NULL
monthly_macro$value.x <- NULL
monthly_macro$value.y <- NULL
monthly_macro$ten_year <- NULL
monthly_macro$series_id.x <- NULL
monthly_macro$series_id.y <- NULL


head(monthly_macro)

### m2

m2 <- fredr(series_id="M2SL")

m2 <- m2 %>%
  as_tibble() %>%
  mutate(date = ymd(date))

monthly_macro <- merge(monthly_macro, m2, by ="date")

monthly_macro$m2 <- monthly_macro$value

monthly_macro$realtime_start <- NULL
monthly_macro$realtime_end <- NULL
monthly_macro$series_id<- NULL
monthly_macro$value<- NULL

### energy index

energy_index <- fredr(series_id="PNRGINDEXM")

energy_index <- energy_index %>%
  as_tibble() %>%
  mutate(date = ymd(date))

monthly_macro <- merge(monthly_macro, energy_index, by ="date")

colnames(monthly_macro)
monthly_macro$enegy_index <- monthly_macro$value

monthly_macro$realtime_start <- NULL
monthly_macro$realtime_end <- NULL
monthly_macro$value <- NULL
monthly_macro$series_id <- NULL
monthly_macro$realtime_end <- NULL

### index values to 2000 and plot

monthly_macro %>%
  mutate(
    Employed = (employed / 109794)*100,
    infl = (inf / 142.8) *100,
    Auto_production = (auto_production / 512.9) * 100,
    Ten_year = (ten_year / 3.267908) * 100,
    M2 = (m2 / 3419.1) * 100) %>%
  ggplot()+
  geom_line(aes(date, Employed, color="employed")) +
  geom_line(aes(date, infl, color="inflation")) +
  geom_line(aes(date, Auto_production, color="auto_production")) +
  geom_line(aes(date, Ten_year, color="ten_year_yield")) +
  geom_line(aes(date, M2, color="m2_money_stock")) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position="right", legend.direction="vertical")


### deep dive on inflation
### energy
### housing
### food
### education
### assets
### healthcare
### asset prices
## stock market cap to gdp ratio
###percent 
stock_market <- fredr(series_id="DDDM01USA156NWDB")
stock_market <-stock_market %>%
  as_tibble() %>%
  mutate(date = ymd(date))
### brent crude oil
### dollars
oil <- fredr(series_id="DCOILBRENTEU")
oil <- oil %>%
  as_tibble() %>%
  mutate(date = ymd(date))

oil <- oil %>%
  tidyr::fill(value)
### rent of us city residence
### indexed 1982 = 100
rent <- fredr(series_id="CUUR0000SEHA")
rent <- rent %>%
  as_tibble() %>%
  mutate(date = ymd(date))
###tuition
### indexed 1982-1984 = 100
tuition <- fredr(series_id="CUSR0000SEEB")
tuition <- tuition %>%
  as_tibble() %>%
  mutate(date = ymd(date))
### household income
### dollars
household_income <- fredr(series_id="MEHOINUSA672N")
household_income <- household_income %>%
  as_tibble() %>%
  mutate(date = ymd(date))

### food
### indexed 1982 = 100
food <- fredr(series_id = "CUSR0000SAF11")
food <- food %>%
  as_tibble() %>%
  mutate(date = ymd(date))
sum(is.na(food))

### natl price index

house_index <- fredr(series_id= "CSUSHPINSA")
tail(house_index)
house_index <- house_index %>%
  as_tibble() +
  mutate(date = ymd(date))

house_index <- house_index %>%
  filter(date >= "1987-01-01")

sum(is.na(house_index))

     ### master set
cpi_decompose <- merge(cpi_decompose, oil, by ="date")
tail(cpi_decompose)

cpi_decompose$oil <- cpi_decompose$value
cpi_decompose$househoold_income <- cpi_decompose$value.x

cpi_decompose$value <- NULL
cpi_decompose$realtime_start.y <- NULL
cpi_decompose$realtime_end.y <- NULL
cpi_decompose$series_id.x <- NULL
cpi_decompose$realtime_start.x <- NULL
cpi_decompose$realtime_end.x <- NULL
cpi_decompose$value.y <- NULL




### plot indexed price inflation

cpi_decompose %>%
  mutate(income = (household_income/60115)*100,
         crude_oil = (oil/17.60)*100,
         stock_cap = (stock_market/53)*100) %>%
  ggplot(aes(lwd=1)) +
  geom_line(aes(date, income, color="Household Income")) +
  geom_line(aes(date, crude_oil, color="Crude Oil")) +
  geom_line(aes(date, stock_cap, color="Stock Cap")) +
  geom_line(aes(date, tuition, color="Tuition")) +
  geom_line(aes(date, house_index, color="Sciller US Home Price Index")) +
  geom_line(aes(date, food, color="Food")) +
ggtitle("Price Inflation") +
labs(subtitle = "1988 = 100",
     y="Index", x="Date",
     caption="Source: FRED") +
  theme_bw() 

library(dplyr)
library(fpp3)
cpi_decompose <- cpi_decompose %>%
  as_tsibble(index=date)

cpi_decompose %>%
  filter_index(. ~ "2015-01-01") %>%
  fill_gaps(oil = mean(oil)) %>%
  model(
    arima=ARIMA(oil),
    ets=ETS(oil),
    naive=NAIVE(oil)) %>%
  fabletools::forecast(h="6 years") %>%
  fabletools::accuracy(cpi_decompose)


  ggplot() +
  fabletools::geom_forecast(aes(x=date, y=oil)) +
  fabletools::facet_wrap(~model, scales="free_y") +
  labs(title="Oil Forecast")


fabletools::autoplot(cpi_decompose) +
  labs(title="Oil Forecast")



###gdp cleaning
gdp <- gdp %>%
  mutate(date = as.Date(date, format="%Y%m%d"))

gdp$index <- gdp$date

gdp <- gdp %>%
  as_tsibble(index=index)

gdp <- gdp %>%
  filter_index("1954-08-01" ~ .)


###cpi cleaning
cpi$cpi <- cpi$value
head(jobless)
head(cpi)
cpi <- cpi %>%
  mutate(qtr = as.Date(date, format="%Y%m%d"),
         qtr = yearquarter(date)) %>%
  group_by(qtr) %>%
  summarize_all(mean)

cpi <- cpi %>%
  as_tsibble(index=qtr) %>%
  filter_index("1954-07-01" ~ .)

cpi$index <- cpi$qtr

cpi <- cpi %>%
  mutate(index=index)

###unemployment cleaning

###funds cleaning

fed <- fed %>% 
  as_tsibble(index=date)

###merge funds & unemployment

taylor <- merge(fed, jobless, by="date")
head(taylor) 

taylor$date <- as.Date(taylor$date, format="%Y%m%d")

###convert date to quarterly

taylor_qtr <- taylor %>%
  mutate(
    qtr = yearquarter(date)
  ) %>%
  group_by(qtr) %>%
  summarize_all(mean)

glimpse(taylor_qtr)

###cleaning quarterly tibble
taylor_qtr$funds_rate <- taylor_qtr$value.x
taylor_qtr$unemployment <- taylor_qtr$value.y


taylor_qtr$value.y=NULL
taylor_qtr$value.x=NULL
taylor_qtr$realtime_start.x=NULL
taylor_qtr$realtime_end.x=NULL
taylor_qtr$series_id.x=NULL
taylor_qtr$series_id.y=NULL
taylor_qtr$endtime_start.y=NULL
taylor_qtr$endtime_start.x=NULL

### as_tsibble

taylor_tsibble <- as_tsibble(taylor_qtr, index=qtr)
taylor_tsibble$index <- taylor_tsibble$qtr
library(ggplot2)
ggplot(taylor_tsibble, aes(funds_rate, unemployment),
       lwd=1.2, fill=T) +
  geom_point(position="jitter") +
  geom_smooth() +
  labs(title="Unemployment & Fed Funds Rate Plot",
       subtitle="correlation = 0.07",
       caption = "Source: FRED") +
  theme_bw() 

cor(taylor_tsibble$funds_rate, 
    taylor_tsibble$unemployment, method="pearson")

###non stationary regression
###funds ~ unemployment
taylor_reg_unemployment <- lm(taylor_tsibble$funds_rate ~ 
            taylor_tsibble$unemployment)

summary(taylor_reg1)

###stationary

ndiffs(taylor_tsibble$funds_rate)
ndiffs(taylor_tsibble$unemployment)

funds_stat <- diff(taylor_tsibble$funds_rate)

###stationary regression 
###federal funds rate ~ unemployment rate
unemploy_regress <- lm(funds_stat ~ unemploy_stat)
summary(lm(funds_stat ~ unemploy_stat))


###filter index
taylor_tsibble %>%
  filter_index("1954-08-01" ~ .)

###taylor full
taylor_full <- right_join(taylor_tsibble, gdp, by="index")

taylor_full$gdp <- taylor_full$value
  
head(taylor_full)

###clean taylor full data set 
taylor_full$value=NULL
taylor_full$value.x=NULL
taylor_full$realtime_start=NULL
taylor_full$realtime_end=NULL
taylor_full$series_id=NULL
taylor_full$date.y=NULL
taylor_full$date.x=NULL

### log gdp  add column

taylor_full$log_gdp <- log(taylor_full$gdp)
head(taylor_all)

ndiffs(taylor_all$cpi)
ndiffs(taylor_all$unemployment)
diff_cpi <- diff(taylor_all$cpi, differences = 2)

diff_unemploy <- taylor_all$unemployment

diff_unemploy <- diff_unemploy[-2]

summary(lm(diff_cpi ~ diff_unemploy))

ggplot(taylor_all, aes(cpi, unemployment)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

###quarterly regression values differenced and logged
diff_rate <- diff(taylor_full$funds_rate)
diff_unemploy <- taylor_full$unemployment[-1]
diff_log_gdp <- taylor_full$log_gdp[-1]

###regression funds ~ unemployment + gdp
diff_log_gdp <- diff(taylor_full$log_gdp)

adf.test(diff_log_gdp)

###2nd regression
###regress unemployment + log gdp
second_regression <- lm(diff_rate ~ diff_unemploy + diff_log_gdp)
summary(lm(diff_rate ~ diff_unemploy + diff_log_gdp))

###add cpi

taylor_all <- merge(taylor_full, cpi, by="index")

head(taylor_all)
taylor_all$value=NULL
taylor_all$realtime_start=NULL
taylor_all$realtime_end=NULL
taylor_all$qtr.y=NULL
taylor_all$series_id=NULL
taylor_all$date=NULL

ndiffs(taylor_all$cpi)

diff_cpi <- diff(taylor_all$cpi, differences = 2)

###regression all
###final regression
diff_rate_2 <- diff_rate[-1]  
diff_unemploy_2 <- diff_unemploy[-1]
diff_log_gdp_2 <- diff_log_gdp[-1]

final_regression <- lm(diff_rate_2 ~ diff_unemploy_2 + diff_log_gdp_2 +
                         diff_cpi)
plot(final_regression)
summary(final_regression)
###train and set
set.seed(2020)

train.size <- 0.8
train.index <- sample.int(length
            (taylor_all$funds_rate),
            round(length(taylor_all$funds_rate) ^ train.size))

train.sample <- taylor_all[train.index,]
valid.sample <- taylor_all[-train.index,]

fit <- lm(funds_rate ~ unemployment + 
             log_gdp + cpi, data=train.sample)
summary(fit) ###R2 = 0.61
plot(fit)

train.sample$Pred.Price <- predict(fit,
                       newdata=subset(train.sample,
                      select=c(unemployment,
                               log_gdp,
                               cpi)))

valid.sample$Pred.Price <- predict(fit,
                                   newdata=subset(valid.sample,
                                        select=c(unemployment,
                                                 log_gdp,
                                                 cpi)))
summary(fit)

### accuracy tests
train.corr <- round(cor(train.sample$Pred.Price,
                        train.sample$Pred.Price),
                    2)

train.RMSE <- round(sqrt(mean(
  valid.sample$Pred.Price -
    valid.sample$funds_rate)
  ^2))

train.MAE <- round(mean(abs(train.sample$Pred.Price -
                              train.sample$funds_rate)))

train.corr^2 
train.RMSE 
train.MAE

valid.corr <- round(cor(valid.sample$Pred.Price,
                        valid.sample$Pred.Price),
                    2)

valid.RMSE <- round(sqrt(mean(
  valid.sample$Pred.Price -
    valid.sample$funds_rate)
  ^2))

valid.MAE <- round(mean(abs(valid.sample$Pred.Price -
                              valid.sample$funds_rate)))


valid.corr^2 
valid.RMSE 
valid.MAE

colnames(train.sample)
less_regression <- lm(diff_rate_2 ~ diff_unemploy_2 + diff_log_gdp_2)
plot(less_regression)                         
summary(less_regression)
checkresiduals(final_regression)

summary(lm(diff_rate_2 ~ diff_unemploy_2 + diff_log_gdp_2 +
             diff_cpi))

plot(final_regression)
install.packages("GGally")
library(psych)
library(ggplot2)
ggpairs(taylor_all[,3:7], progress=FALSE)

###correlation plus cool stuff
pairs.panels(taylor_all[,5:9], col="red")

head(taylor_all)

final_regression %>%
  par(mfrow=c(2,2))
install.packages("psych")

library(psych)
head(taylor_all)
corPlot(taylor_all[, 3:7])
library(ggcorrplot)
corrdata <- cor(taylor_all[3:7])

corrplot(corrdata,
         method="number",
         type="full",
         tl.pos="tl",
         order="original")

plot(taylor_all$gdp, taylor_all$funds_rate)
cor(taylor_all$gdp, taylor_all$funds_rate)

install.packages("car")
library(car)
vif_values <- vif(final_regression)

#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", 
        horiz = TRUE, col = "steelblue" ) +

abline(v = 5, lwd = 3, lty = 2)

library(tseries)
adf.test(diff_unemploy_2)

###funds rate as a predictor
###independent run

summary(lm(diff_rate_2 ~ diff_log_gdp_2))

 
### monthly job report
### percent form
jobless_t <- as_tibble(jobless)
jobless_t %>%
  mutate(
  date = ymd(date)) %>%
ggplot(aes(date, value)) +
  geom_point()

### All Employees Total Non-farm

payroll <- fredr(series_id = "PAYEMS")

payroll <- payroll %>%
  as_tibble() %>%
  mutate(date = ymd(date))

payroll %>%
  as_tibble() %>%
  mutate(
    mom = value - lag(value),
    date = ymd(date)) %>%
  filter(date > "2021-08-01") %>%
  tail()
  ggplot(aes(date, mom)) +
  geom_line() +
  theme_bw()

ggsave("jobless.pdf")
library(tidyverse)
write_csv(jobless, "jobless.csv")

            ####################################

######break in purpose
######forecasting
jobless.ts <- ts(jobless$value, start=c(1948-01-01),
              frequency=12)

plot(jobless)

decompose(jobless.ts, type="multiplicative") %>%
  autoplot()

### as tsibble

nojob <- as_tsibble(nojob)
nojob_qrt<-aggregate(nojob,nfrequency=4, FUN=mean)


job_cut <- nojob %>%
  filter_index("1954-07-01" ~ .)

###models
###forecasting
###plotting
nojob_fore <- nojob %>%
  filter_index(. ~ "2016-01-01") %>%
  model(ets=ETS(value),
        arima=ARIMA(value),
        dynam = TSLM(value ~ trend())) %>%
  fabletools::forecast(h=52) %>%
  autoplot() +
  autolayer(nojob)
  ###add on
  labs(title="Unemployment Forecast",
       subtitle="4 Year Forecast",
       y="Rate",
       x="Date",
       caption = "Source: FRED")  +
  theme(plot.caption = element_text(hjust=0)) +
  theme_bw()

###accuracy

nojob_fore %>%
fabletools::accuracy(nojob) %>%
arrange(RMSE)

install.packages("fabletools")
library(tsibble)
library(dplyr)
library(fpp3)


### 12 month lag graph

nojob %>%
  mutate(change = value - lag(value, k=12)) %>%
  autoplot(change)
###moving average graph

nojob %>%
 mutate(moving=zoo::rollmean(nojob$value, k=105, fill=NA)) %>%
  ggplot() +
  geom_line(aes(x=index, y=value), color="darkblue", lwd=1.5) +
  geom_line(aes(x=index, y=moving),color="red", lwd=1.5) +
  labs(title="US Unemployment Rate",
       subtitle="5 year moving average",
       y="Unemployment Rate",
       x="Date") +
  theme_bw()

nojob %>%
  filter_index("2015 Jan" ~ .)%>%
  autoplot()

###fed funds rate and unemployment

fed <- (fredr(series_id = "FEDFUNDS"))
                    
fed_funds <- as_tsibble(fed, index=date)
fed_funds$rate <- fed_funds$value
fed_funds$index <- fed_funds$date
###right join
taylor <- right_join(fed_funds, job_cut, by="index")
head(taylor)

###clean dataset
taylor$unemployment <- taylor$value.y
taylor$funds_rate <- taylor$value.x

taylor$value.y=NULL
taylor$value.x=NULL
taylor$realtime_start=NULL
taylor$realtime_end=NULL

taylor$rate=NULL


### k shape recovery

jobless$unemployment <- jobless$value

black <- fredr(series_id="LNS14000006") ### 1972
white <- fredr(series_id="LNS14000003") ###1954

black$black <- black$value
white$white <- white$value
head(black)

black <- black %>%
  mutate(date = as.Date(date, fortmat = "%Y%m%d")) %>%
  as_tsibble(index=date)

white <- white %>%
  mutate(date = as.Date(date, fortmat = "%Y%m%d")) %>%
  as_tsibble(index=date) %>%
  filter_index("1972-01-01" ~ .)

jobless <- jobless %>%
  as_tsibble(index=date) %>%
  filter_index("1972-01-01" ~ .)

jobless$black <- black$black
head(jobless)

jobless$white <- white$white
is_tsibble(jobless)

whites <- jobless %>%
  select(white) %>%
  filter_index("2020-01-01" ~ .)

blacks <- jobless %>%
  select(black) %>%
  filter_index("2020-01-01" ~ .)

###k shape recovery black and white
autoplot(jobless, lwd=1.2) +
  autolayer(whites, lwd=1.2, color="red") +
  autolayer(blacks, lwd=1.2, color="blue") +
  theme_bw() +
  labs(title="K Shape Recovery",
       y="Rate",
       x="Date",
       caption="Source :FRED")

###plot all time unemployment values
###total
###black
###white
ggplot(jobless) +
  geom_line(aes(date, unemployment), color="black") +
  geom_line(aes(date, white), color="blue") +
  geom_line(aes(date, black), color="red") +
  theme_bw()

head(jobless)


###m2
###money supply
m2 <- fredr(series_id="WM2NS")
head(m2)

m2_annual <- m2 %>%
  mutate(
    change = ((value-lag(value,52)) / 
                lag(value, 52)) *100)
tail(m2_annual)
head(m2)

ggplot(m2_annual) +
  geom_area(aes(x=date, y=change, alpha=1/3), color="darkblue") +
  geom_hline(yintercept = 0, color="red") +
  labs(title="Annual Change in the Money Supply",
       subtitle="Measured as M2",
       y="Change %",
       x="Date") +
  theme_bw()
write_csv(labor, "labor.csv")

tail(m2_annual)

m2_tsib %>%
  filter_index("2019" ~ .) %>%
  mutate(
    change = ((value-lag(value, 52)) / lag(value,52) *100)) %>%
  ggplot() +
    geom_line(aes(date, change), color="darkblue", lwd=1.2) +
  geom_hline(yintercept=0, color="red", lwd=1.2) +
  labs(title="M2 Supply",
       y="Annual Chnage %") +
  theme_bw()



df <- m2_tsib %>%
  filter_index("2020" ~ .)
  mutate(
    change = ((value-lag(value, 52.18)) / lag(value,52.18) *100)) %>%
  ggplot() +
  geom_line(aes(date, change), color="darkblue", lwd=1.2) +
  geom_hline(yintercept=0, color="red", lwd=1.2) +
  labs(title="M2 Supply",
       y="Annual Chnage %") +
  theme_bw()

decompose(m2$change)

m2_ts <- ts(m2$value, start=c(1980-11-03),
                       frequency = 52)

m2_ts %>%
  stl(s.window = "periodic") %>%
  autoplot()

plot(m2$value)

par(mfrow=c(3,3))

ggplot(m2, aes(date, value)) +
  geom_line() +
  labs(title="M2 Supply",
       y="Billions of Dollars",
       x="Date") +
  theme_bw()

m2_ts %>%
  stl(s.window = "periodic") %>%
  autoplot() +
  theme_bw()

### forecasting

is_tsibble(m2)

m2_tsib <- m2 %>%
  mutate(date=as.Date(date, format="%y%m%d")) %>%
  as_tsibble(index=date)

m2_ts <- ts(m2$value, start=c(1980-11-03), frequency = 52.18)



###use a piecewise function
library(psych)
View(m2_tsib)

###fit models
fit <- m2_tsib %>%
  filter_index(. ~ "2014-12-29") %>%
  model(
    arima=ARIMA(value),
    ets=ETS(value),
    stl=decomposition_model(STL(value ~ trend(window=7), robust=T),
                            NAIVE(season_adjust)),
    naive=NAIVE(value))

###forecasts

fit %>% fabletools::forecast(h="8 years") %>%
  autoplot(m2_tsib) +
  labs(title="M2 Supply",
       subtitle="M2 = Cash + Checking Deposits + Savings",
       y="Billions of Dollars") +
  theme_bw() 

###accuracy

fore %>%
  fabletools::accuracy(m2_tsib) %>%
  arrange(RMSE)

###check specific model residuals
###gg_tsresiduals

fit %>%
  select(.model="arima") %>%
gg_tsresiduals()


augment(fit)

fit %>% 
  select(.model="arima") %>%
  features(.innov, ljung_box)

tidy(fit)

augment(fit) %>% 
  features(.innov, box_pierce)


###whats m2 ~ fed coefficient?
###kitco convo


dev.off()
library(gridExtra)


library(fpp3)
library(fpp2)
m2_ts <- ts(m2$value, start=c("1980-11-03"), end="2022-12-05", frequency = 12)
             
