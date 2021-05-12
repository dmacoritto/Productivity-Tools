source("../R_Packages/packages.R")

########################################################
#New book: Forecasting principle and practise
########################################################

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")



#Example tsibble
prison <- prison %>%
  mutate(Quarter = yearquarter(Date)) %>%
  select(-Date) %>%
  as_tsibble(key = c(State, Gender, Legal, Indigenous),
             index = Quarter)
#Index indicates the frequency in the time series
#The key is the different subgroups for which the time series can be represented


#autoplot(prison, Count)
#This plots all possible combinations

unique(prison$State)
unique(prison$Gender)
unique(prison$Legal)
unique(prison$Indigenous)

prison%>%
  filter(State=="ACT" & Gender=="Male" & Legal=="Sentenced" & Indigenous=="Non-ATSI")%>%
  autoplot(Count)
#Plot a single combination

#Example gg_season
prison%>%
  summarise(test=sum(Count))%>%
  gg_season(test)

#Example gg_subseries
prison%>%
  summarise(test=sum(Count))%>%
  gg_subseries(test)

#Example of lags
prison%>%
  summarise(test=sum(Count))%>%
  gg_lag(test, geom = "point")


prison%>%
  summarise(test=sum(Count))%>%
  ACF(test)%>%
  autoplot()
#Only trend there
#=> Trend : Decreasing lines, Seasonal : Peaks every season, Both can be combined

aus_production%>%
  as_tsibble(index="Quarter")%>%
  autoplot(Bricks)

vic_elec%>%
  as_tsibble(index="Time")%>%
  autoplot()+
  labs(title="Demand", x="Time")

#2.10.7
aus_arrivals%>%
 ggplot(aes(x=Quarter, y=Arrivals))+
 geom_line()+
 facet_grid(vars(Origin), scales = "free_y")
  
aus_arrivals%>%
  gg_season()

aus_arrivals%>%
  gg_subseries()

#2.10.8
set.seed(12345678)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

autoplot(myseries)

gg_season(myseries)

gg_subseries(myseries)

gg_lag(myseries, lags=1:24)+
  facet_wrap(~ .lag, ncol=6)

ACF(myseries, Turnover, lag_max = 60)%>%
  autoplot()


#2.10.11
aus_livestock%>%
  filter(Animal=="Pigs" & State =="Victoria" & year(Month)>=1990 & year(Month)<=1999)%>%
  ACF(Count)%>%
  autoplot()
#Increasing the time range increases the trend

#2.10.12
dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))
#This must be done because the trading days are not all the days

dgoog%>%
  autoplot(diff)
#Looks white noise

dgoog%>%
  ACF(diff)%>%
  autoplot()
#It is indeed white noise


#Chapter 3 in book

lambda <- aus_production %>%
  features(Gas, features = guerrero) %>%
  pull(lambda_guerrero)

aus_production %>%
  autoplot(box_cox(Gas, lambda)) +
  labs(y = "",
       title = paste0(
         "Transformed gas production with lambda = ",
         round(lambda,2)))


#Exercices, chapter 3

#3.7.1
global_economy%>%
  mutate(GPD_capita = GDP/Population)%>%
  autoplot(GPD_capita)+
  guides(colour = FALSE)

global_economy%>%
  as_tibble()%>%
  mutate(GDP_capita = GDP/Population)%>%
  filter(Year==max(Year, na.rm=T))%>%
  arrange(desc(GDP_capita))


#3.7.2
#1
global_economy%>%
  filter(Country=="United States")%>%
  autoplot()
#We need to make the line linear for easier explanation

lambda_guerrero <- global_economy%>%
  filter(Country=="United States")%>%
  features(GDP, features = guerrero)%>%
  pull(lambda_guerrero)

global_economy%>%
  filter(Country=="United States")%>%
  autoplot(box_cox(GDP, lambda_guerrero))


#2
aus_livestock%>%
  filter(Animal=="Bulls, bullocks and steers" & State =="Victoria")%>%
  autoplot()

#3.7.3
canadian_gas %>%
  autoplot(Volume)
#Because the variation in seasonality increases and decreases
#Power transformations (like the Box-Cox transformation) require the variability of the series to vary proportionately to the level of the series


set.seed(12345678)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))
autoplot(myseries)



lambda_guerrero <- myseries%>%
  features(Turnover, features = guerrero)%>%
  pull(lambda_guerrero)

myseries%>%
  autoplot(box_cox(Turnover, lambda_guerrero))

#3.7.7
gas <- tail(aus_production, 5*4) %>% select(Gas)

#a
autoplot(gas)

#b
decomp <- gas %>%
  model(decomp = classical_decomposition(Gas, type = "multiplicative")) %>%
  components()

#c
autoplot(decomp)
#Yes, it looks the seasonal and trend are captured


#d
as_tsibble(decomp) %>%
  autoplot(season_adjust) 

#e
gas_outlier <- gas

gas_outlier[1,"Gas"] <- gas[1,"Gas"]+300


decomp_outlier <- gas_outlier %>%
  model(decomp = classical_decomposition(Gas, type = "multiplicative")) %>%
  components()

autoplot(decomp_outlier)

decomp_outlier%>%
  as_tsibble()%>%
  autoplot(season_adjust)

gas_outlier_2 <- gas

gas_outlier_2[10,"Gas"] <- gas[10,"Gas"]+300


decomp_outlier_2 <- gas_outlier_2 %>%
  model(decomp = classical_decomposition(Gas, type = "multiplicative")) %>%
  components()

autoplot(decomp_outlier_2)

decomp_outlier_2%>%
  as_tsibble()%>%
  autoplot(season_adjust)

#if in the middle of the time series, the seasonality is impacted badly




#3.10
canadian_gas%>%
  autoplot()

canadian_gas%>%
  gg_season()

canadian_gas%>%
  gg_subseries()

STL_canadian <- canadian_gas%>%
  model(STL(Volume,
            robust = TRUE))%>%
  components()

autoplot(STL_canadian)


as_tsibble(STL_canadian)%>%
  gg_season(season_year)




#Chapter 4


tourism %>%
  features(Trips, list(mean = mean, quantile=quantile)) %>%
  arrange(mean)

tourism %>% features(Trips, feat_acf)
#Give information on the autocorrelation


tourism %>%
  features(Trips, feat_stl)
#Give information on the structure of the time series


tourism %>%
  features(Trips, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year,
             col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State))


#Chapter 5

bricks <- aus_production %>%
  filter_index("1970 Q1" ~ "2004 Q4")

bricks %>% 
  model(MEAN(Bricks))
#All future equal to the mean

bricks %>%
  model(NAIVE(Bricks))
#All future equal to last

bricks %>% 
  model(SNAIVE(Bricks ~ lag("year")))
#Same as naive but with seasonal component

bricks %>% 
  model(RW(Bricks ~ drift())) #NAIVE and RW are identical
#naive with drift ==> 
#This is equivalent to drawing a line between the first and last observations, and extrapolating it into the future.


bricks %>% 
  model(RW(Bricks ~ drift()))






