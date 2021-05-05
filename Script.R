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