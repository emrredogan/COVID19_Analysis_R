library(readr)
setwd("C:/Users/yemre/OneDrive/Masaüstü/R Programming/COVID Project/data.csv")
library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(reshape2)

COVID_data <- read.csv("C:/Users/yemre/OneDrive/Masaüstü/R Programming/COVID Project/data.csv")
  
  
##PART 1
###a) The cumulative number of cases by date

#Set up date as date and reorder ascending
class(COVID_data$dateRep)
COVID_data$dateRep <- as.Date(COVID_data$dateRep, format = "%d/%m")
COVID_data <- COVID_data[order(COVID_data$dateRep),] 



#Replace NA values to 0 in order to find cumulative values
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

#Select 10 countries, then replace NA values to 0
Selected_countries <- COVID_data %>%
  filter(cases>0) %>%
  replace_na(list(cases=0L)) %>%
  filter(countriesAndTerritories=="France"|
           countriesAndTerritories=="Germany"|
           countriesAndTerritories=="Spain"|
           countriesAndTerritories=="Austria"|
           countriesAndTerritories=="Croatia"|
           countriesAndTerritories=="Denmark"|
           countriesAndTerritories=="Finland"|
           countriesAndTerritories=="Greece"|
           countriesAndTerritories=="Iceland"|
           countriesAndTerritories=="Italy") 
#Assign countries and cases to new variable and change dataset with wider func.
Selected_countries_cases <- Selected_countries%>%
  select(countriesAndTerritories, dateRep,cases) %>%
  pivot_wider(names_from = countriesAndTerritories, values_from = cases) %>% 
  na.zero(.)

#Create and calculate cumulative number of cases for each country
ten_countries_cases <- Selected_countries_cases %>% 
  mutate(Cumulative_cases_Austria = cumsum(Austria),
         Cumulative_cases_Croatia= cumsum(Croatia),
         Cumulative_cases_Denmark= cumsum(Denmark),
         Cumulative_cases_Finland= cumsum(Finland),
         Cumulative_cases_France= cumsum(France),
         Cumulative_cases_Germany= cumsum(Germany),
         Cumulative_cases_Greece= cumsum(Greece),
         Cumulative_cases_Iceland= cumsum(Iceland),
         Cumulative_cases_Italy= cumsum(Italy),
         Cumulative_cases_Spain= cumsum(Spain)) %>%
  select(c(-Austria,-Croatia,-Denmark,-Finland,-France,-Germany,-Greece,-Iceland,-Italy,-Spain))



#Draw line graphs to visualize the cumulative number of cases


#Reshape the data set in order to draw line graphs 
tenCase.m <- melt(ten_countries_cases,id.vars = 1)
# 0 values were NA,skip these values to reach better graphs
tenCase.m <- tenCase.m %>% filter(value>0)

#Draw the graph by using ggplot
ggplot(tenCase.m, aes(x=dateRep, y= value, color=variable))+
  geom_line(size=1)+
  #Arrange y axis, by considering min&max cumulative number of cases
  scale_y_continuous(breaks = c(seq(from =5000, to =8000000, by=1601000)))+
  labs(x="Date",
       y="Cumulative Case Number",
       title="The Cumulative Number of Cases By Date") +
  #facet_wrap(~variable)+
  theme_bw()


  
###b)	The cumulative number of deaths by date

#Assign countries and cases to new variable
Selected_countries_deaths <- Selected_countries%>%
  select(countriesAndTerritories, dateRep,deaths) %>%
  pivot_wider(names_from = countriesAndTerritories, values_from = deaths) %>% 
  na.zero(.)


#Create and calculate cumulative number of deaths for each country
ten_countries_deaths <- Selected_countries_deaths %>% 
  mutate(Cumulative_deaths_Austria = cumsum(Austria),
         Cumulative_deaths_Croatia= cumsum(Croatia),
         Cumulative_deaths_Denmark= cumsum(Denmark),
         Cumulative_deaths_Finland= cumsum(Finland),
         Cumulative_deaths_France= cumsum(France),
         Cumulative_deaths_Germany= cumsum(Germany),
         Cumulative_deaths_Greece= cumsum(Greece),
         Cumulative_deaths_Iceland= cumsum(Iceland),
         Cumulative_deaths_Italy= cumsum(Italy),
         Cumulative_deaths_Spain= cumsum(Spain)) %>%
  select(c(-Austria,-Croatia,-Denmark,-Finland,-France,-Germany,-Greece,-Iceland,-Italy,-Spain))


#Draw line graphs to visualize the cumulative number of cases


#Reshape the data set in order to draw line graphs 
tenDeaths.m <- melt(ten_countries_deaths,id.vars = 1)
# 0 values were NA,skip these values to reach better graphs
tenDeaths.m <- tenDeaths.m %>% filter(value>0)


#Draw the graph by using ggplot
ggplot(tenDeaths.m, aes(x=dateRep, y= value, color=variable))+
  geom_line(size=1)+
  #Arrange y axis, by considering min&max cumulative number of deaths
  scale_y_continuous(breaks = c(seq(from =10, to =160000, by=32002)))+
  labs(x="Date",
       y="Cumulative Death Number",
       title="The Cumulative Number of Deaths By Date") +
  facet_wrap(~variable)+
  theme_bw()



###c)	The cumulative number of cases by days since 100000th confirmed case  

options(scipen=99999) #In order to change scientific values to number on y-axis
COVID_data %>% 
  filter(countriesAndTerritories %in% ten_countries) %>%
  filter(cases >0) %>% 
  mutate(dateRep = as.Date(dateRep, '%d/%m/%Y')) %>%
  group_by(countriesAndTerritories) %>%
  arrange(dateRep) %>%
  mutate(cum_cases = cumsum(cases)) %>%
  filter(cum_cases >= 100000) %>% 
  group_by(countriesAndTerritories) %>% 
  mutate(cumulative_days = row_number()) %>% 
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = cumulative_days, y = cum_cases, color = countriesAndTerritories),
            size=1)+
  labs(x="Days Since Confirmed Cases",
       y="Cumulative Case Number",
       title="The Cumulative Number of Cases By Days Since 100000th Confirmed Case") +
  theme_bw()







#Alternative Solution for c (it will not be used)
ten_countries <- c('Denmark', 'Croatia', 'Austria', 'Finland', 'France', 'Germany', 
                   'Greece', 'Iceland', 'Italy', 'Spain')
options(scipen=99999)
COVID_data %>% filter(countriesAndTerritories %in% ten_countries) %>%
  filter(cases >0) %>% 
  mutate(dateRep = as.Date(dateRep, '%d/%m/%Y')) %>%
  group_by(countriesAndTerritories) %>%
  arrange(dateRep) %>%
  mutate(cum_cases = cumsum(cases)) %>%
  filter(cum_cases >= 100000) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = dateRep, y = cum_cases, color = countriesAndTerritories),size=1)+
  labs(x="Date",
       y="Cumulative Case Number",
       title="The Cumulative Number of Cases By Date") +
  theme_bw()





###d)	The cumulative number of deaths by days since 100000th confirmed case

  
#Try finding a reasonable solution for d
ten_countries <- c('Denmark', 'Croatia', 'Austria', 'Finland', 'France', 'Germany', 
                   'Greece', 'Iceland', 'Italy', 'Spain')
COVID_data %>% filter(countriesAndTerritories %in% ten_countries) %>%
  filter(cases >0) %>% 
  filter(deaths >0) %>% 
  mutate(dateRep = as.Date(dateRep, '%d/%m/%Y')) %>%
  group_by(countriesAndTerritories) %>%
  mutate(cum_cases = cumsum(cases)) %>%
  filter(cum_cases >= 100000) %>%
  mutate(cum_deaths = cumsum(deaths)) %>% 
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = dateRep, y = cum_deaths, color = countriesAndTerritories),size=1)+
  scale_y_continuous(breaks = c(seq(from =10, to =16000, by=32002)))+
  labs(x="Date",
       y="Cumulative Case Number",
       title="The Cumulative Number of Cases By Date") +
  theme_bw()
  
options(scipen=99999) #In order to change scientific values to number on y-axis
COVID_data %>% 
  filter(countriesAndTerritories %in% ten_countries) %>%
  filter(cases >0) %>% filter(deaths>0) %>% 
  mutate(dateRep = as.Date(dateRep, '%d/%m/%Y')) %>%
  group_by(countriesAndTerritories) %>%
  arrange(dateRep) %>%
  mutate(cum_cases = cumsum(cases)) %>%
  filter(cum_cases >= 100000) %>% 
  mutate(cum_deaths = cumsum(deaths)) %>% 
  group_by(countriesAndTerritories) %>% 
  mutate(cumulative_days = row_number()) %>% 
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = cumulative_days, y = cum_deaths, color = countriesAndTerritories),
            size=1)+
  labs(x="Days Since Confirmed Cases",
       y="Cumulative Deaths Number",
       title="The Cumulative Number of Deaths By Days Since 100000th Confirmed Case ") +
  theme_bw() 
  
##PART 2
###a)	Number of new cases at each date (absolute number vs per 100.000 population)

# For better printing
COVID_data <- as_tibble(COVID_data)

# Which countries have the highest absolute death toll? 
top10 <- COVID_data %>% 
  group_by(countriesAndTerritories) %>% 
  summarise(TotalDeaths=sum(deaths)) %>% 
  slice_max(TotalDeaths, n=10) %>% 
  distinct(countriesAndTerritories) %>% 
  pull(countriesAndTerritories)
#for new cases according to 100k pop
COVID_data %>% 
  filter(countriesAndTerritories %in% top10) %>% 
  filter(cases >0) %>% 
  mutate(
    deathRate=100000 * deaths / popData2020,
    caseRate=100000 * cases /popData2020,
    Date = dateRep)  %>% 
  arrange(countriesAndTerritories, Date) %>% 
  group_by(countriesAndTerritories) %>% 
  filter(row_number() > 1) %>% 
  ggplot() + 
  geom_line(aes(x=Date, y=caseRate)) +
  labs(x="Date",
       y="New Cases",
       title="New Cases per 100.000 population")+
  facet_wrap(~countriesAndTerritories)

#draw new cases according to absolute (deaths) number
COVID_withAbs <- COVID_data %>% 
  mutate(abs_deaths = sum(deaths))
COVID_withAbs %>% 
  filter(countriesAndTerritories %in% top10) %>% 
  filter(cases >0) %>% 
  mutate(
    deathRate= deaths / abs_deaths  ,
    caseRate=cases / abs_deaths,
    Date = dateRep)  %>% 
  arrange(countriesAndTerritories, Date) %>% 
  group_by(countriesAndTerritories) %>% 
  filter(row_number() > 1) %>% 
  ggplot() + 
  geom_line(aes(x=Date, y=caseRate)) +
  labs(x="Date",
       y="New Cases",
       title="New Cases as regard as Total Deaths")+
  facet_wrap(~countriesAndTerritories)



###b)	Number of new deaths at each date (absolute number vs per 100.000 population)



#New Deaths according to 100k population
COVID_data %>% 
  filter(countriesAndTerritories %in% top10) %>% 
  filter(deaths >0) %>% 
  mutate(
    deathRate=100000 * deaths / popData2020,
    caseRate=100000 * cases /popData2020,
    Date = dateRep)  %>% 
  arrange(countriesAndTerritories, Date) %>% 
  group_by(countriesAndTerritories) %>% 
  filter(row_number() > 1) %>% 
  ggplot() + 
  geom_line(aes(x=Date, y=deathRate)) +
  labs(x="Date",
       y="New Deaths",
       title="New Deaths per 100.000 population")+
  facet_wrap(~countriesAndTerritories)

#draw new deaths according to absolute (deaths) number
COVID_withAbs <- COVID_data %>% 
  mutate(abs_deaths = sum(deaths))
COVID_withAbs %>% 
  filter(countriesAndTerritories %in% top10) %>% 
  filter(deaths >0) %>% 
  mutate(
    deathRate= deaths / abs_deaths  ,
    caseRate=cases / abs_deaths,
    Date = dateRep)  %>% 
  arrange(countriesAndTerritories, Date) %>% 
  group_by(countriesAndTerritories) %>% 
  filter(row_number() > 1) %>% 
  ggplot() + 
  geom_line(aes(x=Date, y=deathRate)) +
  labs(x="Date",
       y="New Deaths",
       title="New Deaths as regard as Total Deaths")+
  facet_wrap(~countriesAndTerritories)

###c)	5 days moving average of new cases at each date (absolute number vs per 100.000 population)


#Find 5 days moving average of new cases at each date then draw the graph according to the pop.
#"rollmean()" function from zoo package is used to calculate the moving average

COVID_data %>% 
  filter(countriesAndTerritories %in% top10) %>% 
  filter(cases >0) %>% 
  mutate(
    deathRate=100000 * deaths / popData2020,
    caseRate=100000 * cases /popData2020,
    Date = dateRep)  %>% 
  mutate(moving_avg_newcases = zoo::rollmean(caseRate, k = 5, fill=0)) %>% 
  arrange(countriesAndTerritories, Date) %>% 
  group_by(countriesAndTerritories) %>% 
  filter(row_number() > 2) %>% 
  ggplot(size = 1) + 
  geom_line(aes(x=Date, y=moving_avg_newcases)) +
  labs(x="Date",
       y="5 Days Moving Avg. New Cases",
       title="5 Days Moving Average New Cases per 100.000 population")+
  facet_wrap(~countriesAndTerritories)

#Find 5 days moving average of new cases at each date then draw the graph according to the total deaths.
#"rollmean()" function from zoo package is used to calculate the moving average
COVID_withAbs <- COVID_data %>% 
  mutate(abs_deaths = sum(deaths))
COVID_withAbs %>% 
  filter(countriesAndTerritories %in% top10) %>% 
  filter(deaths >0) %>% 
  mutate(
    deathRate= deaths / abs_deaths  ,
    caseRate=cases / abs_deaths,
    Date = dateRep)  %>% 
  mutate(moving_avg_newcases = zoo::rollmean(caseRate, k = 5, fill=0)) %>%
  arrange(countriesAndTerritories, Date) %>% 
  group_by(countriesAndTerritories) %>% 
  filter(row_number() > 2) %>% 
  ggplot() + 
  geom_line(aes(x=Date, y=caseRate)) +
  labs(x="Date",
       y="New Cases",
       title="New Cases as regard as Total Deaths")+
  facet_wrap(~countriesAndTerritories)


###d)	5 days moving average of new deaths at each date (absolute number vs per 100.000 population

#Find 5 days moving average of new deaths for 100k pop. 
#"rollmean()" function from zoo package is used to calculate the moving average
COVID_data %>% 
  filter(countriesAndTerritories %in% top10) %>% 
  filter(deaths >0) %>% 
  mutate(
    deathRate=100000 * deaths / popData2020,
    caseRate=100000 * cases /popData2020,
    Date = dateRep)  %>% 
  mutate(moving_avg_newdeaths = zoo::rollmean(deathRate, k = 5, fill=NA)) %>% 
  arrange(countriesAndTerritories, Date) %>% 
  group_by(countriesAndTerritories) %>% 
  filter(row_number() > 2) %>% 
  ggplot(size = 1) + 
  geom_line(aes(x=Date, y=moving_avg_newdeaths)) +
  labs(x="Date",
       y="5 Days Moving Avg. New Deaths",
       title="5 Days Moving Average New Deaths per 100.000 population")+
  facet_wrap(~countriesAndTerritories)


#Find 5 days moving average of new deaths for total deaths
COVID_withAbs <- COVID_data %>% 
  mutate(abs_deaths = sum(deaths))
COVID_withAbs %>% 
  filter(countriesAndTerritories %in% top10) %>% 
  filter(deaths >0) %>% 
  mutate(
    deathRate= deaths / abs_deaths  ,
    caseRate=cases / abs_deaths,
    Date = dateRep)  %>% 
  mutate(moving_avg_newcases = zoo::rollmean(caseRate, k = 5, fill=0)) %>%
  arrange(countriesAndTerritories, Date) %>% 
  group_by(countriesAndTerritories) %>% 
  filter(row_number() > 2) %>% 
  ggplot() + 
  geom_line(aes(x=Date, y=deathRate)) +
  labs(x="Date",
       y="New Deaths",
       title="New Deaths as regard as Total Deaths")+
  facet_wrap(~countriesAndTerritories)

##PART 3
#a)	the number of deaths per 100 confirmed cases

COVID_data %>% 
  filter(countriesAndTerritories %in% ten_countries) %>% 
  filter(cases >0) %>% 
  mutate(cum_cases = sum(cases)) %>% 
  mutate(deaths_per_confirmed = 100*deaths/cum_cases) %>% 
  arrange(countriesAndTerritories) %>% 
  group_by(countriesAndTerritories) %>% 
  filter(row_number() > 1) %>% 
  ggplot(aes(x=dateRep, y=deaths_per_confirmed, fill=countriesAndTerritories))+
  geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.8))+
  xlab("Date") + ylab("Deaths per Confirmed Cases")+
  ggtitle("The Number of Deaths per 100 Confirmed Cases")+
  facet_wrap(~countriesAndTerritories)

#b)	the number of deaths per 100,000 population
COVID_data %>% 
  filter(countriesAndTerritories %in% ten_countries) %>% 
  filter(deaths > 0) %>% 
  mutate(deathRate = 100000 * deaths / popData2020) %>% 
  arrange(countriesAndTerritories) %>% 
  group_by(countriesAndTerritories) %>% 
  filter(row_number() > 1) %>% 
  ggplot(aes(x=dateRep,y=deathRate, fill=countriesAndTerritories))+
  geom_bar(stat = "identity", width= 0.5, position= position_dodge(0.8))+
  xlab("Date") + ylab("Deaths per 100k Population") +
  ggtitle("The Number Of Deaths per 100,000 Population")+
  facet_wrap(~countriesAndTerritories)
















