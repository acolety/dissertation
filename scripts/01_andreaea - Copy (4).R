# exploring preliminary data
# productivity with light, temp, and water

# libraries ----
library(tidyverse)
library(skimr)

# loading data ----
andreaea <- read.csv("data\\andreaea.csv")


# cleaning data ----
view(andreaea)
skim(andreaea)

colnames(andreaea) <- tolower(colnames(andreaea))  # make column names lowercase

andreaea_01 <- andreaea %>%  
  filter(!row_number() == 1) %>%  # remove row of units
  mutate(area = 6.9291) %>%   # change to actual area from corrected_areas
  rename(wetness_ws = wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,
         temp_ws = temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         wind_speed_ws = wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rh_ws = relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station)  # rename weather station variables

andreaea_02 <- andreaea_01 %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf), 
         dco2zp = as.numeric(dco2zp), dco2mp = as.numeric(dco2mp), 
         h2oabs = as.numeric(h2oabs), h2obuf = as.numeric(h2obuf),
         dh2ozp = as.numeric(dh2ozp), dh2omp = as.numeric(dh2omp),
         flow = as.numeric(flow), pamb = as.numeric(pamb), tcuv = as.numeric(tcuv),
         tleaf = as.numeric(tleaf), tamb = as.numeric(tamb), 
         tmin.max = as.numeric(tmin.max), partop = as.numeric(partop), 
         paramb = as.numeric(paramb), rh = as.numeric(rh), e = as.numeric(e),
         vpd = as.numeric(vpd), gh2o = as.numeric(gh2o), a = as.numeric(a), 
         ci = as.numeric(ci), ca = as.numeric(ca), wa = as.numeric(wa), 
         parweatherstation = as.numeric(parweatherstation), 
         weight = as.numeric(weight), wetness_ws = as.numeric(wetness_ws),
         temp_ws = as.numeric(temp_ws), rh_ws = as.numeric(rh_ws), 
         wind_speed_ws = as.numeric(wind_speed_ws), object = as.factor(object), 
         inside.fan = as.factor(inside.fan))  # updating variable types

andreaea_03 <- andreaea_02 %>% 
  mutate(a_corrected = (a / 8) * area)  # correcting assimilation with updated area
  



view(andreaea_03)
skim(andreaea_03)

# basic exploration ----
ggplot(andreaea_03, aes(x = weight, y = a_corrected)) +
  geom_point() +
  theme_classic()

ggplot(andreaea_03, aes(x = tcuv, y = a_corrected)) +
  geom_point() +
  theme_classic()

ggplot(andreaea_03, aes(x = parweatherstation, y = a_corrected)) +
  geom_point() +
  theme_classic()

# analysis ----

# creating outputs ----