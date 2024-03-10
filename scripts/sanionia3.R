# sanionia sample 3 lab data

# to do: 

# libraries ----
library(tidyverse)
library(skimr)
library(brms)
library(tidybayes)
library(MASS)
library(corrplot)
library(MuMIn)
library(readr)


# loading data ----
weights <- read.csv("data\\weights.csv")

sanioniaLab <- list.files(path = ".\\data\\lab", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  filter(Object == 1)
  
View(sanioniaLab)

# cleaning and combining data ----
colnames(sanioniaLab) <- make.names(colnames(sanioniaLab), unique = TRUE)

colnames(sanioniaLab) <- tolower(colnames(sanioniaLab))

sanioniaLab <- sanioniaLab %>% 
  mutate(area = as.numeric(area), co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),             # updating variable types
         dco2zp = as.numeric(dco2zp), dco2mp = as.numeric(dco2mp), 
         h2oabs = as.numeric(h2oabs), h2obuf = as.numeric(h2obuf),
         dh2ozp = as.numeric(dh2ozp), dh2omp = as.numeric(dh2omp),
         flow = as.numeric(flow), pamb = as.numeric(pamb), tcuv = as.numeric(tcuv),
         tleaf = as.numeric(tleaf), tamb = as.numeric(tamb), 
         tmin.max = as.numeric(tmin.max), partop = as.numeric(partop), 
         paramb = as.numeric(paramb), rh = as.numeric(rh), e = as.numeric(e),
         vpd = as.numeric(vpd), gh2o = as.numeric(gh2o), a = as.numeric(a), 
         ci = as.numeric(ci), ca = as.numeric(ca), wa = as.numeric(wa), 
         object = as.factor(object), 
         inside.fan = as.factor(inside.fan))

sanioniaLab <- bind_cols(sanioniaLab, weights$weight) %>%                      # adding weights
  rename(weight = ...41)


view(sanioniaLab)
skim(sanioniaLab)


## making light curve and water curve datasets

### creating column with temperature category to make filtering easier
sanioniaLab <- sanioniaLab %>% 
  mutate(tempCat = case_when(tcuv < 6 ~ 5,
                             tcuv > 9 & tcuv < 11 ~ 10,
                             tcuv > 14 & tcuv < 16 ~ 15, 
                             tcuv > 19 & tcuv < 21 ~ 20,
                             tcuv > 21 ~ 25)) %>% 
  mutate(tempCat = as.factor(tempCat))

view(sanioniaLab)


hist(sanioniaLab$a)                                                    # normal

### moisture curves
WC <- sanioniaLab %>% 
  filter(partop < 600 & !between(partop, 5, 490) & !tempCat == 25)

WCphotosynth <- WC %>% 
  filter(partop > 5)

WCresp <- sanioniaLab %>% 
  filter(partop < 5 & !tempCat == 25)

ggplot(WCphotosynth, aes(x = weight, y = a, color = partop)) +
  geom_point() +
  facet_wrap(WCphotosynth$tempCat) +
  geom_smooth() +
  theme_classic()

ggplot(WCresp, aes(x = weight, y = a, color = partop)) +
  geom_point() +
  facet_wrap(WCresp$tempCat) +
  geom_smooth() +
  theme_classic()


### light curve
LC <- sanioniaLab %>% 
  filter(between(weight, 55, 62))

ggplot(LC, aes(x = partop, y = a, color = tempCat)) +                         # missing points for some reason
  geom_point() +
  theme_classic()


### temperature curve
TC <- sanioniaLab %>% 
  filter(between(weight, 55, 62) & between(partop, 500, 600))
  

ggplot(TC, aes(x = tempCat, y = a, color = weight)) +                      
  geom_point() +
  theme_classic()


# models  ----
summary(lm(a ~ partop * tcuv * weight, data = sanioniaLab))
summary(lm(a ~ partop, data = ))
