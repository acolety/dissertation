# exploring field data
# productivity with light, temp, and water

# MOSSES: Andreaea, chorisodontium, polytrichum, sanionia
# LICHENS: himantormia, stereocaulon, usnea ant, usnea aur

### TO DO: data tranformations (could try scaling), collinearity troubleshooting, determine best family for bayes,
### Mann Whitney U for non-normal data?

### approaches to data transformations:
### scaling, adding constant to aCorrected, and log transform
### (or boxcox), cube root
### alt use nonparametric equivalent or bayesian

# libraries ----
library(tidyverse)
library(skimr)
library(brms)
library(tidybayes)
library(MASS)
library(corrplot)


# loading data ----
andreaeaDf <- read.csv("data\\andreaea.csv") %>% 
    filter(!row_number() == 1)                                                 # removing row with units

chorisodontiumDf <- read.csv("data\\chorisodontium.csv") %>% 
  filter(!row_number() == 1)

himantormiaDf <- read.csv("data\\himantormia.csv") %>% 
  filter(!row_number() == 1)

polytrichumDf <- read.csv("data\\polytrichum_strictum.csv") %>% 
  filter(!row_number() == 1)

sanioniaDf <- read.csv("data\\sanionia.csv") %>% 
  filter(!row_number() == 1)

stereocaulonDf <- read.csv("data\\stereocaulon.csv") %>% 
  filter(!row_number() == 1)

usneaAntDf <- read.csv("data\\usnea_antarctica.csv") %>% 
  filter(!row_number() == 1)

usneaAurDf <- read.csv("data\\usnea_aurantiaco-atra.csv") %>% 
  filter(!row_number() == 1)



# cleaning and combining data ----

## 1. make column names lowercase
colnames(andreaeaDf) <- tolower(colnames(andreaeaDf))

colnames(chorisodontiumDf) <- tolower(colnames(chorisodontiumDf))

colnames(himantormiaDf) <- tolower(colnames(himantormiaDf))

colnames(polytrichumDf) <- tolower(colnames(polytrichumDf))

colnames(sanioniaDf) <- tolower(colnames(sanioniaDf))

colnames(stereocaulonDf) <- tolower(colnames(stereocaulonDf))

colnames(usneaAntDf) <- tolower(colnames(usneaAntDf))

colnames(usneaAurDf) <- tolower(colnames(usneaAurDf))


## 2. checking column names are the same
unique(c(colnames(andreaeaDf), colnames(chorisodontiumDf), colnames(himantormiaDf),
         colnames(polytrichumDf), colnames(sanioniaDf), 
         colnames(stereocaulonDf), colnames(usneaAntDf),
         colnames(usneaAurDf)))


## 3. column for spp. and appropriate area, changing var types

### a. andreaea
andreaeaDf <- andreaeaDf %>% 
  mutate(species = "andreaea",                                                 # adding spp column
         area = 6.9291) %>%                                                    # adding area from corrected_areas.csv
  rename(wetnessWS =                                                           # shorten column names
           wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  
         tempWS = 
           temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         windspeedWS = 
           wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rhWS = 
           relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),             # updating variable types
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
         weight = as.numeric(weight), wetnessWS = as.numeric(wetnessWS),
         tempWS = as.numeric(tempWS), rhWS = as.numeric(rhWS), 
         windspeedWS = as.numeric(windspeedWS), object = as.factor(object), 
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(aCorrected = (a / 8) * area)                                          # correcting 'a' with spp. areas


### b. chorisodontium
chorisodontiumDf <- chorisodontiumDf %>% 
  mutate(species = "chorisodontium",                                           # adding spp column
         area = 107.80784) %>%                                                 # adding area
  mutate(weight = weight..incl..water.) %>%                                    # changing mismatched column name
  rename(wetnessWS =                                                           # shorten column names
           wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  
         tempWS = 
           temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         windspeedWS = 
           wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rhWS = 
           relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),             # updating variable types
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
         weight = as.numeric(weight), wetnessWS = as.numeric(wetnessWS),
         tempWS = as.numeric(tempWS), rhWS = as.numeric(rhWS), 
         windspeedWS = as.numeric(windspeedWS), object = as.factor(object), 
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(aCorrected = (a / 8) * area)                                          # correcting 'a' with spp. areas


### c. himantormia
himantormiaDf <- himantormiaDf %>% 
  mutate(species = "himantormia",                                              # add spp column
         area = 15.83125) %>%                                                  # add correct area
  rename(wetnessWS =                                                           # shorten column names
           wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  
         tempWS = 
           temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         windspeedWS = 
           wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rhWS = 
           relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),             # updating variable types
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
         weight = as.numeric(weight), wetnessWS = as.numeric(wetnessWS),
         tempWS = as.numeric(tempWS), rhWS = as.numeric(rhWS), 
         windspeedWS = as.numeric(windspeedWS), object = as.factor(object), 
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(aCorrected = (a / 8) * area)                                          # correcting 'a' with spp. areas


### d. polytrichum
polytrichumDf <- polytrichumDf %>% 
  mutate(species = "polytrichum",                                              # add spp column
         area = 52.85567) %>%                                                  # add correct area
  rename(wetnessWS =                                                           # shorten column names
           wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  
         tempWS = 
           temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         windspeedWS = 
           wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rhWS = 
           relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),             # updating variable types
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
         weight = as.numeric(weight), wetnessWS = as.numeric(wetnessWS),
         tempWS = as.numeric(tempWS), rhWS = as.numeric(rhWS), 
         windspeedWS = as.numeric(windspeedWS), object = as.factor(object), 
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(aCorrected = (a / 8) * area)                                          # correcting 'a' with spp. areas


### e. sanionia
sanioniaDf <- sanioniaDf %>% 
  mutate(species = "sanionia",                                                 # add spp column
         area = 77.48568) %>%                                                  # add correct area
  rename(wetnessWS =                                                           # shorten column names
           wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  
         tempWS = 
           temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         windspeedWS = 
           wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rhWS = 
           relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),             # updating variable types
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
         weight = as.numeric(weight), wetnessWS = as.numeric(wetnessWS),
         tempWS = as.numeric(tempWS), rhWS = as.numeric(rhWS), 
         windspeedWS = as.numeric(windspeedWS), object = as.factor(object), 
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(aCorrected = (a / 8) * area)                                          # correcting 'a' with spp. areas


### f. stereocaulon
stereocaulonDf <- stereocaulonDf %>% 
  mutate(species = "stereocaulon",                                             # adding spp column
         area = 5.10035) %>%                                                   # adding correct area
  rename(wetnessWS =                                                           # shorten column names
           wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  
         tempWS = 
           temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         windspeedWS = 
           wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rhWS = 
           relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),             # updating variable types
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
         weight = as.numeric(weight), wetnessWS = as.numeric(wetnessWS),
         tempWS = as.numeric(tempWS), rhWS = as.numeric(rhWS), 
         windspeedWS = as.numeric(windspeedWS), object = as.factor(object), 
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(aCorrected = (a / 8) * area)                                          # correcting 'a' with spp. areas


### g. usnea antarctica
usneaAntDf <- usneaAntDf %>% 
  mutate(species = "usneaAnt",                                                 # adding spp. column
         area = if_else(row_number() <= 16, 74.66479, 32.15818)) %>%           # adding correct area
  rename(wetnessWS =                                                           # shorten column names
           wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  
         tempWS = 
           temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         windspeedWS = 
           wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rhWS = 
           relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),             # updating variable types
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
         weight = as.numeric(weight), wetnessWS = as.numeric(wetnessWS),
         tempWS = as.numeric(tempWS), rhWS = as.numeric(rhWS), 
         windspeedWS = as.numeric(windspeedWS), object = as.factor(object), 
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(aCorrected = (a / 8) * area)                                          # correcting 'a' with spp. areas


### h. usnea aurantiaco
usneaAurDf <- usneaAurDf %>% 
  mutate(species = "usneaAur",                                                 # adding spp column
         area = 43.84498) %>%                                                  # adding correct area
  rename(wetnessWS =                                                           # shorten column names
           wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  
         tempWS = 
           temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         windspeedWS = 
           wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rhWS = 
           relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),             # updating variable types
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
         weight = as.numeric(weight), wetnessWS = as.numeric(wetnessWS),
         tempWS = as.numeric(tempWS), rhWS = as.numeric(rhWS), 
         windspeedWS = as.numeric(windspeedWS), object = as.factor(object), 
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(aCorrected = (a / 8) * area)                                          # correcting 'a' with spp. areas


## 4. checking
### View(andreaea)
### View(chorisodontium)
### View(himantormia)
### View(polytrichum)
### View(sanionia)
### View(stereocaulon)
### View(usneaAnt)
### View(usneaAur)


## 5. combining data

### a. all data
all00 <- bind_rows(andreaeaDf, chorisodontiumDf, himantormiaDf, polytrichumDf,
                 sanioniaDf, stereocaulonDf, usneaAntDf, usneaAurDf)                                                
### View(all00)


### b. mosses only
moss00 <- bind_rows(andreaeaDf, chorisodontiumDf, polytrichumDf, sanioniaDf)   
### View(moss00)


### c. lichens only
lichen00 <- bind_rows(himantormiaDf, stereocaulonDf, usneaAntDf, usneaAurDf)   
### View(lichen00)



# data checks ----

## 1. collinearity

### a. all data
pairs(all_00[, c("parweatherstation", "temp_ws", "weight")])                   # seems like par and temp may be related


### b. just lichens
pairs(lichen_00[, c("parweatherstation", "temp_ws", "weight")])


### c. just mosses
pairs(moss_00[, c("parweatherstation", "temp_ws", "weight")])


### d. trying corrplot method
all_01 <- all_00 %>% 
  dplyr::select_if(is.numeric)                                                 # selecting numeric data only

corrplot(cor(all_01, use = "everything"),                                      # isn't showing par numbers
         method = "number", type = "upper")


### e. just using a linear regression for now
collinear <- lm(parweatherstation ~ temp_ws, data = all_00)

summary(collinear)                                                             # par and temp are significantly related



## 2. distribution of response variable (a_corrected)

### a. overall
ggplot(all_00, aes(x = a_corrected)) +
  geom_histogram(bins = 20) +
  theme_classic()


### b. by species
ggplot(all_00, aes(x = a_corrected)) +
  facet_wrap(~ species, nrow = 4, ncol = 2, scales = "free") +
  geom_histogram(bins = 15) +
  theme_classic()



# plotting raw data ----

## 1. light
### a. check partop or parweatherstation
ggplot(all00, aes(x = partop, y = aCorrected, color = species)) +
  facet_wrap(~ species, nrow = 4, ncol = 2, scales = "free") +
  geom_point(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(show.legend = FALSE) +
  theme_classic()
View(all00)

## 2. temperature (weather station)
ggplot(all00, aes(x = tempWS, y = aCorrected, color = species)) +
  facet_wrap(~ species, nrow = 4, ncol = 2, scales = "free") +
  geom_point(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(show.legend = FALSE) +
  theme_classic()


## 3. weight (wetness)
ggplot(all00, aes(x = weight, y = aCorrected, color = species)) +
  facet_wrap(~ species, nrow = 4, ncol = 2, scales = "free") +
  geom_point(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(show.legend = FALSE) +
  theme_classic()



# frequentist modelling approach ----

## 1. himantormia

### a. standard lm

#### i. light, temp, water with interaction
himantormiaMod00 <- lm(aCorrected ~ parweatherstation * tempWS * weight, 
                       data = himantormiaDf)

plot(himantormiaMod00)                                                         # doesn't look super normal
shapiro.test(resid(himantormiaMod00))                                          # says normal
summary(himantormiaMod00)                                                      # no sig results (0.052 - 0.29)


#### ii. individual factors

##### 1. light
himantormiaLight00 <- lm(aCorrected ~ parweatherstation, data = himantormiaDf)

plot(himantormiaLight00)                                                       # not normal
shapiro.test(resid(himantormiaLight00))                                        # confirms not normal


##### 2. temperature
himantormiaTemp00 <- lm(aCorrected ~ tempWS, data = himantormiaDf)

plot(himantormiaTemp00)                                                        # not normal
shapiro.test(resid(himantormiaTemp00))                                         # confirms not normal


##### 3. water
himantormiaWater00 <- lm(aCorrected ~ weight, data = himantormiaDf)

plot(himantormiaWater00)                                                       # not very normal looking
shapiro.test(resid(himantormiaWater00))                                        # says resid normal
summary(himantormiaWater00)                                                    # positive effect (p < 0.05)


### b. boxcox transformation

#### i. adding constant to remove negative aCorrected values
range(himantormiaDf$aCorrected)                                                # min aCorrected is -1.41

himantormiaDf <- himantormiaDf %>% 
  mutate(aCorrectedBox = aCorrected + 2)


#### ii. determining best transformation
himantormiaTrans <- boxcox(lm(aCorrectedBox ~ 1, data = himantormiaDf))

himantormiaTrans$x[which.max(himantormiaTrans$y)]                              # suggests log transformation


#### iii. making transformation
himantormiaDf <- himantormiaDf %>% 
  mutate(aCorrectedBox2 = log(aCorrectedBox) - log(2))                         # is this math right


##### 1. checking if plot looks right

###### a. before transformation
ggplot(himantormiaDf, aes(x = parweatherstation, y = aCorrectedBox)) +
  geom_point() +
  ylim(0, 5) +
  geom_hline(yintercept = 2)  


###### b. transformed
ggplot(himantormiaDf, aes(x = parweatherstation, y = aCorrectedBox2)) +
  geom_point() +
  ylim(-1.5, 0.5) +
  geom_hline(yintercept = 0)                                                   # seems like its worked


### c. retrying lm with log transformed aCorrected

#### i. light, temp, water with interaction just for kicks
himantormiaMod01 <- lm(aCorrectedBox2 ~ parweatherstation * tempWS * weight, 
                    data = himantormiaDf)

plot(himantormiaMod01)                                                         # normal, looks slightly better than non-logged
shapiro.test(resid(himantormiaMod01))                                          # confirms still normal
summary(himantormiaMod01)


#### ii. individual factors

##### 1. light
himantormiaLight01 <- lm(aCorrectedBox2 ~ parweatherstation, data = himantormiaDf)

plot(himantormiaLight01)                                                       # normal, bit wonky
shapiro.test(resid(himantormiaLight01))                                        # confirms normal
summary(himantormiaLight01)                                                    # no sig relationship (0.91)


##### 2. temperature
himantormiaTemp01 <- lm(aCorrectedBox2 ~ tempWS, data = himantormiaDf)

plot(himantormiaTemp01)                                                        # doesn't look normal
shapiro.test(resid(himantormiaTemp01))                                         # confirms normal but I don't trust well
summary(himantormiaTemp01)                                                     # no sig relationship (0.96)



## 2. stereocaulon

#### i. light, temp, water with interaction
stereocaulonMod00 <- lm(aCorrected ~ parweatherstation * tempWS * weight, 
                        data = stereocaulonDf)

plot(stereocaulonMod00)                                                        # doesn't look particularly normal
shapiro.test(resid(stereocaulonMod00))                                         # says it's normal
summary(stereocaulonMod00)                                                     # no sig effects (0.54 - 0.66)


#### ii. individual factors

##### 1. light
stereocaulonLight00 <- lm(aCorrected ~ parweatherstation, data = stereocaulonDf)

plot(stereocaulonLight00)                                                      # looks normal (but scale-location icky)
shapiro.test(resid(stereocaulonLight00))                                       # confirms normal
summary(stereocaulonLight00)                                                   # no effect (0.20)


##### 2. temperature
stereocaulonTemp00 <- lm(aCorrected ~ tempWS, data = stereocaulonDf)

plot(stereocaulonTemp00)                                                       # wonky
shapiro.test(resid(stereocaulonTemp00))                                        # not normal


##### 3. water
stereocaulonWater00 <- lm(aCorrected ~ weight, data = stereocaulonDf)

plot(stereocaulonWater00)                                                      # very not normal
shapiro.test(resid(stereocaulonWater00))                                       # says normal but I don't trust
summary(stereocaulonWater00)                                                   # positive effect (p < 0.05)


### b. boxcox transformation

#### i. adding constant to remove negative aCorrected values
range(stereocaulonDf$aCorrected)                                               # min aCorrected is -0.04

stereocaulonDf <- stereocaulonDf %>% 
  mutate(aCorrectedBox = aCorrected + 1)


#### ii. determining best transformation
stereocaulonTrans <- boxcox(lm(aCorrectedBox ~ 1, data = stereocaulonDf))

stereocaulonTrans$x[which.max(stereocaulonTrans$y)]                            # suggests log transformation


#### iii. making transformation
stereocaulonDf <- stereocaulonDf %>% 
  mutate(aCorrectedBox2 = log(aCorrectedBox) - log(1))                         # is this math right


### c. retrying lm with log transformed aCorrected

#### i. light, temp, water with interaction
stereocaulonMod01 <- lm(aCorrectedBox2 ~ parweatherstation * tempWS * weight, 
                        data = stereocaulonDf)

plot(stereocaulonMod01)                                                        # still looking rough tbh
shapiro.test(resid(stereocaulonMod01))                                         # says it's normal but I don't trust
summary(stereocaulonMod01)                                                     # no sig impacts


#### ii. individual factors

##### 1. temperature
stereocaulonTemp01 <- lm(aCorrectedBox2 ~ tempWS, data = stereocaulonDf)

plot(stereocaulonTemp01)                                                       # overall good w some funkiness
shapiro.test(resid(stereocaulonTemp01))                                        # says resid normal
summary(stereocaulonTemp01)                                                    # no sig relationship (0.77)


##### 2. water just to confirm
stereocaulonWater01 <- lm(aCorrectedBox2 ~ weight, data = stereocaulonDf)

plot(stereocaulonWater01)                                                      # somehow even worse
shapiro.test(resid(stereocaulonWater01))                                       # says normal but def not



## 3. usneaAnt 

### a. standard lm

#### i. light, temp, water with interaction
usneaAntMod00 <- lm(aCorrected ~ parweatherstation * tempWS * weight, 
                    data = usneaAntDf)

plot(usneaAntMod00)                                                            # not normal
shapiro.test(resid(usneaAntMod00))                                             # confirms not normal


#### ii. individual factors

##### 1. light
usneaAntLight00 <- lm(aCorrected ~ parweatherstation, data = usneaAntDf)

plot(usneaAntLight00)                                                          # not normal
shapiro.test(resid(usneaAntLight00))                                           # confirms not normal


##### 2. temperature
usneaAntTemp00 <- lm(aCorrected ~ tempWS, data = usneaAntDf)

plot(usneaAntTemp00)                                                           # not normal
shapiro.test(resid(usneaAntTemp00))                                            # confirms not normal


##### 3. water
usneaAntWater00 <- lm(aCorrected ~ weight, data = usneaAntDf)

plot(usneaAntWater00)                                                          # not normal
shapiro.test(resid(usneaAntWater00))                                           # confirms not normal


### b. boxcox transformation

#### i. adding constant to remove negative aCorrected values
range(usneaAntDf$aCorrected)                                                   # min aCorrected is -7.64

usneaAntDf <- usneaAntDf %>% 
  mutate(aCorrectedBox = aCorrected + 8)


#### ii. determining best transformation
usneaAntTrans <- boxcox(lm(aCorrectedBox ~ 1, data = usneaAntDf))

usneaAntTrans$x[which.max(usneaAntTrans$y)]                                    # suggests log transformation


#### iii. making transformation
usneaAntDf <- usneaAntDf %>% 
  mutate(aCorrectedBox2 = log(aCorrectedBox) - log(8))                         # is this math right


##### 1. checking if plot looks right

###### a. before transformation
ggplot(usneaAntDf, aes(x = parweatherstation, y = aCorrectedBox)) +
  geom_point() +
  ylim(0, 9) +
  geom_hline(yintercept = 8)  


###### b. transformed
ggplot(usneaAntDf, aes(x = parweatherstation, y = aCorrectedBox2)) +
  geom_point() +
  ylim(-3.5, 0.5) +
  geom_hline(yintercept = 0)                                                   # seems like its worked


### c. retrying lm with log transformed aCorrected

#### i. light, temp, water with interaction
usneaAntMod01 <- lm(aCorrectedBox2 ~ parweatherstation * tempWS * weight, 
                    data = usneaAntDf)

plot(usneaAntMod01)                                                            # still not normal
shapiro.test(resid(usneaAntMod01))                                             # confirms not normal


#### ii. individual factors

##### 1. light
usneaAntLight01 <- lm(aCorrectedBox2 ~ parweatherstation, data = usneaAntDf)

plot(usneaAntLight01)                                                          # normal, bit wonky
shapiro.test(resid(usneaAntLight01))                                           # confirms normal
summary(usneaAntLight01)                                                       # no sig relationship (0.37)


##### 2. temperature
usneaAntTemp01 <- lm(aCorrectedBox2 ~ tempWS, data = usneaAntDf)

plot(usneaAntTemp01)                                                           # normal, scale-location funky
shapiro.test(resid(usneaAntTemp01))                                            # confirms normal
summary(usneaAntTemp01)                                                        # no sig relationship (0.96)


##### 3. water
usneaAntWater01 <- lm(aCorrectedBox2 ~ weight, data = usneaAntDf)

plot(usneaAntWater01)                                                          # not normal
shapiro.test(resid(usneaAntWater01))                                           # confirms not normal


###### a. transformation for weight
usneaAntTransW <- boxcox(lm(weight ~ 1, data = usneaAntDf))

usneaAntTransW$x[which.max(usneaAntTransW$y)]                                  # suggests log transformation


###### b. trying with weight log transformation
usneaAntWater02 <- lm(aCorrectedBox2 ~ log(weight), data = usneaAntDf)

plot(usneaAntWater02)                                                          # looks much better
shapiro.test(resid(usneaAntWater02))                                           # still not normal


###### c. scaling weight
usneaAntWater03 <- lm(aCorrectedBox2 ~ scale(weight), data = usneaAntDf)

plot(usneaAntWater03)                                                          # still not normal
shapiro.test(resid(usneaAntWater03))                                           # confirms still not normal


###### d. scaling weight AND aCorrected
usneaAntWater04 <- lm(scale(aCorrected) ~ scale(weight), data = usneaAntDf)

plot(usneaAntWater04)                                                          # not normal
shapiro.test(resid(usneaAntWater04))                                           # not normal


###### e. using non-parametric?


#### iii. interaction with light and temperature with transformations
usneaAntMod02 <- lm(aCorrectedBox2 ~ parweatherstation * tempWS, data = usneaAntDf)

plot(usneaAntMod02)                                                            # looks decent
shapiro.test(resid(usneaAntMod02))                                             # not normal


##### 1. logging indpt variables
usneaAntMod03 <- lm(aCorrectedBox2 ~ log(parweatherstation) * log(tempWS), 
                                          data = usneaAntDf)

plot(usneaAntMod03)                                                            # looks decent
shapiro.test(resid(usneaAntMod03))                                             # normal (by a smidge)
summary(usneaAntMod03)                                                         # no effect



## 4. usnea aurantiaco-atra

#### i. light, temp, water with interaction
usneaAurMod00 <- lm(aCorrected ~ parweatherstation * tempWS * weight, 
                    data = usneaAurDf)

plot(usneaAurMod00)                                                            # not normal
shapiro.test(resid(usneaAurMod00))                                             # confirms not normal


#### ii. individual factors

##### 1. light
usneaAurLight00 <- lm(aCorrected ~ parweatherstation, data = usneaAurDf)

plot(usneaAurLight00)                                                          # not normal
shapiro.test(resid(usneaAurLight00))                                           # confirms not normal


##### 2. temperature
usneaAurTemp00 <- lm(aCorrected ~ tempWS, data = usneaAurDf)

plot(usneaAurTemp00)                                                           # not normal
shapiro.test(resid(usneaAurTemp00))                                            # confirms not normal


##### 3. water
usneaAurWater00 <- lm(aCorrected ~ weight, data = usneaAurDf)

plot(usneaAurWater00)                                                          # not normal
shapiro.test(resid(usneaAurWater00))                                           # confirms not normal


### b. boxcox transformation

#### i. adding constant to remove negative aCorrected values
range(usneaAurDf$aCorrected)                                                   # min aCorrected is -4.43

usneaAurDf <- usneaAurDf %>% 
  mutate(aCorrectedBox = aCorrected + 5)


#### ii. determining best transformation
usneaAurTrans <- boxcox(lm(aCorrectedBox ~ 1, data = usneaAurDf))

usneaAurTrans$x[which.max(usneaAurTrans$y)]                                    # suggests log transformation


#### iii. making transformation
usneaAurDf <- usneaAurDf %>% 
  mutate(aCorrectedBox2 = log(aCorrectedBox) - log(5))                         # is this math right


### c. retrying lm with log transformed aCorrected

#### i. light, temp, water with interaction
usneaAurMod01 <- lm(aCorrectedBox2 ~ parweatherstation * tempWS * weight, 
                    data = usneaAurDf)

plot(usneaAurMod01)                                                            # deosn't look normal
shapiro.test(resid(usneaAurMod01))                                             # says it's normal but I don't trust
summary(usneaAurMod01)


#### ii. individual factors

##### 1. light
usneaAurLight01 <- lm(aCorrectedBox2 ~ parweatherstation, data = usneaAurDf)

plot(usneaAurLight01)                                                          # doesn't look normal
shapiro.test(resid(usneaAurLight01))                                           # says resid normal
summary(usneaAurLight01)                                                       # no sig relationship (0.34)


##### 2. temperature
usneaAurTemp01 <- lm(aCorrectedBox2 ~ tempWS, data = usneaAurDf)

plot(usneaAurTemp01)                                                           # doesn't look normal
shapiro.test(resid(usneaAurTemp01))                                            # says resid normal
summary(usneaAurTemp01)                                                        # no sig relationship (0.77)


##### 3. water
usneaAurWater01 <- lm(aCorrectedBox2 ~ weight, data = usneaAurDf)

plot(usneaAurWater01)                                                          # not normal
shapiro.test(resid(usneaAurWater01))                                           # not normal


###### a. transformation for weight
usneaAurTransW <- boxcox(lm(weight ~ 1, data = usneaAurDf))

usneaAurTransW$x[which.max(usneaAurTransW$y)]                                  # suggests y^-2 transformation


###### b. trying with weight y^-2 transformation
usneaAurWater02 <- lm(aCorrectedBox2 ~ I(weight^-2), data = usneaAurDf)

plot(usneaAurWater02)                                                          # looks better
shapiro.test(resid(usneaAurWater02))                                           # still not normal


###### c. scaling weight
usneaAurWater03 <- lm(aCorrectedBox2 ~ scale(weight), data = usneaAurDf)

plot(usneaAurWater03)                                                          # still not normal
shapiro.test(resid(usneaAurWater03))                                           # confirms still not normal


###### d. scaling weight AND aCorrected
usneaAurWater04 <- lm(scale(aCorrected) ~ scale(weight), data = usneaAurDf)

plot(usneaAurWater04)                                                          # not normal
shapiro.test(resid(usneaAurWater04))                                           # not normal


###### e. using non-parametric?


#### iii. interaction with light and temperature with transformations
usneaAntMod02 <- lm(aCorrectedBox2 ~ parweatherstation * tempWS, data = usneaAntDf)

plot(usneaAntMod02)                                                            # looks decent
shapiro.test(resid(usneaAntMod02))                                             # not normal


##### 1. logging indpt variables
usneaAurMod03 <- lm(aCorrectedBox2 ~ log(parweatherstation) * log(tempWS), 
                    data = usneaAurDf)

plot(usneaAurMod03)                                                            # looks wonky still
shapiro.test(resid(usneaAurMod03))                                             # normal 
summary(usneaAurMod03)   



## 5. sanionia

#### i. light, temp, water with interaction
sanioniaMod00 <- lm(aCorrected ~ parweatherstation * tempWS * weight, 
                    data = sanioniaDf)

plot(sanioniaMod00)                                                            # not normal
shapiro.test(resid(sanioniaMod00))                                             # says normal but I think log transform
summary(sanioniaMod00)                                                         # no effects (0.58 - 0.84)


#### ii. individual factors

##### 1. light
sanioniaLight00 <- lm(aCorrected ~ parweatherstation, data = sanioniaDf)

plot(sanioniaLight00)                                                          # not too bad
shapiro.test(resid(sanioniaLight00))                                           # normal
summary(sanioniaLight00)                                                       # no effect


##### 2. temperature
sanioniaTemp00 <- lm(aCorrected ~ tempWS, data = sanioniaDf)

plot(sanioniaTemp00)                                                           # fairly normal mostly
shapiro.test(resid(sanioniaTemp00))                                            # normal
summary(sanioniaTemp00)                                                        # no effect


##### 3. water
sanioniaWater00 <- lm(aCorrected ~ log(weight), data = sanioniaDf)

plot(sanioniaWater00)                                                          # not normal (even w log transformation)
shapiro.test(resid(sanioniaWater00))                                           # says normal but def not
summary(sanioniaWater00)                                                       # positive effect but definitely not normal residuals


### b. boxcox transformation

#### i. adding constant to remove negative aCorrected values
range(sanioniaDf$aCorrected)                                                   # min aCorrected is -40.8

sanioniaDf <- sanioniaDf %>% 
  mutate(aCorrectedBox = aCorrected + 41)


#### ii. determining best transformation
sanioniaTrans <- boxcox(lm(aCorrectedBox ~ 1, data = sanioniaDf))

sanioniaTrans$x[which.max(sanioniaTrans$y)]                                    # suggests no transformation


# bayes experimentation ----
## brms intro document: vignette("brms_overview")
## brms distribution options: vignette("brms_families")
## setting up model formula: help(brmsformula)
## investigate model results in R: methods(class = "brmsfit")
## help with priors: get_prior

## 1. himantormia

### a. interaction
himantormiaBayes00 <- brms::brm(aCorrected ~ parweatherstation * tempWS * weight, 
                                data = himantormiaDf, family = gaussian(), 
                                chains = 4, iter = 5000, warmup = 1000)

pp_check(himantormiaBayes00)
plot(himantormiaBayes00)                                                       # not good
summary(himantormiaBayes00)


### b. no interaction

#### i. light
himantormiaBayesLight <- brms::brm(aCorrected ~ parweatherstation, 
                                   data = himantormiaDf, family = gaussian(), 
                                   chains = 3, iter = 3000, warmup = 1000)

pp_check(himantormiaBayesLight)
plot(himantormiaBayesLight)                                                    # looks good
summary(himantormiaBayesLight)


#### ii. temp
himantormiaBayesTemp <- brms::brm(aCorrected ~ tempWS, data = himantormiaDf, 
                                  family = gaussian(), chains = 3, iter = 3000, 
                                  warmup = 1000)

pp_check(himantormiaBayesTemp)
plot(himantormiaBayesTemp)                                                     # looks good
summary(himantormiaBayesTemp)


#### iii. water
himantormiaBayesWater <- brms::brm(aCorrected ~ weight, data = himantormiaDf, 
                                   family = gaussian(), chains = 3, iter = 3000, 
                                   warmup = 1000)

pp_check(himantormiaBayesWater)
plot(himantormiaBayesWater)
summary(himantormiaBayesWater)



## 2. stereocaulon

### a. interaction
stereocaulonBayes00 <- brms::brm(aCorrected ~ parweatherstation * tempWS * weight, 
                                 data = stereocaulonDf, family = gaussian(), 
                                 chains = 4, iter = 5000, warmup = 1000)       # lots of issues

pp_check(stereocaulonBayes00)
plot(stereocaulonBayes00)                                                      # very poor
summary(stereocaulonBayes00)


### b. no interaction

#### i. light
stereocaulonBayesLight <- brms::brm(aCorrected ~ parweatherstation, 
                                    data = stereocaulonDf, family = gaussian(), 
                                    chains = 3, iter = 3000, warmup = 1000)

pp_check(stereocaulonBayesLight)
plot(stereocaulonBayesLight)                                                   # looks good
summary(stereocaulonBayesLight)


#### ii. temp
stereocaulonBayesTemp <- brms::brm(aCorrected ~ tempWS, data = stereocaulonDf, 
                                   family = gaussian(), chains = 3, iter = 3000, 
                                   warmup = 1000)

pp_check(stereocaulonBayesTemp)
plot(stereocaulonBayesTemp)                                                    # looks good
summary(stereocaulonBayesTemp)


#### iii. water
stereocaulonBayesWater <- brms::brm(aCorrected ~ weight, data = stereocaulonDf, 
                                    family = gaussian(), chains = 3, iter = 3000, 
                                    warmup = 1000)

pp_check(stereocaulonBayesWater)
plot(stereocaulonBayesWater)                                                   # looks good
summary(stereocaulonBayesWater)



## 3. usnea antarctica

### a. interaction
usneaAntBayes00 <- brms::brm(aCorrected ~ parweatherstation * tempWS * weight, 
                             data = usneaAntDf, family = gaussian(), chains = 4, 
                             iter = 5000, warmup = 1000)                       # lots of warnings, inc chains and iter

pp_check(usneaAntBayes00)                                                     
### plot(usneaAntBayes00)                                                      # don't run this, crashes R


### b. no interactions

#### i. light
usneaAntBayesLight <- brms::brm(aCorrected ~ parweatherstation, data = usneaAntDf,
                                family = gaussian(), chains = 3, iter = 3000,
                                warmup = 1000)

pp_check(usneaAntBayesLight)                                                                                                   
plot(usneaAntBayesLight)                                                       # looks good
summary(usneaAntBayesLight)                                                    # no effect


#### ii. temperature
usneaAntBayesTemp <- brms::brm(aCorrected ~ tempWS, data = usneaAntDf,
                              family = gaussian(), chains = 3, iter = 3000,
                              warmup = 1000)

pp_check(usneaAntBayesTemp)                                                                                                    
plot(usneaAntBayesTemp)                                                       # looks good
summary(usneaAntBayesTemp)                                                    # no effect


#### iii. water
usneaAntBayesWater <- brms::brm(aCorrected ~ weight, data = usneaAntDf,
                                family = gaussian(), chains = 3, iter = 3000,
                                warmup = 1000)

pp_check(usneaAntBayesWater)                                                                                                      
plot(usneaAntBayesWater)                                                       # looks good
summary(usneaAntBayesWater)                                                    # found a positive effect



## 4. usnea aurantiaco-atra

### a. interaction
usneaAurBayes00 <- brms::brm(aCorrected ~ parweatherstation * tempWS * weight, 
                             data = usneaAurDf, family = gaussian(), 
                             chains = 4, iter = 5000, warmup = 1000)

pp_check(usneaAurBayes00)
plot(usneaAurBayes00)                                                          # could be worse
summary(usneaAurBayes00)


### b. no interaction

#### i. light
usneaAurBayesLight <- brms::brm(aCorrected ~ parweatherstation, 
                                data = usneaAurDf, family = gaussian(), 
                                chains = 3, iter = 3000, warmup = 1000)

pp_check(usneaAurBayesLight)
plot(usneaAurBayesLight)                                                       # looks good
summary(usneaAurBayesLight)


#### ii. temp
usneaAurBayesTemp <- brms::brm(aCorrected ~ tempWS, data = usneaAurDf, 
                               family = gaussian(), chains = 3, iter = 3000, 
                               warmup = 1000)

pp_check(usneaAurBayesTemp)
plot(usneaAurBayesTemp)                                                        # looks good
summary(usneaAurBayesTemp)


#### iii. water
usneaAurBayesWater <- brms::brm(aCorrected ~ weight, data = usneaAurDf, 
                                family = gaussian(), chains = 3, iter = 3000, 
                                warmup = 1000)

pp_check(usneaAurBayesWater)
plot(usneaAurBayesWater)                                                       # looks good
summary(usneaAurBayesWater)



## 5. andreaea

### a. interaction
andreaeaBayes00 <- brms::brm(aCorrected ~ parweatherstation * tempWS * weight, 
                            data = andreaeaDf, family = gaussian(), chains = 4, 
                            iter = 5000, warmup = 1000)                        # lots of warnings, inc chains and iter

pp_check(andreaeaBayes00)                                                      
plot(andreaeaBayes00)                                                          # definitely wonky


### b. no interaction

#### i. light
andreaeaBayesLight <- brms::brm(aCorrected ~ parweatherstation, 
                                data = andreaeaDf, family = gaussian(), 
                                chains = 3, iter = 3000, warmup = 1000)

pp_check(andreaeaBayesLight)
plot(andreaeaBayesLight)                                                       # looks good                                                              
summary(andreaeaBayesLight)


#### ii. temperature
andreaeaBayesTemp <- brms::brm(aCorrected ~ tempWS, data = andreaeaDf, 
                               family = gaussian(), chains = 3, iter = 3000, 
                               warmup = 1000)

pp_check(andreaeaBayesTemp)
plot(andreaeaBayesTemp)                                                       # looks good                                                              
summary(andreaeaBayesTemp)


#### iii. water
andreaeaBayesWater <- brms::brm(aCorrected ~ weight, data = andreaeaDf, 
                               family = gaussian(), chains = 3, iter = 3000, 
                               warmup = 1000)

pp_check(andreaeaBayesWater)
plot(andreaeaBayesWater)                                                       # looks good                                                              
summary(andreaeaBayesWater)


#### 1. plot
(andreaeaDf %>% 
    add_predicted_draws(andreaeaBayesWater) %>%                                # add model prediction
    ggplot(aes(x = weight, y = aCorrected)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),
                    alpha = 0.5, color = "black") +                            # add confidence intervals
    geom_point(data = andreaeaDf) +                                            # add raw data points
    scale_fill_brewer(palette = "Greys") +
    labs(x = "\nweight", y = "a_corrected\n") +
    theme_classic())

## 6. sanionia

### a. interaction
sanioniaBayes00 <- brms::brm(aCorrected ~ parweatherstation * tempWS * weight, 
                             data = sanioniaDf, family = gaussian(), chains = 4, 
                             iter = 5000, warmup = 1000)                       # lots of warnings, inc chains and iter

pp_check(sanioniaBayes00)                                                      
plot(sanioniaBayes00)                                                          # not god awful

# *** here ****
### b. no interaction

#### i. light
sanioniaBayesLight <- brms::brm(aCorrected ~ parweatherstation, 
                                data = sanioniaDf, family = gaussian(), 
                                chains = 3, iter = 3000, warmup = 1000)

pp_check(sanioniaBayesLight)
plot(sanioniaBayesLight)                                                       # looks ok?                                                              
summary(sanioniaBayesLight)                                                    # no effect


#### ii. temperature
sanioniaBayesTemp <- brms::brm(aCorrected ~ tempWS, data = sanioniaDf, 
                               family = gaussian(), chains = 3, iter = 3000, 
                               warmup = 1000)

pp_check(sanioniaBayesTemp)
plot(sanioniaBayesTemp)                                                        # looks good                                                              
summary(sanioniaBayesTemp)                                                     # no effect


#### iii. water
sanioniaBayesWater <- brms::brm(aCorrected ~ weight, data = sanioniaDf, 
                                family = gaussian(), chains = 3, iter = 3000, 
                                warmup = 1000)

pp_check(sanioniaBayesWater)
plot(sanioniaBayesWater)                                                       # looks good                                                              
summary(sanioniaBayesWater)                                                    # positive effect