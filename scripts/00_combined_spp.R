# exploring field data
# productivity with light, temp, and water

# MOSSES: Andreaea, chorisodontium, polytrichum, sanionia
# LICHENS: himantormia, stereocaulon, usnea ant, usnea aur

### TO DO: data tranformations (could try scaling), collinearity troubleshooting, determine best family for bayes,
### Mann Whitney U for non-normal data?

### approaches to data transformations:
### scaling, adding 60 to all a_corrected values (min val is -59.19) and log transform
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
ggplot(all00, aes(x = parweatherstation, y = aCorrected, color = species)) +
  facet_wrap(~ species, nrow = 4, ncol = 2, scales = "free") +
  geom_point(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(show.legend = FALSE) +
  theme_classic()


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

## 1. usneaAnt 

### a. standard lm

#### i. L, T, W with interaction
usneaAntMod00 <- lm(aCorrected ~ parweatherstation * tempWS * weight, 
                    data = usneaAntDf)

plot(usneaAntMod00)                                                            # not normal
shapiro.test(resid(usneaAntMod00))                                             # confirms not normal


#### ii. individual factors

##### 1. light
usneaAntLight <- lm(aCorrected ~ parweatherstation, data = usneaAntDf)

plot(usneaAntLight)                                                            # not normal
shapiro.test(resid(usneaAntLight))                                             # confirms not normal


##### 2. temperature
usneaAntTemp <- lm(aCorrected ~ tempWS, data = usneaAntDf)

plot(usneaAntTemp)                                                             # not normal
shapiro.test(resid(usneaAntTemp))                                              # confirms not normal


##### 3. water
usneaAntWater <- lm(aCorrected ~ weight, data = usneaAntDf)

plot(usneaAntWater)                                                            # not normal
shapiro.test(resid(usneaAntWater))                                             # confirms not normal


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












# YOU ARE HERE

usnea_ant_model_00 <- lm(a_corr_8_box ~ parweatherstation, data = usnea_antarctica_02)

plot(usnea_ant_model_00)
shapiro.test(resid(usnea_ant_model_00))  # data are normal! (by a slim margin)
summary(usnea_ant_model_00)  # no impact of light on u. antarctica C assimilation


### what about including other factors?
usnea_ant_model_01 <- lm(a_corr_8_box ~ parweatherstation * temp_ws * weight, 
                         data = usnea_antarctica_02)

plot(usnea_ant_model_01)
shapiro.test(resid(usnea_ant_model_01))  # not normal :( would have to check if
                                         # parweartherstation, temp_ws, and weight are normal?
summary(usnea_ant_model_01)

hist(usnea_antarctica_df$parweatherstation)
hist(usnea_antarctica_df$temp_ws)
hist(usnea_antarctica_df$weight)

### against other var individually
usnea_ant_model_02 <- lm(a_corr_8_box ~ temp_ws,
                         data = usnea_antarctica_02)

shapiro.test(resid(usnea_ant_model_02))  # normal!
plot(usnea_ant_model_02)
summary(usnea_ant_model_02)

usnea_ant_model_03 <- lm(a_corr_8_box ~ weight, 
                         data = usnea_antarctica_02)

shapiro.test(resid(usnea_ant_model_03))  # very non-normal, will have to boxcox weight

### boxcoxing weight

usnea_ant_boxcox2 <- boxcox(lm(weight ~ 1, data = usnea_antarctica_01))
lambda_us_ant2 <- usnea_ant_boxcox2$x[which.max(usnea_ant_boxcox2$y)]
lambda_us_ant2  # suggests log transformation

### updating model
usnea_ant_model_04 <- lm(a_corr_8_box ~ log(weight), 
                         data = usnea_antarctica_02)
shapiro.test(resid(usnea_ant_model_04))  # still very non-normal
plot(usnea_ant_model_04)  # need non-parametric test

### a ~ light * temp
usnea_ant_model_05 <- lm(a_corr_8_box ~ log(parweatherstation) * log(temp_ws), 
                         data = usnea_antarctica_02)

plot(usnea_ant_model_05)
shapiro.test(resid(usnea_ant_model_05))  # not normal :( but normal if light and temp logged
summary(usnea_ant_model_05)  # all non sig

# scaling data?
usnea_antarctica_02 <- usnea_antarctica_02 %>% 
  mutate(a_scaled = scale(a_corrected), par_scaled = scale(parweatherstation),
         temp_scaled = scale(temp_ws), weight_scaled = scale(weight))
  
## modelling scaled data
ant_scale_mod1 <- lm(a_scaled ~ par_scaled * temp_scaled * weight_scaled, 
                     data = usnea_antarctica_02)

plot(ant_scale_mod1)  # sooo not normal
shapiro.test(resid(ant_scale_mod1))  # actually this indicates that it's normal enough?? 
                                     # I imagine I prob still shouldn't use it bc its
                                     # extreme residuals balancing each other out

summary(ant_scale_mod1)  # indicates only weight (wetness) has bearing on 'a'

ggplot(usnea_antarctica_02, aes(x = weight_scaled, y = a_scaled)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

### model 2
ant_scale_mod2 <- lm(a_scaled ~ parweatherstation * temp_ws * weight,
                     data = usnea_antarctica_02)

plot(ant_scale_mod2)  # just as weird as prev plot()
shapiro.test(resid(ant_scale_mod2))  # but shapiro still indicates normal
summary(ant_scale_mod2)  # all non signif


# bayes experimentation ----
## brms intro documet: vignette("brms_overview")
## brms distribution options: vignette("brms_families")
## setting up model formula: help(brmsformula)
## investigate model results in R: methods(class = "brmsfit")
## help with priors: get_prior

### usnea ant

u_ant_bayes <- brms::brm(a_corrected ~ parweatherstation * temp_ws * weight, 
                         data = usnea_antarctica_df, 
                         family = gaussian(), chains = 4, iter = 5000, 
                         warmup = 1000)

pp_check(u_ant_bayes)
plot(u_ant_bayes)


## andreaea

### model
andreaea_bayes <- brms::brm(a_corrected ~ parweatherstation, data = andreaea_df, 
                            family = gaussian(), chains = 3, iter = 3000, 
                            warmup = 1000)
summary(andreaea_bayes)
pp_check(andreaea_bayes)
plot(andreaea_bayes)

andreaea2 <- brms::brm(a_corrected ~ parweatherstation * weight * temp_ws, 
                       data = andreaea_df, family = gaussian(), chains = 3, 
                       iter = 3000, warmup = 1000)

pp_check(andreaea2)
plot(andreaea2)
summary(andreaea2)

prior_summary(andreaea2)


### plot
(model_fit <- andreaea_df %>% 
    add_predicted_draws(andreaea_bayes) %>%                          # add model prediction
    ggplot(aes(x = parweatherstation, y = a_corrected)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),
                    alpha = 0.5, color = "black") +              # add confidence intervals
    geom_point(data = andreaea_df, size = 2) +                      # add raw data points
    scale_fill_brewer(palette = "Greys") +
    labs(x = "\nparweatherstation", y = "a_corrected\n") +
    theme_classic())


## himantormia

### model
himantormia_bayes <- brms::brm(a_corrected ~ parweatherstation, data = himantormia_df, 
                            family = gaussian(), chains = 3, iter = 3000, 
                            warmup = 1000)
summary(himantormia_bayes)  # not sure if there is an effect of par
pp_check(himantormia_bayes)
plot(himantormia_bayes)


### plot
(himantormia_df %>% 
    add_predicted_draws(himantormia_bayes) %>%                          # add model prediction
    ggplot(aes(x = parweatherstation, y = a_corrected)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),
                    alpha = 0.5, color = "black") +              # add confidence intervals
    geom_point(data = himantormia_df, size = 2) +                      # add raw data points
    scale_fill_brewer(palette = "Greys") +
    labs(x = "\nparweatherstation", y = "a_corrected\n") +
    theme_classic())


## lichen group

### model
lichen_bayes <- brms::brm(a_corrected ~ parweatherstation, data = lichen_00, 
                          family = gaussian(), chains = 3, iter = 3000, 
                          warmup = 1000)
summary(lichen_bayes)
pp_check(lichen_bayes)
plot(lichen_bayes)

hist(usnea_aurantiaco_atra_df$a_corrected)

## usnea aur

### model
usnea_aur_bayes <- brms::brm(a_corrected ~ weight, 
                             data = usnea_aurantiaco_atra_df, 
                             family = gaussian(), chains = 3, iter = 3000, 
                             warmup = 1000)
pp_check(usnea_aur_bayes)
plot(usnea_aur_bayes)
summary(usnea_aur_bayes)  # there is an effect :)

### plot
(usnea_aurantiaco_atra_df %>% 
    add_predicted_draws(usnea_aur_bayes) %>%                      # add model prediction
    ggplot(aes(x = weight, y = a_corrected)) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),
                    alpha = 0.5, color = "black") +              # add confidence intervals
    geom_point(data = usnea_aurantiaco_atra_df, size = 2) +      # add raw data points
    scale_fill_brewer(palette = "Greys") +
    labs(x = "\nweight", y = "a_corrected\n") +
    theme_classic())
