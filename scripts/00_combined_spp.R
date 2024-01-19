# exploring field data
# productivity with light, temp, and water

# MOSSES: Andreaea, chorisodontium, polytrichum, sanionia
# LICHENS: himantormia, stereocaulon, usnea ant, usnea aur

### TO DO: data tranformations (could try scaling), determine best 
### covariates, collinearity troubleshooting

### approaches to data transformations:
### scaling, adding 60 to all a_corrected values (min val is -59.19) and log transform
### (or boxcox), cube root
### alt use kurskal wallis or bayesian

# libraries ----
library(tidyverse)
library(skimr)
library(brms)
library(tidybayes)
library(MASS)
library(corrplot)


# loading data ----
andreaea_df <- read.csv("data\\andreaea.csv") %>% 
    filter(!row_number() == 1)  # removing row with units
chorisodontium_df <- read.csv("data\\chorisodontium.csv") %>% 
  filter(!row_number() == 1)
himantormia_df <- read.csv("data\\himantormia.csv") %>% 
  filter(!row_number() == 1)
polytrichum_strictum_df <- read.csv("data\\polytrichum_strictum.csv") %>% 
  filter(!row_number() == 1)
sanionia_df <- read.csv("data\\sanionia.csv") %>% 
  filter(!row_number() == 1)
stereocaulon_df <- read.csv("data\\stereocaulon.csv") %>% 
  filter(!row_number() == 1)
usnea_antarctica_df <- read.csv("data\\usnea_antarctica.csv") %>% 
  filter(!row_number() == 1)
usnea_aurantiaco_atra_df <- read.csv("data\\usnea_aurantiaco-atra.csv") %>% 
  filter(!row_number() == 1)

# cleaning and combining data ----
## make column names lowercase
colnames(andreaea_df) <- tolower(colnames(andreaea_df))
colnames(chorisodontium_df) <- tolower(colnames(chorisodontium_df))
colnames(himantormia_df) <- tolower(colnames(himantormia_df))
colnames(polytrichum_strictum_df) <- tolower(colnames(polytrichum_strictum_df))
colnames(sanionia_df) <- tolower(colnames(sanionia_df))
colnames(stereocaulon_df) <- tolower(colnames(stereocaulon_df))
colnames(usnea_antarctica_df) <- tolower(colnames(usnea_antarctica_df))
colnames(usnea_aurantiaco_atra_df) <- tolower(colnames(usnea_aurantiaco_atra_df))

## checking column names are the same
unique(c(colnames(andreaea_df), colnames(chorisodontium_df), colnames(himantormia_df),
         colnames(polytrichum_strictum_df), colnames(sanionia_df), 
         colnames(stereocaulon_df), colnames(usnea_antarctica_df),
         colnames(usnea_aurantiaco_atra_df)))

## adding column for spp. and appropriate area, changing var types
andreaea_df <- andreaea_df %>% 
  mutate(species = "andreaea",  # adding spp row
         area = 6.9291) %>% # adding area from corrected_areas.csv
  rename(wetness_ws = wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  # update column names
         temp_ws = temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         wind_speed_ws = wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rh_ws = relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),  # updating variable types
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
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(a_corrected = (a / 8) * area)  # correcting 'a' with spp. areas

chorisodontium_df <- chorisodontium_df %>% 
  mutate(species = "chorisodontium",
         area = 107.80784) %>% 
  mutate(weight = weight..incl..water.) %>%  # changing mismatched column name
  rename(wetness_ws = wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  # update column names
         temp_ws = temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         wind_speed_ws = wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rh_ws = relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),  # updating variable types
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
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(a_corrected = (a / 8) * area)  # correcting 'a' with spp. areas

himantormia_df <- himantormia_df %>% 
  mutate(species = "himantormia",
         area = 15.83125) %>% 
  rename(wetness_ws = wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  # update column names
         temp_ws = temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         wind_speed_ws = wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rh_ws = relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),  # updating variable types
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
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(a_corrected = (a / 8) * area)  # correcting 'a' with spp. areas

polytrichum_strictum_df <- polytrichum_strictum_df %>% 
  mutate(species = "polytrichum_strictum",
         area = 52.85567) %>% 
  rename(wetness_ws = wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  # update column names
         temp_ws = temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         wind_speed_ws = wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rh_ws = relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),  # updating variable types
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
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(a_corrected = (a / 8) * area)  # correcting 'a' with spp. areas

sanionia_df <- sanionia_df %>% 
  mutate(species = "sanionia",
         area = 77.48568) %>% 
  rename(wetness_ws = wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  # update column names
         temp_ws = temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         wind_speed_ws = wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rh_ws = relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),  # updating variable types
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
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(a_corrected = (a / 8) * area)  # correcting 'a' with spp. areas

stereocaulon_df <- stereocaulon_df %>% 
  mutate(species = "stereocaulon",
         area = 5.10035) %>% 
  rename(wetness_ws = wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  # update column names
         temp_ws = temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         wind_speed_ws = wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rh_ws = relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),  # updating variable types
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
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(a_corrected = (a / 8) * area)  # correcting 'a' with spp. areas

usnea_antarctica_df <- usnea_antarctica_df %>% 
  mutate(species = "usnea_antarctica",
         area = if_else(row_number() <= 16, 74.66479, 32.15818)) %>% 
  rename(wetness_ws = wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  # update column names
         temp_ws = temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         wind_speed_ws = wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rh_ws = relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),  # updating variable types
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
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(a_corrected = (a / 8) * area)  # correcting 'a' with spp. areas

usnea_aurantiaco_atra_df <- usnea_aurantiaco_atra_df %>% 
  mutate(species = "usnea_aurantiaco_atra",
         area = 43.84498) %>% 
  rename(wetness_ws = wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station,  # update column names
         temp_ws = temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station,
         wind_speed_ws = wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station,
         rh_ws = relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station) %>% 
  mutate(co2abs = as.numeric(co2abs), co2buf = as.numeric(co2buf),  # updating variable types
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
         inside.fan = as.factor(inside.fan)) %>% 
  mutate(a_corrected = (a / 8) * area)  # correcting 'a' with spp. areas


## checking
### View(andreaea_df)
### View(chorisodontium_df)
### View(himantormia_df)
### View(polytrichum_strictum_df)
### View(sanionia_df)
### View(stereocaulon_df)
### View(usnea_antarctica_df)
### View(usnea_aurantiaco_atra_df)


## combining data
all_00 <- bind_rows(andreaea_df, chorisodontium_df, himantormia_df, polytrichum_strictum_df,
                 sanionia_df, stereocaulon_df, usnea_antarctica_df, 
                 usnea_aurantiaco_atra_df)
### View(all_00)

moss_00 <- bind_rows(andreaea_df, chorisodontium_df, polytrichum_strictum_df,
                     sanionia_df)
### View(moss_00)

lichen_00 <- bind_rows(himantormia_df, stereocaulon_df, usnea_antarctica_df,
                       usnea_aurantiaco_atra_df)
### View(lichen_00)

# raw data plots ----
## weight (wetness)
ggplot(all_00, aes(x = weight, y = a_corrected, color = species)) +
  facet_wrap(~ species, nrow = 4, ncol = 2, scales = "free") +
  geom_point(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(show.legend = FALSE, se = FALSE) +
  theme_classic()

## cuvette temp
ggplot(all_00, aes(x = tcuv, y = a_corrected, color = species)) +
  facet_wrap(~ species, nrow = 4, ncol = 2, scales = "free") +
  geom_point(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(show.legend = FALSE, se = FALSE) +
  theme_classic()

## temperature (weather station)
ggplot(all_00, aes(x = temp_ws, y = a_corrected, color = species)) +
  facet_wrap(~ species, nrow = 4, ncol = 2, scales = "free") +
  geom_point(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(show.legend = FALSE, se = FALSE) +
  theme_classic()

## light
ggplot(all_00, aes(x = parweatherstation, y = a_corrected, color = species)) +
  facet_wrap(~ species, nrow = 4, ncol = 2, scales = "free") +
  geom_point(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(show.legend = FALSE, se = FALSE) +
  theme_classic()


## checking collinearity 
pairs(all_00[, c("parweatherstation", "temp_ws", "weight")])
pairs(lichen_00[, c("parweatherstation", "temp_ws", "weight")])
pairs(moss_00[, c("parweatherstation", "temp_ws", "weight")])

ggplot(all_00, aes(x = parweatherstation, y = temp_ws)) +
  geom_point() +
  theme_classic()

all_01 <- all_00 %>% 
  dplyr::select_if(is.numeric)

corrplot(cor(all_01, use = "everything"), method = "number", type = "upper")

colinear <- lm(parweatherstation ~ temp_ws, data = all_00)

summary(colinear)


# modelling the relationships with light, temp, water ----

## looking at distributions
### light by species
ggplot(all_00, aes(x = parweatherstation)) +
  facet_wrap(~ species, nrow = 4, ncol = 2, scales = "free") +
  geom_histogram() +
  theme_classic()

## light by lichen/moss
### lichens
ggplot(lichen_00, aes(x = parweatherstation, fill = species)) +
  geom_histogram() +
  theme_classic()

### mosses
ggplot(moss_00, aes(x = parweatherstation, fill = species)) +
  geom_histogram() +
  theme_classic()

## a_corrected by spp
ggplot(all_00, aes(x = a_corrected)) +
  facet_wrap(~ species, nrow = 4, ncol = 2, scales = "free") +
  geom_histogram() +
  theme_classic()

## a_corrected by lichen/moss
### lichen
ggplot(lichen_00, aes(x = a_corrected, fill = species)) +
  geom_histogram() +
  theme_classic()

### moss
ggplot(moss_00, aes(x = a_corrected, fill = species)) +
  geom_histogram() +
  theme_classic()




## creating base linear model for lichen dataset
### null model
lichen_null_lm <- lm(a_corrected ~ 1, data = lichen_00)

### lichen model base
lichen_lm_00 <- lm(a_corrected ~ parweatherstation, data = lichen_00)  # non-sig relationship

summary(lichen_lm_00)
plot(lichen_lm_00)
shapiro.test(resid(lichen_lm_00))  # data residuals not normal distribution

### lichen mod with log transformation
lichen_lm_01 <- lm(log(a_corrected) ~ parweatherstation, data = lichen_00)  # prod NaN because of neg a values


### determining better transformation that can use neg values (and prod normal resid)
lichen_boxcox <- boxcox(lm(a_corrected ~ 1, data = lichen_00))  # can't boxcox with negative values


### example boxcox from worms project
#### m_boxcox <- boxcox(lm(soil_m_avg ~ 1, data = worms_sub))
#### lambda <- m_boxcox$x[which.max(m_boxcox$y)]
#### lambda

#### ph_boxcox <- boxcox(lm(soil_ph ~ 1, data = worms_sub))
#### lambda_ph <- ph_boxcox$x[which.max(ph_boxcox$y)]
#### lambda_ph

#### worms_sub <- worms_sub %>% 
####   mutate(boxcox_m = (1/sqrt(soil_m_avg)), 
####          boxcox_ph = (1/((soil_ph)^2)))



### trying adding constant to a_corrected for data transformations and modelling

#### adding a_corr_60
all_02 <- all_00 %>% 
  mutate(a_corr_60 = a_corrected + 60)


#### usnea ant
range(usnea_antarctica_df$a_corrected)

usnea_antarctica_01 <- usnea_antarctica_df %>% 
  mutate(a_corr_8 = a_corrected + 8)

#### boxcox transformation
usnea_ant_boxcox <- boxcox(lm(a_corr_8 ~ 1, data = usnea_antarctica_01))
lambda_us_ant <- usnea_ant_boxcox$x[which.max(usnea_ant_boxcox$y)]
lambda_us_ant  # suggests log transformation

usnea_antarctica_02 <- usnea_antarctica_01 %>% 
  mutate(a_corr_8_box = log(a_corr_8) - log(8))
      # not sure if this would be correct?

ggplot(usnea_antarctica_02, aes(x = parweatherstation, y = a_corr_8_box)) +
  geom_point() +
  geom_hline(yintercept = 0)  # checking if it seems like the right # are neg

ggplot(usnea_antarctica_01, aes(x = parweatherstation, y = a_corr_8)) +
  geom_point() +
  geom_hline(yintercept = 8)  # checking if it seems like the right # are neg

usnea_ant_model_00 <- lm(a_corr_8_box ~ parweatherstation, data = usnea_antarctica_02)

plot(usnea_ant_model_00)
shapiro.test(resid(usnea_ant_model_00))  # data are normal! (by a slim margin)
summary(usnea_ant_model_00)  # no impact of light on u. antarctica C assimilation

##### what about including other factors?
usnea_ant_model_01 <- lm(a_corr_8_box ~ log(parweatherstation) * temp_ws * weight, 
                         data = usnea_antarctica_02)

plot(usnea_ant_model_01)
shapiro.test(resid(usnea_ant_model_01))  # not normal :( would have to check if
                                         # parweartherstation, temp_ws, and weight are normal?

hist(usnea_antarctica_df$parweatherstation)
hist(usnea_antarctica_df$temp_ws)
hist(usnea_antarctica_df$weight)




# bayes experimentation ----
## brms distribution options: vignette("brms_families")
## setting up model formula: help(brmsformula)

## andreaea

### model
andreaea_bayes <- brms::brm(a_corrected ~ parweatherstation, data = andreaea_df, 
                            family = gaussian(), chains = 3, iter = 3000, 
                            warmup = 1000)
summary(andreaea_bayes)
pp_check(andreaea_bayes)
plot(andreaea_bayes)

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
