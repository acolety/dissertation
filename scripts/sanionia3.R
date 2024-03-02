# sanionia sample 3 lab data

# to do: weight not binding bc of different lengths

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
colnames(sanioniaLab) <- make.names(colnames(sanioniaLab),unique = TRUE)

colnames(sanioniaLab) <- tolower(colnames(sanioniaLab))

view(sanioniaLab)

sanioniaLab <- sanioniaLab %>% 
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
         object = as.factor(object), 
         inside.fan = as.factor(inside.fan))

sanioniaLab <- bind_cols(sanioniaLab, weights$weight) %>%                            # adding weights
  rename(weight = ...41)

skim(sanioniaLab)


## making light curve and water curve datasets

water <- sanionia %>% 
  slice(1:66)

light <- sanionia %>% 
  slice(67:96)

photosynthesis <- water %>% 
  filter(dco2mp <0)

respiration <- water %>% 
  filter(dco2mp > 0)


ggplot(light, aes(x = partop, y = dco2mp)) +
  geom_point() +
  theme_classic()

hist(photosynthesis$dco2mp)                                                    # normal

# visualizing ----
ggplot(water, aes(x = weight, y = dco2mp)) +
  geom_point() +
  theme_classic()

# basic models  ----

## this sucks and doesn't work idk
photoBayes00 <- brms::brm(bf(dco2mp ~ (b0(weight^2) + b1(weight) + b2), nl = TRUE, b0 + b1 + b2 ~ 1), data = photosynthesis)


brm(bf(y ~ b1 * exp(b2 * x), b1 + b2 ~ 1, nl = TRUE), data = dat1)



pp_check(photoBayes00)
plot(photoBayes00)
summary(photoBayes00)

(photosynthesis %>% 
  add_predicted_draws(photoBayes00) %>% 
  ggplot(aes(x = weight, y = dco2mp)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),
                  alpha = 0.5, color = "black") +
  geom_point(data = photosynthesis) +
  scale_fill_brewer(palette = "Greys") +
  theme_classic())
