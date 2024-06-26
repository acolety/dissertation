# sanionia sample 3 lab data

# to do: take average where there are multiple points?, add error to nls char and equations, 
# see if estimates are sig diff?

# LIBRARIES ----
library(tidyverse)
library(dplyr)
library(skimr)
library(plot3Drgl)
library(broom)
library(nlstools)
library(stats)
library(aomisc)
library(rootSolve)
library(scatterplot3d)
library(patchwork)

# FUNCTIONS AND DESIGN ----
## theme for plots
theme_cust <- function(){            
  theme_classic() +                          
    theme(axis.ticks.length = unit(-0.2, "cm"),
          axis.text.x = element_text(size = 18),       
          axis.text.y = element_text(size =18),
          axis.title = element_text(size = 24),
          panel.grid = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 18),
          legend.position = c(0.12, 0.9),
          legend.margin = margin(6, 6, 6, 6),
          legend.box.background = element_rect(size = 1.1))
}

## color palette

temp.palette <- c("#084c61", "#1fbbcc", "#ffbe38", "#db3a34", "#323031")
names(temp.palette) <- levels(sanioniaLab$tempCat)


# LOADING DATA ----
sanioniaLab <- read.csv("data\\lab\\sanioniaLab.csv") %>% 
  filter(Object == 1)                                                          # removing rows of cuvette calibration


# CLEANING ----
colnames(sanioniaLab) <- make.names(colnames(sanioniaLab), unique = TRUE)      # making column names usable

colnames(sanioniaLab) <- tolower(colnames(sanioniaLab))                        # making column names lowercase

sanioniaLab <- sanioniaLab %>% 
  mutate(area = as.numeric(area), co2abs = as.numeric(co2abs),                 # updating variable types
         co2buf = as.numeric(co2buf), dco2zp = as.numeric(dco2zp), 
         dco2mp = as.numeric(dco2mp), h2oabs = as.numeric(h2oabs), 
         h2obuf = as.numeric(h2obuf),dh2ozp = as.numeric(dh2ozp), 
         dh2omp = as.numeric(dh2omp),flow = as.numeric(flow), 
         pamb = as.numeric(pamb), tcuv = as.numeric(tcuv), 
         tleaf = as.numeric(tleaf), tamb = as.numeric(tamb), 
         tmin.max = as.numeric(tmin.max), partop = as.numeric(partop), 
         paramb = as.numeric(paramb), rh = as.numeric(rh), e = as.numeric(e),
         vpd = as.numeric(vpd), gh2o = as.numeric(gh2o), a = as.numeric(a), 
         ci = as.numeric(ci), ca = as.numeric(ca), wa = as.numeric(wa), 
         object = as.factor(object), weight = as.numeric(weight),
         inside.fan = as.factor(inside.fan))

skim(sanioniaLab)                                                             # checking updated variable types


## creating factors to filter by

### temperature category
sanioniaLab <- sanioniaLab %>% 
  mutate(tempCat = case_when(tcuv < 6 ~ 5,
                             tcuv > 9 & tcuv < 11 ~ 10,
                             tcuv > 14 & tcuv < 16 ~ 15, 
                             tcuv > 19 & tcuv < 21 ~ 20,
                             tcuv > 21 ~ 25)) %>% 
  mutate(tempCat = as.factor(tempCat))

### PAR categories
sanioniaLab <- sanioniaLab %>% 
  mutate(parCat = case_when(partop < 5 ~ 0,
                            partop >= 5 & partop < 75~ 50,
                            partop >= 75 & partop < 150 ~ 100,
                            partop >= 150 & partop < 300 ~ 200,
                            partop >= 300 & partop < 490 ~ 400, 
                            partop >= 490 & partop < 560 ~ 550,
                            partop >= 700 & partop < 900 ~ 800,
                            partop > 1100 ~ 1200)) %>% 
  mutate(parCat = as.factor(parCat))

## adding dry weights and diff measures of water content
sanioniaLab <- sanioniaLab %>%
  mutate(basketWeight = 5.629) %>% 
  mutate(sampleWeight = weight - basketWeight) %>% 
  mutate(dryWeight = 25.285,
         saturatedWeight = 65.361) %>% 
  mutate(waterContent = ((sampleWeight - dryWeight) / dryWeight) * 100,
         waterContentmm = ((sampleWeight - dryWeight) * 1000 / (area * 100)),
         relativeWC = sampleWeight / saturatedWeight)



# DATA EXPLORATION ----

## distribution of response variable
hist(sanioniaLab$a)                                                            # normal

## moisture curves subset and raw data vis
WC <- sanioniaLab %>% 
  filter(parCat %in% c(0, 550) & !tempCat == 25)

WCphotosynth <- WC %>% 
  filter(partop > 5)

WCresp <- sanioniaLab %>% 
  filter(partop < 5 & !tempCat == 25)

ggplot(WC, aes(x = waterContent, y = a, color = tempCat, shape = parCat)) +
  geom_point() +
  facet_wrap(~tempCat, nrow = 2, ncol = 2, scales = "fixed") +
  geom_smooth() +
  theme_classic()

ggplot(WC, aes(x = waterContent, y = a, color = tempCat, shape = parCat)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = F) +
  theme_classic()


### light curve subset and raw data vis
LC <- sanioniaLab %>% 
  filter(between(waterContent, 93, 138) & !tempCat == 25)                       

ggplot(LC, aes(x = partop, y = a, color = tempCat)) +   
  geom_point() +
  facet_wrap(LC$tempCat, scale = "fixed") +
  geom_smooth() +                                                             
  theme_classic()



### temperature curve subset and raw data vis
TC <- sanioniaLab %>%                                                          # temperature:par interaction data
  filter(between(waterContent, 93, 138) & !tempCat == 25)

# TCW <- sanioniaLab %>%                                                         # temperature:WC interaction data
#   filter(!tempCat == 25) %>% 
#   filter(parCat == 1200 & tempCat %in% c(5, 10, 15) |                          # nvm not worth it only 4 points
#          parCat == 800 & tempCat == 20)


ggplot(TC, aes(x = tcuv, y = a, color = parCat)) +   
  geom_point() +
  # geom_smooth(se = F) +                                                       # messed up                                                           
  theme_classic()


# MODELLING  ----

### light curve
#### regressions combined 

LCRegressions <- LC %>% 
  filter(tempCat != 25) %>% 
  group_by(tempCat) %>% 
  do(tidy(nls(a ~ SSasymp(partop, Asym, R0, lrc), data = .))) 

View(LCRegressions)

## have to make indv dataframes anyway :/
LC5dat <- LC %>% 
  filter(tempCat == 5)

LC10dat <- LC %>% 
  filter(tempCat == 10)

LC15dat <- LC %>% 
  filter(tempCat == 15)

LC20dat <- LC %>% 
  filter(tempCat == 20)

#### individual models to assess
LC5 <- LC %>% 
  filter(tempCat == 5) %>% 
  nls(a ~ SSasymp(partop, Asym, R0, lrc), data = .)
summary(LC5)
plot(nlsResiduals(LC5))
overview(LC5)                                                                 # find a way to assess goodness of fit



##### pseudo R2
1 - ((sum(residuals(LC5)^2)) / (sum((LC5dat$a - mean(LC5dat$a))^2)))


LC10 <-  LC %>% 
  filter(tempCat == 10) %>% 
  nls(a ~ SSasymp(partop, Asym, R0, lrc), data = .)
summary(LC10)
plot(nlsResiduals(LC10))
overview(LC10)


##### pseudo R2
1 - ((sum(residuals(LC10)^2)) / (sum((LC10dat$a - mean(LC10dat$a))^2)))


LC15 <- LC %>% 
  filter(tempCat == 15) %>% 
  nls(a ~ SSasymp(partop, Asym, R0, lrc), data = .)
summary(LC15)
plot(nlsResiduals(LC15))
overview(LC15)

##### pseudo R2
1 - ((sum(residuals(LC15)^2)) / (sum((LC15dat$a - mean(LC15dat$a))^2)))


LC20 <-  LC %>% 
  filter(tempCat == 20) %>% 
  nls(a ~ SSasymp(partop, Asym, R0, lrc), data = .)
summary(LC20)
plot(nlsResiduals(LC20))
overview(LC20)

##### pseudo R2
1 - ((sum(residuals(LC20)^2)) / (sum((LC20dat$a - mean(LC20dat$a))^2)))


## solving for light compensation points

LC.5 <- function(x) {
  LCRegressions$estimate[1] + (LCRegressions$estimate[2] - LCRegressions$estimate[1]) * exp(-exp(LCRegressions$estimate[3]) * x)
}

uniroot(LC.5, interval = c(0, 500))

LC.10 <- function(x) {
            LCRegressions$estimate[4] + (LCRegressions$estimate[5] - LCRegressions$estimate[4]) * exp(-exp(LCRegressions$estimate[6]) * x)
}

uniroot(LC.10, interval = c(0, 500))

LC.15 <- function(x) {
            LCRegressions$estimate[7] + (LCRegressions$estimate[8] - LCRegressions$estimate[7]) * exp(-exp(LCRegressions$estimate[9]) * x)
}

uniroot(LC.15, interval = c(0, 500))

LC.20 <- function(x) {
            LCRegressions$estimate[10] + (LCRegressions$estimate[11] - LCRegressions$estimate[10]) * exp(-exp(LCRegressions$estimate[12]) * x)
}

uniroot(LC.20, interval = c(0, 500))

## light saturation points
LSP.5 <- function(x) {
  (LCRegressions$estimate[1] + (LCRegressions$estimate[2] - LCRegressions$estimate[1]) * exp(-exp(LCRegressions$estimate[3]) * x)) - (0.9 * LCRegressions$estimate[1])
}

uniroot(LSP.5, interval = c(0, 1250))

LSP.10 <- function(x) {
  (LCRegressions$estimate[4] + (LCRegressions$estimate[5] - LCRegressions$estimate[4]) * exp(-exp(LCRegressions$estimate[6]) * x)) - (0.9 * LCRegressions$estimate[4])
}

uniroot(LSP.10, interval = c(0, 1250))

LSP.15 <- function(x) {
  (LCRegressions$estimate[7] + (LCRegressions$estimate[8] - LCRegressions$estimate[7]) * exp(-exp(LCRegressions$estimate[9]) * x)) - (0.9 * LCRegressions$estimate[7])
}

uniroot(LSP.15, interval = c(0, 1250))

LSP.20 <- function(x) {
  (LCRegressions$estimate[10] + (LCRegressions$estimate[11] - LCRegressions$estimate[10]) * exp(-exp(LCRegressions$estimate[12]) * x)) - (0.9 * LCRegressions$estimate[10])
}

uniroot(LSP.20, interval = c(0, 1250))



### Water photosynthesis
WCPRegressions <- WCphotosynth %>% 
  filter(tempCat != 25) %>% 
  group_by(tempCat) %>% 
  do(tidy(lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .))) 


#### not sure if doing it in the lm is okay or should figure out how to do it in nls to match Nakatsubo function
WC5 <-  WCphotosynth %>% 
  filter(tempCat == 5)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)
plot(WC5)
summary(WC5)

WC10 <-  WCphotosynth %>% 
  filter(tempCat == 10)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)
plot(WC10)
summary(WC10)
View(WC10)

WC15 <-  WCphotosynth %>% 
  filter(tempCat == 15)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)
plot(WC15)
summary(WC15)

WC20 <-  WCphotosynth %>% 
  filter(tempCat == 20)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)
plot(WC20)
summary(WC20)


### finding WCopt for each temperature
optimize(function(x)                                                           # t = 5
  WCPRegressions$estimate[1] + WCPRegressions$estimate[2] * x 
 + WCPRegressions$estimate[3] * x^2, c(80, 160), maximum = T)                  # 108.74, 2.447

uniroot.all(function(x)                                                       
  (WCPRegressions$estimate[1] + WCPRegressions$estimate[2] * x 
  + WCPRegressions$estimate[3] * x^2) - (0.9 * 2.447388), c(0, 200))          # 92.02337 125.44982


optimize(function(x)                                                           # t = 10
  WCPRegressions$estimate[4] + WCPRegressions$estimate[5] * x 
  + WCPRegressions$estimate[6] * x^2, c(80, 160), maximum = T)                 # 118.693, 2.940 

uniroot.all(function(x)                                                       
  (WCPRegressions$estimate[4] + WCPRegressions$estimate[5] * x 
   + WCPRegressions$estimate[6] * x^2) - (0.9 * 2.939508), c(0, 200))         # 99.65919 137.72717


optimize(function(x)                                                           # t = 15
  WCPRegressions$estimate[7] + WCPRegressions$estimate[8] * x 
  + WCPRegressions$estimate[9] * x^2, c(80, 160), maximum = T)                 #  110.781, 2.322

uniroot.all(function(x)                                                       
  (WCPRegressions$estimate[7] + WCPRegressions$estimate[8] * x 
   + WCPRegressions$estimate[9] * x^2) - (0.9 * 2.322397), c(0, 200))         # 94.68398 126.87784


optimize(function(x)                                                           # t = 20
  WCPRegressions$estimate[10] + WCPRegressions$estimate[11] * x 
  + WCPRegressions$estimate[12] * x^2, c(80, 160), maximum = T)                # 107.741, 2.131

uniroot.all(function(x)                                                       
  (WCPRegressions$estimate[10] + WCPRegressions$estimate[11] * x 
   + WCPRegressions$estimate[12] * x^2) - (0.9 * 2.131495), c(0, 200))         # 92.76788 122.71342



### finding point below which is water limited
uniroot(function(x) WCPRegressions$estimate[1] + WCPRegressions$estimate[2] * x  # t = 5, 55.88 
          + WCPRegressions$estimate[3] * x^2, interval = c(0, 100))

uniroot(function(x)                                                           # t = 10, 58.50
  WCPRegressions$estimate[4] + WCPRegressions$estimate[5] * x 
  + WCPRegressions$estimate[6] * x^2, interval = c(0, 100))

uniroot(function(x)                                                           # t = 15, 59.88
  WCPRegressions$estimate[7] + WCPRegressions$estimate[8] * x 
  + WCPRegressions$estimate[9] * x^2, interval = c(0, 100))

uniroot(function(x)                                                           # t = 20, 60.39
  WCPRegressions$estimate[10] + WCPRegressions$estimate[11] * x 
  + WCPRegressions$estimate[12] * x^2, interval = c(0, 100))

### WC respiration
WCresp <- sanioniaLab %>% 
  filter(partop < 5, !tempCat == 25) 

WCresp %>% 
  filter(between(waterContent, 90, 130)) %>% 
  ggplot(., aes(x = tempCat, y = a, color = waterContent)) +
    geom_point() +
    theme_classic()

WCresp2 <- lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = WCresp)
summary(WCresp2)

ggplot(WCresp, aes(x = tcuv, y = a)) +
  geom_point() +
  geom_function(fun = function(x)                                              # t = 5
    0.597146 + -0.225992 * x 
    + 0.004794 * x^2, color = "red") +
  theme_classic()



WCresp %>% 
  ggplot(., aes(x = waterContent, y = a, color = tempCat)) +
  geom_point() +
  geom_smooth() +
  theme_classic()

WCRregressions <- WCresp %>% 
  group_by(tempCat) %>% 
  do(tidy(lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .))) 



WCR5 <-  WCresp %>% 
  filter(tempCat == 5)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)
plot(WCR5)
summary(WCR5)

WCR10 <-  WCresp %>% 
  filter(tempCat == 10)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)
plot(WCR10)
summary(WCR10)

WCR15 <-  WCresp %>% 
  filter(tempCat == 15)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)
plot(WCR15)
summary(WCR15)

WCR20 <-  WCresp %>% 
  filter(tempCat == 20)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)
plot(WCR20)
summary(WCR20)


## temperature and PAR photosynthesis at WCopt
### WCopts: 110 +/- 20%
### PARopts: t = 5(1200), t = 10(1200), t = 15(1200), t = 20(800)


TCRegressions <- TC %>% 
  group_by(parCat) %>% 
  do(tidy(lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .))) 

TC0 <-  TC %>% 
  filter(parCat == 0)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)
plot(TC0)
summary(TC0)

#### determining Q10 (ratio of R from 15:5degC)

TCRegressions$estimate[1] + TCRegressions$estimate[2] * 5 + TCRegressions$estimate[3] * 5^2
TCRegressions$estimate[1] + TCRegressions$estimate[2] * 20 + TCRegressions$estimate[3] * 20^2

((TCRegressions$estimate[1] + TCRegressions$estimate[2] * 20 + TCRegressions$estimate[3] * 20^2) / (TCRegressions$estimate[1] + TCRegressions$estimate[2] * 5 + TCRegressions$estimate[3] * 5^2))^(10 / (20-5))

TC50 <-  TC %>% 
  filter(parCat == 50)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)
plot(TC50)
summary(TC50)

TC100 <-  TC %>% 
  filter(parCat == 100)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)
plot(TC100)
summary(TC100)

TC200 <-  TC %>% 
  filter(parCat == 200)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)
plot(TC200)
summary(TC200)

TC400 <-  TC %>% 
  filter(parCat == 400)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)
plot(TC400)
summary(TC400)

TC550 <-  TC %>% 
  filter(parCat == 550)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)
plot(TC550)
summary(TC550)

TC800 <-  TC %>% 
  filter(parCat == 800)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)
plot(TC800)
summary(TC800)

TC1200 <-  TC %>% 
  filter(parCat == 800)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)
plot(TC800)
summary(TC800)


(tempParPlot00 <- ggplot(TC, aes(x = tcuv, y = a, color = parCat)) +
                  scale_color_manual(values = c("#084c61", "black", "#ffbe38", "cornsilk3", "#1fbbcc", "orangered", "darkorchid4", "chocolate4")) +
                  geom_jitter(width = 0.5, height = 0, cex = 2, pch = 1) +
                  labs(y = expression("Net photosynthetic rate ("*mu*~mol~m^-2~s^-1*")"),
                       x = "Temperature (°C)",
                       color = "PPFD") +
                  geom_function(fun = function(x)                                              # par = 0
                    TCRegressions$estimate[1] + TCRegressions$estimate[2] * x 
                    + TCRegressions$estimate[3] * x^2, color = "#084c61") +
                  geom_function(fun = function(x)                                              # par = 50
                    TCRegressions$estimate[4] + TCRegressions$estimate[5] * x 
                    + TCRegressions$estimate[6] * x^2, color = "black") +
                  geom_function(fun = function(x)                                              # par = 100
                    TCRegressions$estimate[7] + TCRegressions$estimate[8] * x 
                    + TCRegressions$estimate[9] * x^2, color = "#ffbe38") +
                  geom_function(fun = function(x)                                              # par = 200
                    TCRegressions$estimate[10] + TCRegressions$estimate[11] * x 
                    + TCRegressions$estimate[12] * x^2, color = "cornsilk3") +
                  geom_function(fun = function(x)                                              # par = 400
                    TCRegressions$estimate[13] + TCRegressions$estimate[14] * x 
                    + TCRegressions$estimate[15] * x^2, color = "#1fbbcc") +
                  geom_function(fun = function(x)                                              # par = 550
                    TCRegressions$estimate[16] + TCRegressions$estimate[17] * x 
                    + TCRegressions$estimate[18] * x^2, color = "orangered") +
                  geom_function(fun = function(x)                                              # par = 800
                    TCRegressions$estimate[19] + TCRegressions$estimate[20] * x 
                    + TCRegressions$estimate[21] * x^2, color = "darkorchid4") +
                  geom_function(fun = function(x)                                              # par = 1200
                    TCRegressions$estimate[22] + TCRegressions$estimate[23] * x 
                    + TCRegressions$estimate[24] * x^2, color = "chocolate4") +
                  geom_hline(yintercept = 0, lty = "dashed", size = 1, alpha = 0.5) +
                  theme_cust() +
                  theme(legend.position = c(0.12, 0.15)))

ggsave("img\\par_cat_temp.png", plot = tempParPlot00, width = 8, height = 10)

# VISUALIZATION ----

## NP ~ PPFD and T

## TO DO: troubleshoot error bars, exclude error bars and just use R2 or pseudo r2?,
## check font sizes okay, should 0 be right at 0? makes it hard to see first points
## move numbers slightly further away from axes, or maybe just have all raw data points on and no error bars

### creating sub dataset
PARavg <- LC %>% 
  group_by(parCat, tempCat) %>% 
  mutate(regressionA = mean(a)) %>% 
  mutate(stdev = sd(a)) %>% 
  ungroup() %>% 
  dplyr::select(parCat, tempCat, regressionA, stdev) %>% 
  mutate(parCat = as.character(parCat)) %>% 
  mutate(partop = as.numeric(parCat)) %>% 
  distinct(partop, tempCat, regressionA, stdev)

(parTempPlot <- ggplot(LC, aes(x = partop, y = a, color = tempCat)) +
    scale_color_manual(values = temp.palette) +
    ylim(-3, 4) +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.4) +
    xlab(expression("PPFD ("*mu*mol~m^-2~s^-1*")")) + 
    ylab(expression("Net photosynthetic rate ("*mu*mol~m^-2~s^-1*")"))  +
    labs(color = "Temperature (°C)") +
    geom_function(fun = function(x) 
      LCRegressions$estimate[1] + (LCRegressions$estimate[2] - LCRegressions$estimate[1]) 
      * exp(-exp(LCRegressions$estimate[3]) * x), color = "#084c61", size = 0.8) +
    geom_function(fun = function(x) 
      LCRegressions$estimate[4] + (LCRegressions$estimate[5] - LCRegressions$estimate[4]) 
      * exp(-exp(LCRegressions$estimate[6]) * x), color = "#1fbbcc", size = 0.8) +
    geom_function(fun = function(x) 
      LCRegressions$estimate[7] + (LCRegressions$estimate[8] - LCRegressions$estimate[7]) 
      * exp(-exp(LCRegressions$estimate[9]) * x), color = "#ffbe38", size = 0.8) +
    geom_function(fun = function(x) 
      LCRegressions$estimate[10] + (LCRegressions$estimate[11] - LCRegressions$estimate[10]) 
      * exp(-exp(LCRegressions$estimate[12]) * x), color = "#db3a34", size = 0.8) +
    geom_point(pch = 1, cex = 3) +
    theme_cust() +
    theme(legend.position = c(0.14, 0.9)))

ggsave("img\\partop_temp.png", plot = parTempPlot, width = 11, height = 8)



## NP ~ water content and temp

## TO DO: check font sizes okay, move axis numbers further from axes, point shapes ok?
## weird appearance in legend but not the end of the world, r2 values?

(waterTempPlot <- ggplot(WCphotosynth, aes(x = waterContent, y = a, color = tempCat)) +
    scale_color_manual(values = temp.palette) +
    xlab("Water content (%)") + 
    ylab(expression("Net photosynthetic rate ("*mu*~mol~m^-2~s^-1*")"))  +
    labs(color = "Temperature (°C)") +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.4) +
    geom_function(fun = function(x)                                              # t = 5
      WCPRegressions$estimate[1] + WCPRegressions$estimate[2] * x 
      + WCPRegressions$estimate[3] * x^2, color = "#084c61", size = 0.8) +
    geom_function(fun = function(x)                                              # t = 10
      WCPRegressions$estimate[4] + WCPRegressions$estimate[5] * x 
      + WCPRegressions$estimate[6] * x^2, color = "#1fbbcc", size = 0.8) +
    geom_function(fun = function(x)                                              # t = 15
      WCPRegressions$estimate[7] + WCPRegressions$estimate[8] * x 
      + WCPRegressions$estimate[9] * x^2, color = "#ffbe38", size = 0.8) +
    geom_function(fun = function(x)                                              # t = 20
      WCPRegressions$estimate[10] + WCPRegressions$estimate[11] * x 
      + WCPRegressions$estimate[12] * x^2, color = "#db3a34", size = 0.8) +
    geom_function(fun = function(x)                                              # t = 5
      WCRregressions$estimate[1] + WCRregressions$estimate[2] * x 
      + WCRregressions$estimate[3] * x^2, color = "#084c61", size = 0.8) +
    geom_function(fun = function(x)                                              # t = 10
      WCRregressions$estimate[4] + WCRregressions$estimate[5] * x 
      + WCRregressions$estimate[6] * x^2, color = "#1fbbcc", size = 0.8) +
    geom_function(fun = function(x)                                              # t = 15
      WCRregressions$estimate[7] + WCRregressions$estimate[8] * x 
      + WCRregressions$estimate[9] * x^2, color = "#ffbe38", size = 0.8) +
    geom_function(fun = function(x)                                              # t = 20
      WCRregressions$estimate[10] + WCRregressions$estimate[11] * x 
      + WCRregressions$estimate[12] * x^2, color = "#db3a34", size = 0.8) +
    geom_point(pch = 1, cex = 3) +
    geom_point(data = WCresp, pch = 2, cex = 3) +
    theme_cust() +
    theme(legend.position = c(0.15, 0.9)))



ggsave("img\\water_temp.png", plot = waterTempPlot, width = 10, height = 9)



## A ~ Temp and PAR

## TO DO: add R2?
## data subset
TC550plot <- TC %>% 
  filter(parCat == 550)

TC0plot <- TC %>% 
  filter(parCat == 0)

(tempParPlot01 <- ggplot(TC550plot, aes(x = tcuv, y = a)) +
    geom_point(data = TC550plot, pch = 1) +
    geom_point(data = TC0plot, pch = 2) +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.4) +
    labs(x = "Temperature (°C)", 
         y = expression("Net photosynthetic rate ( "*mu*~mol~m^-2~s^-1*")")) +
    geom_function(fun = function(x)                                              # par = 0
      TCRegressions$estimate[1] + TCRegressions$estimate[2] * x 
      + TCRegressions$estimate[3] * x^2) +
    geom_function(fun = function(x)                                              # par = 550
      TCRegressions$estimate[16] + TCRegressions$estimate[17] * x 
      + TCRegressions$estimate[18] * x^2) +
    theme_cust())

ggsave("img\\temp_par01.png", plot = tempParPlot01, width = 8, height = 8)


## summarizing points
tempAvg <- TC %>% 
  group_by(parCat, tempCat) %>% 
  mutate(olda = a) %>% 
  mutate(a = mean(olda)) %>% 
  mutate(stdev = sd(olda)) %>% 
  ungroup() %>% 
  dplyr::select(parCat, tempCat, a, stdev) %>% 
  mutate(tcuv = as.character(tempCat)) %>% 
  mutate(tcuv = as.numeric(tcuv)) %>% 
  distinct(parCat, tcuv, a, stdev) %>% 
  filter(parCat %in% c(0, 550))


(tempParPlot02 <- ggplot(tempAvg, aes(x = tcuv, y = a, shape = parCat)) +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.4, size = 0.75) +
    labs(x = "Temperature (°C)", 
         y = expression("Net photosynthetic rate ("*mu*~mol~m^-2~s^-1*")")) +
    ylim(-3, 3) +
    geom_function(fun = function(x)                                              # par = 0
      TCRegressions$estimate[1] + TCRegressions$estimate[2] * x 
      + TCRegressions$estimate[3] * x^2) +
    geom_function(fun = function(x)                                              # par = 550
      TCRegressions$estimate[16] + TCRegressions$estimate[17] * x 
      + TCRegressions$estimate[18] * x^2) +
    geom_errorbar(ymin = tempAvg$a - tempAvg$stdev, ymax = tempAvg$a + tempAvg$stdev, width = 0.4) +
    geom_point(cex = 3.5) +
    scale_shape_manual(values = c(2, 1), name = "PPFD") +
    theme_cust() +
    theme(legend.position = c(0.10, 0.7)))

ggsave("img\\temp_par02.png", plot = tempParPlot02, width = 8, height = 8)




# comparison of light and water models for field vs lab data  ----
## creating df of info from light models
### field 
sanLC <- nls(aCorrected ~ SSasymp(partop, Asym, R0, lrc), data = LCSan)
summary(sanLC)
plot(nlsResiduals(sanLC))
overview(sanLC)

### lab (at t = 5)
LC5 <- LC %>% 
  filter(tempCat == 5) %>% 
  nls(a ~ SSasymp(partop, Asym, R0, lrc), data = .)
summary(LC5)
plot(nlsResiduals(LC5))
overview(LC5)       

Regression <- c(rep("Field data", 3), rep("Lab data", 3))
parameter <- rep(c("a", "b", "c"), 2)
estimate <- c(4.1788, 1.2881, -6.0263, 3.1397, -0.6030, -5.2671)
tlower <- c(2.376404, -2.742961, -8.795165, 2.6877742, -0.9233707, -5.7422698)
tupper <- c(5.981278, 5.319116, -3.257440, 3.5915746, -0.2826472, -4.7918849)

lightComparison <- data.frame(regression, parameter, estimate, tlower, tupper)

## visualization for light model comparison
(lightModelComparison <- ggplot(lightComparison, aes(x = parameter, y = estimate, shape = Regression)) +
  geom_point(position = position_dodge(width=0.3), cex = 2.5) +
  labs(x = "Parameter", y = "Estimated value") +
  ylim(-10, 8) +
  geom_hline(yintercept = 0, lty = "dashed", size = 1, alpha = 0.2) +
  geom_errorbar(ymin = tlower, ymax = tupper, width = 0.2, position = position_dodge(width = 0.3)) +
  theme_cust() +
  theme(legend.position = c(0.12, 0.15)))

ggsave("img\\light_mod_comparison.png", plot = lightModelComparison, width = 12, height = 7)


## creating df for water models
### field
sanWC <- lm(aCorrected ~ poly(waterContent, degree = 2, raw = TRUE), data = WCSan)
summary(sanWC)

### lab (photosynth at t = 5)
WC5 <-  WCphotosynth %>% 
  filter(tempCat == 5)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)
summary(WC5)

Regression <- c(rep("Field data", 3), rep("Lab data", 3))
parameter2 <- rep(c("a", "b", "c"), 2)
estimate2 <- c(-1.419e+01, 1.484e-01, -2.762e-04, 
               -7.9119641, 0.1905403, -0.0008762)
se <- c(3.784e+00, 3.678e-02, 8.713e-05,
        1.7454898, 0.0378941, 0.0001900)

waterComparison <- data.frame(Regression, parameter2, estimate2, se)
view(waterComparison)

## visualization for water model comparison
(waterModelComparison <- ggplot(waterComparison, aes(x = parameter2, y = estimate2, shape = Regression)) +
    geom_point(position = position_dodge(width=0.3), cex = 2.5) +
    ylim(-15, 15) +
    geom_hline(yintercept = 0, lty = "dashed", size = 1, alpha = 0.8) +
    geom_errorbar(ymin = estimate2 - se, ymax = estimate2 + se, width = 0.2, position = position_dodge(width = 0.3)) +
    theme_cust() +
    theme(legend.position = c(0.12, 0.9)))

errorbars <- waterComparison %>% 
  mutate(min = estimate2 - (2 * se),
         max = estimate2 + (2 * se))

(waterAComp <- waterComparison %>% 
    filter(parameter2 == "c") %>% 
    ggplot(., aes(x = parameter2, y = estimate2, shape = Regression)) +
      geom_point(position = position_dodge(width = 0.3), cex = 3) +
      geom_hline(yintercept = 0, lty = "dashed", size = 1, alpha = 0.2) +
      geom_errorbar(ymin = c(-0.00036333, -0.00106620), 
                    ymax = c(-0.00018907, -0.00068620), width = 0.2, 
                    position = position_dodge(width = 0.3)) +
      ylim(-0.0013, 0) +
    labs(x = NULL, y = "Estimated value") +
      theme_cust() +
      theme(legend.position = c(0.23, 0.15)))

(waterAComp2 <- waterComparison %>% 
    filter(parameter2 == "c") %>% 
    ggplot(., aes(x = parameter2, y = estimate2, shape = Regression)) +
    geom_point(position = position_dodge(width = 0.3), cex = 3) +
    geom_hline(yintercept = 0, lty = "dashed", size = 1, alpha = 0.2) +
    geom_errorbar(ymin = c(-0.00045046, -0.00125620), 
                  ymax = c(-0.00010194, -0.00049620), width = 0.2, 
                  position = position_dodge(width = 0.3)) +
    ylim(-0.0013, 0) +
    labs(x = NULL, y = NULL) +
    theme_cust() +
    theme(legend.position = "none"))

(waterBComp <- waterComparison %>% 
    filter(parameter2 == "b") %>% 
  ggplot(., aes(x = parameter2, y = estimate2, shape = Regression)) +
  geom_point(position = position_dodge(width = 0.3), cex = 3) +
  geom_errorbar(ymin = c(0.11162000, 0.15264620), 
                ymax = c(0.18518000, 0.22843440), width = 0.2, 
                position = position_dodge(width = 0.3)) +
  ylim(0.05, 0.25) +
    labs(x = "Parameter", y = NULL) +
  theme_cust() +
  theme(legend.position = "none"))

(waterBComp2 <- waterComparison %>% 
    filter(parameter2 == "b") %>% 
    ggplot(., aes(x = parameter2, y = estimate2, shape = Regression)) +
    geom_point(position = position_dodge(width = 0.3), cex = 3) +
    geom_errorbar(ymin = c(0.07484000, 0.11475210), 
                  ymax = c(0.22196000, 0.26632850), width = 0.2, 
                  position = position_dodge(width = 0.3)) +
    ylim(0.05, 0.30) +
    labs(x = "Parameter", y = NULL) +
    theme_cust() +
    theme(legend.position = "none"))

(waterCComp <- waterComparison %>% 
    filter(parameter2 == "a") %>% 
  ggplot(., aes(x = parameter2, y = estimate2, shape = Regression)) +
  geom_point(position = position_dodge(width = 0.3), cex = 3) +
  geom_errorbar(ymin = c(-17.97400000, -9.65745390), 
                ymax = c(-10.40600000, -6.16647430), width = 0.2, 
                position = position_dodge(width = 0.3)) +
    labs(x = NULL, y = NULL) +
  ylim(-20, -5) +
  theme_cust() +
  theme(legend.position = "none"))

(waterCComp2 <- waterComparison %>% 
    filter(parameter2 == "a") %>% 
    ggplot(., aes(x = parameter2, y = estimate2, shape = Regression)) +
    geom_point(position = position_dodge(width = 0.3), cex = 3) +
    geom_errorbar(ymin = c(-21.75800000, -11.40294370), 
                  ymax = c(-6.62200000, -4.42098450), width = 0.2, 
                  position = position_dodge(width = 0.3)) +
    labs(x = NULL, y = "Estimated value") +
    ylim(-24, -5) +
    theme_cust() +
    theme(legend.position = c(0.23, 0.15)))

(waterCoeff <- waterCComp2 + waterBComp2 + waterAComp2)
  

ggsave("img\\water_mod_comparison.png", plot = waterCoeff, width = 16, height = 6)
