# Dissertation final script
# S. uncinata photosynthesis, lab and field conditions
# 08 April 2024


# LIBRARIES ----
library(tidyverse)
library(nlstools)
library(broom)
library(rootSolve)
library(patchwork)



# LOADING DATA ----
# field data
sanioniaField <- read.csv("data\\field\\sanionia.csv") %>% 
  filter(Code == "MP_010")

# lab data
sanioniaLab <- read.csv("data\\lab\\sanioniaLab.csv") %>% 
  filter(Object == 1)     



# FUNCTIONS AND PLOT DESIGN ----
## plot theme
theme_cust <- function(){            
  theme_classic()                                                              +                          
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



# CLEANING ----
## making column names R-friendly
colnames(sanioniaField) <- tolower(make.names(colnames(sanioniaField), 
                                              unique = TRUE))

colnames(sanioniaLab) <- tolower(make.names(colnames(sanioniaLab), 
                                            unique = TRUE))

## changing data types
### field
sanioniaField <- sanioniaField %>% 
  mutate(wetnessWS =                                                           
           as.numeric(wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station),  
         tempWS = 
           as.numeric(temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station),
         windspeedWS = 
           as.numeric(wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station),
         rhWS = 
           as.numeric(relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station)) %>% 
  mutate(area = as.numeric(area), 
         co2abs = as.numeric(co2abs), 
         co2buf = as.numeric(co2buf), 
         dco2zp = as.numeric(dco2zp), 
         dco2mp = as.numeric(dco2mp), 
         h2oabs = as.numeric(h2oabs), 
         h2obuf = as.numeric(h2obuf),
         dh2ozp = as.numeric(dh2ozp), 
         dh2omp = as.numeric(dh2omp),
         flow = as.numeric(flow), 
         pamb = as.numeric(pamb), 
         tcuv = as.numeric(tcuv),
         tleaf = as.numeric(tleaf), 
         tamb = as.numeric(tamb), 
         tmin.max = as.numeric(tmin.max), 
         partop = as.numeric(partop), 
         paramb = as.numeric(paramb), 
         rh = as.numeric(rh), 
         e = as.numeric(e),
         parweatherstation = as.numeric(parweatherstation),
         vpd = as.numeric(vpd), 
         gh2o = as.numeric(gh2o), 
         a = as.numeric(a), 
         ci = as.numeric(ci), 
         ca = as.numeric(ca), 
         wa = as.numeric(wa), 
         object = as.factor(object), 
         weight = as.numeric(weight),
         inside.fan = as.factor(inside.fan), 
         cumulativeSeconds = as.numeric(cumulative.seconds))

### lab
sanioniaLab <- sanioniaLab %>% 
  mutate(area = as.numeric(area), 
         co2abs = as.numeric(co2abs), 
         co2buf = as.numeric(co2buf), 
         dco2zp = as.numeric(dco2zp), 
         dco2mp = as.numeric(dco2mp), 
         h2oabs = as.numeric(h2oabs), 
         h2obuf = as.numeric(h2obuf),
         dh2ozp = as.numeric(dh2ozp), 
         dh2omp = as.numeric(dh2omp),
         flow = as.numeric(flow), 
         pamb = as.numeric(pamb), 
         tcuv = as.numeric(tcuv), 
         tleaf = as.numeric(tleaf), 
         tamb = as.numeric(tamb), 
         tmin.max = as.numeric(tmin.max), 
         partop = as.numeric(partop), 
         paramb = as.numeric(paramb), 
         rh = as.numeric(rh), 
         e = as.numeric(e),
         vpd = as.numeric(vpd), 
         gh2o = as.numeric(gh2o), 
         a = as.numeric(a), 
         ci = as.numeric(ci), 
         ca = as.numeric(ca), 
         wa = as.numeric(wa), 
         object = as.factor(object), 
         weight = as.numeric(weight),
         inside.fan = as.factor(inside.fan))

## adding and correcting columns, making categories
### field
sanioniaField <- sanioniaField %>% 
  mutate(area = 77.48568) %>% 
  mutate(aCorrected = (a * 0.0008) / (area / 10000)) %>% 
  mutate(dryWeight = 35.223, 
         basketWeight = 5.147,
         saturatedWeight = 104.605) %>% 
  mutate(sampleWeight = weight - basketWeight) %>% 
  mutate(waterContent = ((sampleWeight - dryWeight) / dryWeight) * 100,
         waterContentmm = ((sampleWeight - dryWeight) * 1000) / (area * 100),
         relativeWC = sampleWeight / saturatedWeight)

### lab
#### category factors
sanioniaLab <- sanioniaLab %>% 
  mutate(tempCat = case_when(tcuv < 6 ~ 5,
                             tcuv > 9 & tcuv < 11 ~ 10,
                             tcuv > 14 & tcuv < 16 ~ 15, 
                             tcuv > 19 & tcuv < 21 ~ 20,
                             tcuv > 21 ~ 25)) %>% 
  mutate(parCat = case_when(partop < 5 ~ 0,
                            partop >= 5 & partop < 75~ 50,
                            partop >= 75 & partop < 150 ~ 100,
                            partop >= 150 & partop < 300 ~ 200,
                            partop >= 300 & partop < 490 ~ 400, 
                            partop >= 490 & partop < 560 ~ 550,
                            partop >= 700 & partop < 900 ~ 800,
                            partop > 1100 ~ 1200)) %>% 
  mutate(tempCat = as.factor(tempCat)) %>% 
  mutate(parCat = as.factor(parCat))

#### weights and water contents
sanioniaLab <- sanioniaLab %>% 
  mutate(basketWeight = 5.629,
         dryWeight = 25.285,
         saturatedWeight = 65.361) %>% 
  mutate(sampleWeight = weight - basketWeight) %>% 
  mutate(waterContent = ((sampleWeight - dryWeight) / dryWeight) * 100,
         waterContentmm = ((sampleWeight - dryWeight) * 1000 / (area * 100)),
         relativeWC = sampleWeight / saturatedWeight)



# CREATING SUBSETS ----
## field
### light
LCfield <- sanioniaField %>% 
  filter(waterContent > 124,                                                   # water limitation point from field data
         between(tempWS, 3, 6))

### water content
WCfield <- sanioniaField %>% 
  filter(between(tempWS, 3, 6),
         between(partop, 500, 1700))                                           # par near lab LSP



## lab
### light
LC <- sanioniaLab %>% 
  filter(between(waterContent, 93, 138) & !tempCat == 25)                      # at WCopt

#### LC dataframe for each temperature
LC5dat <- LC %>% 
  filter(tempCat == 5)

LC10dat <- LC %>% 
  filter(tempCat == 10)

LC15dat <- LC %>% 
  filter(tempCat == 15)

LC20dat <- LC %>% 
  filter(tempCat == 20)


### water content
WC <- sanioniaLab %>% 
  filter(parCat %in% c(0, 550) & !tempCat == 25)

WCphotosynth <- WC %>% 
  filter(partop > 5)

WCresp <- sanioniaLab %>% 
  filter(partop < 5 & !tempCat == 25)


### temperature
TC <- sanioniaLab %>% 
  filter(between(waterContent, 93, 138) & !tempCat == 25)                      # at WCopt

#### temperature subsets for plotting
TC0plot <- TC %>% 
  filter(parCat == 0)

TC550plot <- TC %>% 
  filter(parCat == 550)



# RAW DATA EXPLORATION ----
## field
### checking for correlations between variables
cor.test(sanioniaField$partop, sanioniaField$tempWS) 
cor.test(sanioniaField$tempWS, sanioniaField$waterContent) 
cor.test(sanioniaField$waterContent, sanioniaField$partop) 

### distribution of response variable, a (NP)
hist(sanioniaField$aCorrected)


## lab
### distribution of response variable, a (NP)
hist(sanioniaLab$a)



# MODELING ----
## field
### light
LCfieldNls <- nls(aCorrected ~ SSasymp(partop, Asym, R0, lrc), data = LCfield)

plot(nlsResiduals(LCfieldNls))
summary(LCfieldNls)
overview(LCfieldNls)
1 - ((sum(residuals(LCfieldNls)^2)) / (sum((LCfield$aCorrected - mean(LCfield$aCorrected))^2)))  # psuedo R2


### water
WCfieldMod <- lm(aCorrected ~ poly(waterContent, degree = 2, raw = TRUE), 
                 data = WCfield)

plot(WCfieldMod)
summary(WCfieldMod)


## lab
### light
#### table of regressions for easier plotting and calculations
LCRegressions <- LC %>% 
  filter(tempCat != 25) %>% 
  group_by(tempCat) %>% 
  do(tidy(nls(a ~ SSasymp(partop, Asym, R0, lrc), data = .))) 

#### t = 5
LC5 <- nls(a ~ SSasymp(partop, Asym, R0, lrc), data = LC5dat)

plot(residuals(LC5))
summary(LC5)
overview(LC5)
1 - ((sum(residuals(LC5)^2)) / (sum((LC5dat$a - mean(LC5dat$a))^2)))           # pseudo R2

#### t = 10
LC10 <- nls(a ~ SSasymp(partop, Asym, R0, lrc), data = LC10dat)

plot(nlsResiduals(LC10))
summary(LC10)
overview(LC10)
1 - ((sum(residuals(LC10)^2)) / (sum((LC10dat$a - mean(LC10dat$a))^2)))        # pseudo R2

#### t = 15
LC15 <- nls(a ~ SSasymp(partop, Asym, R0, lrc), data = LC15dat)

plot(nlsResiduals(LC15))
summary(LC15)
overview(LC15)
1 - ((sum(residuals(LC15)^2)) / (sum((LC15dat$a - mean(LC15dat$a))^2)))        # pseudo R2

#### t = 20
LC20 <- nls(a ~ SSasymp(partop, Asym, R0, lrc), data = LC20dat)

plot(nlsResiduals(LC20))
summary(LC20)
overview(LC20)
1 - ((sum(residuals(LC20)^2)) / (sum((LC20dat$a - mean(LC20dat$a))^2)))        # pseudo R2


### water
#### table of regressions for easier plotting and calculations
##### photosynthesis
WCPRegressions <- WCphotosynth %>% 
  filter(tempCat != 25) %>% 
  group_by(tempCat) %>% 
  do(tidy(lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .))) 

##### dark respiration
WCRregressions <- WCresp %>% 
  group_by(tempCat) %>% 
  do(tidy(lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .))) 


#### photosynthesis
##### t = 5
WC5 <-  WCphotosynth %>% 
  filter(tempCat == 5)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)

plot(WC5)
summary(WC5)

##### t = 10
WC10 <-  WCphotosynth %>% 
  filter(tempCat == 10)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)

plot(WC10)
summary(WC10)

##### t = 15
WC15 <-  WCphotosynth %>% 
  filter(tempCat == 15)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)

plot(WC15)
summary(WC15)

##### t = 20
WC20 <-  WCphotosynth %>% 
  filter(tempCat == 20)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)

plot(WC20)
summary(WC20)


#### dark respiration
##### t = 5
WCR5 <-  WCresp %>% 
  filter(tempCat == 5)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)

plot(WCR5)
summary(WCR5)

##### t = 10
WCR10 <-  WCresp %>% 
  filter(tempCat == 10)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)

plot(WCR10)
summary(WCR10)

##### t = 15
WCR15 <-  WCresp %>% 
  filter(tempCat == 15)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)

plot(WCR15)
summary(WCR15)

##### t = 20
WCR20 <-  WCresp %>% 
  filter(tempCat == 20)  %>% 
  lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = .)

plot(WCR20)
summary(WCR20)


### temperature
#### table of regressions for easier plotting and calculations
TCRegressions <- TC %>% 
  group_by(parCat) %>% 
  do(tidy(lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .))) 

##### par = 0
TC0 <-  TC %>% 
  filter(parCat == 0)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)

plot(TC0)
summary(TC0)

##### par = 50
TC50 <-  TC %>% 
  filter(parCat == 50)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)

plot(TC50)
summary(TC50)

##### par = 100
TC100 <-  TC %>% 
  filter(parCat == 100)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)

plot(TC100)
summary(TC100)

##### par = 200
TC200 <-  TC %>% 
  filter(parCat == 200)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)

plot(TC200)
summary(TC200)

##### par = 400
TC400 <-  TC %>% 
  filter(parCat == 400)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)

plot(TC400)
summary(TC400)

##### par = 550
TC550 <-  TC %>% 
  filter(parCat == 550)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)

plot(TC550)
summary(TC550)

##### par = 800
TC800 <-  TC %>% 
  filter(parCat == 800)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)

plot(TC800)
summary(TC800)

##### par = 1200
TC1200 <-  TC %>% 
  filter(parCat == 800)  %>% 
  lm(a ~ poly(tcuv, degree = 2, raw = TRUE), data = .)

plot(TC1200)
summary(TC1200)


## creating dataframe for lab/field model output for coefficient comparison
### coefficients for light curves from models
Regression <- c(rep("Field data", 3), rep("Lab data", 3))
parameter <- rep(c("a", "b", "c"), 2)
estimate <- c(4.1788, 1.2881, -6.0263, 3.1397, -0.6030, -5.2671)
tlower <- c(2.376404, -2.742961, -8.795165, 2.6877742, -0.9233707, -5.7422698)
tupper <- c(5.981278, 5.319116, -3.257440, 3.5915746, -0.2826472, -4.7918849)

lightComparison <- data.frame(Regression, parameter, estimate, tlower, tupper)


### coefficients for desiccation curves from models
parameter2 <- rep(c("a", "b", "c"), 2)
estimate2 <- c(-1.419e+01, 1.484e-01, -2.762e-04, 
               -7.9119641, 0.1905403, -0.0008762)
se <- c(3.784e+00, 3.678e-02, 8.713e-05,
        1.7454898, 0.0378941, 0.0001900)

waterComparison <- data.frame(Regression, parameter2, estimate2, se)



# CALCULATED VALUES ----
## field
### light
#### light compensation point (LCP)
LCP.field <- function(x) {
  4.1788 + (1.2881 - 4.1788) * exp(-exp(-6.0263) * x)
}

print(uniroot(LCP.field, interval = c(0, 500))$root)                           # can't calculate bc model has it at PPFD < 0

#### light saturation point (LSP)
LSP.field <- function(x) {
  (4.1788 + (1.2881 - 4.1788) * exp(-exp(-6.0263) * x)) - (0.9 * 4.1788)
}

print(uniroot(LSP.field, interval = c(0, 1250))$root)


### water
#### water compensation point (WCP)
WC.field <- function(x) {
  -1.419e+01 + (1.484e-01 * x) + (-2.762e-04 * x^2)
}

print(uniroot(WC.field, interval = c(113, 200))$root)

#### optimal water content (WCopt)
optimize(function(x)                                                           
  -1.419e+01 + (1.484e-01 * x) + (-2.762e-04 * x^2), c(200, 400), maximum = T) # 268.65%, NP = 5.74   

uniroot.all(function(x)                                                        # upper and lower bounds                                                         
  (-1.419e+01 + (1.484e-01 * x) + (-2.762e-04 * x^2)) - (0.9 *5.743526), c(0, 500)) # 223.04 - 314.25%


## lab
### light
#### light compensation point (LCP)
##### t = 5
LC.5 <- function(x) {
  LCRegressions$estimate[1] + (LCRegressions$estimate[2] - LCRegressions$estimate[1]) * exp(-exp(LCRegressions$estimate[3]) * x)
}

print(uniroot(LC.5, interval = c(0, 500))$root)                                # LCP

##### t = 10
LC.10 <- function(x) {
  LCRegressions$estimate[4] + (LCRegressions$estimate[5] - LCRegressions$estimate[4]) * exp(-exp(LCRegressions$estimate[6]) * x)
}

print(uniroot(LC.10, interval = c(0, 500))$root)                               # LCP

##### t = 15
LC.15 <- function(x) {
  LCRegressions$estimate[7] + (LCRegressions$estimate[8] - LCRegressions$estimate[7]) * exp(-exp(LCRegressions$estimate[9]) * x)
}

print(uniroot(LC.15, interval = c(0, 500))$root)                               # LCP

##### t = 20
LC.20 <- function(x) {
  LCRegressions$estimate[10] + (LCRegressions$estimate[11] - LCRegressions$estimate[10]) * exp(-exp(LCRegressions$estimate[12]) * x)
}

print(uniroot(LC.20, interval = c(0, 500))$root)                               # LCP


#### light saturation point (LSP)
##### t = 5
LSP.5 <- function(x) {
  (LCRegressions$estimate[1] + (LCRegressions$estimate[2] - LCRegressions$estimate[1]) * exp(-exp(LCRegressions$estimate[3]) * x)) - (0.9 * LCRegressions$estimate[1])
}

print(uniroot(LSP.5, interval = c(0, 1250))$root)                              # LSP

##### t = 10
LSP.10 <- function(x) {
  (LCRegressions$estimate[4] + (LCRegressions$estimate[5] - LCRegressions$estimate[4]) * exp(-exp(LCRegressions$estimate[6]) * x)) - (0.9 * LCRegressions$estimate[4])
}

print(uniroot(LSP.10, interval = c(0, 1250))$root)                             # LSP

##### t = 15
LSP.15 <- function(x) {
  (LCRegressions$estimate[7] + (LCRegressions$estimate[8] - LCRegressions$estimate[7]) * exp(-exp(LCRegressions$estimate[9]) * x)) - (0.9 * LCRegressions$estimate[7])
}

print(uniroot(LSP.15, interval = c(0, 1250))$root)                             # LSP

##### t = 20
LSP.20 <- function(x) {
  (LCRegressions$estimate[10] + (LCRegressions$estimate[11] - LCRegressions$estimate[10]) * exp(-exp(LCRegressions$estimate[12]) * x)) - (0.9 * LCRegressions$estimate[10])
}

print(uniroot(LSP.20, interval = c(0, 1250))$root)                             # LSP


### water
#### optimal water content (WCopt)
##### t = 5
optimize(function(x)
  WCPRegressions$estimate[1] + WCPRegressions$estimate[2] * x 
  + WCPRegressions$estimate[3] * x^2, c(80, 160), maximum = T)                 # 108.74, 2.447

uniroot.all(function(x)                                                        # upper and lower bounds                                                      
  (WCPRegressions$estimate[1] + WCPRegressions$estimate[2] * x 
   + WCPRegressions$estimate[3] * x^2) - (0.9 * 2.447388), c(0, 200))          # 92.02337 125.44982

##### t = 10
optimize(function(x)
  WCPRegressions$estimate[4] + WCPRegressions$estimate[5] * x 
  + WCPRegressions$estimate[6] * x^2, c(80, 160), maximum = T)                 # 118.693, 2.940 

uniroot.all(function(x)                                                        # upper and lower bounds                                                        
  (WCPRegressions$estimate[4] + WCPRegressions$estimate[5] * x 
   + WCPRegressions$estimate[6] * x^2) - (0.9 * 2.939508), c(0, 200))          # 99.65919 137.72717

##### t = 15
optimize(function(x)                                                           
  WCPRegressions$estimate[7] + WCPRegressions$estimate[8] * x 
  + WCPRegressions$estimate[9] * x^2, c(80, 160), maximum = T)                 #  110.781, 2.322

uniroot.all(function(x)                                                        # upper and lower bounds                                                       
  (WCPRegressions$estimate[7] + WCPRegressions$estimate[8] * x 
   + WCPRegressions$estimate[9] * x^2) - (0.9 * 2.322397), c(0, 200))          # 94.68398 126.87784

##### t = 20
optimize(function(x)                                                        
  WCPRegressions$estimate[10] + WCPRegressions$estimate[11] * x 
  + WCPRegressions$estimate[12] * x^2, c(80, 160), maximum = T)                # 107.741, 2.131

uniroot.all(function(x)                                                        # upper and lower bounds                      
  (WCPRegressions$estimate[10] + WCPRegressions$estimate[11] * x 
   + WCPRegressions$estimate[12] * x^2) - (0.9 * 2.131495), c(0, 200))         # 92.76788 122.71342


#### water compensation point (WCP)
##### t = 5
uniroot(function(x) WCPRegressions$estimate[1] + WCPRegressions$estimate[2] * x  
        + WCPRegressions$estimate[3] * x^2, interval = c(0, 100))              # 55.88 

##### t = 10
uniroot(function(x)                                                    
  WCPRegressions$estimate[4] + WCPRegressions$estimate[5] * x 
  + WCPRegressions$estimate[6] * x^2, interval = c(0, 100))                    # 58.50

##### t = 15
uniroot(function(x)                                                          
  WCPRegressions$estimate[7] + WCPRegressions$estimate[8] * x 
  + WCPRegressions$estimate[9] * x^2, interval = c(0, 100))                    # 59.88

##### t = 20
uniroot(function(x)                                                           
  WCPRegressions$estimate[10] + WCPRegressions$estimate[11] * x 
  + WCPRegressions$estimate[12] * x^2, interval = c(0, 100))                   # 60.39


#### temperature
##### Q10 value
((TCRegressions$estimate[1] + TCRegressions$estimate[2] * 20 + TCRegressions$estimate[3] * 20^2) / (TCRegressions$estimate[1] + TCRegressions$estimate[2] * 5 + TCRegressions$estimate[3] * 5^2))^(10 / (20-5))



# VISUALIZATION ----
## field
### plotting all environmental factors against time with histograms
#### NP
(timeNP <- ggplot(sanioniaField, aes(x = (cumulativeSeconds/60/60/24), 
                                     y = aCorrected))                           +
   scale_x_continuous(expand = c(0, 0), limits = c(0, 42))                      +
   labs(title = "(D1)")                                                         +
   xlab("Days")                                                                 +
   ylab(expression("Net photosynthetic rate ("*mu*mol~CO[2]~m^-2~s^-1*")"))     +
   geom_hline(yintercept = 0, color = "#323031", linetype = "dashed", 
              size = 1, alpha = 0.75)                                           +
   geom_point()                                                                 +
   geom_line()                                                                  +
   theme_cust()                                                                 +
   theme(axis.title = element_text(size = 18),
         title = element_text(size = 18)))

(histNP <- ggplot(sanioniaField, aes(x = aCorrected))                           +
    geom_histogram(binwidth = 0.5)                                              +
    scale_x_continuous(expand = c(0, 0), limits = c(-1, 7))                     +
    scale_y_continuous(expand = expansion(mult = 0, add = c(0, 3)))             +
    labs(title = "(D2)", 
         x = expression("Net photosynthetic rate ("*mu*mol~CO[2]~m^-2~s^-1*")"),
         y = "Frequency")                                                       +
    geom_vline(xintercept = 0, color = "#323031", lty = "dashed", 
               size = 1, alpha = 0.75)                                          +
    theme_cust()                                                                +
    theme(axis.title = element_text(size = 22),
          title = element_text(size = 18)))

#### water content
(timeWC <- ggplot(sanioniaField, aes(x = (cumulativeSeconds/60/60/24), 
                                     y = waterContent))                         +
    labs(title = "(B1)", x = NULL, y = "Water content (%)")                     +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 42))                     +
    geom_hline(yintercept = 124.4, color = "#323031", alpha = 0.75,
               lty = "dashed", size = 1)                                        +   # water limited point from field
    geom_hline(yintercept = 269, color = "#db3a34", lty = "dashed", 
               size = 1, alpha = 0.75)                                          +
    geom_rect(xmin = 0, xmax = 42, ymin = 224, ymax = 314, fill = "#db3a34",
              alpha = 0.005)                                                    +
    geom_point()                                                                +
    geom_line()                                                                 +
    theme_cust()                                                                +
    theme(axis.text.x = element_blank(),
          axis.title = element_text(size = 22),
          title = element_text(size = 18)))

(histWC <-  ggplot(sanioniaField, aes(x = waterContent))                        +
    geom_rect(xmin = 224, xmax = 314, ymin = 0, ymax = 12.25, 
              fill = "#db3a34", alpha = 0.005)                                  +
    geom_histogram(binwidth = 20)                                               +
    scale_x_continuous(expand = c(0, 0), limits = c(50, 325))                   +
    scale_y_continuous(expand = expansion(mult = 0, add = c(0, 0.25)))          +
    labs(title = "(B2)",
         x = "Water content (%)",
         y = "Frequency")                                                       +
    geom_vline(xintercept = 269, color = "#db3a34", lty = "dashed", 
               size = 1, alpha = 0.75)                                          +
    geom_vline(xintercept = 124, color = "#323031", lty = "dashed", 
               size = 1, alpha = 0.75)                                          +
    theme_cust()                                                                +
    theme(axis.title = element_text(size = 22),
          title = element_text(size = 18)))

#### temperature (weather station)
(timeTemp <- ggplot(sanioniaField, aes(x = (cumulativeSeconds/60/60/24), 
                                       y = tempWS))                             +
    labs(title = "(C1)",
         x = NULL, y = "Temperature (°C)")                                      +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 42))                     +
    ylim(2, 6.2)                                                                +
    geom_point()                                                                +
    geom_line()                                                                 +
    theme_cust()                                                                +
    theme(axis.text.x = element_blank(),
          axis.title = element_text(size = 22),
          title = element_text(size = 18)))

(histTemp <- ggplot(sanioniaField, aes(x = tempWS))                             +
    geom_histogram(bins = 10)                                                   +
    scale_x_continuous(expand = expansion(mult = 0, add = c(0, 0.25)))          +
    scale_y_continuous(expand = expansion(mult = 0, add = c(0, 4)))             +
    labs(title = "(C2)", 
         x = "Temperature (°C)",
         y = "Frequency")                                                       +
    theme_cust()                                                                +
    theme(axis.title = element_text(size = 22),
          title = element_text(size = 18)))


#### par (weather station)
(timePAR <- ggplot(sanioniaField, aes(x = (cumulativeSeconds/60/60/24), 
                                      y = as.numeric(parweatherstation)))       +
    xlab(NULL)                                                                  +
    labs(title = "(A1)")                                                        +
    ylab(expression("PPFD ("*mu*mol~photons~m^-2~s^-1*")"))                     +
    geom_hline(yintercept = 34, color = "#323031", lty = "dashed", 
               size = 1, alpha = 0.75)                                          +
    geom_hline(yintercept = 480, color = "#db3a34", alpha = 0.75,               # light sat point from field
               lty = "dashed", size = 1)                                        + 
    scale_x_continuous(expand = c(0, 0), limits = c(0, 42))                     +
    geom_point()                                                                +
    geom_line()                                                                 +
    theme_cust()                                                                +
    theme(axis.text.x = element_blank(),
          axis.title = element_text(size = 22),
          title = element_text(size = 18)))

(histPAR <- ggplot(sanioniaField, aes(x = as.numeric(parweatherstation)))       +
    geom_histogram(binwidth = 150)                                              +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 2150))                   +
    scale_y_continuous(expand = expansion(mult = 0, add = c(0, 6)))             +
    labs(title = "(A2)",
         x = expression("PPFD ("*mu*mol~photons~m^-2~s^-1*")"),
         y = "Frequency")                                                       +
    geom_vline(xintercept = 480, color = "#db3a34", lty = "dashed", 
               size = 1, alpha = 0.75)                                          +
    geom_vline(xintercept = 34, color = "#323031", lty = "dashed", 
               size = 1, alpha = 0.75)                                          +
    theme_cust()                                                                +
    theme(axis.title = element_text(size = 22),
          title = element_text(size = 18)))


#### arranging and saving
(enviro <- ((timePAR / timeWC / timeTemp / timeNP) | (histPAR / histWC / histTemp / histNP)) + 
    plot_layout(widths = c(2, 1)))

ggsave("img\\enviro.png", plot = enviro, width = 17, height = 18)              # saving


### light
(LCfieldPlot <- ggplot(LCfield, aes(x = partop, y = aCorrected))                +
    geom_point(pch = 1, cex = 3)                                                 + 
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.4)               +
    xlab(expression("PPFD ("*mu*mol~photons~m^-2~s^-1*")"))                      +
    ylab(expression("Net photosynthetic rate ("*mu*mol~CO[2]~m^-2~s^-1*")"))     +
    theme_cust())

ggsave("img\\field_light.png", plot = LCfieldPlot, width = 8, height = 8)      # saving


### water
(WCfieldPlot <- ggplot(WCfield, aes(x = waterContent, y = aCorrected))          +
    geom_point(pch = 1, cex = 3)                                                +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.4)              +
    labs(x = "Water content (%)",
         y = expression("Net photosynthetic rate ("*mu*mol~photons~m^-2~s^-1*")"),
         color = "PPFD")                                                        +
    geom_function(fun = function(x)                                     
      -1.419e+01 + 1.484e-01 * x 
      + -2.762e-04 * x^2)                                                       +
    theme_cust())

ggsave("img\\field_water.png", plot = WCfieldPlot, width = 10, height = 8)     # saving


## lab
### light
(parTempPlot <- ggplot(LC, aes(x = partop, y = a, color = tempCat))             +
    scale_color_manual(values = temp.palette)                                    +
    ylim(-3, 4)                                                                  +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.4)               +
    xlab(expression("PPFD ("*mu*mol~photons~m^-2~s^-1*")"))                      + 
    ylab(expression("Net photosynthetic rate ("*mu*mol~CO[2]~m^-2~s^-1*")"))     +
    labs(color = "Temperature (°C)")                                             +
    geom_function(fun = LC.5, color = "#084c61", size = 0.8)                     +
    geom_function(fun = LC.10, color = "#1fbbcc", size = 0.8)                    +
    geom_function(fun = LC.15, color = "#ffbe38", size = 0.8)                    +
    geom_function(fun = LC.20, color = "#db3a34", size = 0.8)                    +
    geom_point(pch = 1, cex = 3)                                                 +
    theme_cust()                                                                 +
    theme(legend.position = c(0.14, 0.9)))

ggsave("img\\partop_temp.png", plot = parTempPlot, width = 11, height = 8)     # saving


### water
(waterTempPlot <- ggplot(WCphotosynth, aes(x = waterContent, y = a, 
                                           color = tempCat))                    +
    scale_color_manual(values = temp.palette)                                   +
    xlab("Water content (%)")                                                   + 
    ylab(expression("Net photosynthetic rate ("*mu*~mol~CO[2]~m^-2~s^-1*")"))   +
    labs(color = "Temperature (°C)")                                            +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.4)              +
    geom_function(fun = function(x)                                            # t = 5
      WCPRegressions$estimate[1] + WCPRegressions$estimate[2] * x 
      + WCPRegressions$estimate[3] * x^2, color = "#084c61", size = 0.8)        +
    geom_function(fun = function(x)                                            # t = 10
      WCPRegressions$estimate[4] + WCPRegressions$estimate[5] * x 
      + WCPRegressions$estimate[6] * x^2, color = "#1fbbcc", size = 0.8)        +
    geom_function(fun = function(x)                                            # t = 15
      WCPRegressions$estimate[7] + WCPRegressions$estimate[8] * x 
      + WCPRegressions$estimate[9] * x^2, color = "#ffbe38", size = 0.8)        +
    geom_function(fun = function(x)                                            # t = 20
      WCPRegressions$estimate[10] + WCPRegressions$estimate[11] * x 
      + WCPRegressions$estimate[12] * x^2, color = "#db3a34", size = 0.8)       +
    geom_function(fun = function(x)                                            # t = 5 resp
      WCRregressions$estimate[1] + WCRregressions$estimate[2] * x 
      + WCRregressions$estimate[3] * x^2, color = "#084c61", size = 0.8)        +
    geom_function(fun = function(x)                                            # t = 10 resp
      WCRregressions$estimate[4] + WCRregressions$estimate[5] * x 
      + WCRregressions$estimate[6] * x^2, color = "#1fbbcc", size = 0.8)        +
    geom_function(fun = function(x)                                            # t = 15 resp
      WCRregressions$estimate[7] + WCRregressions$estimate[8] * x 
      + WCRregressions$estimate[9] * x^2, color = "#ffbe38", size = 0.8)        +
    geom_function(fun = function(x)                                            # t = 20 resp
      WCRregressions$estimate[10] + WCRregressions$estimate[11] * x 
      + WCRregressions$estimate[12] * x^2, color = "#db3a34", size = 0.8)       +
    geom_point(pch = 1, cex = 3)                                                +
    geom_point(data = WCresp, pch = 2, cex = 3)                                 +
    theme_cust()                                                                +
    theme(legend.position = c(0.15, 0.9)))

(WCfieldPlot <- ggplot(WCfield, aes(x = waterContent, y = aCorrected))          +
    geom_point(pch = 1, cex = 3)                                                +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.4)              +
    labs(x = "Water content (%)",
         y = expression("Net photosynthetic rate ("*mu*mol~photons~m^-2~s^-1*")"),
         color = "PPFD")                                                        +
    geom_function(fun = function(x)                                     
      -1.419e+01 + 1.484e-01 * x 
      + -2.762e-04 * x^2)                                                       +
    theme_cust())


ggsave("img\\water_temp.png", plot = waterTempPlot, width = 10, height = 9)    # saving


### temperature
#### creating summarized points
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

#### plotting
(tempParPlot00 <- ggplot(tempAvg, aes(x = tcuv, y = a, shape = parCat))         +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.4, size = 0.75) +
    labs(x = "Temperature (°C)", 
         y = expression("Net photosynthetic rate ("*mu*~mol~CO[2]~m^-2~s^-1*")")) +
    ylim(-3, 3)                                                                 +
    geom_function(fun = function(x)                                            # par = 0
      TCRegressions$estimate[1] + TCRegressions$estimate[2] * x 
      + TCRegressions$estimate[3] * x^2)                                        +
    geom_function(fun = function(x)                                            # par = 550
      TCRegressions$estimate[16] + TCRegressions$estimate[17] * x 
      + TCRegressions$estimate[18] * x^2)                                       +
    geom_errorbar(ymin = tempAvg$a - tempAvg$stdev, 
                  ymax = tempAvg$a + tempAvg$stdev, width = 0.4) +
    geom_point(cex = 3.5)                                                       +
    scale_shape_manual(values = c(2, 1), name = "PPFD")                         +
    theme_cust()                                                                +
    theme(legend.position = c(0.10, 0.7)))

ggsave("img\\temp_par00.png", plot = tempParPlot00, width = 8, height = 8)     # saving


#### full temperature responses for appendix
(tempParPlot01 <- ggplot(TC, aes(x = tcuv, y = a, color = parCat))              +
    scale_color_manual(values = c("#084c61", "black", "#ffbe38", "cornsilk3", 
                                  "#1fbbcc", "orangered", "darkorchid4", 
                                  "chocolate4"))                                +
    geom_jitter(width = 0.5, height = 0, cex = 2, pch = 1)                      +
    labs(y = expression("Net photosynthetic rate ("*mu*~mol~CO[2]~m^-2~s^-1*")"),
         x = "Temperature (°C)",
         color = "PPFD")                                                        +
    geom_function(fun = function(x)                                            # par = 0
      TCRegressions$estimate[1] + TCRegressions$estimate[2] * x 
      + TCRegressions$estimate[3] * x^2, color = "#084c61")                     +
    geom_function(fun = function(x)                                            # par = 50
      TCRegressions$estimate[4] + TCRegressions$estimate[5] * x 
      + TCRegressions$estimate[6] * x^2, color = "black")                       +
    geom_function(fun = function(x)                                            # par = 100
      TCRegressions$estimate[7] + TCRegressions$estimate[8] * x 
      + TCRegressions$estimate[9] * x^2, color = "#ffbe38")                     +
    geom_function(fun = function(x)                                            # par = 200
      TCRegressions$estimate[10] + TCRegressions$estimate[11] * x 
      + TCRegressions$estimate[12] * x^2, color = "cornsilk3")                  +
    geom_function(fun = function(x)                                            # par = 400
      TCRegressions$estimate[13] + TCRegressions$estimate[14] * x 
      + TCRegressions$estimate[15] * x^2, color = "#1fbbcc")                    +
    geom_function(fun = function(x)                                            # par = 550
      TCRegressions$estimate[16] + TCRegressions$estimate[17] * x 
      + TCRegressions$estimate[18] * x^2, color = "orangered")                  +
    geom_function(fun = function(x)                                            # par = 800
      TCRegressions$estimate[19] + TCRegressions$estimate[20] * x 
      + TCRegressions$estimate[21] * x^2, color = "darkorchid4")                +
    geom_function(fun = function(x)                                            # par = 1200
      TCRegressions$estimate[22] + TCRegressions$estimate[23] * x 
      + TCRegressions$estimate[24] * x^2, color = "chocolate4")                 +
    geom_hline(yintercept = 0, lty = "dashed", size = 1, alpha = 0.5) +
    theme_cust()                                                                +
    theme(legend.position = c(0.12, 0.15)))

ggsave("img\\temp_par01.png", plot = tempParPlot01, width = 8, height = 10)    # saving


## comparison of regression coefficients bt field and lab models
### comparing light regressions
(lightModelComparison <- ggplot(lightComparison, aes(x = parameter, 
                                                     y = estimate, 
                                                     shape = Regression))       +
    geom_point(position = position_dodge(width=0.3), cex = 2.5)                 +
    labs(x = "Coefficient", y = "Estimated value")                                +
    ylim(-10, 8)                                                                +
    geom_hline(yintercept = 0, lty = "dashed", size = 1, alpha = 0.2)           +
    geom_errorbar(ymin = tlower, ymax = tupper, width = 0.2, 
                  position = position_dodge(width = 0.3))                       +
    theme_cust()                                                                +
    theme(legend.position = c(0.12, 0.15)))

ggsave("img\\light_mod_comparison.png", plot = lightModelComparison,           # saving
       width = 12, height = 7)

### comparing water regressions
#### coef a
(waterAComp <- waterComparison %>% 
    filter(parameter2 == "a") %>% 
    ggplot(., aes(x = parameter2, y = estimate2, shape = Regression))           +
    geom_point(position = position_dodge(width = 0.3), cex = 3)                 +
    geom_errorbar(ymin = c(-21.75800000, -11.40294370), 
                  ymax = c(-6.62200000, -4.42098450), width = 0.2, 
                  position = position_dodge(width = 0.3))                       +
    labs(x = NULL, y = "Estimated value")                                       +
    ylim(-24, -5)                                                               +
    theme_cust()                                                                +
    theme(legend.position = c(0.23, 0.15)))

#### coef b
(waterBComp <- waterComparison %>% 
    filter(parameter2 == "b") %>% 
    ggplot(., aes(x = parameter2, y = estimate2, shape = Regression))           +
    geom_point(position = position_dodge(width = 0.3), cex = 3)                 +
    geom_errorbar(ymin = c(0.07484000, 0.11475210), 
                  ymax = c(0.22196000, 0.26632850), width = 0.2, 
                  position = position_dodge(width = 0.3))                       +
    ylim(0.05, 0.30)                                                            +
    labs(x = "Coefficient", y = NULL)                                             +
    theme_cust()                                                                +
    theme(legend.position = "none"))

#### coef c
(waterCComp <- waterComparison %>% 
    filter(parameter2 == "c") %>% 
    ggplot(., aes(x = parameter2, y = estimate2, shape = Regression))           +
    geom_point(position = position_dodge(width = 0.3), cex = 3)                 +
    geom_errorbar(ymin = c(-0.00045046, -0.00125620), 
                  ymax = c(-0.00010194, -0.00049620), width = 0.2, 
                  position = position_dodge(width = 0.3))                       +
    ylim(-0.0013, 0)                                                            +
    labs(x = NULL, y = NULL)                                                    +
    theme_cust()                                                                +
    theme(legend.position = "none"))

#### arranging plots
(waterCoeff <- waterAComp + waterBComp +waterCComp)

ggsave("img\\water_mod_comparison.png", plot = waterCoeff,                     # saving
       width = 16, height = 6)
