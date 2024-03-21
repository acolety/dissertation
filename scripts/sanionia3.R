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


# LOADING DATA ----
sanioniaLab <- read.csv("data\\lab\\sanioniaLab.csv") %>% 
  filter(Object == 1)                                                          # removing rows of cuvette calibration
View(sanioniaLab)

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
  filter(between(waterContent, 90, 130) & !tempCat == 25)                       

ggplot(LC, aes(x = partop, y = a, color = tempCat)) +   
  geom_point() +
  facet_wrap(LC$tempCat, scale = "fixed") +
  geom_smooth() +                                                             
  theme_classic()



### temperature curve subset and raw data vis
TC <- sanioniaLab %>%                                                          # temperature:par interaction data
  filter(between(waterContent, 90, 130) & !tempCat == 25)

# TCW <- sanioniaLab %>%                                                         # temperature:WC interaction data
#   filter(!tempCat == 25) %>% 
#   filter(parCat == 1200 & tempCat %in% c(5, 10, 15) |                          # nvm not worth it only 4 points
#          parCat == 800 & tempCat == 20)


ggplot(TC, aes(x = tcuv, y = a, color = parCat)) +   
  geom_point() +
  # geom_smooth(se = F) +                                                       # messed up                                                           
  theme_classic()


# MODELLING  ----
modNull <- lm(a ~ 1, data = sanioniaLab)

modAll <- lm(a ~ partop + tcuv + weight + partop:tcuv + partop:weight + tcuv:weight + partop:tcuv:weight, data = sanioniaLab)

plot(modAll)
summary(modAll)

modLess <- lm(a ~ partop + tcuv + weight + partop:tcuv + partop:weight + tcuv:weight, data = sanioniaLab)

plot(modLess)
summary(modLess)

AIC(modAll, modLess)  # comparable

par_colmod <- lm(a ~ partop:tempCat, data = sanioniaLab)
summary(par_colmod)

par_colmodfit <- data.frame(predictions = predict(par_colmod, sanioniaLab), x = sanioniaLab$a)



## attempting nonlinear models

### light curve
#### regressions combined 

LCRegressions <- LC %>% 
  filter(tempCat != 25) %>% 
  group_by(tempCat) %>% 
  do(tidy(nls(a ~ SSasymp(partop, Asym, R0, lrc), data = .))) 

View(LCRegressions)

#### individual models to assess
LC5 <- LC %>% 
  filter(tempCat == 5) %>% 
  nls(a ~ SSasymp(partop, Asym, R0, lrc), data = .)
summary(LC5)
plot(nlsResiduals(LC5))
overview(LC5)                                                                 # find a way to assess goodness of fit

LC10 <-  LC %>% 
  filter(tempCat == 10) %>% 
  nls(a ~ SSasymp(partop, Asym, R0, lrc), data = .)
summary(LC10)
plot(nlsResiduals(LC10))
overview(LC10)

LC15 <- LC %>% 
  filter(tempCat == 15) %>% 
  nls(a ~ SSasymp(partop, Asym, R0, lrc), data = .)
summary(LC15)
plot(nlsResiduals(LC15))
overview(LC15)

LC20 <-  LC %>% 
  filter(tempCat == 20) %>% 
  nls(a ~ SSasymp(partop, Asym, R0, lrc), data = .)
summary(LC20)
plot(nlsResiduals(LC20))
overview(LC20)



#### plotting

(parTempPlot <- ggplot(LC, aes(x = partop, y = a, color = tempCat)) +
                    scale_color_manual(values = c("red", "orange", "green", "blue", "purple")) +
                    geom_point(pch = 1) +                       # make sure to exclude tempCat 25
                    geom_function(fun = function(x) 
                      LCRegressions$estimate[1] + (LCRegressions$estimate[2] - LCRegressions$estimate[1]) 
                      * exp(-exp(LCRegressions$estimate[3]) * x), color = "red") +
                    geom_function(fun = function(x) 
                      LCRegressions$estimate[4] + (LCRegressions$estimate[5] - LCRegressions$estimate[4]) 
                      * exp(-exp(LCRegressions$estimate[6]) * x), color = "orange") +
                    geom_function(fun = function(x) 
                      LCRegressions$estimate[7] + (LCRegressions$estimate[8] - LCRegressions$estimate[7]) 
                      * exp(-exp(LCRegressions$estimate[9]) * x), color = "green") +
                    geom_function(fun = function(x) 
                      LCRegressions$estimate[10] + (LCRegressions$estimate[11] - LCRegressions$estimate[10]) 
                      * exp(-exp(LCRegressions$estimate[12]) * x), color = "blue") +
                    theme_classic())

ggsave("img\\partop_temp.png", plot = parTempPlot, width = 7, height = 5)


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

(waterTempPlot <- ggplot(WCphotosynth, aes(x = waterContent, y = a, color = tempCat)) +
                    geom_point() +
                    geom_function(fun = function(x)                                              # t = 5
                      WCPRegressions$estimate[1] + WCPRegressions$estimate[2] * x 
                      + WCPRegressions$estimate[3] * x^2, color = "red") +
                    geom_function(fun = function(x)                                              # t = 10
                      WCPRegressions$estimate[4] + WCPRegressions$estimate[5] * x 
                      + WCPRegressions$estimate[6] * x^2, color = "green") +
                    geom_function(fun = function(x)                                              # t = 15
                      WCPRegressions$estimate[7] + WCPRegressions$estimate[8] * x 
                      + WCPRegressions$estimate[9] * x^2, color = "blue") +
                    geom_function(fun = function(x)                                              # t = 20
                      WCPRegressions$estimate[10] + WCPRegressions$estimate[11] * x 
                      + WCPRegressions$estimate[12] * x^2, color = "purple") +
                    theme_classic())

ggsave("img\\water_temp.png", plot = waterTempPlot, width = 7, height = 5)


### finding WCopt for each temperature
optimize(function(x)                                                           # t = 5
  WCPRegressions$estimate[1] + WCPRegressions$estimate[2] * x 
 + WCPRegressions$estimate[3] * x^2, c(80, 160), maximum = T)                  # 108.74, 2.447

optimize(function(x)                                                           # t = 10
  WCPRegressions$estimate[4] + WCPRegressions$estimate[5] * x 
  + WCPRegressions$estimate[6] * x^2, c(80, 160), maximum = T)                 # 118.693, 2.940 

optimize(function(x)                                                           # t = 15
  WCPRegressions$estimate[7] + WCPRegressions$estimate[8] * x 
  + WCPRegressions$estimate[9] * x^2, c(80, 160), maximum = T)                 #  110.781, 2.322

optimize(function(x)                                                           # t = 20
  WCPRegressions$estimate[10] + WCPRegressions$estimate[11] * x 
  + WCPRegressions$estimate[12] * x^2, c(80, 160), maximum = T)                # 107.741, 2.131



### attempting with nls
selfStartQuad <- function(input, a, b, c, d) {
  a(b * input^2 + c * input + d)
}

nls(a ~ NLS.poly2(waterContent, a, b, c), data = WC5)

ggplot(WC5, aes(x = waterContent, y = a)) +
  geom_point()

model2 <- lm(a ~ poly(waterContent, degree = 2, raw = TRUE), data = WC5)
summary(model2)          

plot(model2)

geom_function(fun = function(x) 
  LCRegressions$estimate[10] + (LCRegressions$estimate[11] - LCRegressions$estimate[10]) 
  * exp(-exp(LCRegressions$estimate[12]) * x), color = "blue")



### WC respiration--Uchida used polynomial, not sure if my data needs this tho
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

(waterRespPlot <- ggplot(WCresp, aes(x = waterContent, y = a, color = tempCat)) +
    geom_point() +
    geom_function(fun = function(x)                                              # t = 5
      WCRregressions$estimate[1] + WCRregressions$estimate[2] * x 
      + WCRregressions$estimate[3] * x^2, color = "red") +
    geom_function(fun = function(x)                                              # t = 10
      WCRregressions$estimate[4] + WCRregressions$estimate[5] * x 
      + WCRregressions$estimate[6] * x^2, color = "green") +
    geom_function(fun = function(x)                                              # t = 15
      WCRregressions$estimate[7] + WCRregressions$estimate[8] * x 
      + WCRregressions$estimate[9] * x^2, color = "blue") +
    geom_function(fun = function(x)                                              # t = 20
      WCRregressions$estimate[10] + WCRregressions$estimate[11] * x 
      + WCRregressions$estimate[12] * x^2, color = "purple") +
    theme_classic())

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


(tempParPlot <- ggplot(TC, aes(x = tcuv, y = a, color = parCat)) +
                  geom_point() +
                  geom_function(fun = function(x)                                              # par = 0
                    TCRegressions$estimate[1] + TCRegressions$estimate[2] * x 
                    + TCRegressions$estimate[3] * x^2, color = "red") +
                  geom_function(fun = function(x)                                              # par = 50
                    TCRegressions$estimate[4] + TCRegressions$estimate[5] * x 
                    + TCRegressions$estimate[6] * x^2, color = "orange") +
                  geom_function(fun = function(x)                                              # par = 100
                    TCRegressions$estimate[7] + TCRegressions$estimate[8] * x 
                    + TCRegressions$estimate[9] * x^2, color = "yellowgreen") +
                  geom_function(fun = function(x)                                              # par = 200
                    TCRegressions$estimate[10] + TCRegressions$estimate[11] * x 
                    + TCRegressions$estimate[12] * x^2, color = "green") +
                  geom_function(fun = function(x)                                              # par = 400
                    TCRegressions$estimate[13] + TCRegressions$estimate[14] * x 
                    + TCRegressions$estimate[15] * x^2, color = "blue") +
                  geom_function(fun = function(x)                                              # par = 550
                    TCRegressions$estimate[16] + TCRegressions$estimate[17] * x 
                    + TCRegressions$estimate[18] * x^2, color = "darkblue") +
                  geom_function(fun = function(x)                                              # par = 800
                    TCRegressions$estimate[19] + TCRegressions$estimate[20] * x 
                    + TCRegressions$estimate[21] * x^2, color = "purple") +
                  geom_function(fun = function(x)                                              # par = 1200
                    TCRegressions$estimate[22] + TCRegressions$estimate[23] * x 
                    + TCRegressions$estimate[24] * x^2, color = "pink") +
                  theme_classic())

ggsave("img\\temp_par.png", plot = tempParPlot, width = 7, height = 5)



### temperature and respiration
  
  
  
# VISUALIZATION ----

## 2d plots with regression for sig interactions LINEAR
# a ~ partop, a ~ tcuv*, a ~ weight, a ~ partop:tcuv*, a ~ partop:weight*, a ~ tcuv:weight*

(ggplot(sanioniaLab, aes(x = partop, y = a, color = tcuv)) +
   geom_point() +
   geom_abline(slope = coef(par_colmod)[["partop:tcuv"]],
                            intercept = coef(par_colmod)[["Intercept"]]))

ggplot(sanioniaLab, aes(x = partop, y = a, color = tempCat)) +
    geom_point() +
    geom_abline(slope = par_colmod$coefficients[2],
                intercept = par_colmod$coefficients[1]) +
    theme_classic()


## trying plot3Drgl 
### removing any lines where variables = NA
data3d <- sanioniaLab %>% 
  drop_na(weight, tcuv, partop)






# creating predicted values on xy plane
x <- sanioniaLab %>% 
  drop_na(weight) %>% 
  pull(partop)
y <- sanioniaLab %>% 
  drop_na(weight) %>% 
  pull(weight)
z <- sanioniaLab %>% 
  drop_na(weight) %>% 
  pull(tcuv)
plotCol <- sanioniaLab %>% 
  drop_na(weight) %>% 
  pull(a)

fit <- lm(z ~ x + y)

grid.lines <- 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y, na.rm = T), max(y, na.rm = T), length.out = grid.lines)
xy <- expand.grid(x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(fit)

(fitpoints)
length(fitpoints)
length(LC$tcuv)




### with lm
scatter3D(x, y, z,            # x, y, z coordinates
       colvar = plotCol,                                              # determines colors of points
       pch = 19, cex = 1,                                                   # point size and shape
       colkey = list(side = 1, length = 0.5),                               # position and scale of legend
       bty = "b",                                                          # box style
       theta = 30, phi = 20,                                               # viewing angle
       xlab = "PPFD", ylab = "weight", zlab = "temperature",                # axis labels
       ticktype = "detailed", surf = list(x = x.pred, y = y.pred, 
                                          z = z.pred, facets = NA, col = "grey", fit = fitpoints)                                                # adding ticks, can specify nticks = 
)

### no lm
scatter3D(x, y, z,            # x, y, z coordinates
          colvar = plotCol,                                              # determines colors of points
          pch = 19, cex = 1,                                                   # point size and shape
          colkey = list(side = 1, length = 0.5),                               # position and scale of legend
          bty = "b2",                                                          # box style
          theta = 30, phi = 15,                                               # viewing angle
          xlab = "PPFD", ylab = "weight", zlab = "temperature",                # axis labels
          ticktype = "detailed")


plot3d(x, y, z,            # x, y, z coordinates
          colvar = plotCol,                                              # determines colors of points
          pch = 19, cex = 1,                                                # viewing angle
          xlab = "PPFD", ylab = "weight", zlab = "temperature",                # axis labels
          ticktype = "detailed", surf = list(x = x.pred, y = y.pred, 
                                             z = z.pred, facets = NA, col = "red", fit = fitpoints)                                                # adding ticks, can specify nticks = 
          )
length(plotCol$x)



