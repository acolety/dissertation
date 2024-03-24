# analysis of field data
# 8 March 2024

# TO DO: 

# libraries ----
library(tidyverse)
library(dplyr)
library(skimr)
library(scatterplot3d)
library(broom)
library(nlstools)
library(patchwork)

# FUNCTIONS AND DESIGN ----
## theme for plots
theme_cust <- function(){            
  theme_classic() +                          
    theme(axis.ticks.length = unit(-0.2, "cm"),
          axis.text.x = element_text(size = 12),       
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          panel.grid = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 10),
          legend.position = c(0.12, 0.9),
          legend.margin = margin(6, 6, 6, 6),
          legend.box.background = element_rect(size = 1.1))
}

## color palette

temp.palette <- c("#084c61", "#1fbbcc", "#ffbe38", "#db3a34", "#323031")


# loading data ----
fieldData <- list.files(path = ".\\data\\field", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  filter(Code == "MP_010")


# cleaning data ----
colnames(fieldData) <- make.names(colnames(fieldData), unique = TRUE)
colnames(fieldData) <- tolower(colnames(fieldData))


fieldData <- fieldData %>% 
  mutate(wetnessWS =                                                           
           as.numeric(wetness..lgr.s.n..21432090..sen.s.n..21367120....weather.station),  
         tempWS = 
           as.numeric(temperature..lgr.s.n..21432090..sen.s.n..21420856....weather.station),
         windspeedWS = 
           as.numeric(wind.speed..lgr.s.n..21432090..sen.s.n..21435605....weather.station),
         rhWS = 
           as.numeric(relative.humidity..lgr.s.n..21432090..sen.s.n..21420856....weather.station)) %>% 
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
         object = as.factor(object), weight = as.numeric(weight),
         inside.fan = as.factor(inside.fan), cumulativeSeconds = as.numeric(cumulative.seconds))

view(fieldData)  
skim(fieldData)

## adding and correcting columns
fieldData <- fieldData %>% 
  mutate(species = case_when(object == 1 ~ "chorisodontium",                  # column of species names
                             object == 2 ~"sanionia",
                             object == 3 ~ "polytrichum",
                             object == 4 ~ "andreaea",
                             object == 5 ~ "stereocaulon",
                             object == 6 ~ "usneaAnt0",
                             object == 7 ~ "usneaAur",
                             object == 8 ~ "himantormia",
                             object == 12 ~ "usneaAnt")) %>% 
  mutate(area = case_when(object == 1 ~ 107.8078,                             # correct areas
                          object == 2 ~ 77.48568,
                          object == 3 ~ 52.85567,
                          object == 4 ~ 6.9291,
                          object == 5 ~ 5.10035,
                          object == 6 ~ 74.66479,
                          object == 7 ~ 43.84498,
                          object == 8 ~ 15.83125,
                          object == 12 ~ 32.15818)) %>% 
  mutate(aCorrected = (a * 0.0008) / (area / 10000))                          # correcting a based on corrected area


fieldData <- fieldData %>% 
  mutate(dryWeight = case_when(species == "chorisodontium" ~ 33.992,          # adding dry weight, none for usneaAnt0
                               species == "sanionia" ~ 35.223,
                               species == "polytrichum" ~ 6.758,
                               species == "andreaea" ~ 1.076,
                               species == "stereocaulon" ~ 0.404,
                               species == "usneaAnt" ~ 5.294,
                               species == "usneaAur" ~ 4.202,
                               species == "himantormia" ~ 1.288)) %>% 
  mutate(basketWeight = case_when(species == "chorisodontium" ~ 5.934,       # adding basket weight, none for usneaAnt0
                                  species == "sanionia" ~ 5.147,
                                  species == "polytrichum" ~ 5.449,
                                  species == "andreaea" ~ 5.619,
                                  species == "stereocaulon" ~ 5.555,
                                  species == "usneaAnt" ~ 5.705,
                                  species == "usneaAur" ~ 5.933,
                                  species == "himantormia" ~ 5.681)) %>% 
  mutate(saturatedWeight = case_when(species == "chorisodontium" ~ 157.896,       # adding basket weight, none for usneaAnt0
                                     species == "sanionia" ~ 104.605,
                                     species == "polytrichum" ~ 24.433,
                                     species == "andreaea" ~ 5.687,
                                     species == "stereocaulon" ~ 1.635,
                                     species == "usneaAnt" ~ 10.433,
                                     species == "usneaAur" ~ 10.687,
                                     species == "himantormia" ~ 3.756)) %>% 
  mutate(sampleWeight = weight - basketWeight) %>% 
  mutate(waterContent = ((sampleWeight - dryWeight) / dryWeight) * 100) %>%         # content as a percent of dry weight
  mutate(waterContentmm = ((sampleWeight - dryWeight) * 1000) / (area * 100)) %>%       # p sure this is good but double check           
  mutate(relativeWC = sampleWeight / saturatedWeight)                            # some RWC > 100, perhaps oversaturated as samples not shaken in field before measuring

write.csv(fieldData, file = "data\\originalData\\field_data_corrected.csv")

## dataset for each species
andreaea <- fieldData %>% 
  filter(species == "andreaea")

chorisodontium <- fieldData %>% 
  filter(species == "chorisodontium")

himantormia <- fieldData %>% 
  filter(species == "himantormia")

polytrichum <- fieldData %>% 
  filter(species == "polytrichum")

sanionia <- fieldData %>% 
  filter(species == "sanionia")

stereocaulon <- fieldData %>% 
  filter(species == "stereocaulon")

usneaAnt <- fieldData %>% 
  filter(species == "usneaAnt")

usneaAur <- fieldData %>% 
  filter(species == "usneaAur")


# plotting raw data ----

## 1. A ~ partop
ggplot(fieldData, aes(x = partop, y = aCorrected, color = species)) +
  facet_wrap(~ species, nrow = 5, ncol = 2, scales = "free") +
  geom_point(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(show.legend = FALSE) +
  theme_classic()


## 2. A ~ tempWS
ggplot(fieldData, aes(x = tempWS, y = aCorrected, color = species)) +
  facet_wrap(~ species, nrow = 5, ncol = 2, scales = "free") +
  geom_point(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(show.legend = FALSE) +
  theme_classic()


## 3. A ~ waterContent(%)
ggplot(fieldData, aes(x = waterContent, y = aCorrected, color = species)) +
  facet_wrap(~ species, nrow = 5, ncol = 2, scales = "free") +
  geom_point(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(show.legend = FALSE) +
  theme_classic()

## 4. A ~ time
ggplot(fieldData, aes(x = (cumulativeSeconds/60/60/24), y = aCorrected, color = partop)) +
  facet_wrap(~ species, nrow = 5, ncol = 2, scales = "free") +
  labs(x = "Days") +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  theme_classic()


## plotting raw data without driest days

## 1. A ~ partop
fieldData %>% 
  filter(waterContent >= 60.00) %>% 
  ggplot(aes(x = partop, y = aCorrected, color = waterContent)) +
    facet_wrap(~ species, nrow = 5, ncol = 2, scales = "free") +
    geom_point(show.legend = FALSE) +
    geom_hline(yintercept = 0, color = "red") +
    geom_smooth() +
    theme_classic() 


## 2. A ~ tempWS
fieldData %>% 
  filter(waterContent >= 60.00) %>% 
  ggplot(aes(x = tempWS, y = aCorrected, color = waterContent)) +
  facet_wrap(~ species, nrow = 5, ncol = 2, scales = "free") +
  geom_point(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth() +
  theme_classic()

## 3. A ~ waterContent(%)
fieldData %>% 
  filter(waterContent >= 60.00) %>% 
  ggplot(aes(x = waterContent, y = aCorrected)) +
    facet_wrap(~ species, nrow = 5, ncol = 2, scales = "free") +
    geom_point(show.legend = FALSE) +
    geom_hline(yintercept = 0, color = "red") +
    geom_smooth() +
    theme_classic()

## 4. A ~ time
fieldData %>% 
  filter(waterContent >= 60.00) %>% 
  ggplot(aes(x = (cumulativeSeconds / 60 / 60 / 24), y = aCorrected)) +
    facet_wrap(~ species, nrow = 5, ncol = 2, scales = "fixed") +
    labs(x = "Days") +
    geom_point() +
  geom_smooth() +
    geom_hline(yintercept = 0, color = "red") +
    theme_classic()


# modelling ----

## checking correlations
cor.test(fieldData$partop, fieldData$tempWS)                                  # 0.365
cor.test(fieldData$tempWS, fieldData$waterContent)                            # -0.044
cor.test(fieldData$waterContent, fieldData$partop)                            # -0.004

## checking distribution of response variable
hist(fieldData$aCorrected)                                                    # normal

## does aCorrected differ between species?
speciesDiff <- aov(aCorrected ~ species, data = fieldData)
summary(speciesDiff)                                                           # yeah

## models (can also filter for WC)

fullModels <- fieldData %>% 
  filter(species %in% c("usneaAnt", "sanionia", "andreaea", "chorisodontium", "himantormia", "polytrichum", "stereocaulon", "usneaAur")) %>% 
  group_by(species) %>% 
  do(tidy(lm(aCorrected ~ partop * tempWS * waterContent, data = .)))

view(fullModels)

### all three factors for each spp
andreaeaFull <- andreaea %>% 
                  filter(waterContent >= 60) %>% 
                  lm(aCorrected ~ partop * waterContent * tempWS, data = .)

plot(andreaeaFull)
summary(andreaeaFull)

andreaea01 <- andreaea %>% 
                filter(waterContent >= 60) %>%
                lm(aCorrected ~ partop + waterContent + tempWS + partop:waterContent + partop:tempWS
                           + partop:waterContent:tempWS, data = .)
summary(andreaea01)

andreaea02 <- andreaea %>% 
  filter(waterContent >= 60) %>%
  lm(aCorrected ~ partop + waterContent + tempWS + partop:waterContent 
     + partop:waterContent:tempWS, data = .)

summary(andreaea02)

andreaea03 <- andreaea %>% 
  filter(waterContent >= 60) %>%
  lm(aCorrected ~ partop + waterContent + tempWS +
     + partop:waterContent:tempWS, data = .)
summary(andreaea03)

chorisodontium <- fieldData %>% 
  filter(species == "chorisodontium")

himantormia <- fieldData %>% 
  filter(species == "himantormia")

polytrichum <- fieldData %>% 
  filter(species == "polytrichum")

sanionia <- fieldData %>% 
  filter(species == "sanionia")

stereocaulon <- fieldData %>% 
  filter(species == "stereocaulon")

usneaAnt <- fieldData %>% 
  filter(species == "usneaAnt")

usneaAur <- fieldData %>% 
  filter(species == "usneaAur")






### combined coefficients for modelling
partopModels <- fieldData %>% 
                  group_by(species) %>% 
                  filter(waterContent > 60) %>% 
                  do(tidy(lm(aCorrected ~ partop, data = fieldData))) 


## checking linear model fit for partop and comparing to asymptotic exponential
andreaeaPARlm <- fieldData %>% 
  filter(species == "andreaea", waterContent > 60) %>% 
  lm(a ~ partop, data = .)

summary(andreaeaPARlm)                                                        # not a good fit
plot(andreaeaPARlm)

andreaeaPARexp <- fieldData %>% 
  filter(species == "andreaea", waterContent > 60) %>% 
  nls(a ~ SSasymp(partop, Asym, R0, lrc), data = .)                           # can't converge

chorisodontiumPARlm <- fieldData %>% 
  filter(species == "chorisodontium", waterContent > 60) %>% 
  lm(a ~ partop, data = .)

plot(chorisodontiumPARlm)
summary(chorisodontiumPARlm)


fieldData %>% 
  filter(species %in% c("andreaea", "chorisodontium"), waterContent > 60) %>% 
  ggplot(., aes(x = partop, y = aCorrected, color = species)) +
    geom_point() +
    geom_function(fun = function(x)                                            # andreaea
      partopModels$estimate[2] * x + partopModels$estimate[1], color = "red") +
  geom_function(fun = function(x)                                              # chorisodontium
    partopModels$estimate[4] * x + partopModels$estimate[2], color = "blue") +
    theme_classic()


view(partopModels)

# SANIONIA ----
## linear models
sanioniaPARlm <- fieldData %>% 
  filter(species == "sanionia", waterContent > 60) %>% 
  lm(a ~ partop, data = .)
summary(sanioniaPARlm)
plot(sanioniaPARlm)

fieldData %>% 
  filter(species == "sanionia", waterContent >= 60) %>% 
  ggplot(., aes(x = partop, y = aCorrected)) +
  geom_point() +
  geom_function(fun = function(x)                                           
    partopModels$estimate[10] * x + partopModels$estimate[9], color = "red") +
  theme_classic()

## trying nonlinear
### A ~ light
LCSan <- fieldData %>% 
  filter(species == "sanionia", 
         waterContent > 90,
         between(tempWS, 3, 6))

ggplot(LCSan, aes(x = partop, y = aCorrected, color = tempWS)) +   
  geom_point() +
  theme_classic()

sanLC <- nls(aCorrected ~ SSasymp(partop, Asym, R0, lrc), data = LCSan)
summary(sanLC)
plot(nlsResiduals(sanLC))
overview(sanLC)    

(LCSanPlot <- ggplot(LCSan, aes(x = partop, y = aCorrected)) +
    geom_point(pch = 1) + 
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.4) +
    xlab(expression("PPFD ( "*mu*~mol~m^-2~s^-1*")")) +
    ylab(expression("Net photosynthetic rate ( "*mu*~mol~m^-2~s^-1*")")) +
    geom_function(fun = function(x) 
      3.6479 + (0.3799 - 3.6479) * exp(-exp(-5.5157) * x)) +
    theme_cust())

ggsave("img\\field_light.png", plot = LCSanPlot, width = 8, height = 8)



### a ~ water content
WCSan <- fieldData %>% 
  filter(species == "sanionia", 
         between(tempWS, 3, 6),
         between(partop, 500, 1700))

sanWC <- lm(aCorrected ~ poly(waterContent, degree = 2, raw = TRUE), data = WCSan)
plot(sanWC)
summary(sanWC)

(sanWCPlot <- ggplot(WCSan, aes(x = waterContent, y = aCorrected)) +
    geom_point(pch = 1) +
    geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.4) +
    labs(x = "Water content (%)",
         y = expression("Net photosynthetic rate ( "*mu*~mol~m^-2~s^-1*")"),
         color = "PPFD") +
    geom_function(fun = function(x)                                              # t = 5
      -1.419e+01 + 1.484e-01 * x 
      + -2.762e-04 * x^2) +
    theme_cust())

ggsave("img\\field_water.png", plot = sanWCPlot, width = 8, height = 8)


### controlling partop to be above light sat
WCSan2 <- fieldData %>% 
  filter(species == "sanionia", 
         between(tempWS, 3, 6),
         partop > 550)

sanWC2 <- lm(aCorrected ~ poly(waterContent, degree = 2, raw = TRUE), data = WCSan2)
plot(sanWC2)
summary(sanWC2)

(sanWCPlot2 <- ggplot(WCSan2, aes(x = waterContent, y = aCorrected, color = partop)) +
    geom_point() +
    geom_function(fun = function(x)                                              # t = 5
      -7.119e+00 + 5.202e-02 * x 
      + 2.745e-05 * x^2, color = "red") +
    theme_classic())


### plotting factors through time
## A ~ time
(timeNP <- ggplot(sanionia, aes(x = (cumulativeSeconds/60/60/24), y = aCorrected)) +
              scale_x_continuous(expand = c(0, 0), limits = c(0, 42)) +
              xlab("Days") +
              ylab(expression("Net photosynthetic rate ( "*mu*~mol~m^-2~s^-1*")")) +
              geom_hline(yintercept = 0, linetype = "longdash", alpha = 0.4) +
              geom_point() +
              geom_line() +
              theme_cust())

## WC ~ time
(timeWC <- ggplot(sanionia, aes(x = (cumulativeSeconds/60/60/24), y = waterContent)) +
            labs(x = NULL, y = "Water content(%)") +
            scale_x_continuous(expand = c(0, 0), limits = c(0, 42)) +
            annotate("rect", xmin = 0, xmax = 42, ymin = 90, ymax = 130,
                     alpha = .1) +
            geom_point() +
            geom_line() +
            theme_cust() +
            theme(axis.text.x = element_blank()))

## tempWS ~ time
(timeTemp <- ggplot(sanionia, aes(x = (cumulativeSeconds/60/60/24), y = tempWS)) +
              labs(x = NULL, y = "Temperature (°C)") +
              scale_x_continuous(expand = c(0, 0), limits = c(0, 42)) +
              ylim(2, 6.2) +
              geom_point() +
              geom_line() +
              theme_cust() +
              theme(axis.text.x = element_blank()))

## partop ~ time
(timePAR <- ggplot(sanionia, aes(x = (cumulativeSeconds/60/60/24), y = partop)) +
             xlab(NULL) +
             ylab(expression("PPFD ( "*mu*~mol~m^-2~s^-1*")")) +
             geom_hline(yintercept = 480, alpha = 0.6, lty = "dotted") +  # light sat point from lab
             scale_x_continuous(expand = c(0, 0), limits = c(0, 42)) +
             geom_point() +
             geom_line() +
             theme_cust() +
             theme(axis.text.x = element_blank()))

## arranging together
environment <- timePAR / timeTemp / timeWC / timeNP

ggsave("img\\enviro.png", plot = environment, width = 14, height = 16)
