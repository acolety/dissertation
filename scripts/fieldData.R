# analysis of field data
# 8 March 2024

# TO DO: redo plots removing low moisture samples, photosynth through time

# libraries ----
library(tidyverse)
library(skimr)

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
         inside.fan = as.factor(inside.fan))

view(fieldData)  
skim(fieldData)

## adding and correcting columns
fieldData <- fieldData %>% 
  mutate(species = case_when(object == 1 ~ "chorisodontium",                  # column of species names
                             object == 2 ~"sanionia",
                             object == 3 ~ "polytrichum",
                             object == 4 ~ "andreaea",
                             object == 5 ~ "stereocaulon",
                             object == 6 ~ "usneaAnt",
                             object == 7 ~ "usneaAur",
                             object == 8 ~ "himantormia",
                             object == 12 ~ "usneaAnt0")) %>% 
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
                               species == "usneaAnt" ~ 12.081,
                               species == "usneaAur" ~ 4.202,
                               species == "himantormia" ~ 1.288)) %>% 
  mutate(waterContent = ((weight - dryWeight) / dryWeight) * 100) %>%         # content as a percent of dry weight
  mutate(waterContentmm = ((weight - dryWeight) * 1000) / (area * 100))       # p sure this is good but double check           
  
view(fieldData)


ggplot(fieldData, aes(x = partop, y = aCorrected, color = species)) +
  facet_wrap(~ species, nrow = 5, ncol = 2, scales = "free") +
  geom_point(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(show.legend = FALSE) +
  theme_classic()

ggplot(fieldData, aes(x = tempWS, y = aCorrected, color = species)) +
  facet_wrap(~ species, nrow = 5, ncol = 2, scales = "free") +
  geom_point(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(show.legend = FALSE) +
  theme_classic()

ggplot(fieldData, aes(x = waterContent, y = aCorrected, color = species)) +
  facet_wrap(~ species, nrow = 5, ncol = 2, scales = "free") +
  geom_point(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "red") +
  geom_smooth(show.legend = FALSE) +
  theme_classic()


