# exploring preliminary data
# productivity with light, temp, and water

# libraries ----
library(tidyverse)
library(skimr)

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

## adding column for spp.
andreaea_df$species <- "andreaea"
chorisodontium_df$species <- "chorisodontium"
himantormia_df$species <- "himantormia"
polytrichum_strictum_df$species <- "polytrichum_strictum"
sanionia_df$species <- "sanionia"
stereocaulon_df$species <- "stereocaulon"
usnea_antarctica_df$species <- "usnea_antarctica"
usnea_aurantiaco_atra_df$species <- "usnea_aurantiaco_atra"

## checking
View(andreaea_df)
View(chorisodontium_df)
View(himantormia_df)
View(polytrichum_strictum_df)
View(sanionia_df)
View(stereocaulon_df)
View(usnea_antarctica_df)
View(usnea_aurantiaco_atra_df)

## altering mismatched column names
chorisodontium_df <- chorisodontium_df %>% 
  mutate(weight = weight..incl..water.)

## combining data
all_00 <- bind_rows(andreaea_df, chorisodontium_df, himantormia_df, polytrichum_strictum_df,
                 sanionia_df, stereocaulon_df, # usnea_antarctica_df, 
                 usnea_aurantiaco_atra_df)

View(all)

## adding corrected area
all_01 <- all_00 %>% 
  mutate(area = case_when(all_00$species == "andreaea" ~ 6.9291, 

    all_00$species == "chorisodontium" ~ 107.80784, 

    all_00$species == "himantormia" ~ 15.83125,

    all_00$species == "polytrichum_strictum" ~ 52.85567, 

    all_00$speices == "sanionia" ~ 77.48568, 

    all_00$species == "stereocaulon" ~ 5.10035, 

    all_00$species == "usnea_aurantiaco_atra" ~ 43.84498))

View(all_01)
df %>% mutate(z = case_when(
  x < 3  & x > 1 & y < 6  & y > 3  ~ "apple" ,
  x < 6  & x > 4 & y < 9  & y > 7  ~ "ball"  ,
  x < 2  & x > 0 & y < 5  & y > 3  ~ "pine"  ,
  x < 12 & x > 7 & y < 15 & y > 11 ~ "orange"
)

## adjusting 'a' with correct areas



