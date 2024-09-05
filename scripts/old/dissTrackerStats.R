# analysing dissertation tracker
# 25 April 2024

# LIBRARIES ----
library(skimr)
library(tidyverse)

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

# DATA IMPORT ----
tracker <- read.csv("data\\diss_time.csv")

skim(tracker)

# DATA WRANGLING ----
tracker00 <- tracker %>% 
  mutate(day = c(1:87)) %>% 
  slice(1:(n()-3)) %>% 
  select(-c(10:12)) %>% 
  mutate(distress = (-1) * distress)

View(tracker00)


# VIS ----
## joy vs distress
ggplot(tracker00, aes(y = joy, x = day)) +
  labs(y = "Joy/distress level", x = "Day") +
  geom_hline(yintercept = 0, lty = "dashed", alpha = 0.5) +
  geom_bar(stat = "identity", fill = "green") +
  geom_bar(stat = "identity", aes(x = day, y = distress), fill = "red") +
  theme_cust()

## cumulative hours spent
### cumSums
tracker01 <- tracker00 %>% 
  mutate(cumSumHours = cumsum(hours),
         csSnacks = cumsum(replace_na(snacks, 0)),
         csDrinks = cumsum(replace_na(drinks, 0)),
         csEmails = cumsum(replace_na(emailsSent, 0)))

ggplot(tracker01, aes(x = day, y = cumSumHours)) +
  labs(x = "Days", y = "Cumulative hours") +
  geom_line() +
  theme_cust()

View(tracker01)

ggplot(tracker01, aes(x = day, y = csSnacks)) +
  labs(x = "Days", y = "Number consumed") +
  geom_line() +
  geom_line(aes(x = day, y = csDrinks), color = "red") +
  theme_cust()

