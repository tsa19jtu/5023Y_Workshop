library(tidyverse)
library(lubridate)

penguins <- read_csv("data/penguins_simple.csv") #import data 
penguins %>% 
  select(flipper_length_mm)

penguins <- penguins %>% 
  select(-flipper_length_mm) ## removes the flipper length mm from data set 

penguins %>% 
  select(species:flipper_length_mm)

penguins %>% 
  select(starts_with("b")) ## keeps columns startign with the letter b and shows them 

penguins <- penguins %>% 
  mutate(date_proper=dmy(date))
penguins %>% 
  mutate(sex=case_when(sex == "male" ~ "M",
                       sex == "female" ~ "F")) 
#unless the results of mutate are assigned to an object using <- the changes wont be saved

