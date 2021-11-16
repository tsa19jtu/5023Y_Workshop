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

penguins %>% 
  filter(sex == "female", #specified sex
         species == "Adelie", #specified species
         flipper_length_mm < 180) #specified flipper length less then 180 mm

penguins %>% 
  filter(species == "Adelie", #specified species 
         flipper_length_mm <180 |  #specified less than 180 mm
           flipper_length_mm >200) # specified more than 200mm, so gives a range of flipper lengths 
#less than 180mm or more than 200mm in female adelie penguins 

penguins %>% 
  filter(species == "Adelie",
         between(flipper_length_mm, 180,200)) #alternative to above, shows flipper length between 180 and 200mm

penguins_ordered <- penguins %>% 
  arrange(sex,body_mass_g)

# arrange data first by sex - all females then all males, within each sex order body mass from low to high

penguins_ordered <- penguins %>% 
  arrange(sex,body_mass_g)

# arrange data first by sex - all females then all males, within each sex order body mass from low to high

penguins_ordered %>% 
  select(penguin_id, sex, body_mass_g)
# view just a few variables

penguins_reverse_ordered <- penguins %>% 
  arrange(desc(sex,body_mass_g))



penguins_reverse_ordered %>% 
  select(penguin_id, sex, body_mass_g)


# we can also apply this to specific variables
penguins_reverse_ordered <- penguins %>% 
  arrange(sex,desc(body_mass_g))
