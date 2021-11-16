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

penguins %>% 
  summarise(n_distinct(penguin_id)) #number of distinct penguins in data set 

penguins %>% 
  group_by(species, sex) %>% 
  summarise(n_distinct(penguin_id)) #number of each sex in each species 

penguins %>% 
  summarise(mean_flipper_length = mean(flipper_length_mm, na.rm=TRUE),
            mean_bill_length = mean(bill_length_mm, na.rm=TRUE))
#summary of mean flipper and bill lengths 

penguins %>% 
  summarise(n=n(),
            num_penguins = n_distinct(penguin_id),
            mean_flipper_length = mean(flipper_length_mm, na.rm=TRUE),
            prop_female = sum(sex == "female", na.rm=TRUE) / n())
#summary of number of penguins, mean flipper length, prop female 

penguins %>% 
  summarise(across(.cols = everything(), 
                   .fns = ~sum(is.na(.)))) %>% 
  glimpse()
#summary across columns so find out how many NAs there are 

# the mean of ALL numeric columns in the data, where(is.numeric) hunts for numeric columns

penguins %>% 
  summarise(across(.cols = where(is.numeric), 
                   .fns = ~mean(., na.rm=TRUE)))
#mean of all numeric columns in the data

# number of distinct penguins, as only one column contains the word penguin

penguins %>% 
  summarise(across(.cols = contains("penguin"), 
                   .fns = ~n_distinct(.))) %>% 
  glimpse()
#more complex way of running n_distinct for number of individual penguins 

penguin_stats <- penguins %>% 
  group_by(penguin_id) %>% 
  summarise(num=n())
#reassign penguins to penguin_stats, group by penguin id and summarise number, how many times each penguin monitored

penguins_grouped <- penguins %>% 
  group_by(sex, species) #penguins grouped, by sex and species 

penguin_summary <- penguins_grouped %>% 
  summarise(mean_flipper = mean(flipper_length_mm, na.rm=TRUE))
#calculate mean flipper length in each of the 6 combinations, species and sex 

centered_penguins <- penguins %>% 
  group_by(sex, species) %>% 
  mutate(flipper_centered = flipper_length_mm-mean(flipper_length_mm, na.rm=TRUE))
#creating a group centered mean 