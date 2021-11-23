#week 9
#pipes
penguins_grouped <- group_by(penguins, species, sex)

penguins_grouped_full <- drop_na(penguins_grouped, sex)

penguins_flipper_length <- summarise(penguins_grouped_full, mean=mean(flipper_length_mm, na.rm=TRUE))

penguins_flipper_length

#alternative method for above code, gives identical output
#gives mean for each sex of each species 
penguins %>% 
  group_by(., species, sex) %>% 
  drop_na(., sex) %>% 
  summarise(., mean=mean(flipper_length_mm))

#removes the ., from each bracket as pipe function automatically uses the first set argument
#same output as above 2 chunks of code 
penguins %>% 
  group_by(species, sex) %>% 
  drop_na(sex) %>% 
  summarise(mean=mean(flipper_length_mm))
