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

#strings
#for all names in penguins replace lower case e with capital E (category headings)
str_replace_all(names(penguins), c("e"= "E"))

#separate
#create data 'tibble' and then separate into one column
df <- tibble(label=c("a-1", "a-2", "a-3")) 
#make a one column tibble
df
#separates out into 2 columns of treatment and replicate
df %>% 
  separate(label, c("treatment", "replicate"), sep="-")

#more stringr
penguins %>% 
  mutate(species=str_to_upper(species))
#removes the letter e from all species names 
penguins %>% 
  mutate(species=str_remove_all(species, "e"))
#trim leading or tailing data (spaces before or after) as considered different values by R
df2 <- tibble(label=c("penguin", " penguin", "penguin ")) 
df2
#return the names of all different levels it can find in the data frame
df2 %>% 
  distinct()
#pipe data through trim and pipe to distinct then white space is removed and R recognises it as one level
df2 %>% 
  mutate(label=str_trim(label, side="both")) %>% 
  distinct()
#find all penguin ids that begin with N1 using filter and distinct 
penguins %>% 
  filter(grepl("N1", individual_id)) %>% 
  distinct(individual_id)

#date and time, make sure date names selected match the file you're pulling from 
penguins %>% 
  select(date, date_proper) %>% 
  head()
#min and max date, so earliest and latest date summary 
penguins %>% 
  summarise(min_date=min(date_proper),
            max_date=max(date_proper))

#how many times was each penguin measured and across what time period
#make sure id and dates match the data set 
penguins %>% 
  group_by(penguin_id) %>% 
  summarise(min=min(date_proper), 
            max=max(date_proper), 
            difference = max-min, 
            n=n())

penguins %>% 
  group_by(penguin_id) %>% 
  summarise(min=min(date_proper), 
            max=max(date_proper), 
            years = (max-min)/dyears(1), #converts into years by max-min 
            n=n()) %>% 
  arrange(desc(difference))

#9.5 pivot 
#makes a table with tibble data, how many penguins from each species eat each type of prey 
#df3 means make a table with 3 rows
penguin_id <- c("N1A1","N1A2","N2A1")
krill <- c(70,20,43)
fish <- c(5, 19,4)
squid <- c(11,5,0)


df3 <- tibble(penguin_id, krill, fish, squid)
df3

#make into tidy format where each row is a unique observation
#longer data frame with fewer columns
df3_long <- df3 %>% 
  pivot_longer(cols=(krill:squid), names_to = "prey_type", values_to = "consumption_per_week")
df3_long

#ggplot data from a tidy data format 
df3_long %>% #pipe df3 long 
  ggplot(aes(x=penguin_id, y=consumption_per_week, fill=prey_type))+ #ggplot, define axis and fill
  geom_bar(stat="identity", position=position_dodge()) #geom bar for graph type 
