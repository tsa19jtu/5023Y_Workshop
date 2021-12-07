#week 9
library(patchwork)
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
  arrange(desc(years))

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



#chapter 10 
glimpse(penguins)
adelie_penguins <- penguins %>% 
  filter(species=="Adelie")

adelie_summary <- adelie_penguins %>% 
  summarise(mean=mean(body_mass_g, 
                      na.rm=TRUE))

ggplot()+
  geom_histogram(data= adelie_penguins,
                 aes(x=body_mass_g),
                 bins=10)+ #remember it is a good idea to try multiple bins of data
  geom_vline(data=adelie_summary,
             aes(xintercept=mean),
             colour="red",
             linetype="dashed")
#graph showing that the data is normally distributed , with a red intercept line of mean

ggplot()+
  geom_histogram(data=adelie_penguins,
                 aes(x=body_mass_g),
                 bins=50, # fifty bins
                 fill="steelblue",
                 colour="darkgrey",
                 alpha=0.8)+
  labs(x="Body mass (g) of Adelie penguins",
       y = "Count")

#separate below graph by sex
adelie_penguins %>% 
  drop_na(sex) %>% 
  ggplot()+
  geom_histogram(aes(x=body_mass_g,
                     fill=sex),
                 bins=50, # fifty bins
                 colour="darkgrey",
                 alpha=0.8,
                 position="identity")+
  labs(x="Body mass (g) of Adelie penguins",
       y = "Count")

#shows density within the dataset
adelie_penguins %>% 
  drop_na(sex) %>% 
  ggplot()+
  geom_density(aes(x=body_mass_g,
                   fill=sex),
               colour="darkgrey",
               alpha=0.8,
               position="identity")+
  labs(x="Body mass (g) of Adelie penguins",
       y = "Count")

#separate by sex on the y axis 
adelie_penguins %>% 
  drop_na(sex) %>% 
  ggplot()+
  ggridges::geom_density_ridges(aes(x=body_mass_g,
                                    y=sex),
                                alpha=0.8)
library(ggridges) #needed to be installed to separate by sex on y axis 
adelie_penguins %>% 
  drop_na(sex) %>% 
  ggplot()+
  ggridges::geom_density_ridges(aes(x=body_mass_g,
                                    y=sex),
                                alpha=0.8)

#10.3 descriptive stats
penguin_body_mass_summary <- penguins %>% 
  summarise(mean_body_mass=mean(body_mass_g, na.rm=T),
            median_body_mass=median(body_mass_g, na.rm=T))

penguin_body_mass_summary
#1x2 tibble of mean and median body masses 

#IQR
penguins %>% 
  summarise(IQR_body_mass = IQR(body_mass_g, na.rm=TRUE))
#shows all quartiles (25, 50, 75)
penguins %>%
  summarise(q_body_mass = quantile(body_mass_g, c(0.25, 0.5, 0.75), na.rm=TRUE),
            quantile = scales::percent(c(0.25, 0.5, 0.75))) 
# scales package allows easy converting from data values to perceptual properties

#10.3.3 visualising dispersion 
#box and whisker plot of bodyweight vs mass with colours and set width and theme
penguins %>% 
  ggplot()+
  geom_boxplot(aes(x="",
                   y= body_mass_g),
               fill="darkorange",
               colour="steelblue",
               width=0.4)+
  labs(x= "Bodyweight",
       y = "Mass (g)")+
  theme_minimal()

#10.3.4 combining histograms and box plots
library(patchwork) # put this at the TOP of your script

penguins_na_sex <- penguins %>% 
  drop_na(sex)

colours <-  c("darkorange", "cyan") # set colour scheme here to save on repeating code
lims <- c(3000,6000) # set axis limits here to save on repeating code

p1 <- ggplot(data = penguins_na_sex,
             aes(x = species,
                 y = body_mass_g,
                 fill = sex))+
  geom_boxplot()+
  scale_fill_manual(values = colours)+
  scale_y_continuous(limits=lims)+
  labs(x="",
       y="")+
  coord_flip()+ # rotate box plot 90 degrees
  theme_minimal()+
  theme(legend.position="none")

p2 <- ggplot(data = penguins_na_sex,
             aes(x = body_mass_g,
                 y = species,
                 fill = sex))+
  ggridges::stat_density_ridges(quantile_lines = TRUE)+
  scale_fill_manual(values = colours)+
  scale_x_continuous(limits=lims)+
  labs(y="",
       x = "Body Mass (g)")+
  theme_minimal()

(p1/p2) # patchwork command to layer one plot above the other

#10.4 categorical variables 
# show what species are in the data 
penguins %>% 
  distinct(species)
# count how many are in each species
penguins %>% 
  count(species, sort=TRUE)
# show some quick data summaries (int and dbl)
prob_obs_species <- penguins %>% 
  count(species, sort=TRUE) %>% 
  mutate(prob_obs = n/sum(n))

prob_obs_species
#shows about 44% of data is from adelie 
#bar for count of species 
penguins %>% 
  ggplot()+
  geom_bar(aes(x=species))

#stacked bar to show % add up to 100% of the data 
penguins %>% 
  ggplot(aes(x="",
             fill=species))+ # specify fill = species to ensure colours are defined by species
  geom_bar(position="fill")+ # specify fill forces geom_bar to calculate percentages
  scale_y_continuous(labels=scales::percent)+ #use scales package to turn y axis into percentages easily
  labs(x="",
       y="")+
  theme_minimal()

#improved stacked bar
#species on y axis (flipped), % labels, and good colour
penguins %>% 
  mutate(species=factor(species, levels=c("Adelie",
                                          "Gentoo",
                                          "Chinstrap"))) %>% # set as factor and provide levels
  ggplot()+
  geom_bar(aes(x=species),
           fill="steelblue",
           width=0.8)+
  labs(x="Species",
       y = "Number of observations")+
  geom_text(data=prob_obs_species,
            aes(y=(n+10),
                x=species,
                label=scales::percent(prob_obs)))+
  coord_flip()+
  theme_minimal() 
#end of chapter 10 30th November 2021

#chapter 11, 7th December 2021
library(rstatix)

penguins %>% 
  cor_test(bill_length_mm, bill_depth_mm)
# shows the strength of correlation 1 is positive, -1 is negative
penguins %>% select(bill_length_mm, 
                    bill_depth_mm) %>% 
  drop_na() %>% #remove na
  mutate(rank_length=dense_rank((bill_length_mm)), 
         rank_depth=dense_rank((bill_depth_mm)))
#ranks from lowest to highest for bill length and depth
penguins %>% 
  cor_test(bill_length_mm, bill_depth_mm, method="spearman")
#spearmans rank correlation, specify with method 
#shows that pearsons and spearmans are almost identical 
length_depth_scatterplot <- ggplot(penguins, aes(x= bill_length_mm, 
                                                 y= bill_depth_mm)) +
  geom_point()

length_depth_scatterplot
#length and depth scatterplot for bills, define axis, geom point
library(patchwork) # package calls should be placed at the TOP of your script

bill_depth_marginal <- penguins %>% 
  ggplot()+
  geom_density(aes(x=bill_depth_mm), fill="darkgrey")+
  theme_void()+
  coord_flip() # this graph needs to be rotated

bill_length_marginal <- penguins %>% 
  ggplot()+
  geom_density(aes(x=bill_length_mm), fill="darkgrey")+
  theme_void()

layout <- "
AA#
BBC
BBC"
# layout is easiest to organise using a text distribution, where ABC equal the three plots in order, and the grid is how much space they take up. We could easily make the main plot bigger and marginals smaller with

# layout <- "
# AAA#
# BBBC
# BBBC"
# BBBC

bill_length_marginal+length_depth_scatterplot+bill_depth_marginal+ # order of plots is important
  plot_layout(design=layout) # uses the layout argument defined above to arrange the size and position of plots
#produes the length and depth scatterplot with density graph along the edges

