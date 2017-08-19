surveys <-read.csv("data/portal_data_joined.csv")

install.packages("tidyverse")
library(tidyverse)

##
select(surveys, plot_id, species_id, weight)

##using filter selectrows where year is 1995
filter(surveys, year == 1995)

#Pipes!!!
## This is a pipe %>%

surveys_sml <- surveys %>% 
  filter(year == 1995)  %>% 
  select(plot_id, species_id, weight)

surveys_sml <- surveys %>% 
  filter(weight < 5)  %>% 
  select(plot_id, sex, weight)

surveys_sml

surveys %>% #mutate adds a column
  mutate(weight_kg = weight / 1000, 
         weight_kg2 = weight_kg * 2)  %>%
  tail

surveys %>% #mutate adds a column
  filter(!is.na(weight)) %>% 
 mutate(weight_kg = weight / 1000, 
         weight_kg2 = weight_kg * 2)  %>%
  head


#Challenge
#Create a new data frame from the surveys 
#data that meets the following criteria: 
#contains only the species_id column and a new column called 
#hindfoot_half containing values that are 
#half the hindfoot_length values. In this hindfoot_half column,
#there are no NAs and all values are less than 30.

surveys %>%
  mutate(hindfoot_half = hindfoot_length / 2) %>% #
  filter(!is.na(hindfoot_half), hindfoot_half < 30) %>% 
  select(species_id, hindfoot_half) %>%
  head

#group_by and summarize

surveys %>%
  filter(!is.na(weight), #Removing rows where weigh column has nas 
         sex =="F" | sex == "M")%>% # Also filter where sex is = to M or F
  group_by(sex, species_id)%>% #
  summarize(mean_weight = mean(weight, na.rm = T),
            min_weight = min(weight, na.rm = T))

weight <- surveys %>%
  select(weight)

##Challenge

## 1. How many individuals were caught in each plot_type surveyed?

## 2. Use group_by() and summarize() to find the mean, min, and 
## max hindfoot length for each species (using species_id).

## 3. What was the heaviest animal measured in each year? Return
## the columns year, genus, species_id, and weight.

## 4. You saw above how to count the number of individuals of each sex using a
## combination of group_by() and tally(). How could you get the same result using
## group_by() and summarize()? Hint: see ?n.

surveys %>%
  group_by(plot_type) %>%
  tally

surveys %>%
  filter(!is.na(hindfoot_length))%>%
  group_by(species_id)%>% 
  summarize(mean_hindfoot_length = mean(hindfoot_length, na.rm = T),
            min_hindfoot_length = min(hindfoot_length, na.rm = T), 
            max_hindfoot_length = max(hindfoot_length, na.rm = T))
surveys %>%
  select(year, genus, species_id, weight) %>%
  group_by(species_id)%>% 
  top_n(1, weight)

#Answer 2
surveys %>%
  filter(!is.na(weight))%>%
  select(year, genus, species_id, weight) %>%
  filter(!is.na(weight == max(weight)))%>%
  group_by(species_id)%>% 
  arrange(year)

#Challenge 4
surveys %>%
  group_by(sex)%>%
  summarize(n())

## EXPORTING DATA ----
surveys_complete <- surveys %>%
  filter(species_id != "",
         !is.na(weight), 
         !is.na(hindfoot_length),
          sex != "") #remove missing species_id, etc

#Extract the most common species_id

species_counts <- surveys_complete %>%
  group_by(species_id) %>% 
  tally %>%
  filter( n >= 50)

## Only keep the most common species

surveys_comm_spp <- surveys_complete %>%
  filter (species_id %in% species_counts$species_id)
  

write.csv(surveys_comm_spp, file = "data_output/surveys_complete.csv")


##Data Visualization

library(tidyverse)

surveys_complete <- read.csv('data_output/surveys_complete.csv')

## ggplot2

ggplot(data = surveys_complete, aes(x = weight, y = hindfoot_length)) + 
  geom_point(alpha = 0.1, aes(color = weight))

## Chalenge
## Create a scatter plot of weight over species_id
## with the plot types showing in different colors. Is this a 
## good way to show this type of data?

ggplot(data = surveys_complete, aes(y = weight, x = species_id)) + 
  geom_point(alpha = 0.9, aes(color = species_id)) # Alpha = 0 is completely translucent

ggplot(data = surveys_complete, aes(y = weight, x = species_id)) + 
  geom_boxplot(aes(color = species_id))+
labs(x = "species",
     y = "weight",
title = "Plot")


ggplot(data = surveys_complete, aes(y = weight, x = species_id)) + 
  geom_col(aes(color = species_id))+
  facet_grid(sex ~.)+ # creates two plots depending on sex (something in x and somethingn in y)
  labs(x = "species",
       y = "weight",
       title = "Plot")

##Time series ----
yearly_counts <-surveys_complete %>%
  group_by(year, species_id)%>%
  tally

ggplot(data = yearly_counts, 
       aes(x = year, y = n, 
           group = species_id,
           color = species_id)) +
geom_line()+
  facet_wrap(~ species_id) #Gives you a plot per species

yearly_sex_counts <- surveys_complete %>%
  group_by(year, species_id, sex) %>%
  tally

ggplot(data = yearly_sex_counts, 
       aes(x = year, y = n, color = sex)) +
       geom_line() +
  facet_wrap(~ species_id)
           
## Challenge
## Use what you just learned to create a plot that
## depicts how the average weight of each species
## changes through the years.


yearly_mean_weight <- surveys_complete %>%
  group_by(year, species_id) %>%
  summarize(mean_weight = mean(weight))
  
ggplot(data = yearly_mean_weight, 
        aes(x = year, y = mean_weight,
            color = species_id))+
  geom_line()+
    facet_wrap(~ species_id)+
    labs(x = "year",
         y = "mean_weight (g)")+
    theme_bw()
  
my_plot <- ggplot(data = yearly_mean_weight, 
                  aes(x = year, y = mean_weight,
                      color = species_id))+
  geom_line()+
  facet_wrap(~ species_id)+
  labs(x = "year",
       y = "mean_weight (g)")+
  theme_bw()

ggsave("my_plot.png", my_plot, width = 15, height = 10)