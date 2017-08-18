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
  top_n(1, weight)






