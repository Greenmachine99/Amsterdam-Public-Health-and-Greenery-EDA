### Checking Libraries
library('rio')
library('dplyr')
library('ggplot2')

### Importing Datasets
population = import('./datasets/amsterdam_population.csv')
health = import('./datasets/amsterdam_health.csv')
public_space = import('./datasets/amsterdam_publicspace.csv')
housing = import('./datasets/amsterdam_housing.csv')
income = import('./datasets/amsterdam_income.csv')
social = import('./datasets/amsterdam_social.csv')

### Knitting Datasets Together
health_pubspace = inner_join(health, public_space)
pop_health = inner_join(population, health)
pop_pubspace = inner_join(population, public_space)
soc_health = inner_join(health, social)

all_data = inner_join(pop_health, health_pubspace)
all_data = inner_join(all_data, housing)
all_data = inner_join(all_data, income)
all_data = inner_join(all_data, social)

### Summarise Datasets

all(complete.cases(health))
all(complete.cases(public_space))
all(complete.cases(population))

### Overall stats
all_data %>%
  filter(year == 2020) %>%
  summarise(population = sum(pop_total),
            avg_health = mean(hea_good),
            avg_greenery = mean((pub_area_green / (pub_area_land + pub_area_water))),
            avg_income = mean(inc_disposable),
            tot_social_facilities = sum(soc_fac_cultural) + sum(soc_fac_sports),
            avg_hou_value = mean(hou_value)) 

all_data %>% 
  filter(year == 2020) %>%
  group_by(district) %>%
  summarise(population = sum(pop_total),
            avg_health = mean(hea_good),
            avg_greenery = mean((pub_area_green / (pub_area_land + pub_area_water))),
            avg_income = mean(inc_disposable),
            tot_social_facilities = sum(soc_fac_cultural) + sum(soc_fac_sports),
            avg_hou_value = mean(hou_value)) %>%
  arrange(desc(population))

  ### Overall Population Stats
  ggplot(population, aes(reorder(district, (pop_unemployed / pop_laborforce) * 100, FUN = median), pop_unemployed / pop_laborforce, fill = district)) +
    geom_boxplot(outlier.shape = NA) +
    scale_y_log10()

  ### Overall Health Stats
    ggplot(health, aes(reorder(district, hea_good, FUN = median), hea_good, fill = district)) +
      geom_violin() 
  
    ggplot(health, aes(reorder(district, hea_overweight, FUN = median), hea_overweight, fill = district)) +
      geom_violin()
  
    ggplot(health, aes(reorder(district, hea_psych, FUN = median), hea_psych, fill = district)) +
      geom_violin()
    
    ### Overall Public Space Stats
    ggplot(public_space, aes(reorder(district, pub_area_green, FUN = median), pub_area_green, fill = district)) +
      geom_violin() +
      scale_y_log10()
    
    ggplot(public_space, aes(reorder(district, pub_area_sports, FUN = median), pub_area_sports,fill = district)) +
      geom_violin() +
      scale_y_log10()
    
    ggplot(public_space, aes(district, fill = pub_setup)) +
      geom_bar(position = 'fill')

  ### Plotting Health vs Greenery

    ### Overall Health vs Actual Greenery
    health_pubspace %>%
      group_by(district) %>%
      summarise(avg_greenery = mean(pub_area_green),
                avg_health = mean(hea_good),
                avg_psyhea = mean(hea_psych),
                avg_obesity = mean(hea_overweight),
                avg_lone = mean(hea_lonely)) %>%
      arrange(avg_greenery) %>%
      ggplot(aes(avg_greenery, avg_health, color = district)) + 
      geom_point()

    ggplot(health_pubspace, aes(hea_good, reorder(district, pub_area_green, FUN = mean))) +
      geom_violin()
    
    ### Scatter Plot Health vs Greenery
    ggplot(health_pubspace, aes(100 - hea_good, pub_area_green)) +
      geom_point(alpha = 0.1) +
      scale_y_log10() +
      labs(x = "Population with Bad Health", y = "Public Greenery (m2)")
    
    ### Overall Health vs Perceived Greenery
    ggplot(health_pubspace, aes(hea_good, fill = pub_main_green)) + 
      geom_histogram(alpha = 0.5, position = 'identity')

    ### Psychological Health vs Actual Greenery
    ggplot(health_pubspace, aes(pub_area_green, hea_psych)) +
      geom_point(alpha = 0.1) +
      facet_wrap(~district)
    
    ### Psychological Health vs Perceived Aesthetics Environment ###
    ggplot(health_pubspace, aes(hea_good, fill = pub_setup)) + 
      geom_density(alpha = 0.2, position = 'identity') +
      facet_wrap(~district)
    
    ggplot(health_pubspace, aes(hea_good, fill = pub_setup)) + 
      geom_density(alpha = 0.2, position = 'identity') +
      coord_flip()
    
    public_space %>%
      filter(year == 2017) %>%
      ggplot(aes(district, fill = pub_setup)) + 
      geom_bar(position = 'fill')
    
    ### Perceived Greenery vs Greenery vs Psychological Health
    
  
  ### Plotting Unemployment vs Health
    
    ### Psychological Health
    ggplot(all_data, aes(pop_unemployed, hea_psych, color = district)) +
      geom_point(alpha = 0.1)
  
  ### Plotting Health vs Sports Facilities
  
    ### Obesity
    ggplot(all_data, aes(hea_overweight, pub_area_sports)) + 
      geom_point(alpha = 0.1)
  
  
### Checking for Progress

  ### Nieuw West
  health_pubspace %>%
    filter(district == 'F Nieuw-West') %>%
    ggplot(aes(year, fill = pub_setup)) +
    geom_bar()
  health_pubspace %>%
    filter(district == 'F Nieuw-West') %>%
    ggplot(aes(year, pub_main_green, group = year)) +
    geom_boxplot()
  health_pubspace %>%
    filter(district == 'F Nieuw-West') %>%
    ggplot(aes(year, hea_psych, group = year)) +
    geom_boxplot()
  health_pubspace %>%
    filter(district == 'F Nieuw-West') %>%
    ggplot(aes(year, hea_lonely, group = year)) +
    geom_boxplot()

  ### Oost
  health_pubspace %>%
    filter(district == 'M Oost') %>%
    ggplot(aes(year, fill = pub_setup)) +
    geom_bar()
  health_pubspace %>%
    filter(district == 'M Oost') %>%
    ggplot(aes(year, pub_main_green, group = year)) +
    geom_boxplot()
  health_pubspace %>%
    filter(district == 'M Oost') %>%
    ggplot(aes(year, hea_psych, group = year)) +
    geom_boxplot()
  health_pubspace %>%
    filter(district == 'M Oost') %>%
    ggplot(aes(year, hea_lonely, group = year)) +
    geom_boxplot()
  
  ### Zuidoost
  health_pubspace %>%
    filter(district == 'T Zuidoost') %>%
    ggplot(aes(year, fill = pub_setup)) +
    geom_bar()
  health_pubspace %>%
    filter(district == 'T Zuidoost') %>%
    ggplot(aes(year, pub_main_green, group = year)) +
    geom_boxplot()
  health_pubspace %>%
    filter(district == 'T Zuidoost') %>%
    ggplot(aes(year, hea_psych, group = year)) +
    geom_boxplot()
  health_pubspace %>%
    filter(district == 'T Zuidoost') %>%
    ggplot(aes(year, hea_lonely, group = year)) +
    geom_boxplot()

  ### Zoom in F Nieuw-West (Buurt 5 Noord)
  health_pubspace %>%
    filter(district == 'F Nieuw-West' & pub_setup == '1.ugly') %>%
    ggplot(aes(reorder(neighborhood, pub_setup, FUN = median), fill = pub_setup)) +
    geom_bar()
  
  health_pubspace %>%
    filter(neighborhood == 'Buurt 5  Noord') %>%
    ggplot(aes(year, fill = pub_setup)) + 
    geom_bar()
  
  health_pubspace %>%
    filter(neighborhood == 'Buurt 5  Noord') %>%
    ggplot(aes(year, pub_main_green)) +
    geom_point()
  
  health_pubspace %>%
    filter(neighborhood == 'Buurt 5  Noord') %>%
    ggplot(aes(year, hea_psych)) + 
    geom_point()
  health_pubspace %>%
    filter(neighborhood == 'Buurt 5  Noord') %>%
    ggplot(aes(year, hea_lonely)) + 
    geom_point()
  
  ### Zoom in M Oost (Weespertrekvaart)
  health_pubspace %>%
    filter(district == 'M Oost' & pub_setup == '1.ugly') %>%
    ggplot(aes(reorder(neighborhood, pub_setup, FUN = median), fill = pub_setup)) +
    geom_bar()
  
  health_pubspace %>%
    filter(neighborhood == 'Weespertrekvaart') %>%
    ggplot(aes(year, fill = pub_setup)) + 
    geom_bar()
  
  health_pubspace %>%
    filter(neighborhood == 'Weespertrekvaart') %>%
    ggplot(aes(year, pub_main_green)) +
    geom_point()
  
  health_pubspace %>%
    filter(neighborhood == 'Weespertrekvaart') %>%
    ggplot(aes(year, hea_psych)) + 
    geom_point()
  health_pubspace %>%
    filter(neighborhood == 'Weespertrekvaart') %>%
    ggplot(aes(year, hea_lonely)) + 
    geom_point()
  
### Health & Wealth
ggplot(all_data, aes(inc_disposable, hea_good)) +
  geom_point(alpha = 0.1)
ggplot(all_data, aes(hou_value, hea_good)) + 
  geom_point(alpha = 0.1)
ggplot(all_data, aes(reorder(district, inc_disposable, FUN = median), hea_good)) +
  geom_boxplot()

ggplot(all_data, aes(inc_disposable, hea_psych)) +
  geom_point(alpha = 0.1)
ggplot(all_data, aes(hou_value, hea_psych)) + 
  geom_point(alpha = 0.1)
ggplot(all_data, aes(reorder(district, inc_disposable, FUN = median), hea_psych)) +
  geom_boxplot()

ggplot(all_data, aes(inc_disposable, hea_lonely)) +
  geom_point(alpha = 0.1)
ggplot(all_data, aes(hou_value, hea_lonely)) + 
  geom_point(alpha = 0.1)
ggplot(all_data, aes(reorder(district, inc_disposable, FUN = median), hea_lonely)) +
  geom_boxplot()

ggplot(all_data, aes(inc_disposable, hea_overweight)) + 
  geom_point(alpha = 0.1)
ggplot(all_data, aes(hou_value, hea_overweight)) +
  geom_point(alpha = 0.1)
ggplot(all_data, aes(reorder(district, inc_disposable, FUN = median), hea_overweight)) +
  geom_boxplot()

### Health & Facilities
soc_health %>%
  mutate(tot_facilities = soc_fac_cultural + soc_fac_sports) %>%
  ggplot(aes(tot_facilities, hea_good)) + 
  geom_point(alpha = 0.1)
soc_health %>%
  mutate(tot_facilities = soc_fac_cultural + soc_fac_sports) %>%
  ggplot(aes(reorder(district, tot_facilities, FUN = mean), hea_good)) + 
  geom_boxplot()

soc_health %>%
  mutate(tot_facilities = soc_fac_cultural + soc_fac_sports) %>%
  ggplot(aes(tot_facilities, hea_psych)) + 
  geom_point(alpha = 0.1)
soc_health %>%
  mutate(tot_facilities = soc_fac_cultural + soc_fac_sports) %>%
  ggplot(aes(reorder(district, tot_facilities, FUN = mean), hea_psych)) + 
  geom_boxplot()

soc_health %>%
  mutate(tot_facilities = soc_fac_cultural + soc_fac_sports) %>%
  ggplot(aes(tot_facilities, hea_lonely)) + 
  geom_point(alpha = 0.1)
soc_health %>%
  mutate(tot_facilities = soc_fac_cultural + soc_fac_sports) %>%
  ggplot(aes(reorder(district, tot_facilities, FUN = mean), hea_lonely)) + 
  geom_boxplot()

soc_health %>%
  mutate(tot_facilities = soc_fac_cultural + soc_fac_sports) %>%
  ggplot(aes(tot_facilities, hea_overweight)) + 
  geom_point(alpha = 0.1)
soc_health %>%
  mutate(tot_facilities = soc_fac_cultural + soc_fac_sports) %>%
  ggplot(aes(reorder(district, tot_facilities, FUN = mean), hea_overweight)) + 
  geom_boxplot()


### Health vs Unemployment
pop_health %>%
  mutate(per_unemployed = pop_unemployed / pop_laborforce) %>%
  ggplot(aes(reorder(district, per_unemployed, FUN = median), hea_good)) + 
  geom_boxplot()

pop_health %>%
  mutate(per_unemployed = pop_unemployed / pop_laborforce) %>%
  ggplot(aes(per_unemployed, hea_good)) + 
  geom_point()

### Health vs Age
pop_health %>%
  mutate(elder_pop = (pop_60_69 + pop_70_79 + pop_80_89 + pop_90) / pop_total) %>%
  ggplot(aes(reorder(district, elder_pop, FUN = mean), hea_good, fill = district)) +
  geom_boxplot()

###Correlation between self-perceived good health and hectares of natural terrains.

health_pubspace %>%
  filter(year == 2020) %>%
  select(district,pub_area_green, hea_good) 

ggplot(health_pubspace, aes(pub_area_green, hea_good)) +
  geom_point() +
  facet_wrap(~district)

###Correlation between obesity and hectares of natural terrains.
health_pubspace %>%
  filter(year == 2020) %>%
  select(district,pub_area_green, hea_overweight) 

ggplot(health_pubspace, aes(pub_area_green, hea_overweight)) +
  geom_point() +
  facet_wrap(~district)

### Correlation between loneliness and hectares of natural terrains.
health_pubspace %>%
  filter(year == 2020) %>%
  select(district,pub_area_green, hea_lonely) %>%
  ggplot(aes(pub_area_green, hea_lonely)) +
  geom_point() +
  facet_wrap(~district)

### Correlation between perceived quality of the living env and self-perceived health

health_pubspace %>%
  filter(year == 2020) %>%
  select(district,pub_setup, hea_good) %>% 
  ggplot(aes(pub_setup, hea_good)) +
  geom_point() +
  facet_wrap(~district)

### Crrelation between average self-perceived looks of the green spaces and self-perceived health

health_pubspace %>%
  filter(year == 2020) %>%
  select(district,pub_green, hea_good) %>%
  ggplot(aes(pub_green, hea_good)) +
  geom_boxplot() +
  facet_wrap(~district)


### Greenery
ggplot(public_space, aes(district, fill = pub_area_green, color = pub_setup)) +
  geom_bar()

population %>%
  filter(district == "F Nieuw-West") %>%
  ggplot(aes(year, pop_total, group = year)) + 
  geom_boxplot()

### Environment vs Greenery
public_space %>%
  mutate(greenery_per = pub_area_green / (pub_area_land + pub_area_water)) %>%
  ggplot(aes(greenery_per, fill = pub_setup)) +
  geom_histogram(alpha = 0.75) +
  scale_y_log10()

ggplot(public_space, aes(pub_area_green, fill = pub_setup)) +
  geom_bar() + 
  scale_y_log10()

### Greenery vs Public Setup Bridge
public_space %>%
  ggplot(aes(pub_green, fill = pub_setup)) +
  geom_bar(position = 'fill')

### Perception Pub Setup per District
public_space %>%
  group_by(year) %>%
  ggplot(aes(year, fill = pub_setup)) +
  geom_bar(position = 'fill') + 
  facet_wrap(~ district)

### Average Income
income %>%
  group_by(district) %>%
  ggplot(aes(reorder(district, inc_disposable, FUN = median), inc_disposable)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = '')

health_pubspace %>%
  group_by(district) %>%
  summarise(avg_greenery = mean(pub_area_green / (pub_area_land + pub_area_water)),
            avg_health = mean(hea_good),
            avg_psyhea = mean(hea_psych),
            avg_lonely = mean(hea_lonely),
            avg_obesity = mean(hea_overweight)) %>%
  arrange(avg_greenery) %>%
  ggplot(aes(avg_greenery, avg_health, color = district)) +
  geom_point() + 
  labs(x = 'Average Greenery (m2)', y = 'Average % of Population with Good Health' )

health_pubspace %>%
  group_by(district) %>%
  summarise(avg_greenery = mean(pub_area_green / (pub_area_land + pub_area_water)),
            avg_health = mean(hea_good),
            avg_psyhea = mean(hea_psych),
            avg_lonely = mean(hea_lonely),
            avg_obesity = mean(hea_overweight)) %>%
  arrange(avg_greenery) %>%
  ggplot(aes(avg_greenery, avg_psyhea, color = district)) +
  geom_point() + 
  labs(x = 'Average Greenery (m2)', y = 'Average % of Population with Good Health' )

all_data %>%
  ggplot(aes(reorder(district, pub_area_green, FUN = mean), fill = pub_setup)) +
  geom_bar(position = 'fill') + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = '')
  
  