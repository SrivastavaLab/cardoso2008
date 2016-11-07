
## read from CSVs

abds<- read_csv("data/abundance.csv")

env <- read_csv("data/environmental_variables.csv")

macroinverts <- read_csv("data/macroinvertebrate_names_mass.csv")


abds %>% 
  left_join(macroinverts) %>% View
  group_by(morphospecies, Bromeliad) %>% 
  nest %>% 
  mutate(tot = map_dbl(data, ~ sum(.x[["abundance"]]))) %>% 
  glimpse


prednames <- read_csv("data/predator_names.csv")

## check names

abds %>% 
  left_join(macroinverts) %>% 
  left_join(prednames) %>% 
  select(field_name, morphospecies, Taxa) %>% 
  ## drop any without matches (ie predators)
  filter(!is.na(Taxa)) %>% 
  distinct %>% 
  View


occur_data <- abds %>% 
  left_join(macroinverts) %>% 
  left_join(prednames) %>% 
  ## drop any without matches (ie predators)
  filter(!is.na(Taxa))  %>%
  select(Bromeliad, Taxa, average_percapita_biomass, abundance) %>% 
  group_by(Taxa, Bromeliad)


occur_data %>% 
  mutate(metabolic_cap = (average_percapita_biomass * abundance) ^ 0.69) %>% 
  summarize(total_metabolic_cap = sum(metabolic_cap)) %>% 
  View

