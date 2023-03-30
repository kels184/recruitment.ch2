## ---- loadFunctions
source('../R/functions.R') # includes setup fns, packages, brms DHARMA fns and SUYR Prior_Posterior fn
## ----end

## ---- Joining for LW

## Read and factorise fish ======================================================
fishdata1 <- read_csv(paste0(DATA_PATH,
                             "primary/fish1.csv"),
                      trim_ws=TRUE)

fishdata <- fishdata1 %>% 
  select(-c(Other, Replacement, `Patch note`, Transparency, `ID note`, `Behaviour note`)) %>% 
  
  mutate(Date = dmy(Date), #using lubridate, convert Date to proper date format
  ) %>% 
  mutate_at(c(2:5), ## in the specified cols
            ~replace_na(.,"empty")) %>%   ## replace NAs with "empty" (there were no fish)
  mutate_at(c(2:5), ~factor(.) ) %>% 
  filter(Family != "Apogonidae")   ## removing apogonidae as they were not counted consistently (and are cryptic)


## add some length weight parameters to the dataset ============================= 
seychelles.fish <- read_csv(paste0(DATA_PATH,
                                   "primary/Seychelles.fish.csv"),
                            trim_ws = TRUE)
glimpse(seychelles.fish)

sey.fish <- seychelles.fish %>% 
  dplyr::select(Family, Species, a, b) %>% ##select only these cols
  mutate(across(where(is.character), as.factor) ) %>% ##factorise Family and species
  distinct() ##remove rows with duplicate values
sey.fish %>% glimpse

fish.lw <- fishdata %>% left_join (sey.fish) ##add the a's and b's for my species to my dataset
glimpse(fish.lw) ##looks like some common species weren't in the seychelles set (or were spelled differently). Damn

## Which species did it match with?
fishdata %>% dplyr::select(Species) %>% 
  distinct %>% ## remove repeat rows
  inner_join(sey.fish) ##join, return only rows that match
## just 4 of 35 species

## ----end