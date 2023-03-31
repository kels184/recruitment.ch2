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
kulbicki <- read_csv(paste0(DATA_PATH,
                                   "primary/Length-weight relationships.csv"),
                            trim_ws = TRUE)
glimpse(kulbicki)

lw.fish <- kulbicki %>% 
  dplyr::select(-c(11:15)) %>% ##remove these cols 
  rename(Species = Name...2) %>% ##rename this col to "species"
  mutate(Familiy = factor(Family),
         Species = factor(Species),
         Shape = factor(Shape))

lw.fish %>% glimpse

#l.min and l.max were character and have been converted to factor. some entries had an extra decimal

#which.nonnum <- function(x) {
#  badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
#  which(badNum & !is.na(x))
#}
#lapply(lw.fish %>% select(L.min, L.max), which.nonnum)

fish.lw <- fishdata %>% left_join (lw.fish) ##add the parameters for my species to my dataset
glimpse(fish.lw) ##looks like some common species weren't in the seychelles set (or were spelled differently). Damn

## Which species did it match with?
fishdata %>% dplyr::select(Species) %>% 
  distinct %>% ## remove repeat rows
  inner_join(lw.fish) ##join, return only rows that match
## 15 of 35 species


## Length weight calculation ====================================================

fish.lw <- fish.lw %>% 
  mutate(weight = a * Length^b)

glimpse(fish.lw)


write_csv(fish.lw, paste0(DATA_PATH,
                 "processed/fish.lw.csv"))

## ----end