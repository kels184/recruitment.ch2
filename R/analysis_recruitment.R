## ---- loadFunctions
source('../R/functions.R') # includes setup fns, packages, brms DHARMA fns and SUYR Prior_Posterior fn
## ----end

# Fish Recruitment in Macroalgal Patch Experiment **************************************************


## ---- recruitment readData

fishdata <- read_csv(paste0(DATA_PATH,
                        "primary/fish1.csv"),
                 trim_ws=TRUE)
algaedata <- read_csv(paste0(DATA_PATH,
                        "primary/algae.csv"),
                 trim_ws=TRUE)

## ---- end

## ---- recruitment glimpse

fishdata %>% glimpse() #render failing here - fishdata not found
algaedata %>% glimpse()

## ---- end

## ---- recruitment report table
fishdata %>% report::report_table() 
algaedata %>% report::report_table() 
## ----end

## ----factorise and clean
fishdata <- fishdata %>% 
  mutate(Date = factor(Date), #note this will probs need to be ordered later
         Treatment = factor(Treatment),
         Replicate = factor(Replicate),
         Family = factor(Family),
         Species = factor(Species)) %>% 
  select(-c(Other, Replacement, `Patch note`, Transparency, `ID note`, `Behaviour note`))
glimpse(fishdata)

algaedata <- algaedata %>% 
  mutate(Treatment = factor(Treatment),
         Replicate = factor(Replicate),
         Thallus = factor(Thallus)) %>% 
  rename(Length = `Length (cm)`,
         Weight = `Weight (g)`)

glimpse(algaedata)
## ----end


## ---- recruitment algae EDA
algae.sum <- algaedata %>% 
  group_by(Treatment) %>% 
  summarise(mean.len = mean(Length),
            n.thalli = length(Replicate),
            SE.len = sd(Length)/sqrt(length(Replicate)),
            mean.wt = mean(Weight),
            SE.wt = sd(Weight)/sqrt(length(Replicate)) 
            )
algae.sum
g.len <- ggplot(algaedata) + aes(y = Length, x = Treatment) + 
  geom_point(alpha = 0.1) +
  geom_violin(fill = NA) + 
  theme_bw()

g.len  

g.wt <- ggplot(algaedata) + aes(y = Weight, x = Treatment) +
  geom_point(alpha = 0.1) + 
  geom_violin(fill = NA) +
  labs(y = "Length (cm)") +
  theme_bw(text = element_text(family = "Calibri", size = 8, color = "black"),
           axis.text = element_text(size = 10),
           axis.title = element_text(size = 12)
           )
## ----end