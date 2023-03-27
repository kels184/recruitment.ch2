## ---- loadFunctions
source('../R/functions.R') # includes setup fns, packages, brms DHARMA fns and SUYR Prior_Posterior fn
## ----end

# Fish Recruitment in Macroalgal Patch Experiment **************************************************


## ---- recruitment readData

fishdata1 <- read_csv(paste0(DATA_PATH,
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
fishdata <- fishdata1 %>% 
  select(-c(Other, Replacement, `Patch note`, Transparency, `ID note`, `Behaviour note`)) %>% 
  
  mutate(Date = dmy(Date), #using lubridate, convert Date to proper date format
  ) %>% 
  mutate_at(c(2:5), ## in the specified cols
            ~replace_na(.,"empty")) %>%   ## replace NAs with "empty" (there were no fish)
  mutate_at(c(2:5), ~factor(.) ) %>% 
filter(Family != "Apogonidae")   ## removing apogonidae as they were not counted consistently (and are cryptic)
                                 ## NB - filter had to be done AFTER converting NAs to values, or they are removed

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
            SE.wt = sd(Weight)/sqrt(length(Replicate)),
)


algaedata %>% 
  group_by(Treatment,Replicate) %>% 
  summarise(plot.weight = sum(Weight)) %>% 
  ungroup() %>% group_by(Treatment) %>% 
  summarise(mean.plot.wt = mean(plot.weight),
            SE.plot.wt = (sd(plot.weight)/sqrt(5))
            ) %>% 
  left_join(algae.sum,.) -> algae.sum

algae.sum <- mutate(algae.sum,
                    )

algae.sum


g.len <- ggplot(algaedata) + aes(y = Length, x = Treatment) + 
  geom_point(alpha = 0.1) +
  geom_violin(fill = NA)
  labs(y = "Length (cm)") +
  theme(family = "calibri", text = element_text( size = 8, color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        #panel.grid.major.x = element_blank(), #remove vertical gridlines
  ) +
  theme_bw()

g.len  

g.wt <- ggplot(algaedata) + aes(y = Weight, x = Treatment) +
  geom_point(alpha = 0.1) + 
  geom_violin(fill = NA) +
  labs(y = "Biomass (g wet wt)") +
  theme(family = "calibri", text = element_text( size = 8, color = "black"),
           axis.text = element_text(size = 10),
           axis.title = element_text(size = 12),
        panel.grid.major.x = element_line(colour = "transparent"),
           ) +
  theme_bw()
g.wt


g.plot.wt <- algaedata %>% 
  group_by(Treatment,Replicate) %>% ## first create a vector with the 
  summarise(plot.weight = sum(Weight)) %>% ## total biomass of each plot
  ggplot() + 
  aes(y = plot.weight, x = Treatment) +
  geom_point(alpha = 0.1) + 
  geom_violin(fill = NA) +
  ylab(expression("Plot Biomass (g wet wt)") ) +
  theme(family = "calibri", text = element_text( size = 8, color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid = element_blank()
  ) +
  theme_bw()
g.plot.wt

ggsave(filename = paste0(FIGS_PATH, "/Alg.len.png"),
       g.len,
       width = 10,
       height = 5,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/Alg.wt.png"),
       g.wt,
       width = 10,
       height = 5,
       dpi = 100)

ggsave(filename = paste0(FIGS_PATH, "/Alg.plot.wt.png"),
       g.plot.wt,
       width = 10,
       height = 5,
       dpi = 100)


## ----end


## ---- recruitment fish EDA

fish.sum <- fishdata %>% 
  group_by(Treatment,Replicate, Date) %>% 
  summarise(abundance = sum(count)) %>% ## sum of count will be 0 for empty plots (unlike count[])
ungroup %>% group_by(Treatment, Replicate) %>% 
  summarise(mean.abnd = mean(abundance),
            se.abnd = sd(abundance)/sqrt(length(abundance)))

fish.sum

   ## plot fish abundance
g.fish.abnd1 <-fishdata %>% 
  group_by(Treatment, Replicate, Date) %>% 
  summarise(abundance = sum(count)) %>% 
  ggplot() + aes(y = abundance, x = Treatment) +
    geom_count(alpha = 0.1) + 
    geom_violin(fill = NA) +
    ylab(expression("Fish Abundance") ) +
    theme(family = "calibri", text = element_text( size = 8, color = "black"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 12),
          panel.grid = element_blank()
    ) +
    theme_bw()

g.fish.abnd1
ggsave(filename = paste0(FIGS_PATH, "/EDAfish1.png"),
       g.fish.abnd1,
       width = 10,
       height = 5,
       dpi = 100)


  ## plot fish abundance, now over time:

g.fish.abnd.time <-fishdata %>% 
  group_by(Treatment, Replicate, Date) %>% 
  summarise(abundance = sum(count)) %>% 
  ggplot() + aes(y = abundance, x = Date, colour = Treatment) +
  geom_point(position = position_jitterdodge(jitter.width = 0.02, dodge.width = 0.9), alpha = 1) +
  geom_smooth() +
  ylab(expression("Fish Abundance") ) +
  theme(family = "calibri", text = element_text( size = 8, color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid = element_blank()
  ) +
  theme_bw()

g.fish.abnd.time
ggsave(filename = paste0(FIGS_PATH, "/EDAfish2.png"),
       g.fish.abnd.time,
       width = 10,
       height = 5,
       dpi = 100)

  ## plot species richness


## ----end