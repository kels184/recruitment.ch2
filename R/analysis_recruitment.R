## ---- loadFunctions
source('../R/functions.R') # includes setup fns, packages, brms DHARMA fns and SUYR Prior_Posterior fn
## ----end

# Fish Recruitment in Macroalgal Patch Experiment ===============================

## Read data ====================================================================

## ---- FishRecruitment readData

fishdata1 <- read_csv(paste0(DATA_PATH,
                        "primary/fish1.csv"),
                 trim_ws=TRUE)
algaedata <- read_csv(paste0(DATA_PATH,
                        "primary/algae.csv"),
                 trim_ws=TRUE)

## ---- end

## ---- recruitment glimpse

fishdata1 %>% glimpse() #render failing here - fishdata not found
algaedata %>% glimpse()

## ---- end

## ---- recruitment report table
fishdata1 %>% report::report_table() 
algaedata %>% report::report_table() 
## ----end

## Process data =================================================================

## ---- factorise and clean
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



fishalgaedata <- algaedata %>% 
  group_by(Treatment,Replicate) %>% 
  summarise(plot.weight = sum(Weight)) %>% 
  left_join(fishdata, .) # add plot.weight column to fishdata
fishalgaedata %>% glimpse()

#write_csv(fishalgaedata, file = paste0(DATA_PATH, "processed/fishalgaedata.csv"))

## ----end

## EDA ==========================================================================

### Algae =======================================================================

## ---- recruitment algae summary table
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

algae.sum
## ----end


## ---- recruitment algae summary figures
g.len <- ggplot(algaedata) + aes(y = Length, x = Treatment) + 
  geom_point(alpha = 0.1) +
  geom_violin(fill = NA) +
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

### Fish ========================================================================



   #### Total fish abundance ====================================================

## ---- fish EDA total abundance
fish.sum <- fishdata %>% 
  group_by(Treatment,Replicate, Date) %>% 
  summarise(abundance = sum(count)) %>% ## sum of count will be 0 for empty plots (unlike count[])
ungroup %>% group_by(Treatment, Replicate) %>% 
  summarise(mean.abnd = mean(abundance),
            se.abnd = sd(abundance)/sqrt(length(abundance)))

fish.sum


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

## ----end
  #### Total abundance over time: ===============================================

## ---- fish EDA abundance time
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

fishdata %>% 
  group_by(Treatment, Replicate, Date) %>% 
  summarise(abundance = sum(count)) %>% 
  ggplot() + aes(y = abundance, x = Date ) +
  #geom_point(position = position_jitterdodge(jitter.width = 0.02, dodge.width = 0.9), alpha = 1) +
  geom_line() +
  facet_wrap(~interaction(Treatment,Replicate) )+
  ylab(expression("Fish Abundance") ) +
  theme(family = "calibri", text = element_text( size = 8, color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid = element_blank()
  ) +
  theme_bw()


   #### Total Abundance vs algal biomass ========================================

## ---- fish EDA abundance vs biomass

fishalgaedata <- read_csv(file = paste0(DATA_PATH, "processed/fishalgaedata.csv")) %>% 
  mutate_at(c(2:5), factor)

fishalgaedata %>% 
  group_by(Treatment, Replicate, Date) %>% 
  summarise(abundance = sum(count),
            x = mean(plot.weight) )%>% ## mean won't change anything, but will ensure plot.weight is kept in the output
  ggplot() + aes(y = abundance, x = x) + ## set overall aesthetic
  geom_count(aes(colour = Treatment),  ## make colour vary by treatment in the points
             alpha = 0.2)  + 
  geom_smooth(method = "lm") + ## but only a smoother on x and y (not separate for groups)
  ylab(expression("Fish Abundance") ) +
  xlab("Plot Biomass (g wet wt)") + 
  theme(family = "calibri", text = element_text( size = 8, color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid = element_blank()
  ) +
  theme_bw() -> g.alg.fish.abnd

g.alg.fish.abnd

ggsave(filename = paste0(FIGS_PATH, "/EDAfish.alg.abnd.png"),
       g.alg.fish.abnd,
       width = 10,
       height = 5,
       dpi = 100)

## ----end



  #### Species Richness =========================================================

## ---- fish EDA Species richness
g.sp.richness <- fishdata %>% 
  group_by(Treatment, Replicate, Date) %>% 
  summarise(sp.richness = if_else(Species != "empty", ## unless the plot had no fish
                                  true = length(unique(Species)), ##count number of unique species levels
                                  false = 0) ## or else input zero
            ) %>%  distinct() %>% ##remove rows with duplicate values
  ggplot() + aes(x = Treatment, y = sp.richness) +
  geom_count(alpha = 0.1) + 
  geom_violin(fill = NA, 
              adjust = 2.5) + ## increases bandwidth for kernel density function. without this too 'wiggly'
  ylab(expression("Species Richness") ) +
  theme(family = "calibri", text = element_text( size = 8, color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid = element_blank() ## still not working
  ) +
  theme_bw()

g.sp.richness

ggsave(filename = paste0(FIGS_PATH, "/EDAfish.sp1.png"),
       g.sp.richness,
       width = 10,
       height = 5,
       dpi = 100)

## ----end

   #### Species richness over time: =============================================

## ---- fish EDA richness vs time
g.sp.time <- fishdata %>% 
  group_by(Treatment, Replicate, Date) %>% 
  summarise(sp.richness = if_else(Species != "empty", 
                                  true = length(unique(Species)), 
                                  false = 0) 
  ) %>%  distinct() %>% 
  ggplot() + aes(y = sp.richness, x = Date, colour = Treatment) +
  geom_point(position = position_jitterdodge(jitter.width = 0.02, dodge.width = 0.9), alpha = 1) +
  geom_smooth() +
  ylab(expression("Species Richness") ) +
  theme(family = "calibri", text = element_text( size = 8, color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid = element_blank()
  ) +
  theme_bw()

g.sp.time



ggsave(filename = paste0(FIGS_PATH, "/EDAfish.sp2.png"),
       g.sp.time,
       width = 10,
       height = 5,
       dpi = 100)


 #### Species richness vs algal biomass =========================================

## ---- fish EDA richness vs biomass
fishalgaedata %>% 
  group_by(Treatment, Replicate, Date) %>% 
  summarise(sp.richness = if_else(Species != "empty", 
                                  true = length(unique(Species)), ##as for sp richness calculation above
                                  false = 0) ,
            x = mean(plot.weight) )%>% 
  distinct %>% 
  ggplot() + aes(y = sp.richness, x = x) + ##as for biomass/abundance plot
  geom_count(aes(colour = Treatment),  
             alpha = 0.2)  + 
  geom_smooth(method = "lm") + 
  ylab(expression("Species Richness") ) +
  xlab("Plot Biomass (g wet wt)") + 
  theme(family = "calibri", text = element_text( size = 8, color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid = element_blank()
  ) +
  theme_bw() -> g.alg.fish.sp  ##gam perhaps too many knots, but pattern generally increasing, perhaps with asymptote

g.alg.fish.sp

ggsave(filename = paste0(FIGS_PATH, "/EDAfish.alg.sp.png"),
       g.alg.fish.sp,
       width = 10,
       height = 5,
       dpi = 100)

## ----end

 #### Abundance/Commonness ======================================================
## ---- fish EDA commonness

#fishalgaedata <- read_csv(file = paste0(DATA_PATH, "processed/fishalgaedata.csv")) %>% 
#mutate_at(c(2:5), factor)
#glimpse(fishalgaedata)


##what species are the most common/are likely to have enough data to do something with

sp.abnd <- fishalgaedata %>% 
  group_by(Species) %>% 
    summarise(total.abnd = sum(count)) %>% #total occurrences of each species
  mutate(abnd.prop = total.abnd/sum(total.abnd)) %>% #as a proportion
  arrange(-total.abnd) %>%  #order descending
  mutate(cum.prop = cumsum(abnd.prop)) #as a cumulative proportion

#which species occurred on the most days?

days.observed <- fishalgaedata %>% 
  group_by(Species,Date) %>% 
 summarise(total.abnd = sum(count)) %>% 
  count(sort = TRUE) #count occurences (rows) according to SpeciesxDate and sort descending

sp.abnd <- sp.abnd %>% left_join(days.observed) %>% 
  rename(days.observed = n)

sp.abnd %>% head(10)

write_csv(sp.abnd, paste0(DATA_PATH, "summarised/species.abundance.csv") )

## ----end

#### Common species patterns =====================================================

## ---- fish EDA common species1

### set up dataset

commondata <- fishalgaedata %>% 
  filter(Species %in% paste0(sp.abnd$Species[1:6]) ) %>% #include only the commonest 6 sp
  droplevels #remove the levels with no data
glimpse(commondata)
commondata$Species %>% levels

## plot abundance
commondata %>% 
  group_by(Treatment, Replicate, Date, Species) %>% 
  summarise(abundance = sum(count) )%>%
 #ungroup() %>% ## 
  ggplot() + aes(y = abundance, x = Treatment) + ## 
  geom_count(alpha = 0.1)  +
  geom_violin(fill = NA) +
  facet_wrap(~Species) + #separate panel for each species
  ylab("Abundance" ) + 
  theme(family = "calibri", text = element_text( size = 8, color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid = element_blank()
  ) +
  theme_bw() -> g.common.abnd

g.common.abnd 

ggsave(filename = paste0(FIGS_PATH, "/EDAfish.common.png"),
       g.common.abnd,
       width = 10,
       height = 5,
       dpi = 100)

## ----end

## ---- fish EDA common species2
## plot abundance over time

commondata %>% 
  group_by(Treatment, Replicate, Date, Species) %>% 
  summarise(abundance = sum(count) )%>%
  #ungroup() %>% ## 
  ggplot() + aes(y = abundance, x = Date, colour = Treatment) + ## 
  geom_point(position = position_jitterdodge(jitter.width = 0.02, dodge.width = 0.9), alpha = 1) +
  geom_smooth() +
  facet_wrap(~Species) +
  ylab("Abundance" ) + 
  theme(family = "calibri", text = element_text( size = 8, color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid = element_blank()
  ) +
  theme_bw() -> g.common.abnd.time

ggsave(filename = paste0(FIGS_PATH, "/EDAfish.common.time.png"),
       g.common.abnd.time,
       width = 10,
       height = 5,
       dpi = 100)

## ----end


## ----fish EDA common species3
## plot abundance vs algal biomass
commondata %>% 
  group_by(Treatment, Replicate, Date, Species) %>% 
  summarise(abundance = sum(count), 
           x = mean(plot.weight) )%>% ## mean won't change anything, but will ensure plot.weight is kept in the output
  ggplot() + aes(y = abundance, x = x) + ## set overall aesthetic
  geom_count(aes(colour = Treatment),  ## make colour vary by treatment in the points
             alpha = 0.2)  + 
  geom_smooth(method = "lm") + ##smoother on x and y (not separate for groups). Linear model method instead of gam
  facet_wrap(~Species) +
  ylab("Abundance") +
  xlab("Plot Biomass (g wet wt)") + 
  theme(family = "calibri", text = element_text( size = 8, color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid = element_blank()
  ) +
  theme_bw() -> g.common.abnd.alg
g.common.abnd.alg



ggsave(filename = paste0(FIGS_PATH, "/EDAfish.common.alg.png"),
       g.common.abnd.alg,
       width = 10,
       height = 5,
       dpi = 100)

## ----end


 ####Size distributions =========================================================

## ----fish EDA sizes1
fishalgaedata <- read_csv(file = paste0(DATA_PATH, "processed/fishalgaedata.csv")) %>% 
  mutate_at(c(2:5), factor)
#glimpse(fishalgaedata)

dat <- fishalgaedata %>%
  filter(!Family == "empty" ) %>% #remove rows with no fish
           group_by(Length, Date, Treatment,Replicate) %>% 
  summarise(count.per.day = sum(count)) %>% 
  group_by(Treatment, 
           Length = cut(Length, breaks = seq(0,max(Length),1), #create bins for Length
                        right = FALSE,#bin intervals closed on the left
                        include.lowest = TRUE) #include all bins (no NA bin)
           )%>%
  summarise(mean.count = mean(count.per.day),
            med.count = median(count.per.day))

#dat %>% view()


#size hist by treatment (all fish) (mean frequency of each length per day)
g <- dat %>% ggplot(aes(x = Length)) +
  geom_col(aes(y = mean.count), #Hist with frequency on y axis
                 colour ="black", fill = "white")+
  facet_wrap(~Treatment) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

g

## ----end

ggsave(filename = paste0(FIGS_PATH, "/EDAfish.size.trt.png"),
       g,
       width = 10,
       height = 5,
       dpi = 100)

#size hist by treatment (all fish) (dens overlay) #(not rendered)#
fishalgaedata %>% ggplot(aes(x = Length)) +
  geom_histogram(aes(y = ..density..), #Hist with density on y axis
                 binwidth = 1,
colour ="black", fill = "white") +
  geom_density(alpha = .2, fill = "#FF6666") +#Overlay with transparent density plot
  facet_wrap(~Treatment) + 
  theme_bw()

## ----fish EDA sizes2 common species
sp.abnd <- read_csv(paste0(DATA_PATH, "summarised/species.abundance.csv"))

commondata <- fishalgaedata %>% 
  filter(Species %in% paste0(sp.abnd$Species[1:6]) ) %>% #include only the commonest 6 sp
  droplevels #remove the levels with no data
glimpse(commondata)

dat <- commondata %>%
  filter(!Family == "empty" ) %>% #remove rows with no fish
  group_by(Species,Length, Date, Treatment, Replicate) %>% 
  summarise(count.per.day = sum(count)) %>% 
  group_by(Species, Treatment,
           Length = cut(Length, breaks = seq(0,max(Length),0.5), #create bins for Length
                        right = FALSE,#bin intervals closed on the left
                        include.lowest = TRUE) #include all bins (no NA bin)
  )%>%
  summarise(med.count = median(count.per.day),
            mean.count = mean(count.per.day))
#dat %>% view()

g.hm <- dat %>% filter(Species == "Halichoeres miniatus") %>% 
  ggplot(aes(x = Length)) +
  geom_col(aes(y = mean.count), #Hist with frequency on y axis
           colour ="black", fill = "white")+
  facet_wrap(~Treatment) +
  theme_bw() +
  labs(title = "Halichoeres miniatus") +
  theme(axis.text.x = element_text(angle = 90))

g.ps <- dat %>% filter(Species == "Petroscirtes sp.") %>% 
  ggplot(aes(x = Length)) +
  geom_col(aes(y = mean.count), #Hist with frequency on y axis
           colour ="black", fill = "white")+
  facet_wrap(~Treatment) +
  theme_bw() +
  labs(title = "Petroscirtes sp.") +
  theme(axis.text.x = element_text(angle = 90))

g.sd <- dat %>% filter(Species == "Siganus doliatus") %>% 
  ggplot(aes(x = Length)) +
  geom_col(aes(y = mean.count), #Hist with frequency on y axis
           colour ="black", fill = "white")+
  facet_wrap(~Treatment) +
  theme_bw() +
  labs(title = "Siganus doliatus") +
  theme(axis.text.x = element_text(angle = 90))

g.pt <- dat %>% filter(Species == "Pomacentrus tripunctatus") %>% 
  ggplot(aes(x = Length)) +
  geom_col(aes(y = mean.count), #Hist with frequency on y axis
           colour ="black", fill = "white")+
  facet_wrap(~Treatment) +
  theme_bw() +
  labs(title = "Pomacentrus tripunctatus") +
  theme(axis.text.x = element_text(angle = 90))

g.la <- dat %>% filter(Species == "Lethrinus atkinsoni") %>% 
  ggplot(aes(x = Length)) +
  geom_col(aes(y = mean.count), #Hist with frequency on y axis
           colour ="black", fill = "white")+
  facet_wrap(~Treatment) +
  theme_bw() +
  labs(title = "Lethrinus atkinsoni") +
  theme(axis.text.x = element_text(angle = 90))

g.sf<- dat %>% filter(Species == "Siganus fuscescens") %>% 
  ggplot(aes(x = Length)) +
  geom_col(aes(y = mean.count), #Hist with frequency on y axis
           colour ="black", fill = "white")+
  facet_wrap(~Treatment) +
  theme_bw() +
  labs(title = "Siganus fuscescens") +
  theme(axis.text.x = element_text(angle = 90))

g.hm
g.ps
g.sd
g.pt
g.la
g.sf

## ---end

ggsave(filename = paste0(FIGS_PATH, "/EDAfish.hm.size.trt.png"),
       g.hm,
       width = 10,
       height = 5,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/EDAfish.ps.size.trt.png"),
       g.ps,
       width = 10,
       height = 5,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/EDAfish.sd.size.trt.png"),
       g.sd,
       width = 10,
       height = 5,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/EDAfish.pt.size.trt.png"),
       g.pt,
       width = 10,
       height = 5,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/EDAfish.la.size.trt.png"),
       g.la,
       width = 10,
       height = 5,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/EDAfish.sf.size.trt.png"),
       g.sf,
       width = 10,
       height = 5,
       dpi = 100)


## ----fish EDA sizes2 over time

dat <- fishalgaedata %>%
  filter(!Family == "empty" ) %>% #remove rows with no fish
  group_by(Length, Date, Treatment,Replicate) %>% 
  summarise(count.per.day = sum(count)) %>% 
  group_by(Treatment, Date,
           Length = cut(Length, breaks = seq(0,max(Length),1), #create bins for Length
                        right = FALSE,#bin intervals closed on the left
                        include.lowest = TRUE) #include all bins (no NA bin)
  )%>%
  summarise(mean.count = mean(count.per.day),
            med.count = median(count.per.day))

all.length.date <- dat %>% ggplot(aes(x = Length)) +
  geom_col(aes(y = mean.count), #Hist with frequency on y axis
           colour ="black", fill = "white")+
  facet_grid(rows = vars(Date), cols = vars(Treatment)) +
  theme_bw()

 dat <- fishalgaedata %>%
  filter(Species == "Petroscirtes sp." ) %>% #remove rows with no fish
  group_by(Length, Date, Treatment, Replicate) %>% 
  summarise(count.per.day = sum(count)) %>% 
  group_by(Treatment, Date,
           Length = cut(Length, breaks = seq(0,max(Length),0.5), #create bins for Length
                        right = FALSE,#bin intervals closed on the left
                        include.lowest = TRUE) #include all bins (no NA bin)
  )%>%
  summarise(mean.count = mean(count.per.day),
            med.count = median(count.per.day))

 petro.length.date <- dat %>% 
  ggplot(aes(x = Length)) +
  geom_col(aes(y = mean.count), #Hist with frequency on y axis
           colour ="black", fill = "white")+
  facet_grid(rows = vars(Date), cols = vars(Treatment)) +
  labs(title = "Petroscirtes sp.") +
 theme_bw()

dat <- fishalgaedata %>%
  filter(Species == "Siganus doliatus" ) %>% #remove rows with no fish
  group_by(Length, Date, Treatment, Replicate) %>% 
  summarise(count.per.day = sum(count)) %>% 
  group_by(Treatment, Date,
           Length = cut(Length, breaks = seq(0,max(Length),0.5), #create bins for Length
                        right = FALSE,#bin intervals closed on the left
                        include.lowest = TRUE) #include all bins (no NA bin)
  )%>%
  summarise(mean.count = mean(count.per.day),
            med.count = median(count.per.day))
doli.length.date <- dat %>% 
  ggplot(aes(x = Length)) +
  geom_col(aes(y = mean.count), #Hist with frequency on y axis
           colour ="black", fill = "white")+
  facet_grid(rows = vars(Date), cols = vars(Treatment)) +
  labs(title = "Siganus doliatus") +
  theme_bw()

all.length.date
petro.length.date
doli.length.date

## ---end

ggsave(filename = paste0(FIGS_PATH, "/EDAfish.all.length.date.png"),
       all.length.date,
       width = 40,
       height = 25,
       dpi = 100)

ggsave(filename = paste0(FIGS_PATH, "/EDAfish.petro.length.date.png"),
       petro.length.date,
       width = 40,
       height = 25,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/EDAfish.doli.length.date.png"),
       doli.length.date,
       width = 40,
       height = 25,
       dpi = 100)



 ## Univariate modelling ========================================================

  ### Abundance =================================================================
## ----recruitment univariate setup data
fishalgaedata <- read_csv(file = paste0(DATA_PATH, "processed/fishalgaedata.csv")) %>% 
  mutate_at(c(2:5), factor)
glimpse(fishalgaedata)


fish.sp.abnd <-fishalgaedata %>% 
  group_by(Treatment, Replicate, Date) %>% 
  summarise(abundance = sum(count), #calculate total abnd
            sp.richness = if_else(Species != "empty", 
                                  true = length(unique(Species)), 
                                  false = 0), #calculate sp. richness
            plot.weight = plot.weight # include the biomass of each plot in the output
  ) %>% distinct() %>% #remove duplicate rows
  mutate(plotID = factor(paste0(Treatment, Replicate)),
         Density = recode_factor(Treatment, "W" = 9, "BH" = 9, "BQ" = 9,
                                 "DM" = 5, "DL" = 3)) %>% 
  as.data.frame() #make into a data frame instead of tbl (prevented ggpredict[]

fish.sp.abnd %>% glimpse()
fish.sp.abnd$Density %>% levels()
## ----end

   #### Fit =====================================================================

## ---- recruitment univariate abundance initial models

abnd.glmmTMB1 <- glmmTMB(abundance ~ 1 + (1|plotID), #Random intercept model
                       data = fish.sp.abnd,
                       family = poisson(link = "log"), #Poisson model
                       REML = TRUE) 

abnd.glmmTMB2 <- update(abnd.glmmTMB1, .~. + Treatment) #add Treatment as fixed

abnd.glmmTMB3 <- update(abnd.glmmTMB1, .~. + plot.weight) #plot.weight fixed

abnd.glmmTMB4 <- update(abnd.glmmTMB1, .~. + Density)#Density fixed

abnd.glmmTMB5 <- update(abnd.glmmTMB3, .~. + Density)#plot.weight + Density

abnd.glmmTMB6 <- update(abnd.glmmTMB3, .~. * Density) #plot.weight*Density

abnd.glmmTMB7 <- update(abnd.glmmTMB2, ~. + plot.weight) #Treatment + plot.weight

abnd.glmmTMB8 <- update(abnd.glmmTMB2, ~. * plot.weight) #Treatment * plot.weight

MuMIn::AICc(abnd.glmmTMB1,abnd.glmmTMB2, abnd.glmmTMB3, abnd.glmmTMB4,
     abnd.glmmTMB5, abnd.glmmTMB6,abnd.glmmTMB7, abnd.glmmTMB8) #compare models

## ----end

   #### Validate/ Refit =========================================================
## ----recruitment univariate abundance validate
abnd.resid <- abnd.glmmTMB2 %>% DHARMa::simulateResiduals(plot = T)
abnd.resid %>% testDispersion()

#are the observations of the same plot temporally autocorrelated? DHARMA won't 
#like that there are multiple observations at the same time, but maybe if I 
#give each plot their own set of Time
fish.sp.abnd <- fish.sp.abnd %>% 
  mutate(TIME = as.numeric(plotID) + as.numeric(Date)*10^-5)
fish.sp.abnd %>% head
fish.sp.abnd %>% tail

abnd.resid %>% testTemporalAutocorrelation(time = fish.sp.abnd$TIME)
abnd.resid %>% testZeroInflation()
## ----end

## ---- recruitment univariate abundance refit autocorrelation

abnd.glmmTMB.ac <- update(abnd.glmmTMB2, .~. + 
                            ar1(0 + factor(Date)| plotID) #ac part.No intercept(0), 
 #Date must be a factor, with evenly spaced time steps. autocorrelation separate 
 #for each plot 
                          )

AICc(abnd.glmmTMB.ac, abnd.glmmTMB2)

## ----end

## ----recruitment univariate abundance revalidate
abnd.resid <- abnd.glmmTMB.ac %>% simulateResiduals(plot = T)
abnd.resid %>% testTemporalAutocorrelation(time = fish.sp.abnd$TIME)


## ----end

## ----recruitment univariate abundance revalidate2

abnd.resid %>% testDispersion()
abnd.resid %>% testUniformity()

## ----end


    #### Partial plot ===========================================================
## ----recruitment univariate abundance partial
abnd.glmmTMB2 %>% ggpredict(terms = "Treatment") %>% plot() 
abnd.glmmTMB.ac %>% ggpredict(terms = "Treatment") %>% plot()
## ----end

   #### Bayesian Model ==========================================================
   ##### Priors =================================================================
## ---- recruitment univariate abundance priors1

abnd.form <- bf(abundance ~ Treatment
                    + (1|plotID),
                   autocor = ~ ar(time = Date, gr = plotID, 
                                 p = 1), #order of the autoregressive (1st order)
                           family = poisson(link = "log"))

abnd.form %>%  get_prior(data = fish.sp.abnd)

## priors for Intercept
fish.sp.abnd %>% group_by(Treatment) %>%  summarise(log(median(abundance)), 
                                                    log(mad(abundance)))
## ----end

## ---- recruitment univariate abundance priors2
##priors for Effects
log(sd(fish.sp.abnd$abundance)) #1.61
model.matrix(~Treatment, data = fish.sp.abnd) %>% head
apply(model.matrix(~Treatment, data = fish.sp.abnd), 2, sd)
log(sd(fish.sp.abnd$abundance))/apply(model.matrix(~Treatment, data = fish.sp.abnd), 2, sd)


standist::visualize("gamma(2,1)", "cauchy(0,2)","student_t(3, 0, 2.5)", xlim = c(0,10))

## ----end

##### Fitting ===================================================================

## ----recruitment univariate abundance fit brm1.prior

## eval set to FALSE
priors <- prior(normal(2,1), class = "Intercept") +
  prior(normal(0,5), class = "b") + 
  prior(cauchy(0,2), class = "sd") +
  prior(uniform(-1,1), class = "ar") +# later changed to 0,1
  prior(cauchy(0,2), class = "sderr")
        
abnd.brm1 <- brm(abnd.form,
                         data = fish.sp.abnd,
                       prior = priors,
                        sample_prior = "only", #just to start
                         iter = 5000, warmup = 1000,
                         chains = 3, cores = 3, 
                         thin = 5,
                 seed = 123)
save(abnd.brm1, file = paste0(DATA_PATH, "modelled/abnd.brmprior1.RData"))  
## ----end

## ---- recruitment univariate abundance brm1 error and fix attempt

load(file = paste0(DATA_PATH, "modelled/abnd.brmprior1.RData"))

abnd.brm1 %>% ggpredict(~Treatment) %>% plot(add.data = TRUE)

abnd.brm1$data$Date %>% class()
abnd.brm1$data$Date <- factor(abnd.brm1$data$Date)
abnd.brm1$data$Date %>% class()
##this did not change that ggpredict didn't work

fish.sp.abnd$Date %>% class()
fish.sp.abnd <- fish.sp.abnd %>% 
  mutate(Date = factor(Date))
##this, then rerunning the model, did not change that ggpredict didn't work either
##ggpredict still works for murray's examples (e.g. bglmm_example5, owls.brm2)
##so probably not an update of ggpredict() issue
## more likely something to do with the ar() term

## ----end

## ----recruitment univariate abundance fit brm2
## eval set to FALSE

##the following (i.e. the model without the ar term) ran
abnd.form <- bf(abundance ~ Treatment + (1|plotID),
                family = poisson(link = "log"))

priors <- prior(normal(2,1), class = "Intercept") +
  prior(normal(0,5), class = "b") + 
  prior(cauchy(0,2), class = "sd")

abnd.brm2.prior <- brm(abnd.form,
                 data = fish.sp.abnd,
                 prior = priors,
                 sample_prior = "only", #just to start
                 iter = 5000, warmup = 1000,
                 chains = 3, cores = 3, 
                 thin = 5,
                 seed = 123)

save(abnd.brm2.prior, file = paste0(DATA_PATH, "modelled/abnd.brmprior2.RData")) 
## ----end


## ----recruitment univariate abundance brm2 ggpredict
load(file = paste0(DATA_PATH, "modelled/abnd.brmprior2.RData"))

abnd.brm2.prior %>% ggpredict(~Treatment) %>% plot(add.data = TRUE)


##although the priors for the effects were massively wide (several orders of magnitude
#higher than the data)
standist::visualize("normal(0,5)")
standist::visualize("normal(0,2)")
exp(c(2,3,5,10))
#a normal(0,2) is probably more appropriate
## ----end

#however the fact that it ran adds evidence for there being a problem with the ar()
#term in the formula, or its priors


##removing prior for sderr only did not work
## when the prior for ar was not included the model did not run: 

#"Error: Sampling from priors is not possible as some parameters have no proper
#priors. Error occurred for parameter 'ar'.

##but [https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations] seems
#to suggest a uniform(-1,1) prior is sensible

##therefore there may be a problem with how the ar term is specified, or at least
#an incompatability with that specification and ggpredict


#without any idea how to fix this, lets see if the model will run including the
#data and posteriors

## ---- recruitment univariate abundance fit brm1a_b

## eval set to FALSE
abnd.form <- bf(abundance ~ Treatment
                + (1|plotID),
                autocor = ~ ar(time = Date, gr = plotID, p = 1), 
                family = poisson(link = "log"))

priors <- prior(normal(2,1), class = "Intercept") +
  prior(normal(0,2), class = "b") + 
  prior(cauchy(0,2), class = "sd") +
  prior(uniform(-1,1), class = "ar") +
  prior(cauchy(0,2), class = "sderr")

abnd.brm1a <- update(abnd.brm1, sample_prior = "yes")
save(abnd.brm1a, file = paste0(DATA_PATH, "modelled/abnd.brm1.RData"))

##priors and model I actually went with:
priors <- prior(normal(2,1), class = "Intercept") +
  prior(normal(0,2), class = "b") + 
  prior(cauchy(0,1), class = "sd") +
  prior(uniform(0,1), class = "ar",lb = 0, ub = 1) + #ar prior only between 0 and 1
  prior(cauchy(0,1), class = "sderr")

abnd.brm1b <- update(abnd.brm1a, prior = priors, seed = 123, refresh = 0)

abnd.brm1b %>% prior_summary()

abnd.brm1b %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/abnd.brm1b.rds"))


##this worked
## ----end

## ---- r recruitment univariate abundance vars
load(file = paste0(DATA_PATH, "modelled/abnd.brm1.RData"))
abnd.brm1a %>% ggpredict(~Treatment)
#this didn't 

abnd.brm1a %>% get_variables() %>% head(n = 50)
## ----end


##### Prior Checks ==============================================================


## ---- r recruitment univariate abundance brm1b prior checks

abnd.brm1b <- readRDS(file = paste0(DATA_PATH, "modelled/abnd.brm1b.rds"))

abnd.brm1b %>%
  as_draws_df() %>% #get all the draws for everything estimated
  
  dplyr::select(!matches("^lp|^err|^r_|^\\.") ) %>% #remove variables starting with lp, err or r_ or .
  #Note removing the '.' cols (.iteration, .draw and .chain) changed the class
  
  pivot_longer(everything(), names_to = 'key') %>% #make long, with variable names in a column called 'key'. Note 
  
  mutate(Type = ifelse(str_detect(key, 'prior'), 'Prior', 'Posterior'), #classify within new col 'Type' whether Prior or Posterior using str_detect
         Class = case_when( #create column 'Class' to classify vars as:
           str_detect(key, '(^b|^prior).*Intercept$') ~ 'Intercept', #intercept, if 'key' starts with b or prior followed by any character ('.') with 'Intercept' at the end
           str_detect(key, 'b_Treatment.*|prior_b') ~ 'TREATMENT', #TREATMENT, if the string contains 'b_Treatment followed by any character ('.')
           str_detect(key, 'sd_') ~ 'sd', #sd, if the string contains sd ('sderr' will be included)
           str_detect(key, 'ar') ~ 'ar', #ar, if it contains ar
           str_detect(key, 'sderr') ~ 'sderr'), #sderr, if it contains sderr
         Par = str_replace(key, 'b_', '')) %>% 
  
  ggplot(aes(x = Type,  y = value, color = Par)) + #Plot with these overall aesthetics
  stat_pointinterval(position = position_dodge(), show.legend = FALSE)+ #plot as stat_point intervals
  facet_wrap(~Class,  scales = 'free') #separate plots by Class with each class having its own scales

abnd.brm1b %>% SUYR_prior_and_posterior()
## ----end


##### MCMC diagnostics ==========================================================

## ---- recruitment univariate abundance brm1b MCMC diagnostics

abnd.brm1b <- readRDS(file = paste0(DATA_PATH, "modelled/abnd.brm1b.rds"))
pars <- abnd.brm1b %>% get_variables()

wch <- str_extract(pars, #get the names of the variables
                   '^b_.*|^sd.*|^ar.*') %>% #that start with b, sd or ar
  na.omit # omit the rest, resulting in an object of class 'omit'
wch
##Trace Plots
stan_trace(abnd.brm1b$fit, pars = wch)

##Autocorrelation factor
stan_ac(abnd.brm1b$fit, pars = wch)

##rhat - Scale reduction factor
stan_rhat(abnd.brm1b$fit, pars = wch)

##ESS (effective sample size)
stan_ess(abnd.brm1b$fit, pars = wch)

##Density plot
stan_dens(abnd.brm1b$fit, pars = wch, separate_chains = TRUE)

##Density overlay
abnd.brm1b%>% pp_check(type = 'dens_overlay', ndraws = 100)

## ----end

##### DHARMA Residuals ==========================================================

## ---- recruitment univariate abundance DHARMA residuals

abnd.brm1b <- readRDS(file = paste0(DATA_PATH, "modelled/abnd.brm1b.rds"))

#step 1. Draw out predictions
preds <- abnd.brm1b %>% posterior_predict(ndraws = 250, #extract 250 posterior draws from the posterior model
                                         summary = FALSE) #don't summarise - we want the whole distribution of them


#Step 2 create DHARMA resids
abnd.resids <- createDHARMa(simulatedResponse = t(preds), #provide with simulated predictions, transposed with t()
                            observedResponse = fish.sp.abnd$abundance, #real response
                            fittedPredictedResponse = apply(preds, 2, median), #for the fitted predicted response, use the median of preds (in the columns, the second argument of apply defines the MARGIN, 2 being columns for matrices)
                            integerResponse = "TRUE" #is the response an integer? yes
                            #, re.form = "NULL") #this argument not supported (neither in glmmTMB)
                            )

#Step 3 - plot!
plot(abnd.resids)

abnd.resids %>% testDispersion()
abnd.resids %>% testUniformity()
#fert.resids %>% testZeroInflation()

## ----end



   #### Model investigation =====================================================
   ##### Frequentist ============================================================
## ---- recruitment univariate abundance summary, error = "TRUE", cache = "FALSE", warning = "FALSE"

abnd.glmmTMB2 %>% summary()
r.squaredGLMM(abnd.glmmTMB2)

abnd.glmmTMB.ac %>% summary()
r.squaredGLMM(abnd.glmmTMB.ac)

## ----end

   ##### Bayesian ===============================================================
## ---- recruitment univariate abundance brm summary
abnd.brm1b <- readRDS(file = paste0(DATA_PATH, "modelled/abnd.brm1b.rds"))

#abnd.brm1b %>% ggpredict(~Treatment) %>% plot(add.data = TRUE)
abnd.brm1b %>% ggemmeans(~Treatment)
abnd.brm1b %>% ggemmeans(~Treatment) %>% plot()


abnd.brm1b$fit %>% tidyMCMC(pars = wch,
                             estimate.method = "median",
                             conf.int = TRUE,
                             conf.method = "HPDinterval",
                             rhat = TRUE,
                             ess = TRUE)
#Conditional
abnd.brm1b %>% bayes_R2(re.form = ~(1|plotID),
                        summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2

## ----end

## ---- recruitment univariate abundance brm r square
abnd.brm1b <- readRDS(file = paste0(DATA_PATH, "modelled/abnd.brm1b.rds"))
#'Marginal'
abnd.brm1b %>% brms::bayes_R2(re.form = NA, #or ~Treatment
                        summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2


#'Conditional'
abnd.brm1b %>% brms::bayes_R2(re.form = NULL, #or ~(1|plotID)
                        summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2

## ----end



## ---- recruitment univariate abundance all contrasts
abnd.brm1b <- readRDS(file = paste0(DATA_PATH, "modelled/abnd.brm1b.rds"))

##all (Tukey style) contrasts (exceedance)
abnd.brm1b%>%
  emmeans(~Treatment, type = 'response') %>%  #response scale
  pairs


abnd.brm1b%>%
  emmeans(~Treatment, type = 'link') %>% #link scale
  pairs() %>% #pairwise comparison
  gather_emmeans_draws() %>% #take all the draws for these comparisons, gather them (make long)
  #median_hdci(exp(.value)) #this would essentially give us the summary above. Nothing too special yet, but we have more control
  summarise('P>' = sum(.value>0)/n(), #exceedance probabilities
            'P<' = sum(.value<0)/n(),
  ) 

## ----end




## ---- recruitment univariate abundance planned contrasts

##Hi vs Low density, Hi vs Low Biomass

cmat<- cbind("Biomass.NatvsLess" = c(-1/2,-1/2,1/3,1/3,1/3),
             "Biomass.All.Vs.Low" = c(1/4, -1, 1/4, 1/4 ,1/4),
             "Density.9vsLess" = c(1/3,1/3,-1/2,-1/2,1/3),
             "Density.All.Vs.Low" = c(1/4, 1/4, -1,1/4,1/4))
cmat %>% rowSums()

abnd.brm1b %>% emmeans(~Treatment, type = 'link') %>%
  contrast(method = list(Treatment = cmat)) %>%
  gather_emmeans_draws()%>% 
  summarise("P>" = sum(.value>0)/n(),
            "P<" = sum(.value<0)/n())

## ----end

##### Temporal Subset of data ================================================
#I might not use this but while I waited for Murray re fixing the autocorrelation 
#I thought I should see if I could get any results without it

## ----recruitment univariate abundance end
end <- fish.sp.abnd %>% filter(Date == "2022-12-12")
abnd.end.glmmTMB <- glmmTMB(abundance ~ Treatment + (1|plotID), #Random intercept model
                            data = end,
                            family = poisson(link = "log"), #Poisson model
                            REML = TRUE)
abnd.end.glmmTMB.fixed <- update(abnd.end.glmmTMB, .~. - (1|plotID))
AICc(abnd.end.glmmTMB,abnd.end.glmmTMB.fixed)

abnd.end.resid <- simulateResiduals(abnd.end.glmmTMB.fixed, plot = TRUE)
#no problems detected

abnd.end.glmmTMB.fixed %>% summary()
abnd.end.glmmTMB.fixed %>% r.squaredGLMM()

abnd.end.glmmTMB.fixed %>% ggpredict(Terms = "Treatment") %>%  plot()


## ----end



   #### Summary figures =========================================================
## ---- recruitment univariate abundance summary figure
abnd.brm1b <- readRDS(file = paste0(DATA_PATH, "modelled/abnd.brm1b.rds"))

newdata <- abnd.brm1b %>% emmeans(~Treatment, type = "link") %>% 
  gather_emmeans_draws() %>% 
  mutate(Fit = exp(.value)) %>% 
  as.data.frame
head(newdata)

g1 <- newdata %>% ggplot() + 
  stat_slab(aes(
    x = Treatment, y = Fit,
    fill = stat(ggdist::cut_cdf_qi(cdf,
                                   .width = c(0.5, 0.8, 0.95),
                                   labels = scales::percent_format()
    ))
  ), color = "black") +
  scale_fill_brewer("Interval", direction = -1, na.translate = FALSE) +
  ylab("Total Abundance") +
  theme_classic()

abnd.em <- abnd.brm1b %>%
  emmeans(~Treatment, type = "link") %>%
  pairs() %>%
  gather_emmeans_draws() %>%
  mutate(Fit = exp(.value)) %>% as.data.frame()
head(abnd.em)

g2<- abnd.em %>%
  ggplot() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  # geom_vline(xintercept = 1.5, alpha=0.3, linetype = 'dashed') +
  stat_slab(aes(
    x = Fit, y = contrast,
    fill = stat(ggdist::cut_cdf_qi(cdf,
                                   .width = c(0.5, 0.8, 0.95),
                                   labels = scales::percent_format()
    ))
  ), color = "black") +
  scale_fill_brewer("Interval", direction = -1, na.translate = FALSE) +
  scale_x_continuous("Effect",
                     trans = scales::log2_trans(),
                     breaks = c(0.1, 0.5, 1, 1.1, 1.5, 2, 4)
  ) +

  theme_classic()
g1 + g2

  
## ----end

ggsave(filename = paste0(FIGS_PATH, "/bayes.abund.png"),
       g1,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.abund.contr.png"),
       g2,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.abund.both.png"),
       g1 + theme(legend.position = "none") + g2,
       height = 5,
       width = 15,
       dpi = 100)

  ### Species Richness ==========================================================

   #### Fit =====================================================================

## ---- recruitment univariate sp fit
sp.glmmTMB1 <- glmmTMB(sp.richness ~ 1 + (1|plotID), #Random intercept model
                         data = fish.sp.abnd,
                         family = poisson(link = "log"), #Poisson model
                         REML = TRUE) 
sp.glmmTMB2 <- update(sp.glmmTMB1, .~. + Treatment) #add Treatment as fixed

sp.glmmTMB3 <- update(sp.glmmTMB1, .~. + plot.weight) #plot.weight fixed

sp.glmmTMB4 <- update(sp.glmmTMB1, .~. + Density)#Density fixed

sp.glmmTMB5 <- update(sp.glmmTMB3, .~. + Density)#plot.weight + Density

sp.glmmTMB6 <- update(sp.glmmTMB3, .~. * Density) #plot.weight*Density

sp.glmmTMB7 <- update(sp.glmmTMB2, ~. + plot.weight) #Treatment + plot.weight

sp.glmmTMB8 <- update(sp.glmmTMB2, ~. * plot.weight) #Treatment * plot.weight

MuMIn::AICc(sp.glmmTMB1,sp.glmmTMB2, sp.glmmTMB3, sp.glmmTMB4,
            sp.glmmTMB5, sp.glmmTMB6,sp.glmmTMB7, sp.glmmTMB8) #compare models

## ----end
   #### Validate/ Refit =========================================================

## ---- recruitment univariate sp validate
sp.resid <- sp.glmmTMB2 %>% simulateResiduals(plot = TRUE)
sp.resid %>% testDispersion()
sp.resid %>% testUniformity()
#showing problems (underdispersion)
fish.sp.abnd <- fish.sp.abnd %>% 
  mutate(TIME = as.numeric(plotID) + as.numeric(Date)*10^-5)

sp.resid %>% testTemporalAutocorrelation(time = fish.sp.abnd$TIME)
#definitely autocorrelation
## ----end

## recruitment univariate sp refit autocor

sp.glmmTMB.ac <- update(sp.glmmTMB2, .~. + ar1(0 + factor(Date)|plotID))
AICc(sp.glmmTMB.ac,sp.glmmTMB2)

saveRDS(sp.glmmTMB.ac, file = paste0(DATA_PATH, "modelled/sp.glmmTMB.ac.rds") )

## ----end

## ---- recruitment univariate sp revalidate
sp.glmmTMB.ac <- readRDS(file = paste0(DATA_PATH, "modelled/sp.glmmTMB.ac.rds"))

sp.ac.resid <- sp.glmmTMB.ac %>% simulateResiduals(plot = TRUE)
sp.ac.resid %>% testTemporalAutocorrelation((time = fish.sp.abnd$TIME))
sp.ac.resid %>% testDispersion()

## ----end

## ---- recruitment univariate sp ac residuals v date
g1 <- ggplot(fish.sp.abnd) + aes(y = sp.ac.resid$fittedResiduals, x = Date) +
  geom_point() +
  geom_line() +
  facet_wrap(~plotID)


g2 <- ggplot(fish.sp.abnd) + aes(y = sp.ac.resid$scaledResiduals, x = Date) +
  geom_point() +
  geom_line() +
  facet_wrap(~plotID)

#before the ac term:
g3 <- ggplot(fish.sp.abnd) + aes(y = sp.resid$scaledResiduals, x = Date)+
  geom_point() +
  geom_line() +
  facet_wrap(~plotID)

ggsave(filename = paste0(FIGS_PATH, "/spAC.fit.residuals.date.png"),
       g1,
       width = 40,
       height = 25,
       dpi = 100)

ggsave(filename = paste0(FIGS_PATH, "/spAC.sc.residuals.date.png"),
       g2,
       width = 40,
       height = 25,
       dpi = 100)

ggsave(filename = paste0(FIGS_PATH, "/spNOAC.sc.residuals.date.png"),
       g3,
       width = 40,
       height = 25,
       dpi = 100)
## ----end


   #### Partial Plot ============================================================
## ---- recruitment univariate sp partial
sp.glmmTMB.ac %>% ggpredict(Terms = "Treatment") %>%  plot()
## ---- end

   #### Bayesian Model ==========================================================
   ##### Priors =================================================================

## ---- recruitment univariate sp priors1
#convert Date to factor first! (otherwise the large number could cause problems)
fish.sp.abnd <- fish.sp.abnd %>% mutate(Date = factor(Date))


sp.form <- bf(sp.richness ~ Treatment
                + (1|plotID),
                autocor = ~ ar(time = Date, gr = plotID, 
                               p = 1), #order of the autoregressive (1st order)
                family = poisson(link = "log"))

#sp.form %>%  get_prior(data = fish.sp.abnd)

## priors for Intercept
fish.sp.abnd %>% group_by(Treatment) %>%  summarise(log(median(sp.richness)), 
                                                    log(mad(sp.richness)))
## ----end

## ---- recruitment univariate sp priors2

##priors for Effects
log(sd(fish.sp.abnd$sp.richness)) #.73
#model.matrix(~Treatment, data = fish.sp.abnd) %>% head
apply(model.matrix(~Treatment, data = fish.sp.abnd), 2, sd)
log(sd(fish.sp.abnd$sp.richness))/apply(model.matrix(~Treatment, data = fish.sp.abnd), 2, sd)

##prior for sds
standist::visualize("cauchy(0,2)", xlim = c(0,10))

## ----end

## ----recruitment univariate sp prior fit
## eval set to 'false'

## without data (priors only) and without ar term to start (ggpredict won't work
##otherwise)
sp.form <- bf(sp.richness ~ Treatment
              + (1|plotID),
              # autocor = ~ ar(time = Date, gr = plotID, 
              #               p = 1), 
              family = poisson(link = "log"))

priors <- prior(normal(1,0.4), class = "Intercept") +
  prior(normal(0,2), class = "b") + 
  prior(cauchy(0,2), class = "sd") #+
  #prior(cauchy(0,2), class = "sderr") #+
  #prior(uniform(0,1), class = "ar")# ar prior as for abundance

sp.brm1 <- brm(sp.form,
                 data = fish.sp.abnd,
                 prior = priors,
                 sample_prior = "only", #just to start
                 iter = 5000, warmup = 1000,
                 chains = 3, cores = 3, 
                 thin = 5,
               seed = 123)
#save(sp.brm1, file = paste0(DATA_PATH, "modelled/sp.brmprior1.RData"))  
## ----end

## ----recruitment univariate sp brm1.prior check
load(file = paste0(DATA_PATH, "modelled/sp.brmprior1.RData"))

sp.brm1 %>% ggpredict(~Treatment) %>% plot (add.data = TRUE)
## ----end

     ##### Fitting ==============================================================
 
## ----recruitment univariate sp brmsfit
sp.form <- bf(sp.richness ~ Treatment
              + (1|plotID),
               autocor = ~ ar(time = Date, #as a factor
                              gr = plotID, 
                             p = 1), 
              family = poisson(link = "log"))

priors <- prior(normal(1,0.4), class = "Intercept") +
  prior(normal(0,2), class = "b") + 
  prior(cauchy(0,2), class = "sd") +
prior(cauchy(0,2), class = "sderr") +
prior(uniform(0,1), class = "ar")# ar prior as for abundance

sp.brm1a <- brm(sp.form,
               data = fish.sp.abnd,
               prior = priors,
               sample_prior = "yes", #sample priors and posteriors
               iter = 5000, warmup = 1000,
               chains = 3, cores = 3, 
               thin = 5,
               seed = 123)
#2026 divergent transitions
sp.brm1a %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/sp.brm1.rds"))
## ----end

##### Prior Checks ==============================================================

## ----recruitment univariate sp brm1a prior checks

sp.brm1a <- readRDS(file = paste0(DATA_PATH, "modelled/sp.brm1.rds"))

sp.brm1a %>%
  as_draws_df() %>% #get all the draws for everything estimated
  
  dplyr::select(!matches("^lp|^err|^r_|^\\.") ) %>% #remove variables starting with lp, err or r_ or .
  #Note removing the '.' cols (.iteration, .draw and .chain) changed the class
  
  pivot_longer(everything(), names_to = 'key') %>% #make long, with variable names in a column called 'key'. Note 
  
  mutate(Type = ifelse(str_detect(key, 'prior'), 'Prior', 'Posterior'), #classify within new col 'Type' whether Prior or Posterior using str_detect
         Class = case_when( #create column 'Class' to classify vars as:
           str_detect(key, '(^b|^prior).*Intercept$') ~ 'Intercept', #intercept, if 'key' starts with b or prior followed by any character ('.') with 'Intercept' at the end
           str_detect(key, 'b_Treatment.*|prior_b') ~ 'TREATMENT', #TREATMENT, if the string contains 'b_Treatment followed by any character ('.')
           str_detect(key, 'sd_') ~ 'sd', #sd, if the string contains sd ('sderr' will be included)
           str_detect(key, 'ar') ~ 'ar', #ar, if it contains ar
           str_detect(key, 'sderr') ~ 'sderr'), #sderr, if it contains sderr
         Par = str_replace(key, 'b_', '')) %>% 
  
  ggplot(aes(x = Type,  y = value, color = Par)) + #Plot with these overall aesthetics
  stat_pointinterval(position = position_dodge(), show.legend = FALSE)+ #plot as stat_point intervals
  facet_wrap(~Class,  scales = 'free') #separate plots by Class with each class having its own scales


## ----end

   ##### MCMC Diagnostics ======================================================
## ----recruitment univariate sp MCMC1
sp.brm1a <- readRDS(file = paste0(DATA_PATH, "modelled/sp.brm1.rds"))
pars <- sp.brm1a %>% get_variables()

wch <- str_extract(pars, #get the names of the variables
                   '^b_.*|^sd.*|^ar.*') %>% #that start with b, sd or ar
  na.omit # omit the rest, resulting in an object of class 'omit'
wch
##Trace Plots
stan_trace(sp.brm1a$fit, pars = wch)

##rhat - Scale reduction factor
stan_rhat(sp.brm1a$fit, pars = wch)


## ----recruitment univariate sp MCMC2

##problem MCMCs
##Autocorrelation factor
stan_ac(sp.brm1a$fit, pars = wch)

##ESS (effective sample size)
stan_ess(sp.brm1a$fit, pars = wch)

##Density plot
stan_dens(sp.brm1a$fit, pars = wch, separate_chains = TRUE)

##Density overlay
sp.brm1a%>% pp_check(type = 'dens_overlay', ndraws = 100)

## ----end


##### Refitting =================================================================

## ----recruitment univariate sp brms refit

#sp.brm1b <- update(sp.brm1a,
#                   control = list(adapt_delta = 0.99), #devote more of warmup to step-length determination
#                seed = 123)
#1999 divergent transitions
#ess and ac still poor
#sp.brm1b %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/sp.brm1b.rds") )

standist::visualize("cauchy(0,2)","cauchy(0,1)","cauchy(0,0.5)", xlim = c(0,10))
priors <- prior(normal(1,0.5), class = "Intercept") +
  prior(normal(0,2), class = "b") + 
  prior(cauchy(0,.5), class = "sd") +
  prior(cauchy(0,.5), class = "sderr") +
  prior(uniform(0,1), class = "ar")# ar prior as for abundance
#sp.brm1c <- update(sp.brm1a,
              #     seed = 123,
            #      prior = priors)
#1786 divergent transitions. Still ac and ess problems
#sp.brm1c %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/sp.brm1c.rds"))

#doubling iter, warmup, and thinning factor and with new priors
sp.brm1d <- brm(sp.form,
                data = fish.sp.abnd,
                prior = priors,
                sample_prior = "yes", #sample priors and posteriors
                iter = 10000, warmup = 2000,
                chains = 3, cores = 3, 
                thin = 10,
                seed = 123)
#2121 divergent transitions but ac and ess now acceptable
sp.brm1d %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/sp.brm1d.rds"))

## ----end

## ----recruitment univariate sp recheck priors and mcmc

sp.brm1d <- readRDS(file = paste0(DATA_PATH, "modelled/sp.brm1d.rds"))


sp.brm1d %>%
  as_draws_df() %>% #get all the draws for everything estimated
  
  dplyr::select(!matches("^lp|^err|^r_|^\\.") ) %>% #remove variables starting with lp, err or r_ or .
  #Note removing the '.' cols (.iteration, .draw and .chain) changed the class
  
  pivot_longer(everything(), names_to = 'key') %>% #make long, with variable names in a column called 'key'. Note 
  
  mutate(Type = ifelse(str_detect(key, 'prior'), 'Prior', 'Posterior'), #classify within new col 'Type' whether Prior or Posterior using str_detect
         Class = case_when( #create column 'Class' to classify vars as:
           str_detect(key, '(^b|^prior).*Intercept$') ~ 'Intercept', #intercept, if 'key' starts with b or prior followed by any character ('.') with 'Intercept' at the end
           str_detect(key, 'b_Treatment.*|prior_b') ~ 'TREATMENT', #TREATMENT, if the string contains 'b_Treatment followed by any character ('.')
           str_detect(key, 'sd_') ~ 'sd', #sd, if the string contains sd ('sderr' will be included)
           str_detect(key, 'ar') ~ 'ar', #ar, if it contains ar
           str_detect(key, 'sderr') ~ 'sderr'), #sderr, if it contains sderr
         Par = str_replace(key, 'b_', '')) %>% 
  
  ggplot(aes(x = Type,  y = value, color = Par)) + #Plot with these overall aesthetics
  stat_pointinterval(position = position_dodge(), show.legend = FALSE)+ #plot as stat_point intervals
  facet_wrap(~Class,  scales = 'free') #separate plots by Class with each class having its own scales

##Autocorrelation factor
stan_ac(sp.brm1d$fit, pars = wch)

##ESS (effective sample size)
stan_ess(sp.brm1d$fit, pars = wch)

## ----end

## ----recruitment univariate sp recheck recheck mcmc2
##Trace Plots
stan_trace(sp.brm1d$fit, pars = wch)

##rhat - Scale reduction factor
stan_rhat(sp.brm1d$fit, pars = wch)

##Density overlay
sp.brm1d %>% pp_check(type = 'dens_overlay', ndraws = 100)

##Density plot
sp.brm1d$fit %>% stan_dens( pars = wch, separate_chains = TRUE)
## ----end

## ----recruitment univariate sp refit again

#priors <- prior(normal(1,0.5), class = "Intercept") +
#  prior(normal(0,2), class = "b") + 
#  prior(cauchy(0,.5), class = "sd") +
#  prior(cauchy(0,.5), class = "sderr") #
#  prior(uniform(.75,1), class = "ar")

#sp.brm1e <- update(sp.brm1d, prior = priors)
#many warnings - largest r-hat 1.81, bulk and tail ess too low, 2280 divergent transitions, 1 chain where estimated BF of missing information was low
#sp.brm1e %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/sp.brm1e.rds"))

priors <- prior(normal(1,0.5), class = "Intercept") +
  prior(normal(0,2), class = "b") + 
  prior(cauchy(0,1), class = "sd") + #wider
  prior(cauchy(0,1), class = "sderr") +# wider
  prior(uniform(-1,1), class = "ar") #wider

sp.brm1f <- update(sp.brm1d, prior = priors, seed = 123)
#no divergent transitions. prior_summary still has uniform(0,1) ar prior (source = user)
sp.brm1f %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/sp.brm1f.rds"))
## ----end

## ----recruitment univariate sp priors mcmc sp.brm1f
sp.brm1f <- readRDS(file = paste0(DATA_PATH, "modelled/sp.brm1f.rds"))

sp.brm1f %>%
  as_draws_df() %>% #get all the draws for everything estimated
  
  dplyr::select(!matches("^lp|^err|^r_|^\\.") ) %>% #remove variables starting with lp, err or r_ or .
  #Note removing the '.' cols (.iteration, .draw and .chain) changed the class
  
  pivot_longer(everything(), names_to = 'key') %>% #make long, with variable names in a column called 'key'. Note 
  
  mutate(Type = ifelse(str_detect(key, 'prior'), 'Prior', 'Posterior'), #classify within new col 'Type' whether Prior or Posterior using str_detect
         Class = case_when( #create column 'Class' to classify vars as:
           str_detect(key, '(^b|^prior).*Intercept$') ~ 'Intercept', #intercept, if 'key' starts with b or prior followed by any character ('.') with 'Intercept' at the end
           str_detect(key, 'b_Treatment.*|prior_b') ~ 'TREATMENT', #TREATMENT, if the string contains 'b_Treatment followed by any character ('.')
           str_detect(key, 'sd_') ~ 'sd', #sd, if the string contains sd ('sderr' will be included)
           str_detect(key, 'ar') ~ 'ar', #ar, if it contains ar
           str_detect(key, 'sderr') ~ 'sderr'), #sderr, if it contains sderr
         Par = str_replace(key, 'b_', '')) %>% 
  
  ggplot(aes(x = Type,  y = value, color = Par)) + #Plot with these overall aesthetics
  stat_pointinterval(position = position_dodge(), show.legend = FALSE)+ #plot as stat_point intervals
  facet_wrap(~Class,  scales = 'free') #separate plots by Class with each class having its own scales


##Autocorrelation factor
stan_ac(sp.brm1f$fit, pars = wch)

##ESS (effective sample size)
stan_ess(sp.brm1f$fit, pars = wch)

##Density plot
sp.brm1f$fit %>% stan_dens( pars = wch, separate_chains = TRUE)

##Trace Plots
stan_trace(sp.brm1f$fit, pars = wch)

##rhat - Scale reduction factor
stan_rhat(sp.brm1f$fit, pars = wch)

##Density overlay
sp.brm1f %>% pp_check(type = 'dens_overlay', ndraws = 100)

## ----end

    ##### DHARMA Residuals ======================================================

## ----recruitment univariate sp DHARMA residuals


#step 1. Draw out predictions
preds <- sp.brm1f %>% posterior_predict(ndraws = 250, #extract 250 posterior draws from the posterior model
                                          summary = FALSE) #don't summarise - we want the whole distribution of them


#Step 2 create DHARMA resids
sp.resids <- createDHARMa(simulatedResponse = t(preds), #provide with simulated predictions, transposed with t()
                            observedResponse = fish.sp.abnd$sp.richness, #real response
                            fittedPredictedResponse = apply(preds, 2, median), #for the fitted predicted response, use the median of preds (in the columns, the second argument of apply defines the MARGIN, 2 being columns for matrices)
                            integerResponse = "TRUE" #is the response an integer? yes
                            #, re.form = "NULL") #this argument not supported (neither in glmmTMB)
)

#Step 3 - plot!
plot(sp.resids)


## ----end


   #### Model Investigation - Sp richness =======================================
   ##### Frequentist ============================================================
## ----recruitment univariate sp frequentist summary
sp.glmmTMB.ac %>% summary()
sp.glmmTMB.ac %>% r.squaredGLMM() # will return error
sp.glmmTMB2 %>% r.squaredGLMM()
## ----end
   

    ##### Bayesian ==============================================================
## ----recruitment univariate sp bayesian summary

#something is the matter here. why is 
sp.brm1f$fit %>% tidyMCMC(pars = wch,
                          estimate.method = "median",
                          conf.int = TRUE,
                          conf.method = "HPDinterval",
                          rhat = TRUE,
                          ess = TRUE)
#not giving a positive ar estimate???

sp.brm1f %>% brms::bayes_R2(re.form = NA, #or ~Treatment
                              summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2


#'Conditional'
sp.brm1f %>% brms::bayes_R2(re.form = NULL, #or ~(1|plotID)
                              summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2


## ----end

## ---- recruitment univariate sp all contrasts

##all (Tukey style) contrasts (exceedance)

sp.brm1f%>%
  emmeans(~Treatment, type = 'link') %>% #link scale
  pairs() %>% #pairwise comparison
  gather_emmeans_draws() %>% #take all the draws for these comparisons, gather them (make long)
  #median_hdci(exp(.value)) #this would essentially give us the summary above. Nothing too special yet, but we have more control
  summarise('P>' = sum(.value>0)/n(), #exceedance probabilities
            'P<' = sum(.value<0)/n(),
  ) 

## ----end

   ##### Temporal Subset of data ================================================

## ----recruitment univariate sp end
end <- fish.sp.abnd %>% filter(Date == "2022-12-12")
sp.end.glmmTMB <- glmmTMB(sp.richness ~ Treatment + (1|plotID), #Random intercept model
                          data = end,
                          family = poisson(link = "log"), #Poisson model
                          REML = TRUE)
sp.end.glmmTMB.fixed <- update(sp.end.glmmTMB, .~. - (1|plotID))
AICc(sp.end.glmmTMB,sp.end.glmmTMB.fixed)
sp.end.resid <- simulateResiduals(sp.end.glmmTMB.fixed, plot = TRUE)
#no dramas

sp.end.glmmTMB.fixed %>% summary()
#only the BH-W contrast was significant
sp.end.glmmTMB.fixed %>% r.squaredGLMM()

sp.end.glmmTMB.fixed %>% ggpredict(Terms = "Treatment") %>%  plot()
## ----end

## ----recruitment univariate sp end bayes fit
sp.end.form <- bf(sp.richness ~ Treatment,
              family = poisson(link = "log"))
priors <- prior(normal(1,0.4), class = "Intercept") +
  prior(normal(0,2), class = "b")

sp.end.brm1 <- brm(sp.end.form,
                      data = end,
                      prior = priors,
                      sample_prior = "yes", #sample priors and posteriors
                      iter = 5000, warmup = 1000,
                      chains = 3, cores = 3, 
                      thin = 5,
                   seed = 123)
## ----end

## ----recruitment univariate sp end bayes checks
sp.end.brm1 %>% SUYR_prior_and_posterior()

pars <- sp.end.brm1 %>% get_variables()

wch <- str_extract(pars, #get the names of the variables
                   '^b_.*') %>% #that start with b_
  na.omit # omit the rest, resulting in an object of class 'omit'
wch

##Trace Plots
stan_trace(sp.end.brm1$fit, pars = wch)

##Autocorrelation factor
stan_ac(sp.end.brm1$fit, pars = wch)

##rhat - Scale reduction factor
stan_rhat(sp.end.brm1$fit, pars = wch)

##ESS (effective sample size)
stan_ess(sp.end.brm1$fit, pars = wch)

##Density plot
stan_dens(sp.end.brm1$fit, pars = wch, separate_chains = TRUE)

##Density overlay
sp.end.brm1%>% pp_check(type = 'dens_overlay', ndraws = 100)

#DHARMA residuals

preds <- sp.end.brm1 %>% posterior_predict(ndraws = 250, 
                                        summary = FALSE) 


#Step 2 create DHARMA resids
sp.end.resids <- createDHARMa(simulatedResponse = t(preds), 
                          observedResponse = end$sp.richness, 
                          fittedPredictedResponse = apply(preds, 2, median),
                          integerResponse = "TRUE" 
)

#Step 3 - plot!
plot(sp.end.resids)


## ---end

## ----recruitment univariate sp end bayes summary

sp.end.brm1$fit %>% tidyMCMC(pars = wch,
                          estimate.method = "median",
                          conf.int = TRUE,
                          conf.method = "HPDinterval",
                          rhat = TRUE,
                          ess = TRUE)

#'Conditional'
sp.end.brm1 %>% brms::bayes_R2(re.form = NULL, #or ~(1|plotID)
                            summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2



## ----end

## ---- recruitment univariate sp all contrasts

##all (Tukey style) contrasts (exceedance)

sp.end.brm1f%>%
  emmeans(~Treatment, type = 'link') %>% #link scale
  pairs() %>% #pairwise comparison
  gather_emmeans_draws() %>% #take all the draws for these comparisons, gather them (make long)
  #median_hdci(exp(.value)) #this would essentially give us the summary above. Nothing too special yet, but we have more control
  summarise('P>' = sum(.value>0)/n(), #exceedance probabilities
            'P<' = sum(.value<0)/n(),
  ) 

sp.end.brm1%>%
  emmeans(~Treatment, type = 'link') %>% #link scale
  pairs() %>% #pairwise comparison
  gather_emmeans_draws() %>% #take all the draws for these comparisons, gather them (make long)
  #median_hdci(exp(.value)) #this would essentially give us the summary above. Nothing too special yet, but we have more control
  summarise('P>' = sum(.value>0)/n(), #exceedance probabilities
            'P<' = sum(.value<0)/n(),
  ) 

#still seeing most of the same contrasts with high evidence, with the addition 
#of BH-DL and BH-DM not having evidence (below 90%)
## ---end


   #### Summary figures =========================================================

## ---- recruitment univariate sp figures
sp.brm1f %>% ggemmeans(~Treatment) %>% plot


## ----end

  ### Halichoeres miniatus ======================================================

## ----recruitment univariate hm data

#remember when doing this, I need to include all zeros
common.abnd <- fishalgaedata %>% 
  mutate(plotID = factor(paste0(Treatment, Replicate)),
         Density = recode_factor(Treatment, "W" = 9, "BH" = 9, "BQ" = 9,
                                 "DM" = 5, "DL" = 3)) %>% 
  group_by(plotID, Date) %>% 
  summarise(hal.min = sum(count[Species == "Halichoeres miniatus"]),
            )

glimpse(hm.abnd)
## ----end

 ## Multivariate ================================================================

## ---- Recruitment Multivariate
  ### convert to abund/plot (eventually could use biomass if I need to)

fish.wide <- fishdata %>% 
  group_by(Treatment, Replicate, Date, Species) %>%
  summarise(abundance = sum(count)) %>% ## get abund/plot per species (eventually could use biomass if I need to)
  pivot_wider(names_from = Species,   ## convert to wide
              values_from = abundance,
              values_fill = 0)
  
 ## create distance matrix
#fish.dist <- vegdist(wisconsin(fish.wide[,-c(1:3,5)]^0.25), ## 4th root transformation, remove factors and 'empty' col
#                     "bray") #bray standardisation, 
### warning: you have empty rows: their dissimilarities may be meaningless in method “bray”

### withhold empty rows:

#fish.no.0 <- fish.wide %>% 
#  as.data.frame() %>% ##fish.wide was a tibble. Without changing to df first, the factors were added back after select
#  dplyr::select(is.numeric) %>% ##select numeric cols
#  filter(rowSums(.) != 0 ) %>% ## keep only rows with non zero sums
#  left_join(.,fish.wide, by = c("Halichoeres miniatus", "Siganus doliatus", 
#                                "Pomacentrus tripunctatus")   ) ## join the factors back on


### tried several methods to remove empty rows while keeping the factor cols, but so far this is unfortunately my best:
wide.row.sums <- fish.wide %>% 
  as.data.frame() %>% 
  dplyr::select(is.numeric) %>% 
  mutate(r.s = rowSums(.)) %>%  
  select(r.s)

fish.no.0 <- fish.wide[which(wide.row.sums != 0), ]

fish.no.0 %>% glimpse()


fish.dist <- vegdist(wisconsin(fish.no.0[,-c(1:3,5)]^0.25), "bray")


 ## mds
fish.mds <- metaMDS(fish.dist, k=2, seed = 123)
#no convergence -- monoMDS stopping criteria:
# 2: no. of iterations >= maxit - it reached 20 iterations
# 18: stress ratio > sratmax
stressplot(fish.mds)

plot(fish.mds, type="text", display="sites" ) #something weird happening at row 258 - 1 lethrinus nebulosus, that's it

 ## rerun without row 258
metaMDS(fish.no.0[-258,-c(1:3,5)]) %>% plot(type = "text", display = "sites")
##more sensible plot, higher stress and need more plotting to see if treatments had any influence


 ### 3D, no row 258

fish.dist <- vegdist(wisconsin(fish.no.0[-258,
                                         -c(1:3,5)]^0.25), "bray") ##something about this messed up the mds and scores couldn't be obtained (ndim = 1)
fish.mds <- metaMDS(fish.dist, k=3, 
                    seed = 123)
#stress improvement below 0.2, no convergence

fish.mds.scores <- fish.mds %>%
  fortify() ## Error in rep("sites", nrow(df)) : invalid 'times' argument

scores(fish.mds, choices = 1:3, display = c("sites")) #this seemed to work, although 333 rows - shouldn't there be 446, the number of rows of fish.no.0 - 1?
#also couldn't get "species" scores

## ----end