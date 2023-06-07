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
  summarise(plot.weight = sum(Weight)) %>% # add plot.weight column to fishdata
  left_join(fishdata, .) %>% 
  mutate(plotID = factor(paste0(Treatment, Replicate)),
         Density = recode_factor(Treatment, "W" = 9, "BH" = 9, "BQ" = 9,
                                 "DM" = 5, "DL" = 3),
         Day = Date %>% factor %>% as.numeric() %>% 
           factor) #create day, a factor of numbers 1-18
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

algae.sum %>% filter(Treatment == "W") %>% pull(mean.plot.wt)

algae.sum %>% 
  mutate(W.ratio = mean.plot.wt/
           algae.sum %>% filter(Treatment == "W") %>% pull (mean.plot.wt),
         DL.ratio = mean.plot.wt/
           algae.sum %>% filter(Treatment == "DL") %>% pull (mean.plot.wt))

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
  mutate_at(c(2:5,9,11), factor)

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

fishalgaedata <- read_csv(file = paste0(DATA_PATH, "processed/fishalgaedata.csv")) %>% 
mutate_at(c(2:5,9), factor)
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

common.abnd <- commondata %>% 
  count(Treatment, Replicate ,Date, Species, #count occurrences of these combinations
        name = "abundance") %>% #call it abundance
  complete(Treatment, Replicate ,Date, Species, #complete according to these
           fill = list(abundance = 0)) %>% #by adding a 0 where incomplete 
  left_join(.,fishalgaedata %>% #join back together with fishalgaedata
              select(-c(Length, count, Family, Species)), #(a version without these cols)
            multiple = "first") %>%  #if there are repeat combinations, use the first value 
  left_join(.,fishalgaedata %>% select(Species,Family), #join Family back on
            multiple = "first")

write_csv(common.abnd, paste0(DATA_PATH, "summarised/common.abnd.csv") )

## plot abundance
common.abnd%>%
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



## ----end

ggsave(filename = paste0(FIGS_PATH, "/EDAfish.common.png"),
       g.common.abnd,
       width = 10,
       height = 5,
       dpi = 100)

## ---- fish EDA common species2
## plot abundance over time

common.abnd %>% 
  ggplot() + aes(y = abundance, x = Date, colour = Treatment) + ## 
  geom_point(position = position_jitterdodge(jitter.width = 0.02, dodge.width = 0.9), alpha = 1) +
  
  facet_wrap(~Species) +
  geom_smooth() +
  ylab("Abundance" ) + 
  theme(family = "calibri", text = element_text( size = 8, color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid = element_blank()
  ) +
  theme_bw() -> g.common.abnd.time
g.common.abnd.time



## ----end

ggsave(filename = paste0(FIGS_PATH, "/EDAfish.common.time.png"),
       g.common.abnd.time,
       width = 10,
       height = 5,
       dpi = 100)

## ----fish EDA common species3
## plot abundance vs algal biomass
common.abnd %>% 
  ggplot() + aes(y = abundance, x = plot.weight) + ## set overall aesthetic
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

#### Temporal Patterns in more detail ===========================================

## ----fish EDA temporal hump when
fishalgaedata <- read_csv(file = paste0(DATA_PATH, "processed/fishalgaedata.csv")) %>% 
  mutate_at(c(2:5,9,11), factor)
glimpse(fishalgaedata)


fish.sp.abnd <-fishalgaedata %>% 
  group_by(Treatment, Replicate, Date) %>% 
  summarise(abundance = sum(count), #calculate total abnd
            sp.richness = if_else(Species != "empty", 
                                  true = length(unique(Species)), 
                                  false = 0), #calculate sp. richness
            plot.weight = plot.weight # include the biomass of each plot in the output
  ) %>% 
  left_join(.,fishalgaedata %>% #join back together with fishalgaedata
              select(-c(Length, count, Family, Species)) ) %>% #a version without these
  distinct() %>% 
  as.data.frame()

dm.dat <- fish.sp.abnd %>% 
    filter(Treatment == "DM")


#treatment DM as a whole
dm.dat%>%
  group_by(Treatment, Date) %>% 
  summarise(sp.rich = mean(sp.richness),
            abund = mean(abundance)) %>%
  filter(abund == max(abund) | sp.rich == max(sp.rich))
## ----end

## ----fish EDA temporal hump abundance

#individual DM plot level abundance
dm.dat %>% 
  filter(abundance == max(abundance) | sp.richness == max(sp.richness))


g1 <- dm.dat %>% 
  ggplot() + aes(x = Day, y = abundance) +
  geom_point() + 
  facet_wrap(~plotID) +
  theme_bw()

g1
## ----end



## ----fish EDA temporal hump sp.richness

g2 <- dm.dat %>% 
  ggplot() + aes(x = Day, y = sp.richness) +
  geom_point() + 
  facet_wrap(~plotID) +
  theme_bw()
g2

## ----end

ggsave(filename = paste0(FIGS_PATH, "/DM.hump.abund.png"),
       g1,
       height = 5,
       width= 10)

ggsave(filename = paste0(FIGS_PATH, "/DM.hump.sp.png"),
       g2,
       height = 5,
       width= 10)


## ----fish EDA temporal hump day 6 - 10

dm.dat %>% 
  group_by(Replicate) %>% 
  filter(abundance == max(abundance) 
         | sp.richness == max(sp.richness)) %>% 
 filter(Day %in% c(6:10))

## ----end


## ----fish EDA temporal hump species increase
dm.dat.sub <- fishalgaedata %>% 
  filter(plotID %in% c("DM4", "DM2", "DM5")) %>% 
  group_by(plotID, Day, Species) %>% 
  summarise(abundance = sum(count)) %>% 
  ungroup() %>% droplevels() %>% 
  complete(plotID, Day, Species,
           fill = list(abundance = 0))


dm.dat.sub %>% select(Species) %>% unique()

#order the dataset descending according to count and show me the first 20
dm.dat.sub %>% arrange(desc(abundance)) %>% head(20)

##----end


##---- fish EDA temporal hump quick nmds
dm.dat.wide <- dm.dat.sub %>% 
  pivot_wider(names_from = "Species", values_from ="abundance") %>% 
  mutate(ID.Day = paste0(plotID, ".", Day)) %>% 
  column_to_rownames(var = "ID.Day") %>% 
  data.frame()

View(dm.dat.wide)

dm.mds <- metaMDS(dm.dat.wide %>% select(!c(plotID, Day)), seed = 123)
dm.mds

scores <- dm.mds %>% fortify
scores
g <-
  ggplot(data = NULL, aes(y=NMDS2, x=NMDS1)) +
  geom_hline(yintercept=0, linetype='dotted') +
  geom_vline(xintercept=0, linetype='dotted') +
  geom_point(data=scores %>%
               filter(Score=='sites'),
             aes(color=dm.dat.wide$plotID))  +#colour the points according to their plotID
  geom_text_repel(data = scores %>% 
                    filter(Score == 'sites'),
                    aes(label = dm.dat.wide$Day, hjust=-0.2) ) +
  labs(color = "Patch")
g

##----end

ggsave(filename = paste0(FIGS_PATH, "/nmds.DM245.png"),
                         g,
                         height = 10,
                         width = 10,
                         dpi = 100 )

##---- fish EDA temporal hump look at raw data for those DMS
dm.dat.wide %>% View()

fishalgaedata %>% 
  filter(plotID %in% c("DM4", "DM2", "DM5")) %>% 
  arrange(plotID) %>% View ()
# 

##----end

## ----fish EDA temporal hump specific patch.species.plots
## plot abundance over time


g.dm <- 
  ggplot(dm.dat.sub %>% 
           filter(Species %in% c("Halichoeres miniatus", "Petroscirtes sp.", 
                                 "Pomacentrus tripunctatus", "Siganus doliatus")
                  )) + aes(x = Day, y = abundance) + 
  geom_point() +
  facet_grid(rows = vars(Species), cols = vars(plotID)) +
  theme(family = "calibri", text = element_text( size = 8, color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        panel.grid = element_blank()
  ) +
  theme_bw()

g.dm

## ----end

ggsave(filename = paste0(FIGS_PATH, "/4commonDMhump.png"),
       g.dm,
       width = 15,
       height = 15,
       dpi = 100)

## Univariate modelling ========================================================

  ### Abundance =================================================================
## ----recruitment univariate setup data
fishalgaedata <- read_csv(file = paste0(DATA_PATH, "processed/fishalgaedata.csv")) %>% 
  mutate_at(c(2:5,9,11), factor)
glimpse(fishalgaedata)


fish.sp.abnd <-fishalgaedata %>% 
  group_by(Treatment, Replicate, Date) %>% 
  summarise(abundance = sum(count), #calculate total abnd
            sp.richness = if_else(Species != "empty", 
                                  true = length(unique(Species)), 
                                  false = 0), #calculate sp. richness
            plot.weight = plot.weight # include the biomass of each plot in the output
  ) %>% 
  left_join(.,fishalgaedata %>% #join back together with fishalgaedata
              select(-c(Length, count, Family, Species)) ) %>% #a version without these
  distinct() %>% 
  as.data.frame() #make into a data frame instead of tbl (prevented ggpredict[]



fish.sp.abnd %>% glimpse()
fish.sp.abnd$Day %>% levels()
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
#fish.sp.abnd <- fish.sp.abnd %>% 
#  mutate(TIME = as.numeric(plotID) + as.numeric(Day)*10^-2)
#fish.sp.abnd %>% head
#fish.sp.abnd %>% tail

#abnd.resid %>% testTemporalAutocorrelation(time = fish.sp.abnd$TIME)

acf(residuals(abnd.glmmTMB2, type = "pearson"))
#definite autocorrelation
testTemporalAutocorrelation(abnd.resid, time = fish.sp.abnd$Date)#won't work

resid1 <- recalculateResiduals(abnd.resid, group = fish.sp.abnd$Date)
testTemporalAutocorrelation(resid1, time = unique(fish.sp.abnd$Date))
## In my case, DHARMa DOES detect the autocorrelation here


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


abnd.ac.resid <- abnd.glmmTMB.ac %>% simulateResiduals(plot = T)
#abnd.ac.resid %>% testTemporalAutocorrelation(time = fish.sp.abnd$TIME)

acf(residuals(abnd.glmmTMB.ac, type = "pearson"))
#autocorrelation accounted for

resid1 <- recalculateResiduals(abnd.ac.resid, group = fish.sp.abnd$Date)
testTemporalAutocorrelation(resid1, time = unique(fish.sp.abnd$Date))
#despite DHARMA not seeing this

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
                   autocor = ~ ar(time = Day, gr = plotID, 
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
                autocor = ~ ar(time = Day, gr = plotID, p = 1), 
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

#test ac
#acf(residuals(abnd.brm1b, method = "pearson"))

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
#fish.sp.abnd <- fish.sp.abnd %>% 
#  mutate(TIME = as.numeric(plotID) + as.numeric(Day)*10^-2)

#sp.resid %>% testTemporalAutocorrelation(time = fish.sp.abnd$TIME)

#sp.resid %>% testTemporalAutocorrelation(time =fish.sp.abnd$Day)

#test <- data.frame(Resid = sp.resid$scaledResiduals, group = fish.sp.abnd$plotID, 
#           Day = fish.sp.abnd$Day) %>%
#  group_by(Day) %>%
#  summarise(Resid = mean(Resid)) #something murray tried

sp.resid.recalc <- sp.resid %>% recalculateResiduals(group = fish.sp.abnd$Date)

sp.resid.recalc %>% testTemporalAutocorrelation(time = unique(fish.sp.abnd$Date))
#definitely autocorrelation, and DHARMa is picking it up

acf(residuals(sp.glmmTMB2, type = "pearson"))
c#acf also shows autocorrelation
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

sp.ac.resid.recalc <- sp.ac.resid %>% recalculateResiduals(group = fish.sp.abnd$Date)

sp.ac.resid.recalc %>%  testTemporalAutocorrelation(time = unique(fish.sp.abnd$Date))

acf(residuals(sp.glmmTMB.ac, type = "pearson"))

## ----end

## ---- recruitment univariate sp ac residuals v date
g1 <- ggplot(fish.sp.abnd) + aes(y = sp.ac.resid$fittedResiduals, x = Day) +
  geom_point() +
  geom_line() +
  facet_wrap(~plotID)


g2 <- ggplot(fish.sp.abnd) + aes(y = sp.ac.resid$scaledResiduals, x = Day) +
  geom_point() +
  geom_line() +
  facet_wrap(~plotID)

#before the ac term:
g3 <- ggplot(fish.sp.abnd) + aes(y = sp.resid$scaledResiduals, x = Day)+
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
glimpse(fish.sp.abnd)


sp.form <- bf(sp.richness ~ Treatment
                + (1|plotID),
                autocor = ~ ar(time = Day, gr = plotID, 
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
              # autocor = ~ ar(time = Day, gr = plotID, 
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
               autocor = ~ ar(time = Day, #as a factor
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

sp.brm1f <- readRDS(file = paste0(DATA_PATH, "modelled/sp.brm1f.rds"))
sp.brm1f %>% ggemmeans(~Treatment) %>% plot


newdata <- sp.brm1f %>% emmeans(~Treatment, type = "link") %>% 
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
  ylab("Species Richness") +
  theme_classic()

sp.em <- sp.brm1f %>%
  emmeans(~Treatment, type = "link") %>%
  pairs() %>%
  gather_emmeans_draws() %>%
  mutate(Fit = exp(.value)) %>% as.data.frame()
head(sp.em)

g2<- sp.em %>%
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

ggsave(filename = paste0(FIGS_PATH, "/bayes.sp.png"),
       g1,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.sp.contr.png"),
       g2,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.sp.both.png"),
       g1 + theme(legend.position = "none") + g2,
       height = 5,
       width = 15,
       dpi = 100)

  ### Halichoeres miniatus abundance=============================================

   #### Fit =====================================================================
## ----recruitment univariate hm fit

common.abnd <- read_csv( paste0(DATA_PATH, "summarised/common.abnd.csv") ) %>% 
  mutate_at(c(1:4,7,9, 10), factor) %>% 
  data.frame()

#Fewer candidate models (ones that make theoretical sense, instead of dredging)
hm.glmmTMB1 <- glmmTMB(abundance ~ 1 + (1|plotID), #random intercept mode
                      data = common.abnd %>% 
                        filter(Species == "Halichoeres miniatus"),
                      family = poisson(link = "log"),
                      REML = TRUE)

hm.glmmTMB2 <- update(hm.glmmTMB1, .~. + Treatment) #Treatment fixed, Random int

hm.glmmTMB3 <- update(hm.glmmTMB1, .~. + plot.weight) #plot.weight fixed, rand int



MuMIn::AICc(hm.glmmTMB1,hm.glmmTMB2, hm.glmmTMB3)


## ----end

 #### Validate ==================================================================

## ---- recruitment univariate hm validate
hm.resid <- hm.glmmTMB2 %>% simulateResiduals(plot = TRUE)

##check autocorrelation
#common.abnd <- common.abnd %>% 
#  mutate(TIME = as.numeric(plotID) + as.numeric(Day)*10^-2)

#hm.resid %>% testTemporalAutocorrelation(time = common.abnd %>% 
 #                                          filter(Species == "Halichoeres miniatus") %>% 
#                                           pull(TIME) ) #extract just the Time column as a vector

acf(residuals(hm.glmmTMB2, method = "pearson"))


## ----end

## ----recruitment univariate hm refit revalidate

hm.glmmTMB.ac <- update(hm.glmmTMB2, .~. + ar1(0 + factor(Date)|plotID) )

acf(residuals(hm.glmmTMB.ac, method = "pearson"))

hm.ac.resid <- hm.glmmTMB.ac %>% simulateResiduals(plot = TRUE)
#hm.ac.resid %>% testTemporalAutocorrelation(time = common.abnd %>% 
#                                           filter(Species == "Halichoeres miniatus") %>% 
#                                           pull(TIME) )

hm.ac.resid %>% testDispersion()
#stil evidence of underdispersion

#### Partial ====================================================================

## ----recruitment univariate hm partial
hm.glmmTMB2 %>% ggpredict(terms = "Treatment") %>% plot()

## ----end

 #### Bayesian ==================================================================
 ##### Priors ===================================================================

## ----recruitment univariate hm priors1
common.abnd %>% filter(Species == "Halichoeres miniatus") %>% 
  group_by(Treatment) %>%  summarise(log(median(abundance)), 
                                       log(mad(abundance)))

##priors for Effects
common.abnd %>% filter(Species == "Halichoeres miniatus") %>% 
pull(abundance) %>% sd() %>% 
  log()/apply(model.matrix(~Treatment, data = common.abnd), 2, sd)

##

    ##### Fitting ===============================================================

## ----recruitment univariate hm brmsfit
hm.form <- bf(abundance ~ Treatment
              + (1|plotID),
              autocor = ~ ar(time = Day, gr = plotID, 
                             p = 1),
              family = poisson(link = "log") )
priors <- prior(normal(1.6,0.5), class = "Intercept") +
  prior(normal(0,2), class = "b") + 
  prior(cauchy(0,0.5), class = "sd") + # narrower, initially 0,1
  prior(cauchy(0,0.5), class = "sderr") +# narrower, initially 0,1
  prior(uniform(-1,1), class = "ar") #initially -1,1

hm.brm1 <- brm(hm.form,
                data = common.abnd %>% 
                 filter(Species == "Halichoeres miniatus"),
                prior = priors,
                sample_prior = "yes", #sample priors and posteriors
                iter = 10000, warmup = 2000,
               control = list(adapt_delta = 0.99), #devote more of warmup to step-length determination
                chains = 3, cores = 3, 
                thin = 10,
                seed = 123)

hm.brm1 %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/hm.brm1.rds"))


#common.abnd.num.day <- common.abnd %>% 
#  mutate(Day = as.numeric(Day))


#hm.day.num.brm <- brm(hm.form,
#                      data = common.abnd.num.day %>% 
#                        filter(Species == "Halichoeres miniatus"),
#                      prior = priors,
#                      sample_prior = "yes", #sample priors and posteriors
#                      iter = 5000, warmup = 1000,
#                      chains = 3, cores = 3, 
#                      thin = 5,
#                      seed = 123)
#identical prior check,MCMC,Dharma residuals and summary
## ----end

 ##### Prior Checks =============================================================
## ----recruitment univariate hm brm1 prior check
hm.brm1 <- readRDS(file = paste0(DATA_PATH, "modelled/hm.brm1.rds"))

hm.brm1 %>% 
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

  #####MCMC =====================================================================

## ----recruitment univariate hm brm1 MCMC
hm.brm1 <- readRDS(file = paste0(DATA_PATH, "modelled/hm.brm1.rds"))

pars <- hm.brm1 %>% get_variables()

wch <- str_extract(pars, #get the names of the variables
                   '^b_.*|^sd.*|^ar.*') %>% #that start with b, sd or ar
  na.omit # omit the rest, resulting in an object of class 'omit'
wch
##Trace Plots
stan_trace(hm.brm1$fit, pars = wch)

##Autocorrelation factor
stan_ac(hm.brm1$fit, pars = wch)

##rhat - Scale reduction factor
stan_rhat(hm.brm1$fit, pars = wch)

##ESS (effective sample size)
stan_ess(hm.brm1$fit, pars = wch)

##Density plot
stan_dens(hm.brm1$fit, pars = wch, separate_chains = TRUE)

##Density overlay
hm.brm1%>% pp_check(type = 'dens_overlay', ndraws = 100)

## ----end

##### DHARMA Residuals ==========================================================

## ---- recruitment univariate hm brm1 DHARMA

#step 1. Draw out predictions
preds <- hm.brm1 %>% posterior_predict(ndraws = 250, #extract 250 posterior draws from the posterior model
                                        summary = FALSE) #don't summarise - we want the whole distribution of them


#Step 2 create DHARMA resids
resids <- createDHARMa(simulatedResponse = t(preds), #provide with simulated predictions, transposed with t()
                          observedResponse = common.abnd %>% 
                         filter(Species == "Halichoeres miniatus") %>% 
                         pull(abundance), #real response
                          fittedPredictedResponse = apply(preds, 2, median), #for the fitted predicted response, use the median of preds (in the columns, the second argument of apply defines the MARGIN, 2 being columns for matrices)
                          integerResponse = "TRUE" #is the response an integer? yes
                          #, re.form = "NULL") #this argument not supported (neither in glmmTMB)
)

#Step 3 - plot!
plot(resids)

## ----end

#### Model Investigation ========================================================
#Frequentist
## ----recruitment univariate hm frequentist summary
hm.glmmTMB.ac %>% summary()

hm.glmmTMB2 %>% r.squaredGLMM()
## ----end

#Bayesian

## ---- recruitment univariate hm brm1 summary
hm.brm1$fit %>% tidyMCMC(pars = wch,
                     estimate.method = "median",
                     conf.int = TRUE,
                     conf.method = "HPDinterval",
                     rhat = TRUE,
                     ess = TRUE)

#"Marginal"
hm.brm1 %>% brms::bayes_R2(re.form = NA, #or ~Treatment
                            summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2

# "Conditional"
hm.brm1 %>% brms::bayes_R2(re.form = NULL, #or ~(1|plotID)
                            summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2


## ----end

## ---- recruitment univariate hm brm contrasts
hm.brm1%>%
  emmeans(~Treatment, type = 'link') %>% #link scale
  pairs() %>% #pairwise comparison
  gather_emmeans_draws() %>% #take all the draws for these comparisons, gather them (make long)
  #median_hdci(exp(.value)) #this would essentially give us the summary above. Nothing too special yet, but we have more control
  summarise('P>' = sum(.value>0)/n(), #exceedance probabilities
            'P<' = sum(.value<0)/n(),
  ) 

## ----end

#### Summary figures =========================================================

## ---- recruitment univariate hm figures

hm.brm1 <- readRDS(file = paste0(DATA_PATH, "modelled/hm.brm1.rds"))
hm.brm1 %>% ggemmeans(~Treatment) %>% plot


newdata <- hm.brm1 %>% emmeans(~Treatment, type = "link") %>% 
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
  ylab("H. miniatus abundance") +
  theme_classic()

hm.em <- hm.brm1 %>%
  emmeans(~Treatment, type = "link") %>%
  pairs() %>%
  gather_emmeans_draws() %>%
  mutate(Fit = exp(.value)) %>% as.data.frame()
head(sp.em)

g2<- hm.em %>%
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

ggsave(filename = paste0(FIGS_PATH, "/bayes.hm.png"),
       g1,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.hm.contr.png"),
       g2,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.hm.both.png"),
       g1 + theme(legend.position = "none") + g2,
       height = 5,
       width = 15,
       dpi = 100)

### Siganus doliatus abundance  =================================================

#### Fit =====================================================================
## ----recruitment univariate sd fit

common.abnd <- read_csv( paste0(DATA_PATH, "summarised/common.abnd.csv") ) %>% 
  mutate_at(c(1:4,7,9, 10), factor) %>% 
  data.frame()

#Fewer candidate models (ones that make theoretical sense, instead of dredging)
sd.glmmTMB1 <- glmmTMB(abundance ~ 1 + (1|plotID), #random intercept mode
                       data = common.abnd %>% 
                         filter(Species == "Siganus doliatus"),
                       family = poisson(link = "log"),
                       REML = TRUE)

sd.glmmTMB2 <- update(sd.glmmTMB1, .~. + Treatment) #Treatment fixed, Random int

sd.glmmTMB3 <- update(sd.glmmTMB1, .~. + plot.weight) #plot.weight fixed, rand int



MuMIn::AICc(sd.glmmTMB1,sd.glmmTMB2, sd.glmmTMB3)


## ----end

#### Validate ==================================================================

## ---- recruitment univariate sd validate
sd.resid <- sd.glmmTMB2 %>% simulateResiduals(plot = TRUE)

##check autocorrelation
#common.abnd <- common.abnd %>% 
#  mutate(TIME = as.numeric(plotID) + as.numeric(Day)*10^-2)

#hm.resid %>% testTemporalAutocorrelation(time = common.abnd %>% 
#                                          filter(Species == "Halichoeres miniatus") %>% 
#                                           pull(TIME) ) #extract just the Time column as a vector

acf(residuals(sd.glmmTMB2, method = "pearson"))


## ----end

## ----recruitment univariate sd refit revalidate

sd.glmmTMB.ac <- update(sd.glmmTMB2, .~. + ar1(0 + factor(Date)|plotID) )

sd.glmmTMB.ac %>% AICc(., sd.glmmTMB2)

acf(residuals(sd.glmmTMB.ac, method = "pearson"))

sd.ac.resid <- sd.glmmTMB.ac %>% simulateResiduals(plot = TRUE)
#sd.ac.resid %>% testTemporalAutocorrelation(time = common.abnd %>% 
#                                           filter(Species == "Halichoeres miniatus") %>% 
#                                           pull(TIME) )

sd.ac.resid %>% testDispersion()
#some evidence of underdispersion

#### Partial ====================================================================

## ----recruitment univariate sd partial
sd.glmmTMB2 %>% ggpredict(terms = "Treatment") %>% plot()

## ----end

#### Bayesian ==================================================================
##### Priors ===================================================================

## ----recruitment univariate sd priors1

common.abnd %>% filter(Species == "Siganus doliatus") %>% 
  group_by(Treatment) %>%  summarise(log(median(abundance)), 
                                     log(mad(abundance)))

##priors for Effects
common.abnd %>% filter(Species == "Siganus doliatus") %>% 
  pull(abundance) %>% sd() %>% 
  log()/apply(model.matrix(~Treatment, data = common.abnd), 2, sd)


## ----end

## ----recruitment univariate sd treatment reorder
dat.sub <- common.abnd %>% filter(Species == "Siganus doliatus") %>% 
  droplevels() %>% 
  mutate(Treatment = forcats::fct_relevel(Treatment, "W")) #change first level to W

## ----end

    ##### Fitting ===============================================================

## ----recruitment univariate sd brmsfit
sd.form <- bf(abundance ~ Treatment
              + (1|plotID),
              autocor = ~ ar(time = Day, gr = plotID, 
                             p = 1),
              family = poisson(link = "log") )
priors <- prior(normal(0.7, 0.4), class = "Intercept") +
  prior(normal(0,2), class = "b") +
  #prior(normal(0,0.4), class = "Intercept") +
  #prior(normal(0,1), class = "b") + 
  prior(cauchy(0,0.5), class = "sd") + 
  prior(cauchy(0,0.5), class = "sderr") +
  prior(uniform(-1,1), class = "ar") 

sd.brm2 <- brm(sd.form,
               data = dat.sub,
               prior = priors,
               sample_prior = "yes", #sample priors and posteriors
               iter = 10000, warmup = 2000,
               control = list(adapt_delta = 0.99), #devote more of warmup to step-length determination
               chains = 3, cores = 3, 
               thin = 10,
               seed = 123)

#sd.brm1 %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/sd.brm1.rds"))
sd.brm2 %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/sd.brm2.rds"))

## ----end

 ##### Prior Checks =============================================================
## ----recruitment univariate sd brm2 prior check
sd.brm2 <- readRDS(file = paste0(DATA_PATH, "modelled/sd.brm2.rds"))

sd.brm2 %>% 
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
         Par = str_replace(key, 'b_', '')) %>% filter (Class == "ar") %>% View()
  
  ggplot(aes(x = Type,  y = value, color = Par)) + #Plot with these overall aesthetics
  stat_pointinterval(position = position_dodge(), show.legend = FALSE)+ #plot as stat_point intervals
  facet_wrap(~Class,  scales = 'free') #separate plots by Class with each class having its own scales


## ----end

  #####MCMC =====================================================================

## ----recruitment univariate sd brm2 MCMC
sd.brm2 <- readRDS(file = paste0(DATA_PATH, "modelled/sd.brm2.rds"))

pars <- sd.brm2 %>% get_variables()

wch <- str_extract(pars, #get the names of the variables
                   '^b_.*|^sd.*|^ar.*') %>% #that start with b, sd or ar
  na.omit # omit the rest, resulting in an object of class 'omit'
wch
##Trace Plots
stan_trace(sd.brm2$fit, pars = wch)

##Autocorrelation factor
stan_ac(sd.brm2$fit, pars = wch)

##rhat - Scale reduction factor
stan_rhat(sd.brm2$fit, pars = wch)

##ESS (effective sample size)
stan_ess(sd.brm2$fit, pars = wch)

##Density plot
stan_dens(sd.brm2$fit, pars = wch, separate_chains = TRUE)

##Density overlay
sd.brm2%>% pp_check(type = 'dens_overlay', ndraws = 100)

## ----end

##### DHARMA Residuals ==========================================================

## ---- recruitment univariate sd brm2 DHARMa

#step 1. Draw out predictions
preds <- sd.brm2 %>% posterior_predict(ndraws = 250, #extract 250 posterior draws from the posterior model
                                       summary = FALSE) #don't summarise - we want the whole distribution of them


#Step 2 create DHARMA resids
resids <- createDHARMa(simulatedResponse = t(preds), #provide with simulated predictions, transposed with t()
                       observedResponse = dat.sub %>% 
                         filter(Species == "Siganus doliatus") %>% 
                         pull(abundance), #real response
                       fittedPredictedResponse = apply(preds, 2, median), #for the fitted predicted response, use the median of preds (in the columns, the second argument of apply defines the MARGIN, 2 being columns for matrices)
                       integerResponse = "TRUE" #is the response an integer? yes
                       #, re.form = "NULL") #this argument not supported (neither in glmmTMB)
)

#Step 3 - plot!
plot(resids)

resids %>% testDispersion()
## ----end

#### Model Investigation ========================================================
#Frequentist
## ----recruitment univariate sd frequentist summary
sd.glmmTMB.ac %>% summary()

sd.glmmTMB2 %>% r.squaredGLMM()
## ----end

#Bayesian

## ---- recruitment univariate sd brm2 summary
sd.brm2$fit %>% tidyMCMC(pars = wch,
                         estimate.method = "median",
                         conf.int = TRUE,
                         conf.method = "HPDinterval",
                         rhat = TRUE,
                         ess = TRUE)

#"Marginal"
sd.brm2 %>% brms::bayes_R2(re.form = NA, #or ~Treatment
                           summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2

# "Conditional"
sd.brm2 %>% brms::bayes_R2(re.form = NULL, #or ~(1|plotID)
                           summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2


## ----end

## ---- recruitment univariate sd brm contrasts
sd.brm2%>%
  emmeans(~Treatment, type = 'link') %>% #link scale
  pairs() %>% #pairwise comparison
  gather_emmeans_draws() %>% #take all the draws for these comparisons, gather them (make long)
  #median_hdci(exp(.value)) #this would essentially give us the summary above. Nothing too special yet, but we have more control
  summarise('P>' = sum(.value>0)/n(), #exceedance probabilities
            'P<' = sum(.value<0)/n(),
  ) 

## ----end

#### Summary figures =========================================================

## ---- recruitment univariate sd figures

sd.brm2 <- readRDS(file = paste0(DATA_PATH, "modelled/sd.brm2.rds"))
sd.brm2 %>% ggemmeans(~Treatment) %>% plot


newdata <- sd.brm2 %>% emmeans(~Treatment, type = "link") %>% 
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
  ylab("S. doliatus abundance") +
  theme_classic()

sd.em <- sd.brm2 %>%
  emmeans(~Treatment, type = "link") %>%
  pairs() %>%
  gather_emmeans_draws() %>%
  mutate(Fit = exp(.value)) %>% as.data.frame()
#head(sp.em)

g2<- sd.em %>%
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

ggsave(filename = paste0(FIGS_PATH, "/bayes.sd.png"),
       g1,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.sd.contr.png"),
       g2,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.sd.both.png"),
       g1 + theme(legend.position = "none") + g2,
       height = 5,
       width = 15,
       dpi = 100)


### Petroscirtes sp. abundance  =================================================

#### Fit =====================================================================
## ----recruitment univariate ps fit

common.abnd <- read_csv( paste0(DATA_PATH, "summarised/common.abnd.csv") ) %>% 
  mutate_at(c(1:4,7,9, 10), factor) %>% 
  data.frame()

#Fewer candidate models (ones that make theoretical sense, instead of dredging)
ps.glmmTMB1 <- glmmTMB(abundance ~ 1 + (1|plotID), #random intercept mode
                       data = common.abnd %>% 
                         filter(Species == "Petroscirtes sp."),
                       family = poisson(link = "log"),
                       REML = TRUE)

ps.glmmTMB2 <- update(ps.glmmTMB1, .~. + Treatment) #Treatment fixed, Random int

ps.glmmTMB3 <- update(ps.glmmTMB1, .~. + plot.weight) #plot.weight fixed, rand int



MuMIn::AICc(ps.glmmTMB1,ps.glmmTMB2, ps.glmmTMB3)


## ----end

#### Validate ==================================================================

## ---- recruitment univariate ps validate
ps.resid <- ps.glmmTMB2 %>% simulateResiduals(plot = TRUE)

##check autocorrelation
#common.abnd <- common.abnd %>% 
 # mutate(TIME = as.numeric(plotID) + as.numeric(Day)*10^-2)

#ps.resid %>% testTemporalAutocorrelation(time = common.abnd %>% 
  #                                        filter(Species == "Petroscirtes sp.") %>% 
   #                                       pull(TIME) ) #extract just the Time column as a vector

acf(residuals(ps.glmmTMB2, method = "pearson"))$acf


## ----end

## ----recruitment univariate ps refit revalidate

ps.glmmTMB.ac <- update(ps.glmmTMB2, .~. + ar1(0 + factor(Date)|plotID) )

ps.glmmTMB.ac %>% AICc(.,ps.glmmTMB2)

acf(residuals(ps.glmmTMB.ac, method = "pearson"))$acf

ps.ac.resid <- ps.glmmTMB.ac %>% simulateResiduals(plot = TRUE)
#ps.ac.resid %>% testTemporalAutocorrelation(time = common.abnd %>% 
#                                           filter(Species == "Halichoeres miniatus") %>% 
#                                           pull(TIME) )

ps.ac.resid %>% testDispersion()
#some evidence of underdispersion

#### Partial ====================================================================

## ----recruitment univariate ps partial
ps.glmmTMB2 %>% ggpredict(terms = "Treatment") %>% plot()

## ----end

#### Bayesian ==================================================================
##### Priors ===================================================================

## ----recruitment univariate ps priors1
common.abnd %>% filter(Species == "Petroscirtes sp.") %>% 
  group_by(Treatment) %>%  summarise(log(median(abundance)), 
                                     log(mad(abundance)))

##priors for Effects
common.abnd %>% filter(Species == "Petroscirtes sp.") %>% 
  pull(abundance) %>% sd() %>% 
  log()/apply(model.matrix(~Treatment, data = common.abnd), 2, sd)


## ----end

## ----recruitment univariate ps treatment reorder
dat.sub <- common.abnd %>% filter(Species == "Petroscirtes sp.") %>% 
  droplevels() %>% 
  mutate(Treatment = forcats::fct_relevel(Treatment, "W")) #change first level to W

## ----end

##### Fitting ===============================================================

## ----recruitment univariate ps brmsfit
ps.form <- bf(abundance ~ Treatment
              + (1|plotID),
              autocor = ~ ar(time = Day, gr = plotID, 
                             p = 1),
              family = poisson(link = "log") )
priors <- prior(normal(1, 1), class = "Intercept") +
  prior(normal(0,4), class = "b") +
  #prior(normal(0,2), class = "b") +
  #prior(normal(0,0.4), class = "Intercept") +
  prior(cauchy(0,0.5), class = "sd") + 
  prior(cauchy(0,0.5), class = "sderr") +
  prior(uniform(-1,1), class = "ar") 


#ps.brm1 <- brm(ps.form,
#               data = common.abnd %>% 
#                 filter(Species == "Petroscirtes sp."),
#               prior = priors,
#               sample_prior = "yes", #sample priors and posteriors
#               iter = 10000, warmup = 2000,
#               #control = list(adapt_delta = 0.99), #devote more of warmup to step-length determination
#               chains = 3, cores = 3, 
#               thin = 10,
#               seed = 123)

ps.brm2 <- brm(ps.form,
               data = dat.sub,
               prior = priors,
               sample_prior = "yes", #sample priors and posteriors
               iter = 10000, warmup = 2000,
               #control = list(adapt_delta = 0.99), #devote more of warmup to step-length determination
               chains = 3, cores = 3, 
              thin = 10,
               seed = 123)

#ps.brm1 %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/ps.brm1.rds"))
ps.brm2 %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/ps.brm2.rds"))

## ----end

##### Prior Checks =============================================================
## ----recruitment univariate ps brm2 prior check
ps.brm2 <- readRDS(file = paste0(DATA_PATH, "modelled/ps.brm2.rds"))

ps.brm2 %>% 
  as_draws_df() %>% #get all the draws for everything estimated
  
  dplyr::select(!matches("^lp|^err|^r_|^\\.") ) %>% #remove variables starting with lp, err or r_ or .
  #Note removing the '.' cols (.iteration, .draw and .chain) changed the class
  
  pivot_longer(everything(), names_to = 'key') %>% #make long, with variable names in a column called 'key'. Note 
  
  mutate(Type = ifelse(str_detect(key, 'prior'), 'Prior', 'Posterior'), #classify within new col 'Type' whether Prior or Posterior using str_detect
         Class = case_when( #create column 'Class' to classify vars as:
           str_detect(key, '(^b|^prior).*Intercept$') ~ 'Intercept', #intercept, if 'key' starts with b or prior followed by any character ('.') with 'Intercept' at the end
           str_detect(key, 'b_Treatment.*|prior_b') ~ 'TREATMENT', #TREATMENT, if the string contains 'b_Treatment followed by any character ('.')
           str_detect(key, 'sd_') ~ 'sd', #sd, if the string contains ps ('sderr' will be included)
           str_detect(key, 'ar') ~ 'ar', #ar, if it contains ar
           str_detect(key, 'sderr') ~ 'sderr'), #sderr, if it contains sderr
         Par = str_replace(key, 'b_', '')) %>% 
  
  ggplot(aes(x = Type,  y = value, color = Par)) + #Plot with these overall aesthetics
  stat_pointinterval(position = position_dodge(), show.legend = FALSE)+ #plot as stat_point intervals
  facet_wrap(~Class,  scales = 'free') #separate plots by Class with each class having its own scales


## ----end

#####MCMC =====================================================================

## ----recruitment univariate ps brm2 MCMC
ps.brm2 <- readRDS(file = paste0(DATA_PATH, "modelled/ps.brm2.rds"))

pars <- ps.brm2 %>% get_variables()

wch <- str_extract(pars, #get the names of the variables
                   '^b_.*|^sd.*|^ar.*') %>% #that start with b, ps or ar
  na.omit # omit the rest, resulting in an object of class 'omit'
wch
##Trace Plots
stan_trace(ps.brm2$fit, pars = wch)

##Autocorrelation factor
stan_ac(ps.brm2$fit, pars = wch)

##rhat - Scale reduction factor
stan_rhat(ps.brm2$fit, pars = wch)

##ESS (effective sample size)
stan_ess(ps.brm2$fit, pars = wch)

##Density plot
stan_dens(ps.brm2$fit, pars = wch, separate_chains = TRUE)

##Density overlay
ps.brm2%>% pp_check(type = 'dens_overlay', ndraws = 100)

## ----end

##### DHARMA Residuals ==========================================================

## ---- recruitment univariate ps brm2 DHARMa

#step 1. Draw out predictions
preds <- ps.brm2 %>% posterior_predict(ndraws = 250, #extract 250 posterior draws from the posterior model
                                       summary = FALSE) #don't summarise - we want the whole distribution of them


#Step 2 create DHARMA resids
resids <- createDHARMa(simulatedResponse = t(preds), #provide with simulated predictions, transposed with t()
                       observedResponse = dat.sub %>% 
                         filter(Species == "Petroscirtes sp.") %>% 
                         pull(abundance), #real response
                       fittedPredictedResponse = apply(preds, 2, median), #for the fitted predicted response, use the median of preds (in the columns, the second argument of apply defines the MARGIN, 2 being columns for matrices)
                       integerResponse = "TRUE" #is the response an integer? yes
                       #, re.form = "NULL") #this argument not supported (neither in glmmTMB)
)

#Step 3 - plot!
plot(resids)

resids %>% testDispersion()
## ----end

## ---- recruitment univariate ps refit simpler

ps.form <- bf(abundance ~ Treatment
              + (1|plotID),
              #autocor = ~ ar(time = Day, gr = plotID, 
              #               p = 1),
              family = poisson(link = "log") )
priors <- prior(normal(1, 1), class = "Intercept") +
  prior(normal(0,4), class = "b") +
  prior(cauchy(0,0.5), class = "sd")



ps.brm3 <- brm(ps.form,
               data = dat.sub,
               prior = priors,
               sample_prior = "yes", #sample priors and posteriors
               iter = 5000, warmup = 1000,
               #control = list(adapt_delta = 0.99), #devote more of warmup to step-length determination
               chains = 3, cores = 3, 
               thin = 5,
               seed = 123)

ps.brm3 %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/ps.brm3.rds"))
## ----end

## ---- recruitment univariate ps retest simpler
ps.brm3 <- readRDS(file = paste0(DATA_PATH, "modelled/ps.brm3.rds"))

ps.brm3 %>% 
  as_draws_df() %>% #get all the draws for everything estimated
  
  dplyr::select(!matches("^lp|^err|^r_|^\\.") ) %>% #remove variables starting with lp, err or r_ or .
  #Note removing the '.' cols (.iteration, .draw and .chain) changed the class
  
  pivot_longer(everything(), names_to = 'key') %>% #make long, with variable names in a column called 'key'. Note 
  
  mutate(Type = ifelse(str_detect(key, 'prior'), 'Prior', 'Posterior'), #classify within new col 'Type' whether Prior or Posterior using str_detect
         Class = case_when( #create column 'Class' to classify vars as:
           str_detect(key, '(^b|^prior).*Intercept$') ~ 'Intercept', #intercept, if 'key' starts with b or prior followed by any character ('.') with 'Intercept' at the end
           str_detect(key, 'b_Treatment.*|prior_b') ~ 'TREATMENT', #TREATMENT, if the string contains 'b_Treatment followed by any character ('.')
           str_detect(key, 'sd_') ~ 'sd', #sd, if the string contains ps ('sderr' will be included)
           str_detect(key, 'ar') ~ 'ar', #ar, if it contains ar
           str_detect(key, 'sderr') ~ 'sderr'), #sderr, if it contains sderr
         Par = str_replace(key, 'b_', '')) %>% 
  
  ggplot(aes(x = Type,  y = value, color = Par)) + #Plot with these overall aesthetics
  stat_pointinterval(position = position_dodge(), show.legend = FALSE)+ #plot as stat_point intervals
  facet_wrap(~Class,  scales = 'free') #separate plots by Class with each class having its own scales



pars <- ps.brm3 %>% get_variables()

wch <- str_extract(pars, #get the names of the variables
                   '^b_.*|^sd.*|^ar.*') %>% #that start with b, ps or ar
  na.omit # omit the rest, resulting in an object of class 'omit'
wch
##Trace Plots
stan_trace(ps.brm3$fit, pars = wch)

##Autocorrelation factor
stan_ac(ps.brm3$fit, pars = wch)

##rhat - Scale reduction factor
stan_rhat(ps.brm3$fit, pars = wch)

##ESS (effective sample size)
stan_ess(ps.brm3$fit, pars = wch)

##Density plot
stan_dens(ps.brm3$fit, pars = wch, separate_chains = TRUE)

##Density overlay
ps.brm3%>% pp_check(type = 'dens_overlay', ndraws = 100)


preds <- ps.brm3 %>% posterior_predict(ndraws = 250, #extract 250 posterior draws from the posterior model
                                       summary = FALSE) #don't summarise - we want the whole distribution of them


#Step 2 create DHARMA resids
resids <- createDHARMa(simulatedResponse = t(preds), #provide with simulated predictions, transposed with t()
                       observedResponse = dat.sub %>% 
                         filter(Species == "Petroscirtes sp.") %>% 
                         pull(abundance), #real response
                       fittedPredictedResponse = apply(preds, 2, median), #for the fitted predicted response, use the median of preds (in the columns, the second argument of apply defines the MARGIN, 2 being columns for matrices)
                       integerResponse = "TRUE" #is the response an integer? yes
                       #, re.form = "NULL") #this argument not supported (neither in glmmTMB)
)

#Step 3 - plot!
plot(resids)

resids %>% testDispersion()
resids %>% testZeroInflation()
## ----end

#### Model Investigation ========================================================
#Frequentist
## ----recruitment univariate ps frequentist summary
ps.glmmTMB.ac %>% summary()

ps.glmmTMB2 %>% r.squaredGLMM()
## ----end

#Bayesian

## ---- recruitment univariate ps brm2 summary
ps.brm2$fit %>% tidyMCMC(pars = wch,
                         estimate.method = "median",
                         conf.int = TRUE,
                         conf.method = "HPDinterval",
                         rhat = TRUE,
                         ess = TRUE)

#"Marginal"
ps.brm2 %>% brms::bayes_R2(re.form = NA, #or ~Treatment
                           summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2

# "Conditional"
ps.brm2 %>% brms::bayes_R2(re.form = NULL, #or ~(1|plotID)
                           summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2


## ----end

## ---- recruitment univariate ps brm contrasts
ps.brm2%>%
  emmeans(~Treatment, type = 'link') %>% #link scale
  pairs() %>% #pairwise comparison
  gather_emmeans_draws() %>% #take all the draws for these comparisons, gather them (make long)
  #median_hdci(exp(.value)) #this would essentially give us the summary above. Nothing too special yet, but we have more control
  summarise('P>' = sum(.value>0)/n(), #exceedance probabilities
            'P<' = sum(.value<0)/n(),
  ) 

## ----end

#### Summary figures =========================================================

## ---- recruitment univariate ps figures

ps.brm2 <- readRDS(file = paste0(DATA_PATH, "modelled/ps.brm2.rds"))
ps.brm2 %>% ggemmeans(~Treatment) %>% plot


newdata <- ps.brm2 %>% emmeans(~Treatment, type = "link") %>% 
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
  ylab("Petroscirtes sp. abundance") +
  theme_classic()

ps.em <- ps.brm2 %>%
  emmeans(~Treatment, type = "link") %>%
  pairs() %>%
  gather_emmeans_draws() %>%
  mutate(Fit = exp(.value)) %>% as.data.frame()
#head(sp.em)

g2<- ps.em %>%
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

ggsave(filename = paste0(FIGS_PATH, "/bayes.ps.png"),
       g1,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.ps.contr.png"),
       g2,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.ps.both.png"),
       g1 + theme(legend.position = "none") + g2,
       height = 5,
       width = 15,
       dpi = 100)

### Pomacentrus tripunctatus abundance  =========================================

#### Fit =====================================================================
## ----recruitment univariate pt fit

common.abnd <- read_csv( paste0(DATA_PATH, "summarised/common.abnd.csv") ) %>% 
  mutate_at(c(1:4,7,9, 10), factor) %>% 
  data.frame()

#Fewer candidate models (ones that make theoretical sense, instead of dredging)
pt.glmmTMB1 <- glmmTMB(abundance ~ 1 + (1|plotID), #random intercept mode
                       data = common.abnd %>% 
                         filter(Species == "Pomacentrus tripunctatus"),
                       family = poisson(link = "log"),
                       REML = TRUE)

pt.glmmTMB2 <- update(pt.glmmTMB1, .~. + Treatment) #Treatment fixed, Random int

pt.glmmTMB3 <- update(pt.glmmTMB1, .~. + plot.weight) #plot.weight fixed, rand int



MuMIn::AICc(pt.glmmTMB1,pt.glmmTMB2, pt.glmmTMB3)


## ----end

#### Validate ==================================================================

## ---- recruitment univariate pt validate
pt.resid <- pt.glmmTMB2 %>% simulateResiduals(plot = TRUE)

pt.resid %>% testDispersion()
pt.resid %>% testUniformity()

acf(residuals(pt.glmmTMB2, method = "pearson"))$acf


## ----end

## ----recruitment univariate pt refit revalidate

pt.glmmTMB.ac <- update(pt.glmmTMB2, .~. + ar1(0 + factor(Date)|plotID) )

pt.glmmTMB.ac %>% AICc(.,pt.glmmTMB2)

acf(residuals(pt.glmmTMB.ac, method = "pearson"))$acf

pt.ac.resid <- pt.glmmTMB.ac %>% simulateResiduals(plot = TRUE)
#pt.ac.resid %>% testTemporalAutocorrelation(time = common.abnd %>% 
#                                           filter(Species == "Halichoeres miniatus") %>% 
#                                           pull(TIME) )

pt.ac.resid %>% testDispersion()
#some evidence of underdispersion

#### Partial ====================================================================

## ----recruitment univariate pt partial
pt.glmmTMB2 %>% ggpredict(terms = "Treatment") %>% plot()

## ----end

#### Bayesian ==================================================================
##### Priors ===================================================================

## ----recruitment univariate pt priors1
common.abnd %>% filter(Species == "Pomacentrus tripunctatus") %>% 
  group_by(Treatment) %>%  summarise(log(median(abundance)), 
                                     log(mad(abundance)))

##priors for Effects
common.abnd %>% filter(Species == "Pomacentrus tripunctatus") %>% 
  pull(abundance) %>% sd() %>% 
  log()/apply(model.matrix(~Treatment, data = common.abnd), 2, sd)


## ----end

## ----recruitment univariate pt treatment reorder
dat.sub <- common.abnd %>% filter(Species == "Pomacentrus tripunctatus") %>% 
  droplevels() %>% 
  mutate(Treatment = forcats::fct_relevel(Treatment, "W")) #change first level to W

## ----end

##### Fitting ===================================================================

## ----recruitment univariate pt brmsfit
pt.form <- bf(abundance ~ Treatment
              + (1|plotID),
              autocor = ~ ar(time = Day, gr = plotID, 
                             p = 1),
              family = poisson(link = "log") )

priors <- prior(normal(0, 1), class = "Intercept") +
  prior(normal(0,3), class = "b") + 
  prior(cauchy(0,0.5), class = "sd") + 
  prior(cauchy(0,0.5), class = "sderr") +
  prior(uniform(-1,1), class = "ar") 

pt.brm1 <- brm(pt.form,
               data = dat.sub,
               prior = priors,
               sample_prior = "yes", #sample priors and posteriors
               iter = 10000, warmup = 2000,
              # control = list(adapt_delta = 0.99), #devote more of warmup to step-length determination
               chains = 3, cores = 3, 
               thin = 10,
               seed = 123)

pt.brm1 %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/pt.brm1.rds"))


## ----end

##### Prior Checks =============================================================
## ----recruitment univariate pt brm1 prior check
pt.brm1 <- readRDS(file = paste0(DATA_PATH, "modelled/pt.brm1.rds"))

pt.brm1 %>% 
  as_draws_df() %>% #get all the draws for everything estimated
  
  dplyr::select(!matches("^lp|^err|^r_|^\\.") ) %>% #remove variables starting with lp, err or r_ or .
  #Note removing the '.' cols (.iteration, .draw and .chain) changed the class
  
  pivot_longer(everything(), names_to = 'key') %>% #make long, with variable names in a column called 'key'. Note 
  
  mutate(Type = ifelse(str_detect(key, 'prior'), 'Prior', 'Posterior'), #classify within new col 'Type' whether Prior or Posterior using str_detect
         Class = case_when( #create column 'Class' to classify vars as:
           str_detect(key, '(^b|^prior).*Intercept$') ~ 'Intercept', #intercept, if 'key' starts with b or prior followed by any character ('.') with 'Intercept' at the end
           str_detect(key, 'b_Treatment.*|prior_b') ~ 'TREATMENT', #TREATMENT, if the string contains 'b_Treatment followed by any character ('.')
           str_detect(key, 'sd_') ~ 'sd', #sd, if the string contains pt ('sderr' will be included)
           str_detect(key, 'ar') ~ 'ar', #ar, if it contains ar
           str_detect(key, 'sderr') ~ 'sderr'), #sderr, if it contains sderr
         Par = str_replace(key, 'b_', '')) %>% 
  
  ggplot(aes(x = Type,  y = value, color = Par)) + #Plot with these overall aesthetics
  stat_pointinterval(position = position_dodge(), show.legend = FALSE)+ #plot as stat_point intervals
  facet_wrap(~Class,  scales = 'free') #separate plots by Class with each class having its own scales


## ----end

#####MCMC =====================================================================

## ----recruitment univariate pt brm1 MCMC
pt.brm1 <- readRDS(file = paste0(DATA_PATH, "modelled/pt.brm1.rds"))

pars <- pt.brm1 %>% get_variables()

wch <- str_extract(pars, #get the names of the variables
                   '^b_.*|^sd.*|^ar.*') %>% #that start with b, pt or ar
  na.omit # omit the rest, resulting in an object of class 'omit'
#wch
##Trace Plots
stan_trace(pt.brm1$fit, pars = wch)

##Autocorrelation factor
stan_ac(pt.brm1$fit, pars = wch)

##rhat - Scale reduction factor
stan_rhat(pt.brm1$fit, pars = wch)

##ESS (effective sample size)
stan_ess(pt.brm1$fit, pars = wch)

##Density plot
stan_dens(pt.brm1$fit, pars = wch, separate_chains = TRUE)

##Density overlay
pt.brm1%>% pp_check(type = 'dens_overlay', ndraws = 100)

## ----end

##### DHARMA Residuals ==========================================================

## ---- recruitment univariate pt brm1 DHARMa

#step 1. Draw out predictions
preds <- pt.brm1 %>% posterior_predict(ndraws = 250, #extract 250 posterior draws from the posterior model
                                       summary = FALSE) #don't summarise - we want the whole distribution of them


#Step 2 create DHARMA resids
resids <- createDHARMa(simulatedResponse = t(preds), #provide with simulated predictions, transposed with t()
                       observedResponse = dat.sub %>% 
                         filter(Species == "Pomacentrus tripunctatus") %>% 
                         pull(abundance), #real response
                       fittedPredictedResponse = apply(preds, 2, median), #for the fitted predicted response, use the median of preds (in the columns, the second argument of apply defines the MARGIN, 2 being columns for matrices)
                       integerResponse = "TRUE" #is the response an integer? yes
                       #, re.form = "NULL") #this argument not supported (neither in glmmTMB)
)

#Step 3 - plot!
plot(resids)

resids %>% testDispersion()



#### Model Investigation ========================================================
#Frequentist
## ----recruitment univariate pt frequentist summary
pt.glmmTMB.ac %>% summary()

pt.glmmTMB2 %>% r.squaredGLMM()
## ----end

#Bayesian

## ---- recruitment univariate pt brm1 summary
pt.brm1$fit %>% tidyMCMC(pars = wch,
                         estimate.method = "median",
                         conf.int = TRUE,
                         conf.method = "HPDinterval",
                         rhat = TRUE,
                         ess = TRUE)

#"Marginal"
pt.brm1 %>% brms::bayes_R2(re.form = NA, #or ~Treatment
                           summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2

# "Conditional"
pt.brm1 %>% brms::bayes_R2(re.form = NULL, #or ~(1|plotID)
                           summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2


## ----end

## ---- recruitment univariate pt brm contrasts
contrast.tbl <- pt.brm1%>%
  emmeans(~Treatment, type = 'link') %>% #link scale
  pairs() %>% #pairwise comparison
  gather_emmeans_draws() %>% #take all the draws for these comparisons, gather them (make long)
  #median_hdci(exp(.value)) #this would essentially give us the summary above. Nothing too special yet, but we have more control
  summarise('P>' = sum(.value>0)/n(), #exceedance probabilities
            'P<' = sum(.value<0)/n(),
  ) %>% 
ungroup()
  

contrast.tbl %>% 
  mutate(evidence = case_when(
    `P>` >= 0.99 | `P>` >= 0.99 ~ "very strong",
    `P>` >= 0.95 |`P<` >= 0.95 ~ "strong",
    `P>` >= 0.90 |`P<` >= 0.90 ~ "evidence",
    TRUE ~ "no evidence"
  )
)
## ----end

#### Summary figures =========================================================

## ---- recruitment univariate pt figures

pt.brm1 <- readRDS(file = paste0(DATA_PATH, "modelled/pt.brm1.rds"))
pt.brm1 %>% ggemmeans(~Treatment) %>% plot


newdata <- pt.brm1 %>% emmeans(~Treatment, type = "link") %>% 
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
  ylab("Pom. tripunctatus abundance") +
  theme_classic()

pt.em <- pt.brm1 %>%
  emmeans(~Treatment, type = "link") %>%
  pairs() %>%
  gather_emmeans_draws() %>%
  mutate(Fit = exp(.value)) %>% as.data.frame()
#head(sp.em)

g2<- pt.em %>%
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

ggsave(filename = paste0(FIGS_PATH, "/bayes.pt.png"),
       g1,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.pt.contr.png"),
       g2,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.pt.both.png"),
       g1 + theme(legend.position = "none") + g2,
       height = 5,
       width = 15,
       dpi = 100)


### Lethrinus atkinsoni abundance  =========================================

#### Fit =====================================================================
## ----recruitment univariate la fit

common.abnd <- read_csv( paste0(DATA_PATH, "summarised/common.abnd.csv") ) %>% 
  mutate_at(c(1:4,7,9, 10), factor) %>% 
  data.frame()

#Fewer candidate models (ones that make theoretical sense, instead of dredging)
la.glmmTMB1 <- glmmTMB(abundance ~ 1 + (1|plotID), #random intercept mode
                       data = common.abnd %>% 
                         filter(Species == "Lethrinus atkinsoni"),
                       family = poisson(link = "log"),
                       REML = TRUE)

la.glmmTMB2 <- update(la.glmmTMB1, .~. + Treatment) #Treatment fixed, Random int

la.glmmTMB3 <- update(la.glmmTMB1, .~. + plot.weight) #plot.weight fixed, rand int



MuMIn::AICc(la.glmmTMB1,la.glmmTMB2, la.glmmTMB3)


## ----end

#### Validate ==================================================================

## ---- recruitment univariate la validate
la.resid <- la.glmmTMB2 %>% simulateResiduals(plot = TRUE)


acf(residuals(la.glmmTMB2, method = "pearson"))$acf


## ----end

## ----recruitment univariate la refit revalidate

la.glmmTMB.ac <- update(la.glmmTMB2, .~. + ar1(0 + factor(Date)|plotID) )

la.glmmTMB.ac %>% AICc(.,la.glmmTMB2)

acf(residuals(la.glmmTMB.ac, method = "pearson"))$acf

la.ac.resid <- la.glmmTMB.ac %>% simulateResiduals(plot = TRUE)
                                     

la.ac.resid %>% testDispersion()
#some evidence of underdispersion

#### Partial ====================================================================

## ----recruitment univariate la partial
la.glmmTMB2 %>% ggpredict(terms = "Treatment") %>% plot()

## ----end

#### Bayesian ==================================================================
##### Priors ===================================================================

## ----recruitment univariate la priors1
#common.abnd %>% filter(Species == "Lethrinus atkinsoni") %>% 
#  group_by(Treatment) %>%  summarise(log(median(abundance)), 
#                                     log(mad(abundance)))

common.abnd %>% filter(Species == "Lethrinus atkinsoni") %>% 
  group_by(Treatment) %>%  summarise(log(mean(abundance)), 
                                     log(sd(abundance)))

##priors for Effects
common.abnd %>% filter(Species == "Lethrinus atkinsoni") %>% 
  pull(abundance) %>% sd() %>% 
  log()/apply(model.matrix(~Treatment, data = common.abnd), 2, sd)


## ----end

## ----recruitment univariate la treatment reorder
dat.sub <- common.abnd %>% filter(Species == "Lethrinus atkinsoni") %>% 
  droplevels() %>% 
  mutate(Treatment = forcats::fct_relevel(Treatment, "W")) #change first level to W

## ----end

##### Fitting ===================================================================

## ----recruitment univariate la brmsfit
la.form <- bf(abundance ~ Treatment
              + (1|plotID),
              autocor = ~ ar(time = Day, gr = plotID, 
                             p = 1),
              family = poisson(link = "log") )

priors <- prior(normal(-1, 1), class = "Intercept") +
  prior(normal(0,3), class = "b") + 
  prior(cauchy(0,0.5), class = "sd") + 
  prior(cauchy(0,0.5), class = "sderr") +
  prior(uniform(-1,1), class = "ar") 

la.brm1 <- brm(la.form,
               data = dat.sub,
               prior = priors,
               sample_prior = "yes", #sample priors and posteriors
               iter = 10000, warmup = 2000,
               # control = list(adapt_delta = 0.99), #devote more of warmup to step-length determination
               chains = 3, cores = 3, 
               thin = 10,
               seed = 123)

la.brm1 %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/la.brm1.rds"))


## ----end

##### Prior Checks =============================================================
## ----recruitment univariate la brm1 prior check
la.brm1 <- readRDS(file = paste0(DATA_PATH, "modelled/la.brm1.rds"))

la.brm1 %>% 
  as_draws_df() %>% #get all the draws for everything estimated
  
  dplyr::select(!matches("^lp|^err|^r_|^\\.") ) %>% #remove variables starting with lp, err or r_ or .
  #Note removing the '.' cols (.iteration, .draw and .chain) changed the class
  
  pivot_longer(everything(), names_to = 'key') %>% #make long, with variable names in a column called 'key'. Note 
  
  mutate(Type = ifelse(str_detect(key, 'prior'), 'Prior', 'Posterior'), #classify within new col 'Type' whether Prior or Posterior using str_detect
         Class = case_when( #create column 'Class' to classify vars as:
           str_detect(key, '(^b|^prior).*Intercept$') ~ 'Intercept', #intercept, if 'key' starts with b or prior followed by any character ('.') with 'intercept' at the end
           str_detect(key, 'b_Treatment.*|prior_b') ~ 'TREATMENT', #TREATMENT, if the string contains 'b_Treatment followed by any character ('.')
           str_detect(key, 'sd_') ~ 'sd', #sd, if the string contains la ('sderr' will be included)
           str_detect(key, 'ar') ~ 'ar', #ar, if it contains ar
           str_detect(key, 'sderr') ~ 'sderr'), #sderr, if it contains sderr
         Par = str_replace(key, 'b_', '')) %>% 
  
  ggplot(aes(x = Type,  y = value, color = Par)) + #Plot with these overall aesthetics
  stat_pointinterval(position = position_dodge(), show.legend = FALSE)+ #plot as stat_point intervals
  facet_wrap(~Class,  scales = 'free') #separate plots by Class with each class having its own scales


## ----end

#####MCMC =====================================================================

## ----recruitment univariate la brm1 MCMC
la.brm1 <- readRDS(file = paste0(DATA_PATH, "modelled/la.brm1.rds"))

pars <- la.brm1 %>% get_variables()

wch <- str_extract(pars, #get the names of the variables
                   '^b_.*|^sd.*|^ar.*') %>% #that start with b, la or ar
  na.omit # omit the rest, resulting in an object of class 'omit'
#wch
##Trace Plots
stan_trace(la.brm1$fit, pars = wch)

##Autocorrelation factor
stan_ac(la.brm1$fit, pars = wch)

##rhat - Scale reduction factor
stan_rhat(la.brm1$fit, pars = wch)

##ESS (effective sample size)
stan_ess(la.brm1$fit, pars = wch)

##Density plot
stan_dens(la.brm1$fit, pars = wch, separate_chains = TRUE)

##Density overlay
la.brm1%>% pp_check(type = 'dens_overlay', ndraws = 100)

## ----end

##### DHARMA Residuals ==========================================================

## ---- recruitment univariate la brm1 DHARMa

#step 1. Draw out predictions
preds <- la.brm1 %>% posterior_predict(ndraws = 250, #extract 250 posterior draws from the posterior model
                                       summary = FALSE) #don't summarise - we want the whole distribution of them


#Step 2 create DHARMA resids
resids <- createDHARMa(simulatedResponse = t(preds), #provide with simulated predictions, transposed with t()
                       observedResponse = dat.sub %>% 
                         filter(Species == "Lethrinus atkinsoni") %>% 
                         pull(abundance), #real response
                       fittedPredictedResponse = apply(preds, 2, median), #for the fitted predicted response, use the median of preds (in the columns, the second argument of apply defines the MARGIN, 2 being columns for matrices)
                       integerResponse = "TRUE" #is the response an integer? yes
                       #, re.form = "NULL") #this argument not supported (neither in glmmTMB)
)

#Step 3 - plot!
plot(resids)



#### Model Investigation ========================================================
#Frequentist
## ----recruitment univariate la frequentist summary
la.glmmTMB.ac %>% summary()

la.glmmTMB2 %>% r.squaredGLMM()
## ----end

#Bayesian

## ---- recruitment univariate la brm1 summary
la.brm1$fit %>% tidyMCMC(pars = wch,
                         estimate.method = "median",
                         conf.int = TRUE,
                         conf.method = "HPDinterval",
                         rhat = TRUE,
                         ess = TRUE)

#"Marginal"
la.brm1 %>% brms::bayes_R2(re.form = NA, #or ~Treatment
                           summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2

# "Conditional"
la.brm1 %>% brms::bayes_R2(re.form = NULL, #or ~(1|plotID)
                           summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2


## ----end

## ---- recruitment univariate la brm contrasts
la.brm1%>%
  emmeans(~Treatment, type = 'link') %>% #link scale
  pairs() %>% #pairwise comparison
  gather_emmeans_draws() %>% #take all the draws for these comparisons, gather them (make long)
  #median_hdci(exp(.value)) #this would essentially give us the summary above. Nothing too special yet, but we have more control
  summarise('P>' = sum(.value>0)/n(), #exceedance probabilities
            'P<' = sum(.value<0)/n(),
  ) %>% 
  ungroup() %>% 
  mutate(evidence = case_when(
    `P>` >= 0.99 | `P>` >= 0.99 ~ "very strong",
    `P>` >= 0.95 |`P<` >= 0.95 ~ "strong",
    `P>` >= 0.90 |`P<` >= 0.90 ~ "evidence",
    TRUE ~ "no evidence"
  )
  )

## ----end

#### Summary figures =========================================================

## ---- recruitment univariate la figures

la.brm1 <- readRDS(file = paste0(DATA_PATH, "modelled/la.brm1.rds"))
la.brm1 %>% ggemmeans(~Treatment) %>% plot


newdata <- la.brm1 %>% emmeans(~Treatment, type = "link") %>% 
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
  ylab("L. atkinsoni abundance") +
  theme_classic()

la.em <- la.brm1 %>%
  emmeans(~Treatment, type = "link") %>%
  pairs() %>%
  gather_emmeans_draws() %>%
  mutate(Fit = exp(.value)) %>% as.data.frame()
#head(sp.em)

g2<- la.em %>%
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

ggsave(filename = paste0(FIGS_PATH, "/bayes.la.png"),
       g1,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.la.contr.png"),
       g2,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.la.both.png"),
       g1 + theme(legend.position = "none") + g2,
       height = 5,
       width = 15,
       dpi = 100)

### Siganus fuscescens abundance  =========================================

#### Fit =====================================================================
## ----recruitment univariate sf fit

common.abnd <- read_csv( paste0(DATA_PATH, "summarised/common.abnd.csv") ) %>% 
  mutate_at(c(1:4,7,9, 10), factor) %>% 
  data.frame()

#Fewer candidate models (ones that make theoretical sense, instead of dredging)
sf.glmmTMB1 <- glmmTMB(abundance ~ 1 + (1|plotID), #random intercept mode
                       data = common.abnd %>% 
                         filter(Species == "Siganus fuscescens"),
                       family = poisson(link = "log"),
                       REML = TRUE)

sf.glmmTMB2 <- update(sf.glmmTMB1, .~. + Treatment) #Treatment fixed, Random int

sf.glmmTMB3 <- update(sf.glmmTMB1, .~. + plot.weight) #plot.weight fixed, rand int



MuMIn::AICc(sf.glmmTMB1,sf.glmmTMB2, sf.glmmTMB3)


## ----end

#### Validate ==================================================================

## ---- recruitment univariate sf validate
sf.resid <- sf.glmmTMB2 %>% simulateResiduals(plot = TRUE)


acf(residuals(sf.glmmTMB2, method = "pearson"))$acf


## ----end

## ----recruitment univariate sf refit revalidate

sf.glmmTMB.ac <- update(sf.glmmTMB2, .~. + ar1(0 + factor(Date)|plotID) )

sf.glmmTMB.ac %>% AICc(.,sf.glmmTMB2)

acf(residuals(sf.glmmTMB.ac, method = "pearson"))$acf

sf.ac.resid <- sf.glmmTMB.ac %>% simulateResiduals(plot = TRUE)


sf.ac.resid %>% testDispersion()
#some evidence of underdispersion

#### Partial ====================================================================

## ----recruitment univariate sf partial
sf.glmmTMB2 %>% ggpredict(terms = "Treatment") %>% plot()

## ----end

#### Bayesian ==================================================================
##### Priors ===================================================================

## ----recruitment univariate sf priors1


common.abnd %>% filter(Species == "Siganus fuscescens") %>% 
  group_by(Treatment) %>%  summarise(log(mean(abundance)), 
                                     log(sd(abundance)))

##priors for Effects
common.abnd %>% filter(Species == "Siganus fuscescens") %>% 
  pull(abundance) %>% sd() %>% 
  log()/apply(model.matrix(~Treatment, data = common.abnd), 2, sd)


## ----end

## ----recruitment univariate sf treatment reorder
dat.sub <- common.abnd %>% filter(Species == "Siganus fuscescens") %>% 
  droplevels() %>% 
  mutate(Treatment = forcats::fct_relevel(Treatment, "W")) #change first level to W

## ----end

##### Fitting ===================================================================

## ----recruitment univariate sf brmsfit
sf.form <- bf(abundance ~ Treatment
              + (1|plotID),
              autocor = ~ ar(time = Day, gr = plotID, 
                             p = 1),
              family = poisson(link = "log") )

priors <- prior(normal(-1, 1), class = "Intercept") +
  prior(normal(0,3), class = "b") + 
  prior(cauchy(0,0.5), class = "sd") + 
  prior(cauchy(0,0.5), class = "sderr") +
  prior(uniform(-1,1), class = "ar") 

sf.brm1 <- brm(sf.form,
               data = dat.sub,
               prior = priors,
               sample_prior = "yes", #sample priors and posteriors
               iter = 10000, warmup = 2000,
                control = list(adapt_delta = 0.99), #devote more of warmup to step-length determination
               chains = 3, cores = 3, 
               thin = 10,
               seed = 123)

sf.brm1 %>% hack_size.brmsfit() %>% saveRDS(file = paste0(DATA_PATH, "modelled/sf.brm1.rds"))


## ----end

##### Prior Checks =============================================================
## ----recruitment univariate sf brm1 prior check
sf.brm1 <- readRDS(file = paste0(DATA_PATH, "modelled/sf.brm1.rds"))

sf.brm1 %>% 
  as_draws_df() %>% #get all the draws for everything estimated
  
  dplyr::select(!matches("^lp|^err|^r_|^\\.") ) %>% #remove variables starting with lp, err or r_ or .
  #Note removing the '.' cols (.iteration, .draw and .chain) changed the class
  
  pivot_longer(everything(), names_to = 'key') %>% #make long, with variable names in a column called 'key'. Note 
  
  mutate(Type = ifelse(str_detect(key, 'prior'), 'Prior', 'Posterior'), #classify within new col 'Type' whether Prior or Posterior using str_detect
         Class = case_when( #create column 'Class' to classify vars as:
           str_detect(key, '(^b|^prior).*Intercept$') ~ 'Intercept', #intercept, if 'key' starts with b or prior followed by any character ('.') with 'intercept' at the end
           str_detect(key, 'b_Treatment.*|prior_b') ~ 'TREATMENT', #TREATMENT, if the string contains 'b_Treatment followed by any character ('.')
           str_detect(key, 'sd_') ~ 'sd', #sd, if the string contains sf ('sderr' will be included)
           str_detect(key, 'ar') ~ 'ar', #ar, if it contains ar
           str_detect(key, 'sderr') ~ 'sderr'), #sderr, if it contains sderr
         Par = str_replace(key, 'b_', '')) %>% 
  
  ggplot(aes(x = Type,  y = value, color = Par)) + #Plot with these overall aesthetics
  stat_pointinterval(position = position_dodge(), show.legend = FALSE)+ #plot as stat_point intervals
  facet_wrap(~Class,  scales = 'free') #separate plots by Class with each class having its own scales


## ----end

#####MCMC =====================================================================

## ----recruitment univariate sf brm1 MCMC
sf.brm1 <- readRDS(file = paste0(DATA_PATH, "modelled/sf.brm1.rds"))

pars <- sf.brm1 %>% get_variables()

wch <- str_extract(pars, #get the names of the variables
                   '^b_.*|^sd.*|^ar.*') %>% #that start with b, sf or ar
  na.omit # omit the rest, resulting in an object of class 'omit'
#wch
##Trace Plots
stan_trace(sf.brm1$fit, pars = wch)

##Autocorrelation factor
stan_ac(sf.brm1$fit, pars = wch)

##rhat - Scale reduction factor
stan_rhat(sf.brm1$fit, pars = wch)

##ESS (effective sample size)
stan_ess(sf.brm1$fit, pars = wch)

##Density plot
stan_dens(sf.brm1$fit, pars = wch, separate_chains = TRUE)

##Density overlay
sf.brm1%>% pp_check(type = 'dens_overlay', ndraws = 100)

## ----end

##### DHARMA Residuals ==========================================================

## ---- recruitment univariate sf brm1 DHARMa

#step 1. Draw out predictions
preds <- sf.brm1 %>% posterior_predict(ndraws = 250, #extract 250 posterior draws from the posterior model
                                       summary = FALSE) #don't summarise - we want the whole distribution of them


#Step 2 create DHARMA resids
resids <- createDHARMa(simulatedResponse = t(preds), #provide with simulated predictions, transposed with t()
                       observedResponse = dat.sub %>% 
                         filter(Species == "Siganus fuscescens") %>% 
                         pull(abundance), #real response
                       fittedPredictedResponse = apply(preds, 2, median), #for the fitted predicted response, use the median of preds (in the columns, the second argument of apply defines the MARGIN, 2 being columns for matrices)
                       integerResponse = "TRUE" #is the response an integer? yes
                       #, re.form = "NULL") #this argument not supported (neither in glmmTMB)
)

#Step 3 - plot!
plot(resids)



#### Model Investigation ========================================================
#Frequentist
## ----recruitment univariate sf frequentist summary
sf.glmmTMB.ac %>% summary()

sf.glmmTMB2 %>% r.squaredGLMM()
## ----end

#Bayesian

## ---- recruitment univariate sf brm1 summary
sf.brm1$fit %>% tidyMCMC(pars = wch,
                         estimate.method = "median",
                         conf.int = TRUE,
                         conf.method = "HPDinterval",
                         rhat = TRUE,
                         ess = TRUE)

#"Marginal"
sf.brm1 %>% brms::bayes_R2(re.form = NA, #or ~Treatment
                           summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2

# "Conditional"
sf.brm1 %>% brms::bayes_R2(re.form = NULL, #or ~(1|plotID)
                           summary = FALSE) %>% #don't summarise - I want ALL the R-squareds
  median_hdci # get the hdci of the r2


## ----end

## ---- recruitment univariate sf brm contrasts
sf.brm1%>%
  emmeans(~Treatment, type = 'link') %>% #link scale
  pairs() %>% #pairwise comparison
  gather_emmeans_draws() %>% #take all the draws for these comparisons, gather them (make long)
  #median_hdci(exp(.value)) #this would essentially give us the summary above. Nothing too special yet, but we have more control
  summarise('P>' = sum(.value>0)/n(), #exceedance probabilities
            'P<' = sum(.value<0)/n(),
  ) %>% 
  ungroup() %>% 
  mutate(evidence = case_when(
    `P>` >= 0.99 | `P>` >= 0.99 ~ "very strong",
    `P>` >= 0.95 |`P<` >= 0.95 ~ "strong",
    `P>` >= 0.90 |`P<` >= 0.90 ~ "evidence",
    TRUE ~ "no evidence"
  )
  )

## ----end

#### Summary figures =========================================================

## ---- recruitment univariate sf figures

sf.brm1 <- readRDS(file = paste0(DATA_PATH, "modelled/sf.brm1.rds"))
sf.brm1 %>% ggemmeans(~Treatment) %>% plot


newdata <- sf.brm1 %>% emmeans(~Treatment, type = "link") %>% 
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
  ylab("L. atkinsoni abundance") +
  theme_classic()

sf.em <- sf.brm1 %>%
  emmeans(~Treatment, type = "link") %>%
  pairs() %>%
  gather_emmeans_draws() %>%
  mutate(Fit = exp(.value)) %>% as.data.frame()
#head(sp.em)

g2<- sf.em %>%
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

ggsave(filename = paste0(FIGS_PATH, "/bayes.sf.png"),
       g1,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.sf.contr.png"),
       g2,
       height = 5,
       width = 10,
       dpi = 100)
ggsave(filename = paste0(FIGS_PATH, "/bayes.sf.both.png"),
       g1 + theme(legend.position = "none") + g2,
       height = 5,
       width = 15,
       dpi = 100)


 ## Multivariate ================================================================


### Data =========================================================================
## ---- recruitment multivariate readData
fishalgaedata <- read_csv(file = paste0(DATA_PATH, "processed/fishalgaedata.csv")) %>% 
  mutate_at(c(2:5,9,11), factor)
## ----end

## ---- recruitment multivariate data set up
  ### convert to abund/plot (eventually could use biomass if I need to)

fish.wide <- fishalgaedata %>%
  group_by(Treatment, Replicate, Date, Species) %>%
  summarise(abundance = sum(count)) %>% ## get abund/plot per species (eventually could use biomass if I need to)
  pivot_wider(names_from = Species,   ## convert to wide
              values_from = abundance,
              values_fill = 0)
glimpse(fish.wide)

fish.wide.end <- fishalgaedata %>% 
  filter(Date == "2022-12-12") %>% 
  group_by(Treatment, Replicate, Date, Species) %>%
  summarise(abundance = sum(count)) %>% ## get abund/plot per species (eventually could use biomass if I need to)
  pivot_wider(names_from = Species,   ## convert to wide
              values_from = abundance,
              values_fill = 0) %>% 
  mutate(plotID = paste0(Treatment, Replicate) )%>% 
           column_to_rownames(var = "plotID")
           
  View(fish.wide.end)
## ----end  
  
 ### NMDS =======================================================================
  
  
##  ----recruitment multivariate end mds
  #distance matrix (for )
  fish.dist <- vegdist(wisconsin(fish.wide.end[,-c(1:3)]^0.25), "bray")
  ## mds
  #fish.mds <- metaMDS(fish.dist, k=2, seed = 123)
  
  fish.dist.mds <- metaMDS(fish.dist, k= 2, seed = 123) # can't figure out how to extract scores
  fish.dist.mds
  
  #do mds on raw data (let it decide on standardisations)
  fish.mds <- metaMDS(fish.wide.end[,-c(1:3)], k = 2, seed = 123)
  fish.mds
  
  #check stressplot:
  stressplot(fish.mds)
## ----end

  
## ----recruitment multivariate end mds ggplot
fish.mds.scores <-   fish.mds %>% fortify()
library(ggrepel)  

g <-
  ggplot(data = NULL, aes(y=NMDS2, x=NMDS1)) +
  geom_hline(yintercept=0, linetype='dotted') +
  geom_vline(xintercept=0, linetype='dotted') +
  geom_point(data=fish.mds.scores %>%
               filter(Score=='sites'),
             aes(color=fish.wide.end$Treatment))  +#colour the points according to their Treatment
  geom_text_repel(data=fish.mds.scores %>%
              filter(Score=='sites'),
            aes(label=Label,
                color=fish.wide.end$Treatment), hjust=-0.2
            ) +
  labs(color = "Treatment")
g

#probs needs some jitter/dodging but you can see some groupings (particularly w)
## ----end
  
ggsave(filename = paste0(FIGS_PATH, "/nmds.end.png"),
       g,
       height = 5,
       width = 10,
       dpi = 100)

## ----recruitment multivariate end adonis
#adonis to be performed on distance matrix (therefore might not match the mds done on raw data)
fish.adonis <- adonis2(fish.dist ~ Treatment, data = fish.wide.end)
fish.adonis
#by levels

treatment <- factor(fish.wide.end$Treatment, levels = c("BH", "BQ", "DL", "DM", "W"))
mm <- model.matrix(~treatment) #predictor matrix
colnames(mm) <-gsub("treatment","",colnames(mm)) #remove "treatment" from the start of each colname

mm<- data.frame(mm)

fish.adonis <- adonis2(fish.dist ~ BQ + DL + DM + W, data = mm,
                       permm = 9999)
fish.adonis
## ----end
  
## ----recruitment multivariate end pairwise

##not sure how well I can trust this but
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis") 
library(pairwiseAdonis)
pair.mod<-pairwise.adonis(fish.dist,factors=fish.wide.end$Treatment)
pair.mod

##none are significantly different once adjusted for multiple comparisons
## ----end



## ----recruitment multivariate end disper
fish.disp <- betadisper(fish.dist, group = fish.wide.end$Treatment)

boxplot(fish.disp)

anova(fish.disp)

permutest(fish.disp, pairwise = TRUE)

plot(fish.disp)
## ----end

##    
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