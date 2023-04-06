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
abnd.resid %>% testDispersion()
abnd.resid %>% testUniformity()

## ----end

    #### Partial plot ===========================================================
## ----recruitment univariate abundance partial
abnd.glmmTMB2 %>% ggpredict(terms = "Treatment") %>% plot() #not working atm, could be to do with incompatible versions of TMB and glmmTMB

## ----end

   #### Model investigation =====================================================

## ---- recruitment univariate abundance summary

abnd.glmmTMB2 %>% summary()
r.squaredGLMM(abnd.glmmTMB2)

## ----end

## ---- recruitment univariate abundance planned contrasts



## ----end


   #### Summary figures =========================================================



  ### Species Richness ==========================================================
   #### Fit =====================================================================

   #### Validate/ Refit =========================================================

   #### Model investigation =====================================================

   #### Summary figures =========================================================


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