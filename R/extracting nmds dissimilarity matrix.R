##From a comment made by Jari Oksanen (vegan creator) on a stackoverflow thread: https://stackoverflow.com/questions/25629645/r-metamds-ordination-distances


##NB: Dissimilarity matrix used to make the NMDS is found in "fish.mds"mds_object_name"$diss. The original order is not preserved
## but its indices are at"mds_object_name"$iidx

#"... internal structures of the monoMDS results. These are undocumented, and you should understand 
#what you look at. ordjac2$dist contains distances among points in the ordination space; the original 
#dissimilarities are in ordjac2$diss. Neither of these are in the original order, but their indices are 
#ordjac2$iidx and ordjac2$jidx. If you order by these, you get the input dissimilarities."


#Also see this thread: https://stackoverflow.com/questions/71941069/species-scores-not-available-as-result-of-metamds
#which details how to input species scores if a dissimilarity is supplied to metamds




library(tidyverse)
library(vegan)
library(ggvegan)
library(GGally)

fishalgaedata <- read_csv(file = paste0(DATA_PATH, "processed/fishalgaedata.csv")) %>% 
  mutate_at(c(2:5,9,11), factor)

fish.wide.end <- fishalgaedata %>% 
  filter(Date == "2022-12-12") %>% 
  group_by(Treatment, Replicate, Date, Species) %>%
  summarise(abundance = sum(count)) %>% ## get abund/plot per species (eventually could use biomass if I need to)
  pivot_wider(names_from = Species,   ## convert to wide
              values_from = abundance,
              values_fill = 0) %>% 
  mutate(plotID = paste0(Treatment, Replicate) )%>% 
  column_to_rownames(var = "plotID") %>% 
  data.frame()

View(fish.wide.end)

#distance matrix 
fish.dist <- vegdist(wisconsin(fish.wide.end[,-c(1:3)]^0.25), "bray")
fish.dist

## mds

fish.dist.mds <- metaMDS(fish.dist, k= 2, trymax = 20, seed = 123)
fish.dist.mds
fish.dist.mds$species
fish.dist.mds$diss


fish.dist.mds$iidx
fish.dist.mds$jidx
##These two indices refer to i (row) and j (columns) and describe how 'diss' and 'dist'
##have been rearranged from their original order (that of fish.dist)
#check if diss has the same values as fish.dist:
df <- data.frame(diss = fish.dist.mds$diss,
                 i = fish.dist.mds$iidx,
                 j = fish.dist.mds$jidx)
View(df)
num1 <- df %>% filter(i == 2 & j == 1) %>% pull(diss) #0.5080523
num1 %>% str()
num2 <- fish.dist[1] #0.5080523
num2 %>% str()
identical(num1, num2) #FALSE apparently, though I can't see why

#reconstruct the diss matrix using the indices
mx <- with(df, matrix(ncol=max(j), nrow=max(i))) #make empty matrix
mx[as.matrix(df[2:3])] <- df$diss #fill with the diss values
rownames(mx) <- rownames(fish.wide.end) #give original rownames
colnames(mx) <- rownames(fish.wide.end)[1:24]
mx <- as.dist(mx) #convert mx into a dist matrix object
View(mx)

all.equal(fish.dist,mx) ##not same

a <- fish.dist %>% as.matrix()
b <- mx %>% as.matrix()


all.equal(a,b) #####[1] TRUE !!


#now I know how to get the dissimilarity matrix from the MDS object, and confirmed that it looks the same as the 
#dist obj made with vegdist, I should be able to perform things on it (e.g. adonis, disper)

adonis2(mx ~ Treatment, data = fish.wide.end) ## WORKS! output the same as adonis2(fish.dist)



##Therefore, even if I'm not getting the same results when using metaMDS() on data as on dist matrix, I can at least extract the 
#distance matrix that is being used in the mds (e.g.if i choose to stick with the mds on data since fortify()) works 

#mds on data, as opposed to distance matrix
fish.mds <- metaMDS(wisconsin(fish.wide.end[,-c(1:3)]^0.25), k = 2, autotransform = FALSE,
                    trymax= 20, seed = 123, distance = "bray")
fish.mds
fish.mds$species
fish.mds$diss
fish.mds$jidx
fish.wide.end[,-c(1:3)]

df2 <-  data.frame(diss = fish.mds$diss,
                        i = fish.mds$iidx,
                        j = fish.mds$jidx)


mx2 <- with(df2, matrix(ncol=max(j), nrow=max(i))) #make empty matrix
mx2[as.matrix(df2[2:3])] <- df2$diss #fill with the diss values
rownames(mx2) <- rownames(fish.wide.end) #give original rownames
colnames(mx2) <- rownames(fish.wide.end)[1:24]
mx2 <- as.dist(mx2) #convert mx2 into a dist matrix object

adonis2(mx2 ~ Treatment, data = fish.wide.end) #similar, but not identical result to the above, ut at least it worked!




#alternatively I could still use the dist matrix to create the nmds, but add species data to the mds:

sppscores(fish.dist.mds) <- fish.wide.end[,-c(1:3)]^0.25 #add species data back in. Be sure to use same transformation

scores <- fish.dist.mds %>% fortify()
scores #works!
fish.dist.mds$species #I can see species data!



##Compare plots
##from dist matrix
dist.mds.scores <- scores

dist.g <-
  ggplot(data = NULL, aes(y=NMDS2, x=NMDS1)) +
  geom_hline(yintercept=0, linetype='dotted') +
  geom_vline(xintercept=0, linetype='dotted') +
  geom_point(data=dist.mds.scores %>%
               filter(Score=='sites'),
             aes(color=fish.wide.end$Treatment))  +#colour the points according to their Treatment
  geom_text_repel(data=dist.mds.scores %>%
                    filter(Score=='sites'),
                  aes(label=Label,
                      color=fish.wide.end$Treatment), hjust=-0.2
  )  +
  geom_point(data=dist.mds.scores %>%
               filter(Score=='species') ) +
  geom_text_repel(data=dist.mds.scores %>%
                    filter(Score=='species'),
                  aes(label=Label )
  ) +
  labs(color = "Treatment")

dist.g


#from community data
data.scores <- fish.mds %>% fortify()

data.g <-
  ggplot(data = NULL, aes(y=NMDS2, x=NMDS1)) +
  geom_hline(yintercept=0, linetype='dotted') +
  geom_vline(xintercept=0, linetype='dotted') +
  geom_point(data=data.scores %>%
               filter(Score=='sites'),
             aes(color=fish.wide.end$Treatment))  +#colour the points according to their Treatment
  geom_text_repel(data=data.scores %>%
                    filter(Score=='sites'),
                  aes(label=Label,
                      color=fish.wide.end$Treatment), hjust=-0.2
  ) +
  geom_point(data=data.scores %>%
               filter(Score=='species') ) +
  geom_text_repel(data=data.scores %>%
                    filter(Score=='species'),
                  aes(label=Label )
  ) +
  labs(color = "Treatment")

data.g


#I'm satisfied that the two are similar enough that I could go with either. 

#Initially thought stress was lower in dist version, but that was because I didn't 
#Wisconsin double standardise the data in the other mds
#

#only 2 differences I can see: the p value in the adonis anova tables - 0.03 for 
#the non dist model, 0.05 for the dist, even though everything else in the tables are the same
#The mds on the data also performs halfchange scaling: " one axis unit means that community dissimilarity halves"
#(see metaMDS help section 5. Scaling of the results)


test <- metaMDS(fish.dist,
                halfchange = TRUE, #force halfchange scaling
                k = 2, autotransform = FALSE,
                trymax= 20, seed = 123, distance = "bray")

df3 <- data.frame(diss = test$diss,
                 i = test$iidx,
                 j = test$jidx)

mx3 <- with(df3, matrix(ncol=max(j), nrow=max(i))) #make empty matrix
mx3[as.matrix(df3[2:3])] <- df3$diss #fill with the diss values
rownames(mx3) <- rownames(fish.wide.end) #give original rownames
colnames(mx3) <- rownames(fish.wide.end)[1:24]
mx3 <- as.dist(mx3)

all.equal(mx3 %>% as.matrix(),mx2 %>% as.matrix()) ##TRUE!

adonis2(mx3 ~ Treatment, data = fish.wide.end) ##same result as mx2

?vegdist()



#compare dispersion results:
disp1 <- betadisper(fish.dist, group = fish.wide.end$Treatment)
betadisper(mx, group = fish.wide.end$Treatment) #same as above
disp2 <- betadisper(mx2, group = fish.wide.end$Treatment) #same as above
disp3 <- betadisper(mx3, group = fish.wide.end$Treatment) #same as above

#permutest(disp1, pairwise = TRUE, seed = 123)
#permutest(disp2, pairwise = TRUE, seed = 123)
#permutest(disp3, pairwise = TRUE, seed = 123)

#3 (slightly) different results? however, could be due to randomness of permuting (different results each time)
#try something that's not random:

anova(disp1)
anova(disp2)
anova(disp3)
#ok, all 3 are identical

#for now I'm satisfied that they are similar enough that I could choose any - 
# mx2 I'm choosing mx2 method (feed community data to metaMDS, don't have to 
#then add species data back in and don't have to specify halfchange scale)

