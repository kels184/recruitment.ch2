
#Working out how to do pairwise comparisons
#https://stats.stackexchange.com/questions/477826/how-is-pairwise-permanova-adonis-a-valid-non-parametric-approach-for-pairwise-co

#simulate data
set.seed(123)
data <- data.frame(group = factor(rep(c(paste0("G",1:5)), c(10,24,10,12,9))),
                   val = c(rnorm(10, mean=1.34,sd=0.17), 
                           rnorm(24, mean = 1.14, sd=0.11),
                           rnorm(10, mean=1.19, sd=0.15),
                           rnorm(12, mean=1.06, sd=0.11),
                           rnorm(9, mean=1.09, sd = 0.10)))
data


library(vegan)
dist.mat <- vegdist(data$val, method = "euclidean")

#overall permanova
adonis2(dist.mat ~ data$group, method = "euclidean")



#GRoup 1 and 5 comparison

library(dplyr)
ex <- c("G1","G5")
dat2 <- data%>%dplyr::filter(., group %in% ex)

#make distance matrix: 
dis.mat <- vegdist(dat2$val, method="euclidean")

dis.mat <- vegdist(dat2$val, method="euclidean")
res2<- adonis2(dis.mat ~ dat2$group, method = "euclidean")
res2

#is making the dat2 first, then rerunning the distance matrix the same as 
#filtering for the relevant potrions of the first distance matrix, then running the adonis2 on that?

mx1 <- dist.mat %>% as.matrix()
View(mx1[,56:66])
view(dis.mat %>% as.matrix)

#looks like it would be the same


wch <- data[data$group %in% ex,] %>% row.names()
wch

test.mat <- dist.mat %>% as.matrix() %>% 
  as_tibble() %>% #do this so tidy functions work
  filter(rownames(.) %in% wch) %>% #get only relevant rows
  select(all_of(wch)) %>%# select relevant columns
  as.matrix() #turn it back into matrix
test.mat

all.equal(test.mat,dis.mat %>% as.matrix)
view(dis.mat %>% as.matrix)
View(test.mat)

res3<- adonis2(test.mat ~ dat2$group, method = "euclidean")
res3


#But wait - do I care if biomass influences composition independently of density 
#or vice versa? or would it be enough to just compare W to everything else?

#I think I do want planned contrasts, purely for the BH-DM contrast


