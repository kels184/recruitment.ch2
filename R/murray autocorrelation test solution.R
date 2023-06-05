library(tidyverse)
library(DHARMa)
library(glmmTMB)
simCor1 <- function(phi=0.8,sdgrp=2,sdres=1,
                    npergrp=20,ngrp=20,
                    seed=NULL,
                    ## set linkinv/simfun for GLMM sims
                    linkinv=identity,
                    simfun=identity) {
  if (!is.null(seed)) set.seed(seed)
  cmat <- sdres*phi^abs(outer(0:(npergrp-1),0:(npergrp-1),"-"))
  errs <- MASS::mvrnorm(ngrp,mu=rep(0,npergrp),Sigma=cmat)
  ranef <- rnorm(ngrp,mean=0,sd=sdgrp)
  d <- data.frame(f=rep(1:ngrp,each=npergrp))
  eta <- ranef[as.numeric(d$f)] + c(t(errs)) ## unpack errors by row
  mu <- linkinv(eta)
  d$y <- simfun(mu)
  d$tt <- factor(rep(1:npergrp,ngrp))
  return(d)
}

set.seed(123)
d <- simCor1(phi = 0.8, seed = 100)

d <- d %>%
  mutate(Time = numFactor(as.numeric(tt)),
         nTime = as.numeric(tt))
dat.sub <- d %>%
  filter(f == 3)
## start by fitting a simple model to just the first group
mod <- glmmTMB(y ~ 1, data = dat.sub)
acf(residuals(mod, type = "pearson"))
## clear temporal autocorrelation present
resid <- simulateResiduals(mod)
plot(resid)
testTemporalAutocorrelation(resid, time = unique(dat.sub$tt))
## temporal autocorrelation suggested by DHARMa

## incorporate temporal autocorrelation into the model as AR1
mod <- glmmTMB(y ~ 1 + ar1(tt + 0 | f), data = dat.sub)
acf(residuals(mod, type = "pearson"))
## autocorrelation appears to have been accounted for
resid <- simulateResiduals(mod)
plot(resid)
testTemporalAutocorrelation(resid, time = unique(dat.sub$tt))
## DHARMa residuals fail to account for the autocorrelation structure

## what about an Ornstein-Uhlenbeck covariance structure
mod <- glmmTMB(y ~ 1 + ou(Time + 0 | f), data = dat.sub)
acf(residuals(mod, type = "pearson"))
resid <- simulateResiduals(mod)
plot(resid)
testTemporalAutocorrelation(resid, time = unique(dat.sub$tt))
## again, acf on Pearson residuals confirms we have accounted for
## autocorrelation, yet DHARMa does not...

## Now lets add nested structure
mod <- glmmTMB(y ~ 1 + (1|f),
               data = d)
acf(residuals(mod, type = "pearson"))
resid <- simulateResiduals(mod)
testTemporalAutocorrelation(resid, time = d$tt)
resid1 <- recalculateResiduals(resid, group = d$tt)
testTemporalAutocorrelation(resid1, time = unique(d$tt))
## DHARMa fails to detect the autocorrelation in the first place...

mod <- glmmTMB(y ~ 1 + (1|f) + ar1(0 + tt|f),
               data = d)
acf(residuals(mod, type = "pearson"))
resid <- simulateResiduals(mod)
resid1 <- recalculateResiduals(resid, group = d$tt)
testTemporalAutocorrelation(resid1, time = unique(d$tt))
## ACF on Pearson residuals does the trick...




##Kelsey testing acf
#putting this here so it's all together in case I need to copy

#same data simulation function as Murray
simCor1 <- function(phi=0.8,sdgrp=2,sdres=1,
                    npergrp=20,ngrp=20,
                    seed=NULL,
                    ## set linkinv/simfun for GLMM sims
                    linkinv=identity,
                    simfun=identity) {
  if (!is.null(seed)) set.seed(seed)
  cmat <- sdres*phi^abs(outer(0:(npergrp-1),0:(npergrp-1),"-"))
  errs <- MASS::mvrnorm(ngrp,mu=rep(0,npergrp),Sigma=cmat)
  ranef <- rnorm(ngrp,mean=0,sd=sdgrp)
  d <- data.frame(f=rep(1:ngrp,each=npergrp))
  eta <- ranef[as.numeric(d$f)] + c(t(errs)) ## unpack errors by row
  mu <- linkinv(eta)
  d$y <- simfun(mu)
  d$tt <- factor(rep(1:npergrp,ngrp))
  return(d)
}

set.seed(123)
d <- simCor1(phi = 0.8, seed = 100)

mod1 <- glmmTMB(y ~ 1 + (1|f),
               data = d)
acf(residuals(mod1, type = "pearson"))
ac1 <- acf(residuals(mod1, type = "pearson"), lag.max = nrow(d))
ac1

#if prompted using the lag.max arg, acf produces as many acf estimates as there 
#are obs, although the default number appears to be 27

#The position of the CIs appears to be at 0.1 which suggests the full number of 
#observations (400) is used to calculate their position: 2/sqrt(T)
2/sqrt(400) # 0.1
# rather than the number of time points
2/sqrt(length(unique(d$tt))) #0.447

sum(abs(ac1$acf)>0.1)/length(ac1$acf) #10.25% of cor > 0.1
#

#the model with autocorrelation structure
mod2 <- glmmTMB(y ~ 1 + (1|f) + ar1(0 + tt|f),
               data = d)

acf(residuals(mod, type = "pearson"))
ac2 <- acf(residuals(mod2, type = "pearson"), lag.max = nrow(d))
ac2
sum(abs(ac2$acf)>0.1)/length(ac1$acf) #3.75% of cor > 0.1
#There is clearly an improvement, however, is it the improvement we want, and 
#is acf the best way of checking this?



#someone on stackoverflow suggested something like this this for a related question (ACF by group in R)


#but note this is acf of the raw results, not the residuals
grouped_acf_values <- d %>%
  tidyr::nest(-f) %>%
  dplyr::mutate(acf_results = purrr::map(data, ~ acf(.x$y, plot = F)),
                acf_values = purrr::map(acf_results, ~ drop(.x$acf))) %>%
  tidyr::unnest(acf_values) %>%
  dplyr::group_by(f) %>%
  dplyr::mutate(lag = seq(0, n() - 1))
grouped_acf_values 

grouped_acf_values %>% filter(f == 2)
d()