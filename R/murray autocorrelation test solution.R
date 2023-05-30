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