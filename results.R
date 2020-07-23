## sRoot2 script to see results and plot figures

.libPaths(c("~/R/intel/3.6/")) #set lib paths

library(dplyr)
library(lme4)
library(lmerTest)
library(effects)

unscale <- function(z, center = attr(z, "scaled:center"), scale = attr(z, "scaled:scale")) {
  if(!is.null(scale))  z <- sweep(z, 2, scale, `*`)
  if(!is.null(center)) z <- sweep(z, 2, center, `+`)
  structure(z,
            "scaled:center"   = NULL,
            "scaled:scale"    = NULL,
            "unscaled:center" = center,
            "unscaled:scale"  = scale
  )
} 

############## Fig 2 #########################################
setwd("/gscratch/dlaughl4/SRL_v1/")

load("f3.RData")
load("g3.RData")
load("w3.RData")

### make xscale data frames
f3@frame$tmin.unscaled <- unscale(f3@frame$tmin)
f3@frame$SRL.unscaled <- unscale(f3@frame$SRL)
lm1 <- lm(tmin.unscaled ~ as.vector(tmin), data=f3@frame)
lm2 <- lm(SRL.unscaled ~ as.vector(SRL), data=f3@frame)
newdat <- data.frame(tmin=seq(-5,5,1), SRL=seq(-5,5,1))
xscale.SRL.forests <- data.frame(sd=seq(-5,5,1), tmin=round(predict(lm1,newdat),0), SRL=round(exp(predict(lm2,newdat)),0))

g3@frame$tmin.unscaled <- unscale(g3@frame$tmin)
g3@frame$SRL.unscaled <- unscale(g3@frame$SRL)
lm1 <- lm(tmin.unscaled ~ as.vector(tmin), data=g3@frame)
lm2 <- lm(SRL.unscaled ~ as.vector(SRL), data=g3@frame)
newdat <- data.frame(tmin=seq(-5,5,1), SRL=seq(-5,5,1))
xscale.SRL.grasslands <- data.frame(sd=seq(-5,5,1), tmin=round(predict(lm1,newdat),0), SRL=round(exp(predict(lm2,newdat)),0))

pdf("/gscratch/dlaughl4/fig2.pdf", width=11.5,height=15)
par(mfrow=c(4,3), cex.lab=1.2)
# TxE Interactions
tmin.plot <- seq(quantile(f3@frame$tmin,0.01), quantile(f3@frame$tmin,0.99), length.out=100)
plot(tmin.plot, fixef(f3)[4] + fixef(f3)[5]*tmin.plot, type="l", col="black",ylim=c(-2,2),
     ylab="SRL effect on Probability of Occurrence", xlab="", lwd=4)
title(sub=list(expression(paste("Minimum temperature (",degree,"C)")), cex=1.2), line=2.5)
title(sub="\n(scaled to unit variance below, native scale above)",line=3.5)
lines(tmin.plot, fixef(g3)[4] + fixef(g3)[5]*tmin.plot, col="burlywood3", lwd=4)
lines(tmin.plot, fixef(w3)[4] + fixef(w3)[5]*tmin.plot, col="lightblue", lwd=4)
abline(h=0,lty=3,col=1)
axis(3, at=xscale.SRL.forests$sd, labels=xscale.SRL.forests$tmin, line=0)
title("(A) SRL-by-temperature interactions",line=2.5)
legend("topright", legend=c("Forests (P < 0.0001)","Grasslands (P < 0.0001)","Wetlands (P = 0.2770)"), lwd=4,
       col=c("black","burlywood3","lightblue"), bty="n", cex=1.2)
colfunc <- colorRampPalette(c("blue", "orangered"))
points(x = tmin.plot, y = rep(-2.1, length(tmin.plot)),pch = 15,cex = 5, col = colfunc(length(tmin.plot)))

### Plot TxE interactions with confidence intervals
newdat <- data.frame(
  SRL = rep(seq(quantile(f3@frame$SRL,0.01),quantile(f3@frame$SRL,0.99),length.out = 100), 2),
  tmin = c(rep(quantile(f3@frame$tmin, 0.01), 100), rep(quantile(f3@frame$tmin, 0.99), 100)),
  tmin2 =c(rep(mean(f3@frame$tmin2), 200)),
  lab = factor(rep(paste0(c("low.", "high."), "tmin"), each = 100))) %>%
  mutate(`tmin:SRL` = tmin * SRL)
mm <- model.matrix( ~ tmin + tmin2 + SRL + tmin:SRL, newdat)
newdat$eta <- mm %*% fixef(f3)
newdat$ProbOccurrence <- exp(newdat$eta) / (1 + exp(newdat$eta))
newdat$VarPred <- diag(mm %*% vcov(f3) %*% t(mm))
newdat$sePred <- sqrt(newdat$VarPred)
newdat$seLow <-
  exp(newdat$eta - 1.96 * newdat$sePred) / (1 + exp(newdat$eta - 1.96 * newdat$sePred))
newdat$seHigh <-
  exp(newdat$eta + 1.96 * newdat$sePred) / (1 + exp(newdat$eta + 1.96 * newdat$sePred))

plot(newdat$SRL[1:100], newdat$ProbOccurrence[1:100], type="l", col="blue",
     ylim=c(0,0.3), xlab="", ylab="Probability of Occurrence", cex.lab=1.2)
title(sub=list(expression(paste("Specific root length (m g",{}^-1,")")), cex=1.2),line=2.5)
title(sub="\n(scaled to unit variance below, log scale above)",line=3.5)
polygon(x=c(seq(min(newdat$SRL),max(newdat$SRL),length.out=100),
            rev(seq(min(newdat$SRL),max(newdat$SRL),length.out=100))),
        y=c(newdat$seLow[1:100], rev(newdat$seHigh[1:100])),
        col=adjustcolor("blue",alpha.f=0.5), border=NA)
lines(newdat$SRL[1:100], newdat$ProbOccurrence[1:100], col="darkblue")
polygon(x=c(seq(min(newdat$SRL),max(newdat$SRL),length.out=100),
            rev(seq(min(newdat$SRL),max(newdat$SRL),length.out=100))),
        y=c(newdat$seLow[101:200], rev(newdat$seHigh[101:200])),
        col=adjustcolor("orangered",alpha.f=0.5), border=NA)
lines(newdat$SRL[101:200], newdat$ProbOccurrence[101:200],col="orangered3")
axis(3, at=xscale.SRL.forests$sd, labels=xscale.SRL.forests$SRL, line=0)
title("(B) Forests",line=2.5)
legend("topright", legend=c("Low temperature (1st %)","High temperature (99th %)"), lty=1,
       col=c("blue","orangered"), lwd=3, bty="n", cex=1.2)
text(-1.5,0.15,"Warm",col="orangered3", cex=1.4)
text(1,0.15,"Cold",col="blue", cex=1.4)
text(-0.3,0.2,"BIDIRECTIONAL", col=1, cex=1.4)

### Plot TxE interactions with confidence intervals
newdat <- data.frame(
  SRL = rep(seq(quantile(g3@frame$SRL,0.01),quantile(g3@frame$SRL,0.99),length.out = 100), 2),
  tmin = c(rep(quantile(g3@frame$tmin, 0.01), 100), rep(quantile(g3@frame$tmin, 0.99), 100)),
  tmin2 =c(rep(mean(g3@frame$tmin2), 200)),
  lab = factor(rep(paste0(c("low.", "high."), "tmin"), each = 100))) %>%
  mutate(`tmin:SRL` = tmin * SRL)
mm <- model.matrix( ~ tmin + tmin2 + SRL + tmin:SRL, newdat)
newdat$eta <- mm %*% fixef(g3)
newdat$ProbOccurrence <- exp(newdat$eta) / (1 + exp(newdat$eta))
newdat$VarPred <- diag(mm %*% vcov(g3) %*% t(mm))
newdat$sePred <- sqrt(newdat$VarPred)
newdat$seLow <-
  exp(newdat$eta - 1.96 * newdat$sePred) / (1 + exp(newdat$eta - 1.96 * newdat$sePred))
newdat$seHigh <-
  exp(newdat$eta + 1.96 * newdat$sePred) / (1 + exp(newdat$eta + 1.96 * newdat$sePred))

plot(newdat$SRL[1:100], newdat$ProbOccurrence[1:100], type="l", col="blue",
     ylim=c(0,0.3), xlab="", ylab="Probability of Occurrence", cex.lab=1.2)
title(sub=list(expression(paste("Specific root length (m g",{}^-1,")")), cex=1.2),line=2.5)
title(sub="\n(scaled to unit variance below, log scale above)",line=3.5)
polygon(x=c(seq(min(newdat$SRL),max(newdat$SRL),length.out=100),
            rev(seq(min(newdat$SRL),max(newdat$SRL),length.out=100))),
        y=c(newdat$seLow[1:100], rev(newdat$seHigh[1:100])),
        col=adjustcolor("blue",alpha.f=0.5), border=NA)
lines(newdat$SRL[1:100], newdat$ProbOccurrence[1:100], col="darkblue")
polygon(x=c(seq(min(newdat$SRL),max(newdat$SRL),length.out=100),
            rev(seq(min(newdat$SRL),max(newdat$SRL),length.out=100))),
        y=c(newdat$seLow[101:200], rev(newdat$seHigh[101:200])),
        col=adjustcolor("orangered",alpha.f=0.5), border=NA)
lines(newdat$SRL[101:200], newdat$ProbOccurrence[101:200],col="orangered3")
axis(3, at=xscale.SRL.grasslands$sd, labels=xscale.SRL.grasslands$SRL, line=0)
title("(C) Grasslands",line=2.5)
legend("topright", legend=c("Low temperature (1st %)","High temperature (99th %)"), lty=1,
       col=c("blue","orangered"), lwd=3, bty="n",cex=1.2)
text(1,0.07,"Cold",col="blue", cex=1.4)
text(-0.3,0.2,"UNIDIRECTIONAL", col=1, cex=1.4)

rm(f3,g3,w3)

load("f4.RData")
load("g4.RData")
load("w4.RData")

### make xscale data frames
f4@frame$r2pet.unscaled <- unscale(f4@frame$r2pet)
f4@frame$SRL.unscaled <- unscale(f4@frame$SRL)
lm1 <- lm(r2pet.unscaled ~ as.vector(r2pet), data=f4@frame)
lm2 <- lm(SRL.unscaled ~ as.vector(SRL), data=f4@frame)
newdat <- data.frame(r2pet=seq(-5,5,1), SRL=seq(-5,5,1))
xscale.SRL.forests <- data.frame(sd=seq(-5,5,1), r2pet=round(exp(predict(lm1,newdat)),2), SRL=round(exp(predict(lm2,newdat)),0))

g4@frame$r2pet.unscaled <- unscale(g4@frame$r2pet)
g4@frame$SRL.unscaled <- unscale(g4@frame$SRL)
lm1 <- lm(r2pet.unscaled ~ as.vector(r2pet), data=g4@frame)
lm2 <- lm(SRL.unscaled ~ as.vector(SRL), data=g4@frame)
newdat <- data.frame(r2pet=seq(-5,5,1), SRL=seq(-5,5,1))
xscale.SRL.grasslands <- data.frame(sd=seq(-5,5,1), r2pet=round(exp(predict(lm1,newdat)),2), SRL=round(exp(predict(lm2,newdat)),0))

# TxE Interactions
r2pet.plot <- seq(quantile(f4@frame$r2pet,0.01), quantile(f4@frame$r2pet,0.99), length.out=100)
plot(r2pet.plot, fixef(f4)[4] + fixef(f4)[5]*r2pet.plot, type="l", col="black",ylim=c(-2,2),
     ylab="SRL effect on Probability of Occurrence", xlab="", lwd=4)
title(sub=list("Precipitation:Potential Evapotranspiration ratio", cex=1.2),line=2.5)
title(sub="\n(scaled to unit variance below, log scale above)",line=3.5)
lines(r2pet.plot, fixef(g4)[4] + fixef(g4)[5]*r2pet.plot, col="burlywood3", lwd=4)
lines(r2pet.plot, fixef(w4)[4] + fixef(w4)[5]*r2pet.plot, col="lightblue", lwd=4)
abline(h=0,lty=3,col=1)
axis(3, at=xscale.SRL.forests$sd, labels=xscale.SRL.forests$r2pet, line=0)
title("(D) SRL-by-P:PET interactions",line=2.5)
legend("topleft", legend=c("Forests (P < 0.0001)","Grasslands (P < 0.0001)","Wetlands (P = 0.8850)"), lwd=4,
       col=c("black","burlywood3","lightblue"), bty="n", cex=1.2)
colfunc <- colorRampPalette(c("gold", "darkgreen"))
points(x = r2pet.plot, y = rep(-2.1, length(r2pet.plot)),pch = 15,cex = 5, col = colfunc(length(r2pet.plot)))

### Plot TxE interactions with confidence intervals
newdat <- data.frame(
  SRL = rep(seq(quantile(f4@frame$SRL,0.01),quantile(f4@frame$SRL,0.99),length.out = 100), 2),
  r2pet = c(rep(quantile(f4@frame$r2pet, 0.01), 100), rep(quantile(f4@frame$r2pet, 0.99), 100)),
  r2pet2 =c(rep(mean(f4@frame$r2pet2), 200)),
  lab = factor(rep(paste0(c("low.", "high."), "r2pet"), each = 100))) %>%
  mutate(`r2pet:SRL` = r2pet * SRL)
mm <- model.matrix( ~ r2pet + r2pet2 + SRL + r2pet:SRL, newdat)
newdat$eta <- mm %*% fixef(f4)
newdat$ProbOccurrence <- exp(newdat$eta) / (1 + exp(newdat$eta))
newdat$VarPred <- diag(mm %*% vcov(f4) %*% t(mm))
newdat$sePred <- sqrt(newdat$VarPred)
newdat$seLow <-
  exp(newdat$eta - 1.96 * newdat$sePred) / (1 + exp(newdat$eta - 1.96 * newdat$sePred))
newdat$seHigh <-
  exp(newdat$eta + 1.96 * newdat$sePred) / (1 + exp(newdat$eta + 1.96 * newdat$sePred))

plot(newdat$SRL[1:100], newdat$ProbOccurrence[1:100], type="l", col="gold",
     ylim=c(0,0.5), xlab="", ylab="Probability of Occurrence", cex.lab=1.2)
title(sub=list(expression(paste("Specific root length (m g",{}^-1,")")), cex=1.2),line=2.5)
title(sub="\n(scaled to unit variance below, log scale above)",line=3.5)
polygon(x=c(seq(min(newdat$SRL),max(newdat$SRL),length.out=100),
            rev(seq(min(newdat$SRL),max(newdat$SRL),length.out=100))),
        y=c(newdat$seLow[1:100], rev(newdat$seHigh[1:100])),
        col=adjustcolor("gold",alpha.f=0.5), border=NA)
lines(newdat$SRL[1:100], newdat$ProbOccurrence[1:100], col="gold")
polygon(x=c(seq(min(newdat$SRL),max(newdat$SRL),length.out=100),
            rev(seq(min(newdat$SRL),max(newdat$SRL),length.out=100))),
        y=c(newdat$seLow[101:200], rev(newdat$seHigh[101:200])),
        col=adjustcolor("darkgreen",alpha.f=0.5), border=NA)
lines(newdat$SRL[101:200], newdat$ProbOccurrence[101:200],col="darkgreen")
axis(3, at=xscale.SRL.forests$sd, labels=xscale.SRL.forests$SRL, line=0)
title("(E) Forests",line=2.5)
legend("topright", legend=c("Low P:PET (1st %)","High P:PET (99th %)"), lty=1,
       col=c("gold","darkgreen"), lwd=3, bty="n", cex=1.2)
text(-1.6,0.2,"Dry",col="gold3", cex=1.4)
text(-0.3,0.35,"UNIDIRECTIONAL", col=1, cex=1.4)

### Plot TxE interactions with confidence intervals
newdat <- data.frame(
  SRL = rep(seq(quantile(g4@frame$SRL,0.01),quantile(g4@frame$SRL,0.99),length.out = 100), 2),
  r2pet = c(rep(quantile(g4@frame$r2pet, 0.0125), 100), rep(quantile(g4@frame$r2pet, 0.99), 100)),
  r2pet2 =c(rep(mean(g4@frame$r2pet2), 200)),
  lab = factor(rep(paste0(c("low.", "high."), "r2pet"), each = 100))) %>%
  mutate(`r2pet:SRL` = r2pet * SRL)
mm <- model.matrix( ~ r2pet + r2pet2 + SRL + r2pet:SRL, newdat)
newdat$eta <- mm %*% fixef(g4)
newdat$ProbOccurrence <- exp(newdat$eta) / (1 + exp(newdat$eta))
newdat$VarPred <- diag(mm %*% vcov(g4) %*% t(mm))
newdat$sePred <- sqrt(newdat$VarPred)
newdat$seLow <-
  exp(newdat$eta - 1.96 * newdat$sePred) / (1 + exp(newdat$eta - 1.96 * newdat$sePred))
newdat$seHigh <-
  exp(newdat$eta + 1.96 * newdat$sePred) / (1 + exp(newdat$eta + 1.96 * newdat$sePred))

plot(newdat$SRL[1:100], newdat$ProbOccurrence[1:100], type="l", col="gold",
     ylim=c(0,0.5), xlab="", ylab="Probability of Occurrence", cex.lab=1.2)
title(sub=list(expression(paste("Specific root length (m g",{}^-1,")")), cex=1.2),line=2.5)
title(sub="\n(scaled to unit variance below, log scale above)",line=3.5)
polygon(x=c(seq(min(newdat$SRL),max(newdat$SRL),length.out=100),
            rev(seq(min(newdat$SRL),max(newdat$SRL),length.out=100))),
        y=c(newdat$seLow[1:100], rev(newdat$seHigh[1:100])),
        col=adjustcolor("gold",alpha.f=0.5), border=NA)
lines(newdat$SRL[1:100], newdat$ProbOccurrence[1:100], col="gold")
polygon(x=c(seq(min(newdat$SRL),max(newdat$SRL),length.out=100),
            rev(seq(min(newdat$SRL),max(newdat$SRL),length.out=100))),
        y=c(newdat$seLow[101:200], rev(newdat$seHigh[101:200])),
        col=adjustcolor("darkgreen",alpha.f=0.5), border=NA)
lines(newdat$SRL[101:200], newdat$ProbOccurrence[101:200],col="darkgreen")
axis(3, at=xscale.SRL.grasslands$sd, labels=xscale.SRL.grasslands$SRL, line=0)
title("(F) Grasslands",line=2.5)
legend("topright", legend=c("Low P:PET (1st %)","High P:PET (99th %)"), lty=1,
       col=c("gold","darkgreen"), lwd=3, bty="n", cex=1.2)
text(-1.5,0.22,"Dry",col="gold3", cex=1.4)
text(-0.3,0.35,"UNIDIRECTIONAL", col=1, cex=1.4)

rm(f4,g4,w4)

setwd("/gscratch/dlaughl4/RTD_v1/")
load("f1.RData")
load("g1.RData")
load("w1.RData")
 
### make xscale data frames
f1@frame$tmin.unscaled <- unscale(f1@frame$tmin)
f1@frame$RTD.unscaled <- unscale(f1@frame$RTD)
lm1 <- lm(tmin.unscaled ~ as.vector(tmin), data=f1@frame)
lm2 <- lm(RTD.unscaled ~ as.vector(RTD), data=f1@frame)
newdat <- data.frame(tmin=seq(-5,5,1), RTD=seq(-5,5,1))
xscale.rtd.forests <- data.frame(sd=seq(-5,5,1), tmin=round(predict(lm1,newdat),0), RTD=round(exp(predict(lm2,newdat)),2))

g1@frame$tmin.unscaled <- unscale(g1@frame$tmin)
g1@frame$RTD.unscaled <- unscale(g1@frame$RTD)
lm1 <- lm(tmin.unscaled ~ as.vector(tmin), data=g1@frame)
lm2 <- lm(RTD.unscaled ~ as.vector(RTD), data=g1@frame)
newdat <- data.frame(tmin=seq(-5,5,1), RTD=seq(-5,5,1))
xscale.rtd.grasslands <- data.frame(sd=seq(-5,5,1), tmin=round(predict(lm1,newdat),0), RTD=round(exp(predict(lm2,newdat)),2))

#pdf("/Volumes/gscratch/dlaughl4/RTD_v1/RTD.pdf", width=11.5,height=7.5)
#par(mfrow=c(2,3), cex.lab=1.2)
# TxE Interactions
tmin.plot <- seq(quantile(f1@frame$tmin,0.01), quantile(f1@frame$tmin,0.99), length.out=100)
plot(tmin.plot, fixef(f1)[4] + fixef(f1)[5]*tmin.plot, type="l", col="black",ylim=c(-1.5,1.5),
     ylab="RTD effect on Probability of Occurrence", xlab="", lwd=4)
title(sub=list(expression(paste("Minimum temperature (",degree,"C)")), cex=1.2), line=2.5)
title(sub="\n(scaled to unit variance below, native scale above)",line=3.5)
lines(tmin.plot, fixef(g1)[4] + fixef(g1)[5]*tmin.plot, col="burlywood3", lwd=4)
lines(tmin.plot, fixef(w1)[4] + fixef(w1)[5]*tmin.plot, col="lightblue", lwd=4)
abline(h=0,lty=3,col=1)
axis(3, at=xscale.rtd.forests$sd, labels=xscale.rtd.forests$tmin, line=0)
title("(G) RTD-by-temperature interactions",line=2.5)
legend("topleft", legend=c("Forests (P < 0.0001)","Grasslands (P < 0.0001)","Wetlands (P = 0.5587)"), lwd=4,
       col=c("black","burlywood3","lightblue"), bty="n", cex=1.2)
colfunc <- colorRampPalette(c("blue", "orangered"))
points(x = tmin.plot, y = rep(-1.6, length(tmin.plot)),pch = 15,cex = 5, col = colfunc(length(tmin.plot)))

### Plot TxE interactions with confidence intervals
newdat <- data.frame(
  RTD = rep(seq(quantile(f1@frame$RTD,0.01),quantile(f1@frame$RTD,0.99),length.out = 100), 2),
  tmin = c(rep(quantile(f1@frame$tmin, 0.01), 100), rep(quantile(f1@frame$tmin, 0.99), 100)),
  tmin2 =c(rep(mean(f1@frame$tmin2), 200)),
  lab = factor(rep(paste0(c("low.", "high."), "tmin"), each = 100))) %>%
  mutate(`tmin:RTD` = tmin * RTD)
mm <- model.matrix( ~ tmin + tmin2 + RTD + tmin:RTD, newdat)
newdat$eta <- mm %*% fixef(f1)
newdat$ProbOccurrence <- exp(newdat$eta) / (1 + exp(newdat$eta))
newdat$VarPred <- diag(mm %*% vcov(f1) %*% t(mm))
newdat$sePred <- sqrt(newdat$VarPred)
newdat$seLow <-
  exp(newdat$eta - 1.96 * newdat$sePred) / (1 + exp(newdat$eta - 1.96 * newdat$sePred))
newdat$seHigh <-
  exp(newdat$eta + 1.96 * newdat$sePred) / (1 + exp(newdat$eta + 1.96 * newdat$sePred))

plot(newdat$RTD[1:100], newdat$ProbOccurrence[1:100], type="l", col="blue",
     ylim=c(0,0.3), xlab="", ylab="Probability of Occurrence", cex.lab=1.2)
title(sub=list(expression(paste("Root tissue density (mg mm",{}^-3,")")), cex=1.2),line=2.5)
title(sub="\n(scaled to unit variance below, log scale above)",line=3.5)
polygon(x=c(seq(min(newdat$RTD),max(newdat$RTD),length.out=100),
            rev(seq(min(newdat$RTD),max(newdat$RTD),length.out=100))),
        y=c(newdat$seLow[1:100], rev(newdat$seHigh[1:100])),
        col=adjustcolor("blue",alpha.f=0.5), border=NA)
lines(newdat$RTD[1:100], newdat$ProbOccurrence[1:100], col="darkblue")
polygon(x=c(seq(min(newdat$RTD),max(newdat$RTD),length.out=100),
            rev(seq(min(newdat$RTD),max(newdat$RTD),length.out=100))),
        y=c(newdat$seLow[101:200], rev(newdat$seHigh[101:200])),
        col=adjustcolor("orangered",alpha.f=0.5), border=NA)
lines(newdat$RTD[101:200], newdat$ProbOccurrence[101:200],col="orangered3")
axis(3, at=xscale.rtd.forests$sd, labels=xscale.rtd.forests$RTD, line=0)
title("(H) Forests",line=2.5)
legend("topright", legend=c("Low temperature (1st %)","High temperature (99th %)"), lty=1,
       col=c("blue","orangered"), lwd=3, bty="n", cex=1.2)
text(-1.3,0.15,"Cold",col="blue", cex=1.4)
text(1,0.15,"Warm",col="orangered3", cex=1.4)
text(-0.5,0.2,"BIDIRECTIONAL", col=1, cex=1.4)

### Plot TxE interactions with confidence intervals
newdat <- data.frame(
  RTD = rep(seq(quantile(g1@frame$RTD,0.01),quantile(g1@frame$RTD,0.99),length.out = 100), 2),
  tmin = c(rep(quantile(g1@frame$tmin, 0.01), 100), rep(quantile(g1@frame$tmin, 0.99), 100)),
  tmin2 =c(rep(mean(g1@frame$tmin2), 200)),
  lab = factor(rep(paste0(c("low.", "high."), "tmin"), each = 100))) %>%
  mutate(`tmin:RTD` = tmin * RTD)
mm <- model.matrix( ~ tmin + tmin2 + RTD + tmin:RTD, newdat)
newdat$eta <- mm %*% fixef(g1)
newdat$ProbOccurrence <- exp(newdat$eta) / (1 + exp(newdat$eta))
newdat$VarPred <- diag(mm %*% vcov(g1) %*% t(mm))
newdat$sePred <- sqrt(newdat$VarPred)
newdat$seLow <-
  exp(newdat$eta - 1.96 * newdat$sePred) / (1 + exp(newdat$eta - 1.96 * newdat$sePred))
newdat$seHigh <-
  exp(newdat$eta + 1.96 * newdat$sePred) / (1 + exp(newdat$eta + 1.96 * newdat$sePred))

plot(newdat$RTD[1:100], newdat$ProbOccurrence[1:100], type="l", col="blue",
     ylim=c(0,0.3), xlab="", ylab="Probability of Occurrence", cex.lab=1.2)
title(sub=list(expression(paste("Root tissue density (mg mm",{}^-3,")")), cex=1.2),line=2.5)
title(sub="\n(scaled to unit variance below, log scale above)",line=3.5)
polygon(x=c(seq(min(newdat$RTD),max(newdat$RTD),length.out=100),
            rev(seq(min(newdat$RTD),max(newdat$RTD),length.out=100))),
        y=c(newdat$seLow[1:100], rev(newdat$seHigh[1:100])),
        col=adjustcolor("blue",alpha.f=0.5), border=NA)
lines(newdat$RTD[1:100], newdat$ProbOccurrence[1:100], col="darkblue")
polygon(x=c(seq(min(newdat$RTD),max(newdat$RTD),length.out=100),
            rev(seq(min(newdat$RTD),max(newdat$RTD),length.out=100))),
        y=c(newdat$seLow[101:200], rev(newdat$seHigh[101:200])),
        col=adjustcolor("orangered",alpha.f=0.5), border=NA)
lines(newdat$RTD[101:200], newdat$ProbOccurrence[101:200],col="orangered3")
axis(3, at=xscale.rtd.grasslands$sd, labels=xscale.rtd.grasslands$RTD, line=0)
title("(I) Grasslands",line=2.5)
legend("topright", legend=c("Low temperature (1st %)","High temperature (99th %)"), lty=1,
       col=c("blue","orangered"), lwd=3, bty="n",cex=1.2)
text(-2,0.05,"Cold",col="blue", cex=1.4)
text(-0.5,0.2,"UNIDIRECTIONAL", col=1, cex=1.4)

rm(f1,g1,w1)

load("f2.RData")
load("g2.RData")
load("w2.RData")

### make xscale data frames
f2@frame$r2pet.unscaled <- unscale(f2@frame$r2pet)
f2@frame$RTD.unscaled <- unscale(f2@frame$RTD)
lm1 <- lm(r2pet.unscaled ~ as.vector(r2pet), data=f2@frame)
lm2 <- lm(RTD.unscaled ~ as.vector(RTD), data=f2@frame)
newdat <- data.frame(r2pet=seq(-5,5,1), RTD=seq(-5,5,1))
xscale.rtd.forests <- data.frame(sd=seq(-5,5,1), r2pet=round(exp(predict(lm1,newdat)),2), RTD=round(exp(predict(lm2,newdat)),2))

g2@frame$r2pet.unscaled <- unscale(g2@frame$r2pet)
g2@frame$RTD.unscaled <- unscale(g2@frame$RTD)
lm1 <- lm(r2pet.unscaled ~ as.vector(r2pet), data=g2@frame)
lm2 <- lm(RTD.unscaled ~ as.vector(RTD), data=g2@frame)
newdat <- data.frame(r2pet=seq(-5,5,1), RTD=seq(-5,5,1))
xscale.rtd.grasslands <- data.frame(sd=seq(-5,5,1), r2pet=round(exp(predict(lm1,newdat)),2), RTD=round(exp(predict(lm2,newdat)),2))

# TxE Interactions
r2pet.plot <- seq(quantile(f2@frame$r2pet,0.01), quantile(f2@frame$r2pet,0.99), length.out=100)
plot(r2pet.plot, fixef(f2)[4] + fixef(f2)[5]*r2pet.plot, type="l", col="black",ylim=c(-1.5,1.5),
     ylab="RTD effect on Probability of Occurrence", xlab="", lwd=4)
title(sub=list("Precipitation:Potential Evapotranspiration ratio", cex=1.2),line=2.5)
title(sub="\n(scaled to unit variance below, log scale above)",line=3.5)
lines(r2pet.plot, fixef(g2)[4] + fixef(g2)[5]*r2pet.plot, col="burlywood3", lwd=4)
lines(r2pet.plot, fixef(w2)[4] + fixef(w2)[5]*r2pet.plot, col="lightblue", lwd=4)
abline(h=0,lty=3,col=1)
axis(3, at=xscale.rtd.forests$sd, labels=xscale.rtd.forests$r2pet, line=0)
title("(J) RTD-by-P:PET interactions",line=2.5)
legend("topleft", legend=c("Forests (P < 0.0001)","Grasslands (P < 0.0001)","Wetlands (P = 0.8406)"), lwd=4,
       col=c("black","burlywood3","lightblue"), bty="n", cex=1.2)
colfunc <- colorRampPalette(c("gold", "darkgreen"))
points(x = r2pet.plot, y = rep(-1.6, length(r2pet.plot)),pch = 15,cex = 5, col = colfunc(length(r2pet.plot)))

### Plot TxE interactions with confidence intervals
newdat <- data.frame(
  RTD = rep(seq(quantile(f2@frame$RTD,0.01),quantile(f2@frame$RTD,0.99),length.out = 100), 2),
  r2pet = c(rep(quantile(f2@frame$r2pet, 0.01), 100), rep(quantile(f2@frame$r2pet, 0.99), 100)),
  r2pet2 =c(rep(mean(f2@frame$r2pet2), 200)),
  lab = factor(rep(paste0(c("low.", "high."), "r2pet"), each = 100))) %>%
  mutate(`r2pet:RTD` = r2pet * RTD)
mm <- model.matrix( ~ r2pet + r2pet2 + RTD + r2pet:RTD, newdat)
newdat$eta <- mm %*% fixef(f2)
newdat$ProbOccurrence <- exp(newdat$eta) / (1 + exp(newdat$eta))
newdat$VarPred <- diag(mm %*% vcov(f2) %*% t(mm))
newdat$sePred <- sqrt(newdat$VarPred)
newdat$seLow <-
  exp(newdat$eta - 1.96 * newdat$sePred) / (1 + exp(newdat$eta - 1.96 * newdat$sePred))
newdat$seHigh <-
  exp(newdat$eta + 1.96 * newdat$sePred) / (1 + exp(newdat$eta + 1.96 * newdat$sePred))

plot(newdat$RTD[1:100], newdat$ProbOccurrence[1:100], type="l", col="gold",
     ylim=c(0,0.3), xlab="", ylab="Probability of Occurrence", cex.lab=1.2)
title(sub=list(expression(paste("Root tissue density (mg mm",{}^-3,")")), cex=1.2),line=2.5)
title(sub="\n(scaled to unit variance below, log scale above)",line=3.5)
polygon(x=c(seq(min(newdat$RTD),max(newdat$RTD),length.out=100),
            rev(seq(min(newdat$RTD),max(newdat$RTD),length.out=100))),
        y=c(newdat$seLow[1:100], rev(newdat$seHigh[1:100])),
        col=adjustcolor("gold",alpha.f=0.5), border=NA)
lines(newdat$RTD[1:100], newdat$ProbOccurrence[1:100], col="gold")
polygon(x=c(seq(min(newdat$RTD),max(newdat$RTD),length.out=100),
            rev(seq(min(newdat$RTD),max(newdat$RTD),length.out=100))),
        y=c(newdat$seLow[101:200], rev(newdat$seHigh[101:200])),
        col=adjustcolor("darkgreen",alpha.f=0.5), border=NA)
lines(newdat$RTD[101:200], newdat$ProbOccurrence[101:200],col="darkgreen")
axis(3, at=xscale.rtd.forests$sd, labels=xscale.rtd.forests$RTD, line=0)
title("(K) Forests",line=2.5)
legend("topright", legend=c("Low P:PET (1st %)","High P:PET (99th %)"), lty=1,
       col=c("gold","darkgreen"), lwd=3, bty="n", cex=1.2)
text(0.5,0.15,"Dry",col="gold3", cex=1.4)
text(-0.5,0.2,"UNIDIRECTIONAL", col=1, cex=1.4)

### Plot TxE interactions with confidence intervals
newdat <- data.frame(
  RTD = rep(seq(quantile(g2@frame$RTD,0.01),quantile(g2@frame$RTD,0.99),length.out = 100), 2),
  r2pet = c(rep(quantile(g2@frame$r2pet, 0.01), 100), rep(quantile(g2@frame$r2pet, 0.99), 100)),
  r2pet2 =c(rep(mean(g2@frame$r2pet2), 200)),
  lab = factor(rep(paste0(c("low.", "high."), "r2pet"), each = 100))) %>%
  mutate(`r2pet:RTD` = r2pet * RTD)
mm <- model.matrix( ~ r2pet + r2pet2 + RTD + r2pet:RTD, newdat)
newdat$eta <- mm %*% fixef(g2)
newdat$ProbOccurrence <- exp(newdat$eta) / (1 + exp(newdat$eta))
newdat$VarPred <- diag(mm %*% vcov(g2) %*% t(mm))
newdat$sePred <- sqrt(newdat$VarPred)
newdat$seLow <-
  exp(newdat$eta - 1.96 * newdat$sePred) / (1 + exp(newdat$eta - 1.96 * newdat$sePred))
newdat$seHigh <-
  exp(newdat$eta + 1.96 * newdat$sePred) / (1 + exp(newdat$eta + 1.96 * newdat$sePred))

plot(newdat$RTD[1:100], newdat$ProbOccurrence[1:100], type="l", col="gold",
     ylim=c(0,0.3), xlab="", ylab="Probability of Occurrence", cex.lab=1.2)
title(sub=list(expression(paste("Root tissue density (mg mm",{}^-3,")")), cex=1.2),line=2.5)
title(sub="\n(scaled to unit variance below, log scale above)",line=3.5)
polygon(x=c(seq(min(newdat$RTD),max(newdat$RTD),length.out=100),
            rev(seq(min(newdat$RTD),max(newdat$RTD),length.out=100))),
        y=c(newdat$seLow[1:100], rev(newdat$seHigh[1:100])),
        col=adjustcolor("gold",alpha.f=0.5), border=NA)
lines(newdat$RTD[1:100], newdat$ProbOccurrence[1:100], col="gold")
polygon(x=c(seq(min(newdat$RTD),max(newdat$RTD),length.out=100),
            rev(seq(min(newdat$RTD),max(newdat$RTD),length.out=100))),
        y=c(newdat$seLow[101:200], rev(newdat$seHigh[101:200])),
        col=adjustcolor("darkgreen",alpha.f=0.5), border=NA)
lines(newdat$RTD[101:200], newdat$ProbOccurrence[101:200],col="darkgreen")
axis(3, at=xscale.rtd.grasslands$sd, labels=xscale.rtd.grasslands$RTD, line=0)
title("(L) Grasslands",line=2.5)
legend("topright", legend=c("Low P:PET (1st %)","High P:PET (99th %)"), lty=1,
       col=c("gold","darkgreen"), lwd=3, bty="n", cex=1.4)
text(-0.5,0.2,"NO TRADE-OFF", col=1, cex=1.4)

#rm(f2,g2,w2)
dev.off()

######## End of figure 2 ##########################################









######## Fig 3 Species curves ################################
setwd("/gscratch/dlaughl4/SRL_v1/")
load("f3.RData")
load("ef3b.RData")
load("f4.RData")
load("ef4b.RData")

f3@frame$tmin.unscaled <- unscale(f3@frame$tmin)
f3@frame$SRL.unscaled <- unscale(f3@frame$SRL)
lm1 <- lm(tmin.unscaled ~ as.vector(tmin), data=f3@frame)
lm2 <- lm(SRL.unscaled ~ as.vector(SRL), data=f3@frame)
newdat <- data.frame(tmin=seq(-5,5,1), SRL=seq(-5,5,1))
xscale.srl.forests <- data.frame(sd=seq(-5,5,1), tmin=round(predict(lm1,newdat),0), SRL=round(exp(predict(lm2,newdat))))

species.pres <- f3@frame[f3@frame$pres==1,]
species <- species.pres %>% group_by(species) %>% summarise_at(vars(pres,tmin,SRL),c(min,max,length,mean))
colnames(species) <- c("species","pres.min","tmin.min","srl.min","pres.max","tmin.max","srl.max","length","length2","length3","pres.mean","tmin.mean","srl.mean")
species$n <- log(species$length)/log(max(species$length)) ### scaled by log
species <- species[order(species$srl.mean), ]
species$cols <- viridis::viridis_pal(option="E")(nrow(species))
col.leg <- c(species$cols[1],species$cols[round((nrow(species)/2),0)],species$cols[nrow(species)])
species <- species[order(species$species), ]
coefs <- coef(ef3b)
tmin <- seq(quantile(ef3b@frame$tmin,0.01),quantile(ef3b@frame$tmin,0.99), length.out=100)
prob <- seq(0,1, length.out=100)
tmin2 <- scale((tmin-mean(tmin))^2)

pdf("/gscratch/dlaughl4/fig3.pdf", width=10,height=16)
par(mfrow=c(4,2),mar=c(5,5,5,2))
plot(tmin,prob, col="white", xlab="",ylab="Probability of occurrence (sqrt)", ylim=c(0,1.05), cex.lab=1.3)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
title(sub=list(expression(paste("Minimum temperature (",degree,"C)")), cex=1.2), line=2.5)
title(sub="\n(scaled to unit variance below, native scale above)",line=3.5)
title("(A) Species distributions = f(Temperature)",line=2.5)
axis(3, at=xscale.srl.forests$sd, labels=xscale.srl.forests$tmin, line=0)
for(i in 1:nrow(species)){
  lines(tmin, sqrt(1/(1+exp(-(coefs$species[i,1] + coefs$species[i,2]*tmin + coefs$species[i,3]*tmin2)))),
        col=species$cols[i], lwd=0.5+species$n[i])
}
legend("top",c("Low SRL", "Moderate SRL", "High SRL"),
       lwd=2, col=col.leg, bty="n", cex=0.85, horiz=TRUE)

#spp optimums
sp.opt <- matrix(0, ncol=nrow(species), nrow=100)
for(i in 1:ncol(sp.opt)) {
  sp.opt[,i] <- 1/(1+exp(-(coefs$species[i,1] + coefs$species[i,2]*tmin + coefs$species[i,3]*tmin2)))
}
sp.opt <- data.frame(sp.opt)
for(i in 1:ncol(sp.opt)) {
  sp.opt[101,i] <- tmin[which.max(sp.opt[1:100,i])]
  sp.opt[102,i] <- max(sp.opt[1:100,i])
}
species$opt <- t(sp.opt[101,1:nrow(species)])
species$maxprob <- t(sp.opt[102,1:nrow(species)])
plot(species$opt, species$srl.mean, col=species$cols, ylim=c(-3,2), xlab="", ylab="")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
points(species$opt, species$srl.mean, cex=2*species$n, pch=19, col=species$cols)
title(sub=list(expression(paste("Optimum minimum temperature (",degree,"C)")), cex=1.2), line=2.5)
title(sub="\n(scaled to unit variance below, native scale above)",line=3.5)
title("(B) Species optimum temperature and SRL",line=2.5)
title(ylab=list(expression(paste("Specific root length (m g",{}^-1,")")),cex=1.3),line=3.5)
title(ylab="\n(scaled to unit variance, log scale at right)",line=2.5)
axis(3, at=xscale.srl.forests$sd, labels=xscale.srl.forests$tmin, line=0)
axis(4, at=xscale.srl.forests$sd, labels=xscale.srl.forests$SRL, line=0)
summary(m1 <- lm(srl.mean ~ as.numeric(opt), data=species, weight=n))
newdat <- data.frame(opt=c(tmin))
conf_interval <- predict(m1, newdata=newdat, interval="prediction", level = 0.95)
matlines(tmin, conf_interval, lty=c(1,2,2), col=1, lwd=c(3,2,2))
legend("bottomright", legend="r = -0.17, P < 0.0001", cex=1.2, bty="n")

### SRL-PPET
f4@frame$r2pet.unscaled <- unscale(f4@frame$r2pet)
f4@frame$SRL.unscaled <- unscale(f4@frame$SRL)
lm1 <- lm(r2pet.unscaled ~ as.vector(r2pet), data=f4@frame)
lm2 <- lm(SRL.unscaled ~ as.vector(SRL), data=f4@frame)
newdat <- data.frame(r2pet=seq(-5,5,1), SRL=seq(-5,5,1))
xscale.srl.forests <- data.frame(sd=seq(-5,5,1), r2pet=round(exp(predict(lm1,newdat)),2), SRL=round(exp(predict(lm2,newdat))))

species.pres <- f4@frame[f4@frame$pres==1,]
species <- species.pres %>% group_by(species) %>% summarise_at(vars(pres,r2pet,SRL),c(min,max,length,mean))
colnames(species) <- c("species","pres.min","tmin.min","srl.min","pres.max","tmin.max","srl.max","length","length2","length3","pres.mean","tmin.mean","srl.mean")
species$n <- log(species$length)/log(max(species$length))
species <- species[order(species$srl.mean), ]
species$cols <- viridis::viridis_pal(option="E")(nrow(species))
col.leg <- c(species$cols[1],species$cols[round((nrow(species)/2),0)],species$cols[nrow(species)])
species <- species[order(species$species), ]
coefs <- coef(ef4b)
r2pet <- seq(quantile(ef4b@frame$r2pet,0.01),quantile(ef4b@frame$r2pet,0.99), length.out=100)
prob <- seq(0,1, length.out=100)
r2pet2 <- scale((r2pet-mean(r2pet))^2)

plot(r2pet,prob, col="white", xlab="",ylab="Probability of occurrence (sqrt)", ylim=c(0,1.05),cex.lab=1.3)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
title(sub=list("Precipitation:Potential Evapotranspiration ratio", cex=1.2),line=2.5)
title(sub="\n(scaled to unit variance below, log scale above)",line=3.5)
title("(C) Species distributions = f(P:PET)",line=2.5)
axis(3, at=xscale.srl.forests$sd, labels=xscale.srl.forests$r2pet, line=0)
#title("Species response curves",line=3)
for(i in 1:nrow(species)){
  lines(r2pet, sqrt(1/(1+exp(-(coefs$species[i,1] + coefs$species[i,2]*r2pet + coefs$species[i,3]*r2pet2)))),
        col=species$cols[i], lwd=0.5+species$n[i])
}
legend("top",c("Low SRL", "Moderate SRL", "High SRL"),
       lwd=2, col=col.leg, bty="n", cex=0.85, horiz=TRUE)

#spp optimums
sp.opt <- matrix(0, ncol=nrow(species), nrow=100)
for(i in 1:ncol(sp.opt)) {
  sp.opt[,i] <- 1/(1+exp(-(coefs$species[i,1] + coefs$species[i,2]*r2pet + coefs$species[i,3]*r2pet2)))
}
sp.opt <- data.frame(sp.opt)
for(i in 1:ncol(sp.opt)) {
  sp.opt[101,i] <- r2pet[which.max(sp.opt[1:100,i])]
  sp.opt[102,i] <- max(sp.opt[1:100,i])
}
species$opt <- t(sp.opt[101,1:nrow(species)])
species$maxprob <- t(sp.opt[102,1:nrow(species)])
plot(species$opt, species$srl.mean, col=species$cols, ylim=c(-3,2), xlab="", ylab="")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
points(species$opt, species$srl.mean, cex=2*species$n, pch=19, col=species$cols)
title(sub=list("Optimum P:PET ratio", cex=1.2), line=2.5)
title(sub="\n(scaled to unit variance below, log scale above)",line=3.5)
title("(D) Species optimum P:PET ratio and SRL",line=2.5)
title(ylab=list(expression(paste("Specific root length (m g",{}^-1,")")),cex=1.3),line=3.5)
title(ylab="\n(scaled to unit variance, log scale at right)",line=2.5)
axis(3, at=xscale.srl.forests$sd, labels=xscale.srl.forests$r2pet, line=0)
axis(4, at=xscale.srl.forests$sd, labels=xscale.srl.forests$SRL, line=0)
summary(m1 <- lm(srl.mean ~ as.numeric(opt), data=species, weight=n))
newdat <- data.frame(opt=c(r2pet))
conf_interval <- predict(m1, newdata=newdat, interval="prediction", level = 0.95)
matlines(r2pet, conf_interval, lty=c(2,2,2), col=c(1,0,0), lwd=c(1,0,0))
legend("bottomright", legend="r = 0.05, P = 0.11", cex=1.2, bty="n")

rm(f3,ef3b,f4,ef4b)

######## Species response curves ################################
setwd("/gscratch/dlaughl4/RTD_v1/")
load("f1.RData")
load("ef1b.RData")
load("f2.RData")
load("ef2b.RData")

f1@frame$tmin.unscaled <- unscale(f1@frame$tmin)
f1@frame$RTD.unscaled <- unscale(f1@frame$RTD)
lm1 <- lm(tmin.unscaled ~ as.vector(tmin), data=f1@frame)
lm2 <- lm(RTD.unscaled ~ as.vector(RTD), data=f1@frame)
newdat <- data.frame(tmin=seq(-5,5,1), RTD=seq(-5,5,1))
xscale.rtd.forests <- data.frame(sd=seq(-5,5,1), tmin=round(predict(lm1,newdat),0), RTD=round(exp(predict(lm2,newdat)),2))

species.pres <- f1@frame[f1@frame$pres==1,]
species <- species.pres %>% group_by(species) %>% summarise_at(vars(pres,tmin,RTD),c(min,max,length,mean))
colnames(species) <- c("species","pres.min","tmin.min","rtd.min","pres.max","tmin.max","rtd.max","length","length2","length3","pres.mean","tmin.mean","rtd.mean")
species$n <- log(species$length)/log(max(species$length))
species <- species[order(species$rtd.mean), ]
species$cols <- viridis::viridis_pal(option="D")(nrow(species))
col.leg <- c(species$cols[1],species$cols[round((nrow(species)/2),0)],species$cols[nrow(species)])
species <- species[order(species$species), ]
coefs <- coef(ef1b)
tmin <- seq(quantile(ef1b@frame$tmin,0.01),quantile(ef1b@frame$tmin,0.99), length.out=100)
prob <- seq(0,1, length.out=100)
tmin2 <- scale((tmin-mean(tmin))^2)

#par(mfrow=c(2,2),mar=c(5,5,5,2))
plot(tmin,prob, col="white", xlab="",ylab="Probability of occurrence (sqrt)", ylim=c(0,1.05), cex.lab=1.3)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
title(sub=list(expression(paste("Minimum temperature (",degree,"C)")), cex=1.2), line=2.5)
title(sub="\n(scaled to unit variance below, native scale above)",line=3.5)
title("(E) Species distributions = f(Temperature)",line=2.5)
axis(3, at=xscale.rtd.forests$sd, labels=xscale.rtd.forests$tmin, line=0)
#title("Species response curves",line=3)
for(i in 1:nrow(species)){
  lines(tmin, sqrt(1/(1+exp(-(coefs$species[i,1] + coefs$species[i,2]*tmin + coefs$species[i,3]*tmin2)))),
        col=species$cols[i], lwd=0.5+species$n[i])
}
legend("top",c("Low RTD", "Moderate RTD", "High RTD"),
       lwd=2, col=col.leg, bty="n", cex=0.85, horiz=TRUE)

#spp optimums
sp.opt <- matrix(0, ncol=nrow(species), nrow=100)
for(i in 1:ncol(sp.opt)) {
  sp.opt[,i] <- 1/(1+exp(-(coefs$species[i,1] + coefs$species[i,2]*tmin + coefs$species[i,3]*tmin2)))
}
sp.opt <- data.frame(sp.opt)
for(i in 1:ncol(sp.opt)) {
  sp.opt[101,i] <- tmin[which.max(sp.opt[1:100,i])]
  sp.opt[102,i] <- max(sp.opt[1:100,i])
}
species$opt <- t(sp.opt[101,1:nrow(species)])
species$maxprob <- t(sp.opt[102,1:nrow(species)])
plot(species$opt, species$rtd.mean, col=species$cols, ylim=c(-3,2), xlab="", ylab="")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
points(species$opt, species$rtd.mean, cex=2*species$n, pch=19, col=species$cols)
title(sub=list(expression(paste("Optimum minimum temperature (",degree,"C)")), cex=1.2), line=2.5)
title(sub="\n(scaled to unit variance below, native scale above)",line=3.5)
title("(F) Species optimum temperature and SRL",line=2.5)
title(ylab=list(expression(paste("Root tissue density (mg mm",{}^-3,")")),cex=1.3),line=3.5)
title(ylab="\n(scaled to unit variance, log scale at right)",line=2.5)
axis(3, at=xscale.rtd.forests$sd, labels=xscale.rtd.forests$tmin, line=0)
axis(4, at=xscale.rtd.forests$sd, labels=xscale.rtd.forests$RTD, line=0)
summary(m1 <- lm(rtd.mean ~ as.numeric(opt), data=species, weight=n))
newdat <- data.frame(opt=c(tmin))
conf_interval <- predict(m1, newdata=newdat, interval="prediction", level = 0.95)
matlines(tmin, conf_interval, lty=c(1,2,2), col=1, lwd=c(3,2,2))
legend("bottomright", legend="r = 0.20, P < 0.0001", bty="n", cex=1.2)

### RTD-PPET
f2@frame$r2pet.unscaled <- unscale(f2@frame$r2pet)
f2@frame$RTD.unscaled <- unscale(f2@frame$RTD)
lm1 <- lm(r2pet.unscaled ~ as.vector(r2pet), data=f2@frame)
lm2 <- lm(RTD.unscaled ~ as.vector(RTD), data=f2@frame)
newdat <- data.frame(r2pet=seq(-5,5,1), RTD=seq(-5,5,1))
xscale.rtd.forests <- data.frame(sd=seq(-5,5,1), r2pet=round(exp(predict(lm1,newdat)),2), RTD=round(exp(predict(lm2,newdat)),2))

species.pres <- f2@frame[f2@frame$pres==1,]
species <- species.pres %>% group_by(species) %>% summarise_at(vars(pres,r2pet,RTD),c(min,max,length,mean))
colnames(species) <- c("species","pres.min","r2pet.min","rtd.min","pres.max","r2pet.max","rtd.max","length","length2","length3","pres.mean","r2pet.mean","rtd.mean")
species$n <- log(species$length)/log(max(species$length))
species <- species[order(species$rtd.mean), ]
species$cols <- viridis::viridis_pal(option="D")(nrow(species))
col.leg <- c(species$cols[1],species$cols[round((nrow(species)/2),0)],species$cols[nrow(species)])
species <- species[order(species$species), ]
coefs <- coef(ef2b)
r2pet <- seq(quantile(ef2b@frame$r2pet,0.01),quantile(ef2b@frame$r2pet,0.99), length.out=100)
prob <- seq(0,1, length.out=100)
r2pet2 <- scale((r2pet-mean(r2pet))^2)

plot(r2pet,prob, col="white", xlab="",ylab="Probability of occurrence (sqrt)", ylim=c(0,1.05), cex.lab=1.3)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
title(sub=list("Precipitation:Potential Evapotranspiration ratio", cex=1.2),line=2.5)
title(sub="\n(scaled to unit variance below, log scale above)",line=3.5)
title("(G) Species distributions = f(P:PET)",line=2.5)
axis(3, at=xscale.rtd.forests$sd, labels=xscale.rtd.forests$r2pet, line=0)
#title("Species response curves",line=3)
for(i in 1:nrow(species)){
  lines(r2pet, sqrt(1/(1+exp(-(coefs$species[i,1] + coefs$species[i,2]*r2pet + coefs$species[i,3]*r2pet2)))),
        col=species$cols[i], lwd=0.5+species$n[i])
}
legend("top",c("Low RTD", "Moderate RTD", "High RTD"),
       lwd=2, col=col.leg, bty="n", cex=0.785, horiz=TRUE)

#spp optimums
sp.opt <- matrix(0, ncol=nrow(species), nrow=100)
for(i in 1:ncol(sp.opt)) {
  sp.opt[,i] <- 1/(1+exp(-(coefs$species[i,1] + coefs$species[i,2]*r2pet + coefs$species[i,3]*r2pet2)))
}
sp.opt <- data.frame(sp.opt)
for(i in 1:ncol(sp.opt)) {
  sp.opt[101,i] <- r2pet[which.max(sp.opt[1:100,i])]
  sp.opt[102,i] <- max(sp.opt[1:100,i])
}
species$opt <- t(sp.opt[101,1:nrow(species)])
species$maxprob <- t(sp.opt[102,1:nrow(species)])
plot(species$opt, species$rtd.mean, col=species$cols, ylim=c(-3,2), xlab="", ylab="")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
points(species$opt, species$rtd.mean, cex=2*species$n, pch=19, col=species$cols)
title(sub=list("Optimum P:PET ratio", cex=1.2), line=2.5)
title(sub="\n(scaled to unit variance below, log scale above)",line=3.5)
title("(H) Species optimum P:PET ratio and SRL",line=2.5)
title(ylab=list(expression(paste("Root tissue density (mg mm",{}^-3,")")),cex=1.3),line=3.5)
title(ylab="\n(scaled to unit variance, log scale at right)",line=2.5)
axis(3, at=xscale.rtd.forests$sd, labels=xscale.rtd.forests$r2pet, line=0)
axis(4, at=xscale.rtd.forests$sd, labels=xscale.rtd.forests$RTD, line=0)
summary(m1 <- lm(rtd.mean ~ as.numeric(opt), data=species, weight=n))
newdat <- data.frame(opt=c(r2pet))
conf_interval <- predict(m1, newdata=newdat, interval="prediction", level = 0.95)
matlines(r2pet, conf_interval, lty=c(2,0,0), col=1, lwd=c(1,0,0))
legend("bottomright", legend="r = -0.06, P = 0.11", bty="n", cex=1.2)

dev.off()

#rm(f1,ef1b,f2,ef2b)
############# End figure 3 ####################################


