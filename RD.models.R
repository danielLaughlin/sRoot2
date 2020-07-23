### RD models

.libPaths(c("~/R/intel/3.6/")) #set lib paths

library(dplyr)
library(lme4)
library(lmerTest)

load("/project/banquo/dlaughl4/sRoot2/RD.env.RData")

# forest
#f7 <- glmer(pres ~ tmin + tmin2 + RD + RD:tmin + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RD.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(f7, file = "f7.RData")

#f8 <- glmer(pres ~ r2pet + r2pet2 + RD + RD:r2pet + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RD.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(f8, file = "f8.RData")

# grassland
#g7 <- glmer(pres ~ tmin + tmin2 + RD + RD:tmin + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RD.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(g7, file = "g7.RData")

#g8 <- glmer(pres ~ r2pet + r2pet2 + RD + RD:r2pet + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RD.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(g8, file = "g8.RData")

# wetland
#w7 <- glmer(pres ~ tmin + tmin2 + RD + RD:tmin + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RD.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(w7, file = "w7.RData")

#w8 <- glmer(pres ~ r2pet + r2pet2 + RD + RD:r2pet + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RD.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(w8, file = "w8.RData")





#### update model
#load("/gscratch/dlaughl4/RD_v1/f7.RData")
#ss <- getME(f7,c("theta","fixef"))
#f7b <- update(f7, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(f7b, file = "f7b.RData")

#load("/gscratch/dlaughl4/RD_v1/f8.RData")
#ss <- getME(f8,c("theta","fixef"))
#f8b <- update(f8, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(f8b, file = "f8b.RData")

#load("/gscratch/dlaughl4/RD_v1/g7b.RData")
#ss <- getME(g7b,c("theta","fixef"))
#g7c <- update(g7b, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=1e5)))
#save(g7c, file = "g7c.RData")

#load("/gscratch/dlaughl4/RD_v1/g8b.RData")
#ss <- getME(g8b,c("theta","fixef"))
#g8c <- update(g8b, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(g8c, file = "g8c.RData")

#load("/gscratch/dlaughl4/RD_v1/w7.RData")
#ss <- getME(w7,c("theta","fixef"))
#w7b <- update(w7, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=1e5)))
#save(w7b, file = "w7b.RData")

#load("/gscratch/dlaughl4/RD_v1/w8.RData")
#ss <- getME(w8,c("theta","fixef"))
#w8b <- update(w8, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(w8b, file = "w8b.RData")




### Env one models
# forest
#ef7 <- glmer(pres ~ tmin + tmin2 + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RD.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=2e5)))
#save(ef7, file = "ef7.RData")

#ef8 <- glmer(pres ~ r2pet + r2pet2 + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RD.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=2e5)))
#save(ef8, file = "ef8.RData")

# grassland
#eg7 <- glmer(pres ~ tmin + tmin2 + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RD.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(eg7, file = "eg7.RData")

#eg8 <- glmer(pres ~ r2pet + r2pet2 + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RD.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(eg8, file = "eg8.RData")

# wetland
#ew7 <- glmer(pres ~ tmin + tmin2 + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RD.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=5e5)))
#save(ew7, file = "ew7.RData")

#ew8 <- glmer(pres ~ r2pet + r2pet2 + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RD.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=5e5)))
#save(ew8, file = "ew8.RData")





#### update model
#load("/gscratch/dlaughl4/RD_v1/ef7.RData")
#ss <- getME(ef7,c("theta","fixef"))
#ef7b <- update(ef7, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=5e5)))
#save(ef7b, file = "ef7b.RData")

#load("/gscratch/dlaughl4/RD_v1/ef8.RData")
#ss <- getME(ef8,c("theta","fixef"))
#ef8b <- update(ef8, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=5e5)))
#save(ef8b, file = "ef8b.RData")

#load("/gscratch/dlaughl4/RD_v1/eg7.RData")
#ss <- getME(eg7,c("theta","fixef"))
#eg7b <- update(eg7, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=2e5)))
#save(eg7b, file = "eg7b.RData")

#load("/gscratch/dlaughl4/RD_v1/eg8.RData")
#ss <- getME(eg8,c("theta","fixef"))
#eg8b <- update(eg8, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=2e5)))
#save(eg8b, file = "eg8b.RData")

#load("/gscratch/dlaughl4/RD_v1/w7.RData")
#ss <- getME(w7,c("theta","fixef"))
#w7b <- update(w7, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=1e5)))
#save(w7b, file = "w7b.RData")

#load("/gscratch/dlaughl4/RD_v1/w8.RData")
#ss <- getME(w8,c("theta","fixef"))
#w8b <- update(w8, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(w8b, file = "w8b.RData")








#load("/project/banquo/dlaughl4/sRoot2/grasslands.trait.env.RData")
#load("/project/banquo/dlaughl4/sRoot2/forests.trait.env.RData")

#m0 <- glmer(pres ~ 1 + (tmin|species) + (1|ECO_NAME/PlotID),
#            data=RD.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=2e5)))
#save(m0, file = "RDm0.RData")

#m1 <- glmer(pres ~ RD * tmin + (tmin|species) + (1|ECO_NAME/PlotID),
#            data=RD.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=2e5)))
#save(m1, file = "RDm1.RData")

#m2 <- glmer(pres ~ RD * map + (map|species) + (1|ECO_NAME/PlotID),
#             data=RD.grasslands.trait.env, 
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=4e5)))
#save(m2, file = "RDm2.RData")

#m3 <- glmer(pres ~ RD * r2pet + (r2pet|species) + (1|ECO_NAME/PlotID),
#            data=RD.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=2e5)))
#save(m3, file = "RDm3.RData")

#m4 <- glmer(pres ~ RD * silt + (silt|species) + (1|ECO_NAME/PlotID),
#            data=RD.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=4e5)))
#save(m4, file = "RDm4.RData")

#m5 <- glmer(pres ~ RD * clay + (clay|species) + (1|ECO_NAME/PlotID),
#            data=RD.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=2e5)))
#save(m5, file = "RDm5.RData")

#m6 <- glmer(pres ~ RD * bulkd + (bulkd|species) + (1|ECO_NAME/PlotID),
#            data=RD.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=4e5)))
#save(m6, file = "RDm6.RData")

#m7 <- glmer(pres ~ RD * n + (n|species) + (1|ECO_NAME/PlotID),
#            data=RD.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                optCtrl=list(maxfun=2e5)))
#save(m7, file = "RDm7.RData")

#m8 <- glmer(pres ~ RD * cec + (cec|species) + (1|ECO_NAME/PlotID),
#            data=RD.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                              optCtrl=list(maxfun=4e5)))
#save(m8, file = "RDm8.RData")

###########

#m57 <- glmer(pres ~ RD * ph + (ph|species) + (1|ECO_NAME/PlotID),
#            data=RD.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=4e5)))
#save(m57, file = "RDm57.RData")

#m58 <- glmer(pres ~ RD * org + (org|species) + (1|ECO_NAME/PlotID),
#            data=RD.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=4e5)))
#save(m58, file = "RDm58.RData")

#m59 <- glmer(pres ~ RD * mat + (mat|species) + (1|ECO_NAME/PlotID),
#            data=RD.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=4e5)))
#save(m59, file = "RDm59.RData")




# on office computer
## load("/Volumes/gscratch/dlaughl4/RDm4.RData")
