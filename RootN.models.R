### rootN models

.libPaths(c("~/R/intel/3.6/")) #set lib paths

library(dplyr)
library(lme4)
library(lmerTest)

load("/project/banquo/dlaughl4/sRoot2/RootN.env.RData")

# forest
#f5 <- glmer(pres ~ tmin + tmin2 + RootN + RootN:tmin + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RootN.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(f5, file = "f5.RData")

#f6 <- glmer(pres ~ r2pet + r2pet2 + RootN + RootN:r2pet + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RootN.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(f6, file = "f6.RData")

# grassland
#g5 <- glmer(pres ~ tmin + tmin2 + RootN + RootN:tmin + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RootN.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(g5, file = "g5.RData")

#g6 <- glmer(pres ~ r2pet + r2pet2 + RootN + RootN:r2pet + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RootN.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(g6, file = "g6.RData")

# wetland
#w5 <- glmer(pres ~ tmin + tmin2 + RootN + RootN:tmin + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RootN.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(w5, file = "w5.RData")

#w6 <- glmer(pres ~ r2pet + r2pet2 + RootN + RootN:r2pet + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RootN.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(w6, file = "w6.RData")



#### update model
#load("/gscratch/dlaughl4/RootN_v1/f5.RData")
#ss <- getME(f5,c("theta","fixef"))
#f5b <- update(f5, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(f5b, file = "f5b.RData")

#load("/gscratch/dlaughl4/RootN_v1/f6b.RData")
#ss <- getME(f6b,c("theta","fixef"))
#f6c <- update(f6b, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(f6c, file = "f6c.RData")

#load("/gscratch/dlaughl4/RootN_v1/g5.RData")
#ss <- getME(g5,c("theta","fixef"))
#g5b <- update(g5, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=1e5)))
#save(g5b, file = "g5b.RData")

#load("/gscratch/dlaughl4/RootN_v1/g6.RData")
#ss <- getME(g6,c("theta","fixef"))
#g6b <- update(g6, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(g6b, file = "g6b.RData")

#load("/gscratch/dlaughl4/RootN_v1/w5.RData")
#ss <- getME(w5,c("theta","fixef"))
#w5b <- update(w5, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=1e5)))
#save(w5b, file = "w5b.RData")

#load("/gscratch/dlaughl4/RootN_v1/w6.RData")
#ss <- getME(w6,c("theta","fixef"))
#w6b <- update(w6, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(w6b, file = "w6b.RData")




### Env only models
# forest
#ef5 <- glmer(pres ~ tmin + tmin2 + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RootN.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=4e5)))
#save(ef5, file = "ef5.RData")

#ef6 <- glmer(pres ~ r2pet + r2pet2 + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RootN.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=4e5)))
#save(ef6, file = "ef6.RData")

# grassland
#eg5 <- glmer(pres ~ tmin + tmin2 + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RootN.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=2e5)))
#save(eg5, file = "eg5.RData")

#eg6 <- glmer(pres ~ r2pet + r2pet2 + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RootN.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=2e5)))
#save(eg6, file = "eg6.RData")

# wetland
#ew5 <- glmer(pres ~ tmin + tmin2 + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RootN.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=5e5)))
#save(ew5, file = "ew5.RData")

#ew6 <- glmer(pres ~ r2pet + r2pet2 + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RootN.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=5e5)))
#save(ew6, file = "ew6.RData")



#### update model
#load("/gscratch/dlaughl4/RootN_v1/f5.RData")
#ss <- getME(f5,c("theta","fixef"))
#f5b <- update(f5, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#                                                 optCtrl=list(maxfun=4e5)))
#save(f5b, file = "f5b.RData")

#load("/gscratch/dlaughl4/RootN_v1/ef6.RData")
#ss <- getME(ef6,c("theta","fixef"))
#ef6b <- update(ef6, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=4e5)))
#save(ef6b, file = "ef6b.RData")

#load("/gscratch/dlaughl4/RootN_v1/g5.RData")
#ss <- getME(g5,c("theta","fixef"))
#g5b <- update(g5, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=1e5)))
#save(g5b, file = "g5b.RData")

#load("/gscratch/dlaughl4/RootN_v1/eg6.RData")
#ss <- getME(eg6,c("theta","fixef"))
#eg6b <- update(eg6, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=2e5)))
#save(eg6b, file = "eg6b.RData")

#load("/gscratch/dlaughl4/RootN_v1/ew5.RData")
#ss <- getME(ew5,c("theta","fixef"))
#ew5b <- update(ew5, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=5e5)))
#save(ew5b, file = "ew5b.RData")

#load("/gscratch/dlaughl4/RootN_v1/ew6.RData")
#ss <- getME(ew6,c("theta","fixef"))
#ew6b <- update(ew6, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=5e5)))
#save(ew6b, file = "ew6b.RData")





#load("/project/banquo/dlaughl4/sRoot2/grasslands.trait.env.RData")
#load("/project/banquo/dlaughl4/sRoot2/forests.trait.env.RData")

#m30 <- glmer(pres ~ 1 + (tmin|species) + (1|ECO_NAME/PlotID),
#            data=RootN.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=2e5)))
#save(m30, file = "RootNm30.RData")

#m31 <- glmer(pres ~ RootN * tmin + (tmin|species) + (1|ECO_NAME/PlotID),
#            data=RootN.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=2e5)))
#save(m31, file = "RootNm31.RData")

#m32 <- glmer(pres ~ RootN * map + (map|species) + (1|ECO_NAME/PlotID),
#            data=RootN.grasslands.trait.env, 
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=4e5)))
#save(m32, file = "RootNm32.RData")

#m33 <- glmer(pres ~ RootN * r2pet + (r2pet|species) + (1|ECO_NAME/PlotID),
#            data=RootN.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=2e5)))
#save(m33, file = "RootNm33.RData")

#m34 <- glmer(pres ~ RootN * silt + (silt|species) + (1|ECO_NAME/PlotID),
#            data=RootN.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=4e5)))
#save(m34, file = "RootNm34.RData")

#m35 <- glmer(pres ~ RootN * clay + (clay|species) + (1|ECO_NAME/PlotID),
#           data=RootN.grasslands.trait.env,
#           family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=2e5)))
#save(m35, file = "RootNm35.RData")

#m36 <- glmer(pres ~ RootN * bulkd + (bulkd|species) + (1|ECO_NAME/PlotID),
#            data=RootN.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=4e5)))
#save(m36, file = "RootNm36.RData")

#m37 <- glmer(pres ~ RootN * n + (n|species) + (1|ECO_NAME/PlotID),
#            data=RootN.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                optCtrl=list(maxfun=2e5)))
#save(m37, file = "RootNm37.RData")

#m38 <- glmer(pres ~ RootN * cec + (cec|species) + (1|ECO_NAME/PlotID),
#            data=RootN.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                              optCtrl=list(maxfun=4e5)))
#save(m38, file = "RootNm38.RData")

###########

#m54 <- glmer(pres ~ RootN * ph + (ph|species) + (1|ECO_NAME/PlotID),
#             data=RootN.grasslands.trait.env,
#             family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                   optCtrl=list(maxfun=4e5)))
#save(m54, file = "RootNm54.RData")

#m55 <- glmer(pres ~ RootN * org + (org|species) + (1|ECO_NAME/PlotID),
#             data=RootN.grasslands.trait.env,
#             family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                   optCtrl=list(maxfun=4e5)))
#save(m55, file = "RootNm55.RData")

#m56 <- glmer(pres ~ RootN * mat + (mat|species) + (1|ECO_NAME/PlotID),
#             data=RootN.grasslands.trait.env,
#             family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                   optCtrl=list(maxfun=4e5)))
#save(m56, file = "RootNm56.RData")



# on office computer
## load("/Volumes/gscratch/dlaughl4/RDm4.RData")
