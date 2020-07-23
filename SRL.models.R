### srl models

.libPaths(c("~/R/intel/3.6/")) #set lib paths

library(dplyr)
library(lme4)
library(lmerTest)

load("/project/banquo/dlaughl4/sRoot2/SRL.env.RData")

# forest
#f3 <- glmer(pres ~ tmin + tmin2 + SRL + SRL:tmin + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=SRL.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(f3, file = "f3.RData")

#f4 <- glmer(pres ~ r2pet + r2pet2 + SRL + SRL:r2pet + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=SRL.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(f4, file = "f4.RData")

# grassland
#g3 <- glmer(pres ~ tmin + tmin2 + SRL + SRL:tmin + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=SRL.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(g3, file = "g3.RData")

#g4 <- glmer(pres ~ r2pet + r2pet2 + SRL + SRL:r2pet + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=SRL.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(g4, file = "g4.RData")

# wetland
#w3 <- glmer(pres ~ tmin + tmin2 + SRL + SRL:tmin + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=SRL.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(w3, file = "w3.RData")

#w4 <- glmer(pres ~ r2pet + r2pet2 + SRL + SRL:r2pet + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=SRL.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(w4, file = "w4.RData")




#### update model
#load("/gscratch/dlaughl4/SRL_v1/f3b.RData")
#ss <- getME(f3b,c("theta","fixef"))
#f3c <- update(f3b, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(f3c, file = "f3c.RData")

#load("/gscratch/dlaughl4/SRL_v1/f4.RData")
#ss <- getME(f4,c("theta","fixef"))
#f4b <- update(f4, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(f4b, file = "f4b.RData")

#load("/gscratch/dlaughl4/SRL_v1/g3b.RData")
#ss <- getME(g3b,c("theta","fixef"))
#g3c <- update(g3b, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=1e5)))
#save(g3c, file = "g3c.RData")

#load("/gscratch/dlaughl4/SRL_v1/g4b.RData")
#ss <- getME(g4b,c("theta","fixef"))
#g4c <- update(g4b, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(g4c, file = "g4c.RData")

#load("/gscratch/dlaughl4/SRL_v1/w3.RData")
#ss <- getME(w3,c("theta","fixef"))
#w3b <- update(w3, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=1e5)))
#save(w3b, file = "w3b.RData")

#load("/gscratch/dlaughl4/SRL_v1/w4.RData")
#ss <- getME(w4,c("theta","fixef"))
#w4b <- update(w4, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(w4b, file = "w4b.RData")



### Env only models
# forest
#ef3 <- glmer(pres ~ tmin + tmin2 + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=SRL.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=2e5)))
#save(ef3, file = "ef3.RData")

#ef4 <- glmer(pres ~ r2pet + r2pet2 + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=SRL.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=2e5)))
#save(ef4, file = "ef4.RData")

# grassland
#eg3 <- glmer(pres ~ tmin + tmin2 + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=SRL.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(eg3, file = "eg3_alt.RData")

#eg4 <- glmer(pres ~ r2pet + r2pet2 + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=SRL.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(eg4, file = "eg4.RData")

# wetland
#ew3 <- glmer(pres ~ tmin + tmin2 + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=SRL.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=2e5)))
#save(ew3, file = "ew3.RData")

#ew4 <- glmer(pres ~ r2pet + r2pet2 + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=SRL.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=2e5)))
#save(ew4, file = "ew4.RData")



#### update model
#load("/gscratch/dlaughl4/SRL_v1/ef3.RData")
#ss <- getME(ef3,c("theta","fixef"))
#ef3b <- update(ef3, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=4e5)))
#save(ef3b, file = "ef3b.RData")

#load("/gscratch/dlaughl4/SRL_v1/ef4.RData")
#ss <- getME(ef4,c("theta","fixef"))
#ef4b <- update(ef4, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=4e5)))
#save(ef4b, file = "ef4b.RData")

#load("/gscratch/dlaughl4/SRL_v1/eg3.RData")
#ss <- getME(eg3,c("theta","fixef"))
#eg3b <- update(eg3, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=2e5)))
#save(eg3b, file = "eg3b.RData")

#load("/gscratch/dlaughl4/SRL_v1/eg4.RData")
#ss <- getME(eg4,c("theta","fixef"))
#eg4b <- update(eg4, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=2e5)))
#save(eg4b, file = "eg4b.RData")

#load("/gscratch/dlaughl4/SRL_v1/ew3.RData")
#ss <- getME(ew3,c("theta","fixef"))
#ew3b <- update(ew3, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=2e5)))
#save(ew3b, file = "ew3b.RData")

#load("/gscratch/dlaughl4/SRL_v1/ew4b.RData")
#ss <- getME(ew4b,c("theta","fixef"))
#ew4c <- update(ew4b, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=4e5)))
#save(ew4c, file = "ew4c.RData")









### variance explained models

#m21.sp <- glmer(pres ~ tmin + (tmin|species),
#            data=SRL.forests.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=2e5)))
#save(m21.sp, file = "SRLm21.sp.RData")

#m23.sp <- glmer(pres ~ r2pet + (r2pet|species),
#            data=SRL.forests.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=2e5)))
#save(m23.sp, file = "SRLm23.sp.RData")

#m25.sp <- glmer(pres ~ clay + (clay|species),
#           data=SRL.forests.trait.env, verbose=1,
#           family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=2e5)))
#save(m25.sp, file = "SRLm25.sp.RData")

#m27.sp <- glmer(pres ~ n + (n|species),
#            data=SRL.forests.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                optCtrl=list(maxfun=2e5)))
#save(m27.sp, file = "SRLm27.sp.RData")



#SRL.forests.trait.env$tmin2 <- scale((SRL.forests.trait.env$tmin-mean(SRL.forests.trait.env$tmin))^2)
#m21.spopt <- glmer(pres ~ tmin + tmin2 + SRL:tmin + (tmin + tmin2|species),
#            data=SRL.forests.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=1e5)))
#save(m21.spopt, file = "SRLm21.spopt.RData")


#m21 <- glmer(pres ~ SRL * tmin + (tmin|species) + (SRL * tmin|ECO_NAME),
#            data=SRL.grasslands.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=1e5)))
#save(m21, file = "SRLm21.RData")

#m23 <- glmer(pres ~ SRL * r2pet + (r2pet|species) + (SRL * r2pet|ECO_NAME),
#            data=SRL.grasslands.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=1e5)))
#save(m23, file = "SRLm23.RData")

#m25 <- glmer(pres ~ SRL * clay + (clay|species) + (SRL * clay|ECO_NAME),
#           data=SRL.grasslands.trait.env, verbose=1,
#           family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=1e5)))
#save(m25, file = "SRLm25.RData")

#m27 <- glmer(pres ~ SRL * n + (n|species) + (SRL * n|ECO_NAME),
#            data=SRL.grasslands.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                optCtrl=list(maxfun=1e5)))
#save(m27, file = "SRLm27.RData")


#### update model

#load("/gscratch/dlaughl4/grasslands_v1/SRLm21b.RData")
#ss <- getME(m21b,c("theta","fixef"))
#m21c <- update(m21b, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=2e5)))
#save(m21c, file = "SRLm21c.RData")

#load("/gscratch/dlaughl4/grasslands_v1/SRLm23.RData")
#ss <- getME(m23,c("theta","fixef"))
#m23b <- update(m23, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#               optCtrl=list(maxfun=2e5)))
#save(m23b, file = "SRLm23b.RData")

#load("/gscratch/dlaughl4/grasslands_v1/SRLm25b.RData")
#ss <- getME(m25b,c("theta","fixef"))
#m25c <- update(m25b, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#               optCtrl=list(maxfun=2e5)))
#save(m25c, file = "SRLm25c.RData")

#load("/gscratch/dlaughl4/grasslands_v1/SRLm27b.RData")
#ss <- getME(m27b,c("theta","fixef"))
#m27c <- update(m27b, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#               optCtrl=list(maxfun=2e5)))
#save(m27c, file = "SRLm27c.RData")

##############

#m20 <- glmer(pres ~ 1 + (tmin|species) + (1|ECO_NAME/PlotID),
#            data=SRL.forests.trait.env, verbose=2,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=2e5)))
#save(m20, file = "SRLm20.RData")

#m22 <- glmer(pres ~ SRL * map + (map|species) + (1|ECO_NAME/PlotID),
#            data=SRL.grasslands.trait.env, 
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=4e5)))
#save(m22, file = "SRLm22.RData")

#m24 <- glmer(pres ~ SRL * silt + (silt|species) + (1|ECO_NAME/PlotID),
#            data=SRL.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=4e5)))
#save(m24, file = "SRLm24.RData")

#m26 <- glmer(pres ~ SRL * bulkd + (bulkd|species) + (1|ECO_NAME/PlotID),
#            data=SRL.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=4e5)))
#save(m26, file = "SRLm26.RData")

#m28 <- glmer(pres ~ SRL * cec + (cec|species) + (1|ECO_NAME/PlotID),
#            data=SRL.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                              optCtrl=list(maxfun=4e5)))
#save(m28, file = "SRLm28.RData")




#m60 <- glmer(pres ~ SRL * ph + (ph|species) + (1|ECO_NAME/PlotID),
#             data=SRL.grasslands.trait.env,
#             family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                   optCtrl=list(maxfun=4e5)))
#save(m60, file = "SRLm60.RData")

#m61 <- glmer(pres ~ SRL * org + (org|species) + (1|ECO_NAME/PlotID),
#             data=SRL.grasslands.trait.env,
#             family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                   optCtrl=list(maxfun=4e5)))
#save(m61, file = "SRLm61.RData")

#m62 <- glmer(pres ~ SRL * mat + (mat|species) + (1|ECO_NAME/PlotID),
#             data=SRL.grasslands.trait.env,
#             family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                   optCtrl=list(maxfun=4e5)))
#save(m62, file = "SRLm62.RData")

# on office computer
## load("/Volumes/gscratch/dlaughl4/RDm4.RData")
