### rtd models

.libPaths(c("~/R/intel/3.6/")) #set lib paths

library(dplyr)
library(lme4)
library(lmerTest)

load("/project/banquo/dlaughl4/sRoot2/RTD.env.RData")

# forest
#f1 <- glmer(pres ~ tmin + tmin2 + RTD + RTD:tmin + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RTD.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(f1, file = "f1.RData")

#f2 <- glmer(pres ~ r2pet + r2pet2 + RTD + RTD:r2pet + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RTD.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(f2, file = "f2.RData")

# grassland
#g1 <- glmer(pres ~ tmin + tmin2 + RTD + RTD:tmin + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RTD.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(g1, file = "g1.RData")

#g2 <- glmer(pres ~ r2pet + r2pet2 + RTD + RTD:r2pet + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RTD.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(g2, file = "g2.RData")

# wetland
#w1 <- glmer(pres ~ tmin + tmin2 + RTD + RTD:tmin + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RTD.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(w1, file = "w1.RData")

#w2 <- glmer(pres ~ r2pet + r2pet2 + RTD + RTD:r2pet + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RTD.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(w2, file = "w2.RData")




#### update model
#load("/gscratch/dlaughl4/RTD_v1/f1.RData")
#ss <- getME(f1,c("theta","fixef"))
#f1b <- update(f1, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(f1b, file = "f1b.RData")

#load("/gscratch/dlaughl4/RTD_v1/f2b.RData")
#ss <- getME(f2b,c("theta","fixef"))
#f2c <- update(f2b, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(f2c, file = "f2c.RData")

#load("/gscratch/dlaughl4/RTD_v1/g1c.RData")
#ss <- getME(g1c,c("theta","fixef"))
#g1d <- update(g1c, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=1e5)))
#save(g1d, file = "g1d.RData")

#load("/gscratch/dlaughl4/RTD_v1/g2b.RData")
#ss <- getME(g2b,c("theta","fixef"))
#g2c <- update(g2b, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(g2c, file = "g2c.RData")

#load("/gscratch/dlaughl4/RTD_v1/w1.RData")
#ss <- getME(w1,c("theta","fixef"))
#w1b <- update(w1, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=1e5)))
#save(w1b, file = "w1b.RData")

#load("/gscratch/dlaughl4/RTD_v1/w2.RData")
#ss <- getME(w2,c("theta","fixef"))
#w2b <- update(w2, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=1e5)))
#save(w2b, file = "w2b.RData")



#### Env only model
# forest
#ef1 <- glmer(pres ~ tmin + tmin2 + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RTD.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=2e5)))
#save(ef1, file = "ef1.RData")

#ef2 <- glmer(pres ~ r2pet + r2pet2 + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RTD.forests.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=2e5)))
#save(ef2, file = "ef2.RData")

# grassland
#eg1 <- glmer(pres ~ tmin + tmin2 + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RTD.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(eg1, file = "eg1.RData")

#eg2 <- glmer(pres ~ r2pet + r2pet2 + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RTD.grasslands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=1e5)))
#save(eg2, file = "eg2.RData")

# wetland
#ew1 <- glmer(pres ~ tmin + tmin2 + (tmin + tmin2|species) + (1|ECO_NAME),
#                   data=RTD.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=2e5)))
#save(ew1, file = "ew1.RData")

#ew2 <- glmer(pres ~ r2pet + r2pet2 + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                   data=RTD.wetlands.trait.env, verbose=1,
#                   family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                         optCtrl=list(maxfun=2e5)))
#save(ew2, file = "ew2.RData")





#### update model
#load("/gscratch/dlaughl4/RTD_v1/ef1.RData")
#ss <- getME(ef1,c("theta","fixef"))
#ef1b <- update(ef1, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=2e5)))
#save(ef1b, file = "ef1b.RData")

#load("/gscratch/dlaughl4/RTD_v1/ef2.RData")
#ss <- getME(ef2,c("theta","fixef"))
#ef2b <- update(ef2, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=2e5)))
#save(ef2b, file = "ef2b.RData")

#load("/gscratch/dlaughl4/RTD_v1/eg1c.RData")
#ss <- getME(eg1c,c("theta","fixef"))
#eg1d <- update(eg1c, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=1e5)))
#save(eg1d, file = "eg1d.RData")

load("/gscratch/dlaughl4/RTD_v1/eg2.RData")
ss <- getME(eg2,c("theta","fixef"))
eg2b <- update(eg2, start=ss, control=glmerControl(optimizer="Nelder_Mead",
              optCtrl=list(maxfun=1e5)))
save(eg2b, file = "eg2b.RData")

#load("/gscratch/dlaughl4/RTD_v1/ew1.RData")
#ss <- getME(ew1,c("theta","fixef"))
#ew1b <- update(ew1, start=ss, control=glmerControl(optimizer="Nelder_Mead",              
#              optCtrl=list(maxfun=2e5)))
#save(ew1b, file = "ew1b.RData")

#load("/gscratch/dlaughl4/RTD_v1/ew2.RData")
#ss <- getME(ew2,c("theta","fixef"))
#ew2b <- update(ew2, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=2e5)))
#save(ew2b, file = "ew2b.RData")









### Variance explained
#m11.sp1 <- glmer(pres ~ tmin + (tmin|species),
#            data=RTD.forests.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=2e5)))
#save(m11.sp1, file = "RTDm11.sp1.RData")

#m11.eco1 <- glmer(pres ~ RTD * tmin + (RTD * tmin|ECO_NAME),
#            data=RTD.forests.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=2e5)))
#save(m11.eco1, file = "RTDm11.eco1.RData")

#m13.sp <- glmer(pres ~ r2pet + (r2pet|species),
#            data=RTD.forests.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=2e5)))
#save(m13.sp, file = "RTDm13.sp.RData")

#m13.eco <- glmer(pres ~ RTD * r2pet + (RTD * r2pet|ECO_NAME),
#            data=RTD.forests.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=2e5)))
#save(m13.eco, file = "RTDm13.eco.RData")

#m15.sp <- glmer(pres ~ clay + (clay|species),
#            data=RTD.forests.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                 optCtrl=list(maxfun=2e5)))
#save(m15.sp, file = "RTDm15.sp.RData")

#m15.eco <- glmer(pres ~ RTD * clay + (RTD * clay|ECO_NAME),
#            data=RTD.forests.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                 optCtrl=list(maxfun=2e5)))
#save(m15.eco, file = "RTDm15.eco.RData")

#m17.sp <- glmer(pres ~ n + (n|species),
#            data=RTD.forests.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                optCtrl=list(maxfun=2e5)))
#save(m17.sp, file = "RTDm17.sp.RData")

#m17.eco <- glmer(pres ~ RTD * n + (RTD * n|ECO_NAME),
#            data=RTD.forests.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                optCtrl=list(maxfun=2e5)))
#save(m17.eco, file = "RTDm17.eco.RData")





#### update variance explained model
#load("/gscratch/dlaughl4/forests_v1/RTDm11.eco1b.RData")
#ss <- getME(m11.eco1b,c("theta","fixef"))
#m11.eco1c <- update(m11.eco1b, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=2e5)))
#save(m11.eco1c, file = "RTDm11.eco1c.RData")

#load("/gscratch/dlaughl4/forests_v1/RTDm13.ecob.RData")
#ss <- getME(m13.ecob,c("theta","fixef"))
#m13.ecoc <- update(m13.ecob, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=2e5)))
#save(m13.ecoc, file = "RTDm13.ecoc.RData")

#load("/gscratch/dlaughl4/forests_v1/RTDm15.ecob.RData")
#ss <- getME(m15.ecob,c("theta","fixef"))
#m15.ecoc <- update(m15.ecob, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=2e5)))
#save(m15.ecoc, file = "RTDm15.ecoc.RData")

#load("/gscratch/dlaughl4/forests_v1/RTDm17.ecob.RData")
#ss <- getME(m17.ecob,c("theta","fixef"))
#m17.ecoc <- update(m17.ecob, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              optCtrl=list(maxfun=2e5)))
#save(m17.ecoc, file = "RTDm17.ecoc.RData")



# change tolerance, maxfun
#m17b <- update(m17, start=ss, control=glmerControl(optimizer="Nelder_Mead",
#              check.conv.grad = .makeCC("warning", tol = 1e-3, relTol = NULL),
#              optCtrl=list(maxfun=5e3)))


#############

#m11 <- glmer(pres ~ RTD * tmin + (tmin|species) + (RTD * tmin|ECO_NAME),
#            data=RTD.grasslands.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=1e5)))
#save(m11, file = "RTDm11.RData")

#m13 <- glmer(pres ~ RTD * r2pet + (r2pet|species) + (RTD * r2pet|ECO_NAME),
#            data=RTD.grasslands.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=1e5)))
#save(m13, file = "RTDm13.RData")

#m15 <- glmer(pres ~ RTD * clay + (clay|species) + (RTD * clay|ECO_NAME),
#            data=RTD.grasslands.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                 optCtrl=list(maxfun=1e5)))
#save(m15, file = "RTDm15.RData")

#m17 <- glmer(pres ~ RTD * n + (n|species) + (RTD * n|ECO_NAME),
#            data=RTD.grasslands.trait.env, verbose=1,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                optCtrl=list(maxfun=1e5)))
#save(m17, file = "RTDm17.RData")


#m10 <- glmer(pres ~ 1 + (tmin|species) + (1|ECO_NAME/PlotID),
#            data=RTD.forests.trait.env, 
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=2e5)))
#save(m10, file = "RTDm10.RData")

#m12 <- glmer(pres ~ RTD * map + (map|species) + (RTD * map|ECO_NAME) + (1|PlotID),
#            data=RTD.grasslands.trait.env, 
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=4e5)))
#save(m12, file = "RTDm12.RData")

#m14 <- glmer(pres ~ RTD * silt + (silt|species) + (RTD * silt|ECO_NAME) + (1|PlotID),
#            data=RTD.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=4e5)))
#save(m14, file = "RTDm14.RData")

#m16 <- glmer(pres ~ RTD * bulkd + (bulkd|species) + (RTD * bulkd|ECO_NAME) + (1|PlotID),
#            data=RTD.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=4e5)))
#save(m16, file = "RTDm16.RData")

#m18 <- glmer(pres ~ RTD * cec + (cec|species) + (RTD * cec|ECO_NAME) + (1|PlotID),
#            data=RTD.grasslands.trait.env,
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                  optCtrl=list(maxfun=4e5)))
#save(m18, file = "RTDm18.RData")




#m10 <- glmer(pres ~ 1 + (tmin|species) + (1|PlotID),
#            data=RTD.grasslands.trait.env, 
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=2e5)))
#save(m10, file = "RTDm10.RData")

#m11 <- glmer(pres ~ RTD * tmin + (tmin|species) + (1|PlotID),
#            data=RTD.grasslands.trait.env, 
#            family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                               optCtrl=list(maxfun=2e5)))
#save(m11, file = "RTDm11.RData")

#test model
#RTD.forests.trait.env <- RTD.forests.trait.env[!is.na(RTD.forests.trait.env$r2pet),]
#RTD.forests.trait.env$r2pet2 <- scale((RTD.forests.trait.env$r2pet-mean(RTD.forests.trait.env$r2pet))^2)
#m13.RTDtest <- glmer(pres ~ r2pet + r2pet2 + RTD + RTD:r2pet + (r2pet + r2pet2|species) + (1|ECO_NAME),
#                     data=RTD.forests.trait.env, verbose=1,
#                     family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                           optCtrl=list(maxfun=1e5)))
#save(m13.RTDtest, file = "RTDm13.RTDtest.RData")

#test r2pet effects in temperature model
#RTD.forests.trait.env <- RTD.forests.trait.env[!is.na(RTD.forests.trait.env$r2pet),]
#m11.r2petmod1 <- glmer(pres ~ tmin + RTD + r2pet + RTD:tmin + (tmin|species) + (1|ECO_NAME),
#                       data=RTD.forests.trait.env, verbose=1,
#                       family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                             optCtrl=list(maxfun=1e5)))
#save(m11.r2petmod1, file = "RTDm11.r2petmod1.RData")

#test r2pet effects in temperature model 2
#RTD.forests.trait.env <- RTD.forests.trait.env[!is.na(RTD.forests.trait.env$r2pet),]
#m11.r2petmod2 <- glmer(pres ~ tmin + RTD + r2pet + RTD:tmin + RTD:r2pet + 
#                         (tmin + r2pet|species) + (1|ECO_NAME),
#                       data=RTD.forests.trait.env, verbose=1,
#                       family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                             optCtrl=list(maxfun=1e5)))
#save(m11.r2petmod2, file = "RTDm11.r2petmod2.RData")

#test r2pet effects in temperature model 3
#RTD.forests.trait.env <- RTD.forests.trait.env[!is.na(RTD.forests.trait.env$r2pet),]
#m11.r2petmod3 <- glmer(pres ~ RTD*tmin*r2pet + (tmin + r2pet|species) + (1|ECO_NAME),
#                       data=RTD.forests.trait.env, verbose=1h,
#                       family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                             optCtrl=list(maxfun=1e5)))
#save(m11.r2petmod3, file = "RTDm11.r2petmod3.RData")

#test r2pet effects in temperature model 4
#RTD.forests.trait.env <- RTD.forests.trait.env[!is.na(RTD.forests.trait.env$r2pet),]
#RTD.forests.trait.env$tmin2 <- scale((RTD.forests.trait.env$tmin-mean(RTD.forests.trait.env$tmin))^2)
#RTD.forests.trait.env$r2pet2 <- scale((RTD.forests.trait.env$r2pet-mean(RTD.forests.trait.env$r2pet))^2)
#m11.r2petmod4 <- glmer(pres ~ tmin2 + r2pet2 + RTD*tmin*r2pet + (tmin + tmin2 + r2pet + r2pet2|species) + (1|ECO_NAME),
#                       data=RTD.forests.trait.env, verbose=1,
#                       family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                             optCtrl=list(maxfun=1e5)))
#save(m11.r2petmod4, file = "RTDm11.r2petmod4.RData")

#test r2pet effects in temperature model 5
#RTD.forests.trait.env <- RTD.forests.trait.env[!is.na(RTD.forests.trait.env$map),]
#RTD.forests.trait.env$tmin2 <- scale((RTD.forests.trait.env$tmin-mean(RTD.forests.trait.env$tmin))^2)
#RTD.forests.trait.env$map2 <- scale((RTD.forests.trait.env$map-mean(RTD.forests.trait.env$map))^2)
#m11.map1 <- glmer(pres ~ tmin2 + map2 + RTD*tmin*map + (tmin + tmin2 + map + map2|species) + (1|ECO_NAME),
#                       data=RTD.forests.trait.env, verbose=1,
#                       family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                             optCtrl=list(maxfun=1e5)))
#save(m11.map1, file = "RTDm11.map1.RData")

#test r2pet effects in temperature model 5
#RTD.forests.trait.env <- RTD.forests.trait.env[!is.na(RTD.forests.trait.env$map),]
#RTD.forests.trait.env$mat2 <- scale((RTD.forests.trait.env$mat-mean(RTD.forests.trait.env$mat))^2)
#RTD.forests.trait.env$map2 <- scale((RTD.forests.trait.env$map-mean(RTD.forests.trait.env$map))^2)
#m11.map2 <- glmer(pres ~ mat2 + map2 + RTD*mat*map + (mat + mat2 + map + map2|species) + (1|ECO_NAME),
#                  data=RTD.forests.trait.env, verbose=1,
#                  family=binomial, control=glmerControl(optimizer="Nelder_Mead",
#                                                        optCtrl=list(maxfun=1e5)))
#save(m11.map2, file = "RTDm11.map2.RData")


# on office computer
## load("/Volumes/gscratch/dlaughl4/RDm4.RData")