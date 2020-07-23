### Data wrangling ###
setwd("~/OneDrive - University of Wyoming/Data/sRoot2/")

# load libraries and data, fix data
library(data.table)
library(tidyverse)
library(labdsv)
library(lme4)
library(lmerTest)
library(visreg)
library(piecewiseSEM)

load("sRoot_sPlot.RData")

# add Ecoregion to header.sRoot 
header.sRoot <- header.sRoot %>%
  left_join(env.sRoot %>% 
              dplyr::select(PlotID, ECO_NAME), by="PlotID")

# merge data tables
DT.sRoot08 <- DT.sRoot08 %>%
  left_join(header.sRoot %>% 
              dplyr::select(PlotObservationID, Country, Biome, CONTINENT, TopHabitat, ECO_NAME), by="PlotObservationID")

#selects grasslands and creates blocks for matrify/dematrify
DT.grasslands <- DT.sRoot08 %>%
  filter(TopHabitat=="Grassland")

#selects forests and creates blocks for matrify/dematrify
DT.forests <- DT.sRoot08 %>%
  filter(TopHabitat=="Forest")

#selects wetlands and creates blocks for matrify/dematrify
DT.wetlands <- DT.sRoot08 %>%
  filter(TopHabitat=="Wetland")


# RTD in forests
```{r}

# Select plots that have sufficient RTD trait coverage (>0.8) and richness > 1
RTD.sRoot <- CWM.sRoot %>%
  filter(variable == "RTD") %>%
  filter(trait.nspecies > 1)

RTD.forests <- DT.forests %>%
  filter(PlotObservationID %in% RTD.sRoot$PlotObservationID)

#add trait
colnames(root_traits)[1] <- "species"
RTD.forests <- RTD.forests %>%
  left_join(root_traits %>% 
              dplyr::select(species, RTD), by="species") %>%
  drop_na(RTD) #drop RTD=NA

# matrify and dematrify
RTD.forests <- droplevels(RTD.forests) #drop unused levels

RTD.forests.long <- list()
tick <- 1
for(i in levels(RTD.forests$ECO_NAME)){
  temp <- matrify(as.data.frame(RTD.forests) %>%
                    filter(ECO_NAME==i) %>% 
                    dplyr::select(PlotObservationID, species, Relative.cover)) 
  RTD.forests.long[[tick]] <- dematrify(temp, thresh=-1)
  tick <- tick + 1
}
RTD.forests.dematrified <- rlist::list.rbind(RTD.forests.long)
colnames(RTD.forests.dematrified) <- c("PlotID","species","Relative.cover")

#create trait-env dataframe
colnames(RTD.forests)[1] <- "PlotID"
RTD.forests.trait.env <- RTD.forests.dematrified %>%
  mutate(PlotID=as.numeric(as.character(PlotID))) %>%
  left_join(env.sRoot %>% 
              dplyr::select(PlotID, Aridity_ind, N.est, BLDFIE, CLYPPT, CECSOL, bio06, bio12, SLTPPT,PHIHOX,ORCDRC,bio01), by="PlotID") %>%
  left_join(root_traits %>%
              mutate(species=as.character(species)) %>%
              dplyr::select(species, RTD), by="species") %>%
  left_join(header.sRoot %>%
              dplyr::select(PlotID, CONTINENT, ECO_NAME, POINT_X, POINT_Y), by="PlotID")

#convert positive values to 1
RTD.forests.trait.env <- RTD.forests.trait.env  %>%
  mutate(pres = (Relative.cover>0)*1) %>%
  mutate(Aridity_ind = Aridity_ind/10000) %>%
  mutate(PHIHOX = PHIHOX/10) #%>% 
#mutate(block=factor(paste0(CONTINENT, Biome, Country)))

#log and scale traits that need transformation
RTD.forests.trait.env$RTD <- scale(log(RTD.forests.trait.env$RTD)) # log
RTD.forests.trait.env$BLDFIE <- scale(RTD.forests.trait.env$BLDFIE)
RTD.forests.trait.env$CLYPPT <- scale(RTD.forests.trait.env$CLYPPT)
RTD.forests.trait.env$N.est <- scale(RTD.forests.trait.env$N.est)
RTD.forests.trait.env$Aridity_ind <- scale(log(RTD.forests.trait.env$Aridity_ind)) # log
RTD.forests.trait.env$CECSOL <- scale(log(RTD.forests.trait.env$CECSOL)) # log
RTD.forests.trait.env$bio06 <- scale(RTD.forests.trait.env$bio06)
RTD.forests.trait.env$bio12 <- scale(RTD.forests.trait.env$bio12)
RTD.forests.trait.env$SLTPPT <- scale(RTD.forests.trait.env$SLTPPT)
RTD.forests.trait.env$PHIHOX <- scale(RTD.forests.trait.env$PHIHOX)
RTD.forests.trait.env$ORCDRC <- scale(log(RTD.forests.trait.env$ORCDRC)) #log
RTD.forests.trait.env$bio01 <- scale(RTD.forests.trait.env$bio01)
colnames(RTD.forests.trait.env)[4:14] <- c("r2pet","n","bulkd","clay", "cec","tmin","map","silt","ph","org","mat")

RTD.forests.trait.env$tmin2 <- scale((RTD.forests.trait.env$tmin - mean(RTD.forests.trait.env$tmin))^2)
RTD.forests.trait.env$r2pet2 <- scale((RTD.forests.trait.env$r2pet - mean(RTD.forests.trait.env$r2pet, na.rm=TRUE))^2)
RTD.forests.trait.env <- RTD.forests.trait.env %>% group_by(ECO_NAME) %>% filter(n()>300)

#sort(unique(RTD.forests.trait.env$ECO_NAME))
#save(RTD.forests.trait.env, file = "RTD.forests.trait.env.RData")
#sum(RTD.forests.trait.env$pres) / length(RTD.forests.trait.env$pres)
#save(RTD.forests.trait.env, file = "RTD.forests.trait.env.RData")
```


# RTD in grasslands
```{r}

# Select plots that have sufficient RTD trait coverage (>0.8) and richness > 1
RTD.sRoot <- CWM.sRoot %>%
  filter(variable == "RTD") %>%
  filter(trait.nspecies > 1)

RTD.grasslands <- DT.grasslands %>%
  filter(PlotObservationID %in% RTD.sRoot$PlotObservationID)

#add trait
colnames(root_traits)[1] <- "species"
RTD.grasslands <- RTD.grasslands %>%
  left_join(root_traits %>% 
              dplyr::select(species, RTD), by="species") %>%
  drop_na(RTD) #drop RTD=NA

# matrify and dematrify
RTD.grasslands <- droplevels(RTD.grasslands) #drop unused levels

RTD.grasslands.long <- list()
tick <- 1
for(i in levels(RTD.grasslands$ECO_NAME)){
  temp <- matrify(as.data.frame(RTD.grasslands) %>%
                    filter(ECO_NAME==i) %>% 
                    dplyr::select(PlotObservationID, species, Relative.cover)) 
  RTD.grasslands.long[[tick]] <- dematrify(temp, thresh=-1)
  tick <- tick + 1
}
RTD.grasslands.dematrified <- rlist::list.rbind(RTD.grasslands.long)
colnames(RTD.grasslands.dematrified) <- c("PlotID","species","Relative.cover")

#create grassland trait-env dataframe
colnames(RTD.grasslands)[1] <- "PlotID"
RTD.grasslands.trait.env <- RTD.grasslands.dematrified %>%
  mutate(PlotID=as.numeric(as.character(PlotID))) %>%
  left_join(env.sRoot %>% 
              dplyr::select(PlotID, Aridity_ind, N.est, BLDFIE, CLYPPT, CECSOL, bio06, bio12, SLTPPT, PHIHOX, ORCDRC,bio01), by="PlotID") %>%
  left_join(root_traits %>%
              mutate(species=as.character(species)) %>%
              dplyr::select(species, RTD), by="species") %>%
  left_join(header.sRoot %>%
              dplyr::select(PlotID, CONTINENT, ECO_NAME, POINT_X, POINT_Y), by="PlotID")

#convert positive values to 1
RTD.grasslands.trait.env <- RTD.grasslands.trait.env  %>%
  mutate(pres = (Relative.cover>0)*1) %>%
  mutate(Aridity_ind = Aridity_ind/10000) #%>%
#mutate(block=factor(paste0(CONTINENT, Biome, Country)))

#log and scale traits that need transformation
RTD.grasslands.trait.env$RTD <- scale(log(RTD.grasslands.trait.env$RTD)) # log
RTD.grasslands.trait.env$BLDFIE <- scale(RTD.grasslands.trait.env$BLDFIE)
RTD.grasslands.trait.env$CLYPPT <- scale(RTD.grasslands.trait.env$CLYPPT)
RTD.grasslands.trait.env$N.est <- scale(RTD.grasslands.trait.env$N.est)
RTD.grasslands.trait.env$Aridity_ind <- scale(log(RTD.grasslands.trait.env$Aridity_ind)) # log
RTD.grasslands.trait.env$CECSOL <- scale(log(RTD.grasslands.trait.env$CECSOL)) # log
RTD.grasslands.trait.env$bio06 <- scale(RTD.grasslands.trait.env$bio06)
RTD.grasslands.trait.env$bio12 <- scale(RTD.grasslands.trait.env$bio12)
RTD.grasslands.trait.env$SLTPPT <- scale(RTD.grasslands.trait.env$SLTPPT)
RTD.grasslands.trait.env$PHIHOX <- scale(RTD.grasslands.trait.env$PHIHOX)
RTD.grasslands.trait.env$ORCDRC <- scale(log(RTD.grasslands.trait.env$ORCDRC)) #log
RTD.grasslands.trait.env$bio01 <- scale(RTD.grasslands.trait.env$bio01)
colnames(RTD.grasslands.trait.env)[4:14] <- c("r2pet","n","bulkd","clay", "cec","tmin","map","silt","ph","org","mat")

RTD.grasslands.trait.env$tmin2 <- scale((RTD.grasslands.trait.env$tmin-mean(RTD.grasslands.trait.env$tmin))^2)
RTD.grasslands.trait.env$r2pet2 <- scale((RTD.grasslands.trait.env$r2pet-mean(RTD.grasslands.trait.env$r2pet, na.rm=TRUE))^2)

RTD.grasslands.trait.env <- RTD.grasslands.trait.env %>% group_by(ECO_NAME) %>% filter(n()>300)

#save(RTD.grasslands.trait.env, file = "RTD.grasslands.trait.env.RData")
#sum(RTD.grasslands.trait.env$pres) / length(RTD.grasslands.trait.env$pres)
#save(RTD.grasslands.trait.env, RTD.grasslands.trait.env, file = "grasslands.trait.env.RData")

```


# RTD in wetlands
```{r}

# Select plots that have sufficient RTD trait coverage (>0.8) and richness > 1
RTD.sRoot <- CWM.sRoot %>%
  filter(variable == "RTD") %>%
  filter(trait.nspecies > 1)

RTD.wetlands <- DT.wetlands %>%
  filter(PlotObservationID %in% RTD.sRoot$PlotObservationID)

#add trait
colnames(root_traits)[1] <- "species"
RTD.wetlands <- RTD.wetlands %>%
  left_join(root_traits %>% 
              dplyr::select(species, RTD), by="species") %>%
  drop_na(RTD) #drop RTD=NA

# matrify and dematrify
RTD.wetlands <- droplevels(RTD.wetlands) #drop unused levels

RTD.wetlands.long <- list()
tick <- 1
for(i in levels(RTD.wetlands$ECO_NAME)){
  temp <- matrify(as.data.frame(RTD.wetlands) %>%
                    filter(ECO_NAME==i) %>% 
                    dplyr::select(PlotObservationID, species, Relative.cover)) 
  RTD.wetlands.long[[tick]] <- dematrify(temp, thresh=-1)
  tick <- tick + 1
}
RTD.wetlands.dematrified <- rlist::list.rbind(RTD.wetlands.long)
colnames(RTD.wetlands.dematrified) <- c("PlotID","species","Relative.cover")

#create grassland trait-env dataframe
colnames(RTD.wetlands)[1] <- "PlotID"
RTD.wetlands.trait.env <- RTD.wetlands.dematrified %>%
  mutate(PlotID=as.numeric(as.character(PlotID))) %>%
  left_join(env.sRoot %>% 
              dplyr::select(PlotID, Aridity_ind, N.est, BLDFIE, CLYPPT, CECSOL, bio06, bio12, SLTPPT, PHIHOX, ORCDRC,bio01), by="PlotID") %>%
  left_join(root_traits %>%
              mutate(species=as.character(species)) %>%
              dplyr::select(species, RTD), by="species") %>%
  left_join(header.sRoot %>%
              dplyr::select(PlotID, CONTINENT, ECO_NAME, POINT_X, POINT_Y), by="PlotID")

#convert positive values to 1
RTD.wetlands.trait.env <- RTD.wetlands.trait.env  %>%
  mutate(pres = (Relative.cover>0)*1) %>%
  mutate(Aridity_ind = Aridity_ind/10000) #%>%
#mutate(block=factor(paste0(CONTINENT, Biome, Country)))

#log and scale traits that need transformation
RTD.wetlands.trait.env$RTD <- scale(log(RTD.wetlands.trait.env$RTD)) # log
RTD.wetlands.trait.env$BLDFIE <- scale(RTD.wetlands.trait.env$BLDFIE)
RTD.wetlands.trait.env$CLYPPT <- scale(RTD.wetlands.trait.env$CLYPPT)
RTD.wetlands.trait.env$N.est <- scale(RTD.wetlands.trait.env$N.est)
RTD.wetlands.trait.env$Aridity_ind <- scale(log(RTD.wetlands.trait.env$Aridity_ind)) # log
RTD.wetlands.trait.env$CECSOL <- scale(log(RTD.wetlands.trait.env$CECSOL)) # log
RTD.wetlands.trait.env$bio06 <- scale(RTD.wetlands.trait.env$bio06)
RTD.wetlands.trait.env$bio12 <- scale(RTD.wetlands.trait.env$bio12)
RTD.wetlands.trait.env$SLTPPT <- scale(RTD.wetlands.trait.env$SLTPPT)
RTD.wetlands.trait.env$PHIHOX <- scale(RTD.wetlands.trait.env$PHIHOX)
RTD.wetlands.trait.env$ORCDRC <- scale(log(RTD.wetlands.trait.env$ORCDRC)) #log
RTD.wetlands.trait.env$bio01 <- scale(RTD.wetlands.trait.env$bio01)
colnames(RTD.wetlands.trait.env)[4:14] <- c("r2pet","n","bulkd","clay", "cec","tmin","map","silt","ph","org","mat")

RTD.wetlands.trait.env$tmin2 <- scale((RTD.wetlands.trait.env$tmin-mean(RTD.wetlands.trait.env$tmin))^2)
RTD.wetlands.trait.env$r2pet2 <- scale((RTD.wetlands.trait.env$r2pet-mean(RTD.wetlands.trait.env$r2pet, na.rm=TRUE))^2)

RTD.wetlands.trait.env <- RTD.wetlands.trait.env %>% group_by(ECO_NAME) %>% filter(n()>300)

#save(RTD.wetlands.trait.env, file = "RTD.wetlands.trait.env.RData")
#sum(RTD.wetlands.trait.env$pres) / length(RTD.wetlands.trait.env$pres)
save(RTD.forests.trait.env, RTD.grasslands.trait.env, RTD.wetlands.trait.env, file = "RTD.env.RData")


```


# SRL in forests
```{r}

# Select plots that have sufficient SRL trait coverage (>0.8) and richness > 1
SRL.sRoot <- CWM.sRoot %>%
  filter(variable == "SRL") %>%
  filter(trait.nspecies > 1)

SRL.forests <- DT.forests %>%
  filter(PlotObservationID %in% SRL.sRoot$PlotObservationID)

#add trait
colnames(root_traits)[1] <- "species"
SRL.forests <- SRL.forests %>%
  left_join(root_traits %>% 
              dplyr::select(species, SRL), by="species") %>%
  drop_na(SRL)

# matrify and dematrify
SRL.forests <- droplevels(SRL.forests) #drop unused levels

SRL.forests.long <- list()
tick <- 1
for(i in levels(SRL.forests$ECO_NAME)){
  temp <- matrify(as.data.frame(SRL.forests) %>%
                    filter(ECO_NAME==i) %>% 
                    dplyr::select(PlotObservationID, species, Relative.cover)) 
  SRL.forests.long[[tick]] <- dematrify(temp, thresh=-1)
  tick <- tick + 1
}
SRL.forests.dematrified <- rlist::list.rbind(SRL.forests.long)
colnames(SRL.forests.dematrified) <- c("PlotID","species","Relative.cover")

#create trait-env dataframe
colnames(SRL.forests)[1] <- "PlotID"
SRL.forests.trait.env <- SRL.forests.dematrified %>%
  mutate(PlotID=as.numeric(as.character(PlotID))) %>%
  left_join(env.sRoot %>% 
              dplyr::select(PlotID, Aridity_ind, N.est, BLDFIE, CLYPPT, CECSOL, bio06, bio12, SLTPPT,PHIHOX,ORCDRC,bio01), by="PlotID") %>%
  left_join(root_traits %>%
              mutate(species=as.character(species)) %>%
              dplyr::select(species, SRL), by="species") %>%
  left_join(header.sRoot %>%
              dplyr::select(PlotID, CONTINENT, ECO_NAME, POINT_X, POINT_Y), by="PlotID")

#convert positive values to 1
SRL.forests.trait.env <- SRL.forests.trait.env  %>%
  mutate(pres = (Relative.cover>0)*1) %>%
  mutate(Aridity_ind = Aridity_ind/10000) %>%
  mutate(PHIHOX = PHIHOX/10)

#log and scale traits that need transformation
SRL.forests.trait.env$SRL <- scale(log(SRL.forests.trait.env$SRL)) # log
SRL.forests.trait.env$BLDFIE <- scale(SRL.forests.trait.env$BLDFIE)
SRL.forests.trait.env$CLYPPT <- scale(SRL.forests.trait.env$CLYPPT)
SRL.forests.trait.env$N.est <- scale(SRL.forests.trait.env$N.est)
SRL.forests.trait.env$Aridity_ind <- scale(log(SRL.forests.trait.env$Aridity_ind)) # log
SRL.forests.trait.env$CECSOL <- scale(log(SRL.forests.trait.env$CECSOL)) # log
SRL.forests.trait.env$bio06 <- scale(SRL.forests.trait.env$bio06)
SRL.forests.trait.env$bio12 <- scale(SRL.forests.trait.env$bio12)
SRL.forests.trait.env$SLTPPT <- scale(SRL.forests.trait.env$SLTPPT)
SRL.forests.trait.env$PHIHOX <- scale(SRL.forests.trait.env$PHIHOX)
SRL.forests.trait.env$ORCDRC <- scale(log(SRL.forests.trait.env$ORCDRC)) #log
SRL.forests.trait.env$bio01 <- scale(SRL.forests.trait.env$bio01)
colnames(SRL.forests.trait.env)[4:14] <- c("r2pet","n","bulkd","clay", "cec","tmin","map","silt","ph","org","mat")

SRL.forests.trait.env$tmin2 <- scale((SRL.forests.trait.env$tmin-mean(SRL.forests.trait.env$tmin))^2)
SRL.forests.trait.env$r2pet2 <- scale((SRL.forests.trait.env$r2pet-mean(SRL.forests.trait.env$r2pet, na.rm=TRUE))^2)

SRL.forests.trait.env <- SRL.forests.trait.env %>% group_by(ECO_NAME) %>% filter(n()>300)

#save(RTD.forests.trait.env, SRL.forests.trait.env, file = "forests.trait.env.RData")
#save(RTD.forests.trait.env, SRL.forests.trait.env, file = "RTD80test.forests.trait.env.RData")
```


# SRL in grasslands
```{r}

# Select plots that have sufficient SRL trait coverage (>0.8) and richness > 1
SRL.sRoot <- CWM.sRoot %>%
  filter(variable == "SRL") %>%
  filter(trait.nspecies > 1)

SRL.grasslands <- DT.grasslands %>%
  filter(PlotObservationID %in% SRL.sRoot$PlotObservationID)

#add trait
colnames(root_traits)[1] <- "species"
SRL.grasslands <- SRL.grasslands %>%
  left_join(root_traits %>% 
              dplyr::select(species, SRL), by="species") %>%
  drop_na(SRL)

# matrify and dematrify
SRL.grasslands <- droplevels(SRL.grasslands) #drop unused levels

SRL.grasslands.long <- list()
tick <- 1
for(i in levels(SRL.grasslands$ECO_NAME)){
  temp <- matrify(as.data.frame(SRL.grasslands) %>%
                    filter(ECO_NAME==i) %>% 
                    dplyr::select(PlotObservationID, species, Relative.cover)) 
  SRL.grasslands.long[[tick]] <- dematrify(temp, thresh=-1)
  tick <- tick + 1
}
SRL.grasslands.dematrified <- rlist::list.rbind(SRL.grasslands.long)
colnames(SRL.grasslands.dematrified) <- c("PlotID","species","Relative.cover")

#create grassland trait-env dataframe
colnames(SRL.grasslands)[1] <- "PlotID"
SRL.grasslands.trait.env <- SRL.grasslands.dematrified %>%
  mutate(PlotID=as.numeric(as.character(PlotID))) %>%
  left_join(env.sRoot %>% 
              dplyr::select(PlotID, Aridity_ind, N.est, BLDFIE, CLYPPT, CECSOL, bio06, bio12, SLTPPT,PHIHOX,ORCDRC,bio01), by="PlotID") %>%
  left_join(root_traits %>%
              mutate(species=as.character(species)) %>%
              dplyr::select(species, SRL), by="species") %>%
  left_join(header.sRoot %>%
              dplyr::select(PlotID, CONTINENT, ECO_NAME, POINT_X, POINT_Y), by="PlotID")

#convert positive values to 1
SRL.grasslands.trait.env <- SRL.grasslands.trait.env  %>%
  mutate(pres = (Relative.cover>0)*1) %>%
  mutate(Aridity_ind = Aridity_ind/10000) #%>%
#mutate(block=factor(paste0(CONTINENT, Biome, Country)))

#log and scale traits that need transformation
SRL.grasslands.trait.env$SRL <- scale(log(SRL.grasslands.trait.env$SRL)) # log
SRL.grasslands.trait.env$BLDFIE <- scale(SRL.grasslands.trait.env$BLDFIE)
SRL.grasslands.trait.env$CLYPPT <- scale(SRL.grasslands.trait.env$CLYPPT)
SRL.grasslands.trait.env$N.est <- scale(SRL.grasslands.trait.env$N.est)
SRL.grasslands.trait.env$Aridity_ind <- scale(log(SRL.grasslands.trait.env$Aridity_ind)) # log
SRL.grasslands.trait.env$CECSOL <- scale(log(SRL.grasslands.trait.env$CECSOL)) # log
SRL.grasslands.trait.env$bio06 <- scale(SRL.grasslands.trait.env$bio06)
SRL.grasslands.trait.env$bio12 <- scale(SRL.grasslands.trait.env$bio12)
SRL.grasslands.trait.env$SLTPPT <- scale(SRL.grasslands.trait.env$SLTPPT)
SRL.grasslands.trait.env$PHIHOX <- scale(SRL.grasslands.trait.env$PHIHOX)
SRL.grasslands.trait.env$ORCDRC <- scale(log(SRL.grasslands.trait.env$ORCDRC)) #log
SRL.grasslands.trait.env$bio01 <- scale(SRL.grasslands.trait.env$bio01)
colnames(SRL.grasslands.trait.env)[4:14] <- c("r2pet","n","bulkd","clay","cec","tmin","map","silt","ph","org","mat")

SRL.grasslands.trait.env$tmin2 <- scale((SRL.grasslands.trait.env$tmin-mean(SRL.grasslands.trait.env$tmin))^2)
SRL.grasslands.trait.env$r2pet2 <- scale((SRL.grasslands.trait.env$r2pet-mean(SRL.grasslands.trait.env$r2pet, na.rm=TRUE))^2)

SRL.grasslands.trait.env <- SRL.grasslands.trait.env %>% group_by(ECO_NAME) %>% filter(n()>300)

```


# SRL in wetlands
```{r}

# Select plots that have sufficient SRL trait coverage (>0.8) and richness > 1
SRL.sRoot <- CWM.sRoot %>%
  filter(variable == "SRL") %>%
  filter(trait.nspecies > 1)

SRL.wetlands <- DT.wetlands %>%
  filter(PlotObservationID %in% SRL.sRoot$PlotObservationID)

#add trait
colnames(root_traits)[1] <- "species"
SRL.wetlands <- SRL.wetlands %>%
  left_join(root_traits %>% 
              dplyr::select(species, SRL), by="species") %>%
  drop_na(SRL)

# matrify and dematrify
SRL.wetlands <- droplevels(SRL.wetlands) #drop unused levels

SRL.wetlands.long <- list()
tick <- 1
for(i in levels(SRL.wetlands$ECO_NAME)){
  temp <- matrify(as.data.frame(SRL.wetlands) %>%
                    filter(ECO_NAME==i) %>% 
                    dplyr::select(PlotObservationID, species, Relative.cover)) 
  SRL.wetlands.long[[tick]] <- dematrify(temp, thresh=-1)
  tick <- tick + 1
}
SRL.wetlands.dematrified <- rlist::list.rbind(SRL.wetlands.long)
colnames(SRL.wetlands.dematrified) <- c("PlotID","species","Relative.cover")

#create grassland trait-env dataframe
colnames(SRL.wetlands)[1] <- "PlotID"
SRL.wetlands.trait.env <- SRL.wetlands.dematrified %>%
  mutate(PlotID=as.numeric(as.character(PlotID))) %>%
  left_join(env.sRoot %>% 
              dplyr::select(PlotID, Aridity_ind, N.est, BLDFIE, CLYPPT, CECSOL, bio06, bio12, SLTPPT,PHIHOX,ORCDRC,bio01), by="PlotID") %>%
  left_join(root_traits %>%
              mutate(species=as.character(species)) %>%
              dplyr::select(species, SRL), by="species") %>%
  left_join(header.sRoot %>%
              dplyr::select(PlotID, CONTINENT, ECO_NAME, POINT_X, POINT_Y), by="PlotID")

#convert positive values to 1
SRL.wetlands.trait.env <- SRL.wetlands.trait.env  %>%
  mutate(pres = (Relative.cover>0)*1) %>%
  mutate(Aridity_ind = Aridity_ind/10000) #%>%
#mutate(block=factor(paste0(CONTINENT, Biome, Country)))

#log and scale traits that need transformation
SRL.wetlands.trait.env$SRL <- scale(log(SRL.wetlands.trait.env$SRL)) # log
SRL.wetlands.trait.env$BLDFIE <- scale(SRL.wetlands.trait.env$BLDFIE)
SRL.wetlands.trait.env$CLYPPT <- scale(SRL.wetlands.trait.env$CLYPPT)
SRL.wetlands.trait.env$N.est <- scale(SRL.wetlands.trait.env$N.est)
SRL.wetlands.trait.env$Aridity_ind <- scale(log(SRL.wetlands.trait.env$Aridity_ind)) # log
SRL.wetlands.trait.env$CECSOL <- scale(log(SRL.wetlands.trait.env$CECSOL)) # log
SRL.wetlands.trait.env$bio06 <- scale(SRL.wetlands.trait.env$bio06)
SRL.wetlands.trait.env$bio12 <- scale(SRL.wetlands.trait.env$bio12)
SRL.wetlands.trait.env$SLTPPT <- scale(SRL.wetlands.trait.env$SLTPPT)
SRL.wetlands.trait.env$PHIHOX <- scale(SRL.wetlands.trait.env$PHIHOX)
SRL.wetlands.trait.env$ORCDRC <- scale(log(SRL.wetlands.trait.env$ORCDRC)) #log
SRL.wetlands.trait.env$bio01 <- scale(SRL.wetlands.trait.env$bio01)
colnames(SRL.wetlands.trait.env)[4:14] <- c("r2pet","n","bulkd","clay","cec","tmin","map","silt","ph","org","mat")

SRL.wetlands.trait.env$tmin2 <- scale((SRL.wetlands.trait.env$tmin-mean(SRL.wetlands.trait.env$tmin))^2)
SRL.wetlands.trait.env$r2pet2 <- scale((SRL.wetlands.trait.env$r2pet-mean(SRL.wetlands.trait.env$r2pet, na.rm=TRUE))^2)

SRL.wetlands.trait.env <- SRL.wetlands.trait.env %>% group_by(ECO_NAME) %>% filter(n()>300)
save(SRL.forests.trait.env, SRL.grasslands.trait.env, SRL.wetlands.trait.env, file = "SRL.env.RData")
```



# RootN in forests
```{r}

# Select plots that have sufficient RootN trait coverage (>0.8) and richness > 1
RootN.sRoot <- CWM.sRoot %>%
  filter(variable == "RootN") %>%
  filter(trait.nspecies > 1)

RootN.forests <- DT.forests %>%
  filter(PlotObservationID %in% RootN.sRoot$PlotObservationID)

#add trait
colnames(root_traits)[1] <- "species"
RootN.forests <- RootN.forests %>%
  left_join(root_traits %>% 
              dplyr::select(species, RootN), by="species") %>%
  drop_na(RootN)

# matrify and dematrify
RootN.forests <- droplevels(RootN.forests) #drop unused levels

RootN.forests.long <- list()
tick <- 1
for(i in levels(RootN.forests$ECO_NAME)){
  temp <- matrify(as.data.frame(RootN.forests) %>%
                    filter(ECO_NAME==i) %>% 
                    dplyr::select(PlotObservationID, species, Relative.cover)) 
  RootN.forests.long[[tick]] <- dematrify(temp, thresh=-1)
  tick <- tick + 1
}
RootN.forests.dematrified <- rlist::list.rbind(RootN.forests.long)
colnames(RootN.forests.dematrified) <- c("PlotID","species","Relative.cover")

#create grassland trait-env dataframe
colnames(RootN.forests)[1] <- "PlotID"
RootN.forests.trait.env <- RootN.forests.dematrified %>%
  mutate(PlotID=as.numeric(as.character(PlotID))) %>%
  left_join(env.sRoot %>% 
              dplyr::select(PlotID, Aridity_ind, N.est, BLDFIE, CLYPPT, CECSOL, bio06, bio12, SLTPPT,PHIHOX,ORCDRC,bio01), by="PlotID") %>%
  left_join(root_traits %>%
              mutate(species=as.character(species)) %>%
              dplyr::select(species, RootN), by="species") %>%
  left_join(header.sRoot %>%
              dplyr::select(PlotID, CONTINENT, ECO_NAME, POINT_X, POINT_Y), by="PlotID")

#convert positive values to 1
RootN.forests.trait.env <- RootN.forests.trait.env  %>%
  mutate(pres = (Relative.cover>0)*1) %>%
  mutate(Aridity_ind = Aridity_ind/10000) %>%
  mutate(PHIHOX = PHIHOX/10)

#log and scale traits that need transformation
RootN.forests.trait.env$RootN <- scale(log(RootN.forests.trait.env$RootN)) # log
RootN.forests.trait.env$BLDFIE <- scale(RootN.forests.trait.env$BLDFIE)
RootN.forests.trait.env$CLYPPT <- scale(RootN.forests.trait.env$CLYPPT)
RootN.forests.trait.env$N.est <- scale(RootN.forests.trait.env$N.est)
RootN.forests.trait.env$Aridity_ind <- scale(log(RootN.forests.trait.env$Aridity_ind)) # log
RootN.forests.trait.env$CECSOL <- scale(log(RootN.forests.trait.env$CECSOL)) # log
RootN.forests.trait.env$bio06 <- scale(RootN.forests.trait.env$bio06)
RootN.forests.trait.env$bio12 <- scale(RootN.forests.trait.env$bio12)
RootN.forests.trait.env$SLTPPT <- scale(RootN.forests.trait.env$SLTPPT)
RootN.forests.trait.env$PHIHOX <- scale(RootN.forests.trait.env$PHIHOX)
RootN.forests.trait.env$ORCDRC <- scale(log(RootN.forests.trait.env$ORCDRC)) #log
RootN.forests.trait.env$bio01 <- scale(RootN.forests.trait.env$bio01)
colnames(RootN.forests.trait.env)[4:14] <- c("r2pet","n","bulkd","clay", "cec","tmin","map","silt","ph","org","mat")

RootN.forests.trait.env$tmin2 <- scale((RootN.forests.trait.env$tmin-mean(RootN.forests.trait.env$tmin))^2)
RootN.forests.trait.env$r2pet2 <- scale((RootN.forests.trait.env$r2pet-mean(RootN.forests.trait.env$r2pet, na.rm=TRUE))^2)

RootN.forests.trait.env <- RootN.forests.trait.env %>% group_by(ECO_NAME) %>% filter(n()>300)

```


# RootN in grasslands
```{r}

# Select plots that have sufficient RootN trait coverage (>0.8) and richness > 1
RootN.sRoot <- CWM.sRoot %>%
  filter(variable == "RootN") %>%
  filter(trait.nspecies > 1)

RootN.grasslands <- DT.grasslands %>%
  filter(PlotObservationID %in% RootN.sRoot$PlotObservationID)

#add trait
colnames(root_traits)[1] <- "species"
RootN.grasslands <- RootN.grasslands %>%
  left_join(root_traits %>% 
              dplyr::select(species, RootN), by="species") %>%
  drop_na(RootN)

# matrify and dematrify
RootN.grasslands <- droplevels(RootN.grasslands) #drop unused levels

RootN.grasslands.long <- list()
tick <- 1
for(i in levels(RootN.grasslands$ECO_NAME)){
  temp <- matrify(as.data.frame(RootN.grasslands) %>%
                    filter(ECO_NAME==i) %>% 
                    dplyr::select(PlotObservationID, species, Relative.cover)) 
  RootN.grasslands.long[[tick]] <- dematrify(temp, thresh=-1)
  tick <- tick + 1
}
RootN.grasslands.dematrified <- rlist::list.rbind(RootN.grasslands.long)
colnames(RootN.grasslands.dematrified) <- c("PlotID","species","Relative.cover")

#create grassland trait-env dataframe
colnames(RootN.grasslands)[1] <- "PlotID"
RootN.grasslands.trait.env <- RootN.grasslands.dematrified %>%
  mutate(PlotID=as.numeric(as.character(PlotID))) %>%
  left_join(env.sRoot %>% 
              dplyr::select(PlotID, Aridity_ind, N.est, BLDFIE, CLYPPT, CECSOL, bio06, bio12, SLTPPT,PHIHOX,ORCDRC,bio01), by="PlotID") %>%
  left_join(root_traits %>%
              mutate(species=as.character(species)) %>%
              dplyr::select(species, RootN), by="species") %>%
  left_join(header.sRoot %>%
              dplyr::select(PlotID, CONTINENT, ECO_NAME, POINT_X, POINT_Y), by="PlotID")

#convert positive values to 1
RootN.grasslands.trait.env <- RootN.grasslands.trait.env  %>%
  mutate(pres = (Relative.cover>0)*1) %>%
  mutate(Aridity_ind = Aridity_ind/10000) %>%
  mutate(PHIHOX = PHIHOX/10)

#log and scale traits that need transformation
RootN.grasslands.trait.env$RootN <- scale(log(RootN.grasslands.trait.env$RootN)) # log
RootN.grasslands.trait.env$BLDFIE <- scale(RootN.grasslands.trait.env$BLDFIE)
RootN.grasslands.trait.env$CLYPPT <- scale(RootN.grasslands.trait.env$CLYPPT)
RootN.grasslands.trait.env$N.est <- scale(RootN.grasslands.trait.env$N.est)
RootN.grasslands.trait.env$Aridity_ind <- scale(log(RootN.grasslands.trait.env$Aridity_ind)) # log
RootN.grasslands.trait.env$CECSOL <- scale(log(RootN.grasslands.trait.env$CECSOL)) # log
RootN.grasslands.trait.env$bio06 <- scale(RootN.grasslands.trait.env$bio06)
RootN.grasslands.trait.env$bio12 <- scale(RootN.grasslands.trait.env$bio12)
RootN.grasslands.trait.env$SLTPPT <- scale(RootN.grasslands.trait.env$SLTPPT)
RootN.grasslands.trait.env$PHIHOX <- scale(RootN.grasslands.trait.env$PHIHOX)
RootN.grasslands.trait.env$ORCDRC <- scale(log(RootN.grasslands.trait.env$ORCDRC)) #log
RootN.grasslands.trait.env$bio01 <- scale(RootN.grasslands.trait.env$bio01)
colnames(RootN.grasslands.trait.env)[4:14] <- c("r2pet","n","bulkd","clay", "cec","tmin","map","silt","ph","org","mat")

RootN.grasslands.trait.env$tmin2 <- scale((RootN.grasslands.trait.env$tmin-mean(RootN.grasslands.trait.env$tmin))^2)
RootN.grasslands.trait.env$r2pet2 <- scale((RootN.grasslands.trait.env$r2pet-mean(RootN.grasslands.trait.env$r2pet, na.rm=TRUE))^2)

RootN.grasslands.trait.env <- RootN.grasslands.trait.env %>% group_by(ECO_NAME) %>% filter(n()>300)

```


# RootN in wetlands
```{r}

# Select plots that have sufficient RootN trait coverage (>0.8) and richness > 1
RootN.sRoot <- CWM.sRoot %>%
  filter(variable == "RootN") %>%
  filter(trait.nspecies > 1)

RootN.wetlands <- DT.wetlands %>%
  filter(PlotObservationID %in% RootN.sRoot$PlotObservationID)

#add trait
colnames(root_traits)[1] <- "species"
RootN.wetlands <- RootN.wetlands %>%
  left_join(root_traits %>% 
              dplyr::select(species, RootN), by="species") %>%
  drop_na(RootN)

# matrify and dematrify
RootN.wetlands <- droplevels(RootN.wetlands) #drop unused levels

RootN.wetlands.long <- list()
tick <- 1
for(i in levels(RootN.wetlands$ECO_NAME)){
  temp <- matrify(as.data.frame(RootN.wetlands) %>%
                    filter(ECO_NAME==i) %>% 
                    dplyr::select(PlotObservationID, species, Relative.cover)) 
  RootN.wetlands.long[[tick]] <- dematrify(temp, thresh=-1)
  tick <- tick + 1
}
RootN.wetlands.dematrified <- rlist::list.rbind(RootN.wetlands.long)
colnames(RootN.wetlands.dematrified) <- c("PlotID","species","Relative.cover")

#create grassland trait-env dataframe
colnames(RootN.wetlands)[1] <- "PlotID"
RootN.wetlands.trait.env <- RootN.wetlands.dematrified %>%
  mutate(PlotID=as.numeric(as.character(PlotID))) %>%
  left_join(env.sRoot %>% 
              dplyr::select(PlotID, Aridity_ind, N.est, BLDFIE, CLYPPT, CECSOL, bio06, bio12, SLTPPT,PHIHOX,ORCDRC,bio01), by="PlotID") %>%
  left_join(root_traits %>%
              mutate(species=as.character(species)) %>%
              dplyr::select(species, RootN), by="species") %>%
  left_join(header.sRoot %>%
              dplyr::select(PlotID, CONTINENT, ECO_NAME, POINT_X, POINT_Y), by="PlotID")

#convert positive values to 1
RootN.wetlands.trait.env <- RootN.wetlands.trait.env  %>%
  mutate(pres = (Relative.cover>0)*1) %>%
  mutate(Aridity_ind = Aridity_ind/10000) %>%
  mutate(PHIHOX = PHIHOX/10)

#log and scale traits that need transformation
RootN.wetlands.trait.env$RootN <- scale(log(RootN.wetlands.trait.env$RootN)) # log
RootN.wetlands.trait.env$BLDFIE <- scale(RootN.wetlands.trait.env$BLDFIE)
RootN.wetlands.trait.env$CLYPPT <- scale(RootN.wetlands.trait.env$CLYPPT)
RootN.wetlands.trait.env$N.est <- scale(RootN.wetlands.trait.env$N.est)
RootN.wetlands.trait.env$Aridity_ind <- scale(log(RootN.wetlands.trait.env$Aridity_ind)) # log
RootN.wetlands.trait.env$CECSOL <- scale(log(RootN.wetlands.trait.env$CECSOL)) # log
RootN.wetlands.trait.env$bio06 <- scale(RootN.wetlands.trait.env$bio06)
RootN.wetlands.trait.env$bio12 <- scale(RootN.wetlands.trait.env$bio12)
RootN.wetlands.trait.env$SLTPPT <- scale(RootN.wetlands.trait.env$SLTPPT)
RootN.wetlands.trait.env$PHIHOX <- scale(RootN.wetlands.trait.env$PHIHOX)
RootN.wetlands.trait.env$ORCDRC <- scale(log(RootN.wetlands.trait.env$ORCDRC)) #log
RootN.wetlands.trait.env$bio01 <- scale(RootN.wetlands.trait.env$bio01)
colnames(RootN.wetlands.trait.env)[4:14] <- c("r2pet","n","bulkd","clay", "cec","tmin","map","silt","ph","org","mat")

RootN.wetlands.trait.env$tmin2 <- scale((RootN.wetlands.trait.env$tmin-mean(RootN.wetlands.trait.env$tmin))^2)
RootN.wetlands.trait.env$r2pet2 <- scale((RootN.wetlands.trait.env$r2pet-mean(RootN.wetlands.trait.env$r2pet, na.rm=TRUE))^2)

RootN.wetlands.trait.env <- RootN.wetlands.trait.env %>% group_by(ECO_NAME) %>% filter(n()>300)
save(RootN.forests.trait.env, RootN.grasslands.trait.env, RootN.wetlands.trait.env, file = "RootN.env.RData")
```




### RD in forest
```{r}

# Select plots that have sufficient RD trait coverage (>0.8) and richness > 1
RD.sRoot <- CWM.sRoot %>%
  filter(variable == "RD") %>%
  filter(trait.nspecies > 1) 

RD.forests <- DT.forests %>%
  filter(PlotObservationID %in% RD.sRoot$PlotObservationID)

#add trait
colnames(root_traits)[1] <- "species"
RD.forests <- RD.forests %>%
  left_join(root_traits %>% 
              dplyr::select(species, RD), by="species") %>%
  drop_na(RD)

# matrify and dematrify
RD.forests <- droplevels(RD.forests) #drop unused levels

RD.forests.long <- list()
tick <- 1
for(i in levels(RD.forests$ECO_NAME)){
  temp <- matrify(as.data.frame(RD.forests) %>%
                    filter(ECO_NAME==i) %>% 
                    dplyr::select(PlotObservationID, species, Relative.cover)) 
  RD.forests.long[[tick]] <- dematrify(temp, thresh=-1)
  tick <- tick + 1
}
RD.forests.dematrified <- rlist::list.rbind(RD.forests.long)
colnames(RD.forests.dematrified) <- c("PlotID","species","Relative.cover")

#create grassland trait-env dataframe
colnames(RD.forests)[1] <- "PlotID"
RD.forests.trait.env <- RD.forests.dematrified %>%
  mutate(PlotID=as.numeric(as.character(PlotID))) %>%
  left_join(env.sRoot %>% 
              dplyr::select(PlotID, Aridity_ind, N.est, BLDFIE, CLYPPT, CECSOL, bio06, bio12, SLTPPT,PHIHOX,ORCDRC,bio01), by="PlotID") %>%
  left_join(root_traits %>%
              mutate(species=as.character(species)) %>%
              dplyr::select(species, RD), by="species") %>%
  left_join(header.sRoot %>%
              dplyr::select(PlotID, CONTINENT, ECO_NAME, POINT_X, POINT_Y), by="PlotID")

#convert positive RA values to 1 (present)
RD.forests.trait.env <- RD.forests.trait.env  %>%
  mutate(pres = (Relative.cover>0)*1) %>%
  mutate(Aridity_ind = Aridity_ind/10000) %>%
  mutate(PHIHOX = PHIHOX/10)

#log and scale traits that need transformation
RD.forests.trait.env$RD <- scale(log(RD.forests.trait.env$RD)) # log
RD.forests.trait.env$BLDFIE <- scale(RD.forests.trait.env$BLDFIE)
RD.forests.trait.env$CLYPPT <- scale(RD.forests.trait.env$CLYPPT)
RD.forests.trait.env$N.est <- scale(RD.forests.trait.env$N.est)
RD.forests.trait.env$Aridity_ind <- scale(log(RD.forests.trait.env$Aridity_ind)) # log
RD.forests.trait.env$CECSOL <- scale(log(RD.forests.trait.env$CECSOL)) # log
RD.forests.trait.env$bio06 <- scale(RD.forests.trait.env$bio06)
RD.forests.trait.env$bio12 <- scale(RD.forests.trait.env$bio12)
RD.forests.trait.env$SLTPPT <- scale(RD.forests.trait.env$SLTPPT)
RD.forests.trait.env$PHIHOX <- scale(RD.forests.trait.env$PHIHOX)
RD.forests.trait.env$ORCDRC <- scale(log(RD.forests.trait.env$ORCDRC)) #log
RD.forests.trait.env$bio01 <- scale(RD.forests.trait.env$bio01)
colnames(RD.forests.trait.env)[4:14] <- c("r2pet","n","bulkd","clay", "cec","tmin","map","silt","ph","org","mat")

RD.forests.trait.env$tmin2 <- scale((RD.forests.trait.env$tmin-mean(RD.forests.trait.env$tmin))^2)
RD.forests.trait.env$r2pet2 <- scale((RD.forests.trait.env$r2pet-mean(RD.forests.trait.env$r2pet, na.rm=TRUE))^2)

RD.forests.trait.env <- RD.forests.trait.env %>% group_by(ECO_NAME) %>% filter(n()>300)
#save(RD.forests.trait.env, file = "RD.forests.trait.env.RData")

```


### RD in grasslands
```{r}

# Select plots that have sufficient RD trait coverage (>0.8) and richness > 1
RD.sRoot <- CWM.sRoot %>%
  filter(variable == "RD") %>%
  filter(trait.nspecies > 1) 

RD.grasslands <- DT.grasslands %>%
  filter(PlotObservationID %in% RD.sRoot$PlotObservationID)

#add trait
colnames(root_traits)[1] <- "species"
RD.grasslands <- RD.grasslands %>%
  left_join(root_traits %>% 
              dplyr::select(species, RD), by="species") %>%
  drop_na(RD)

# matrify and dematrify
RD.grasslands <- droplevels(RD.grasslands) #drop unused levels

RD.grasslands.long <- list()
tick <- 1
for(i in levels(RD.grasslands$ECO_NAME)){
  temp <- matrify(as.data.frame(RD.grasslands) %>%
                    filter(ECO_NAME==i) %>% 
                    dplyr::select(PlotObservationID, species, Relative.cover)) 
  RD.grasslands.long[[tick]] <- dematrify(temp, thresh=-1)
  tick <- tick + 1
}
RD.grasslands.dematrified <- rlist::list.rbind(RD.grasslands.long)
colnames(RD.grasslands.dematrified) <- c("PlotID","species","Relative.cover")

#create grassland trait-env dataframe
colnames(RD.grasslands)[1] <- "PlotID"
RD.grasslands.trait.env <- RD.grasslands.dematrified %>%
  mutate(PlotID=as.numeric(as.character(PlotID))) %>%
  left_join(env.sRoot %>% 
              dplyr::select(PlotID, Aridity_ind, N.est, BLDFIE, CLYPPT, CECSOL, bio06, bio12, SLTPPT,PHIHOX,ORCDRC,bio01), by="PlotID") %>%
  left_join(root_traits %>%
              mutate(species=as.character(species)) %>%
              dplyr::select(species, RD), by="species") %>%
  left_join(header.sRoot %>%
              dplyr::select(PlotID, CONTINENT, ECO_NAME, POINT_X, POINT_Y), by="PlotID")

#convert positive RA values to 1 (present)
RD.grasslands.trait.env <- RD.grasslands.trait.env  %>%
  mutate(pres = (Relative.cover>0)*1) %>%
  mutate(Aridity_ind = Aridity_ind/10000) %>%
  mutate(PHIHOX = PHIHOX/10)

#log and scale traits that need transformation
RD.grasslands.trait.env$RD <- scale(log(RD.grasslands.trait.env$RD)) # log
RD.grasslands.trait.env$BLDFIE <- scale(RD.grasslands.trait.env$BLDFIE)
RD.grasslands.trait.env$CLYPPT <- scale(RD.grasslands.trait.env$CLYPPT)
RD.grasslands.trait.env$N.est <- scale(RD.grasslands.trait.env$N.est)
RD.grasslands.trait.env$Aridity_ind <- scale(log(RD.grasslands.trait.env$Aridity_ind)) # log
RD.grasslands.trait.env$CECSOL <- scale(log(RD.grasslands.trait.env$CECSOL)) # log
RD.grasslands.trait.env$bio06 <- scale(RD.grasslands.trait.env$bio06)
RD.grasslands.trait.env$bio12 <- scale(RD.grasslands.trait.env$bio12)
RD.grasslands.trait.env$SLTPPT <- scale(RD.grasslands.trait.env$SLTPPT)
RD.grasslands.trait.env$PHIHOX <- scale(RD.grasslands.trait.env$PHIHOX)
RD.grasslands.trait.env$ORCDRC <- scale(log(RD.grasslands.trait.env$ORCDRC)) #log
RD.grasslands.trait.env$bio01 <- scale(RD.grasslands.trait.env$bio01)
colnames(RD.grasslands.trait.env)[4:14] <- c("r2pet","n","bulkd","clay", "cec","tmin","map","silt","ph","org","mat")

RD.grasslands.trait.env$tmin2 <- scale((RD.grasslands.trait.env$tmin-mean(RD.grasslands.trait.env$tmin))^2)
RD.grasslands.trait.env$r2pet2 <- scale((RD.grasslands.trait.env$r2pet-mean(RD.grasslands.trait.env$r2pet, na.rm=TRUE))^2)

RD.grasslands.trait.env <- RD.grasslands.trait.env %>% group_by(ECO_NAME) %>% filter(n()>300)
#save(RD.grasslands.trait.env, file = "RD.grasslands.trait.env.RData")

```


### RD in wetlands
```{r}

# Select plots that have sufficient RD trait coverage (>0.8) and richness > 1
RD.sRoot <- CWM.sRoot %>%
  filter(variable == "RD") %>%
  filter(trait.nspecies > 1) 

RD.wetlands <- DT.wetlands %>%
  filter(PlotObservationID %in% RD.sRoot$PlotObservationID)

#add trait
colnames(root_traits)[1] <- "species"
RD.wetlands <- RD.wetlands %>%
  left_join(root_traits %>% 
              dplyr::select(species, RD), by="species") %>%
  drop_na(RD)

# matrify and dematrify
RD.wetlands <- droplevels(RD.wetlands) #drop unused levels

RD.wetlands.long <- list()
tick <- 1
for(i in levels(RD.wetlands$ECO_NAME)){
  temp <- matrify(as.data.frame(RD.wetlands) %>%
                    filter(ECO_NAME==i) %>% 
                    dplyr::select(PlotObservationID, species, Relative.cover)) 
  RD.wetlands.long[[tick]] <- dematrify(temp, thresh=-1)
  tick <- tick + 1
}
RD.wetlands.dematrified <- rlist::list.rbind(RD.wetlands.long)
colnames(RD.wetlands.dematrified) <- c("PlotID","species","Relative.cover")

#create grassland trait-env dataframe
colnames(RD.wetlands)[1] <- "PlotID"
RD.wetlands.trait.env <- RD.wetlands.dematrified %>%
  mutate(PlotID=as.numeric(as.character(PlotID))) %>%
  left_join(env.sRoot %>% 
              dplyr::select(PlotID, Aridity_ind, N.est, BLDFIE, CLYPPT, CECSOL, bio06, bio12, SLTPPT,PHIHOX,ORCDRC,bio01), by="PlotID") %>%
  left_join(root_traits %>%
              mutate(species=as.character(species)) %>%
              dplyr::select(species, RD), by="species") %>%
  left_join(header.sRoot %>%
              dplyr::select(PlotID, CONTINENT, ECO_NAME, POINT_X, POINT_Y), by="PlotID")

#convert positive RA values to 1 (present)
RD.wetlands.trait.env <- RD.wetlands.trait.env  %>%
  mutate(pres = (Relative.cover>0)*1) %>%
  mutate(Aridity_ind = Aridity_ind/10000) %>%
  mutate(PHIHOX = PHIHOX/10)

#log and scale traits that need transformation
RD.wetlands.trait.env$RD <- scale(log(RD.wetlands.trait.env$RD)) # log
RD.wetlands.trait.env$BLDFIE <- scale(RD.wetlands.trait.env$BLDFIE)
RD.wetlands.trait.env$CLYPPT <- scale(RD.wetlands.trait.env$CLYPPT)
RD.wetlands.trait.env$N.est <- scale(RD.wetlands.trait.env$N.est)
RD.wetlands.trait.env$Aridity_ind <- scale(log(RD.wetlands.trait.env$Aridity_ind)) # log
RD.wetlands.trait.env$CECSOL <- scale(log(RD.wetlands.trait.env$CECSOL)) # log
RD.wetlands.trait.env$bio06 <- scale(RD.wetlands.trait.env$bio06)
RD.wetlands.trait.env$bio12 <- scale(RD.wetlands.trait.env$bio12)
RD.wetlands.trait.env$SLTPPT <- scale(RD.wetlands.trait.env$SLTPPT)
RD.wetlands.trait.env$PHIHOX <- scale(RD.wetlands.trait.env$PHIHOX)
RD.wetlands.trait.env$ORCDRC <- scale(log(RD.wetlands.trait.env$ORCDRC)) #log
RD.wetlands.trait.env$bio01 <- scale(RD.wetlands.trait.env$bio01)
colnames(RD.wetlands.trait.env)[4:14] <- c("r2pet","n","bulkd","clay", "cec","tmin","map","silt","ph","org","mat")

RD.wetlands.trait.env$tmin2 <- scale((RD.wetlands.trait.env$tmin-mean(RD.wetlands.trait.env$tmin))^2)
RD.wetlands.trait.env$r2pet2 <- scale((RD.wetlands.trait.env$r2pet-mean(RD.wetlands.trait.env$r2pet, na.rm=TRUE))^2)

RD.wetlands.trait.env <- RD.wetlands.trait.env %>% group_by(ECO_NAME) %>% filter(n()>300)
save(RD.forests.trait.env, RD.grasslands.trait.env, RD.wetlands.trait.env, file = "RD.env.RData")

```

