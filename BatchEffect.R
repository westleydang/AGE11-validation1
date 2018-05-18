

# TRYING TO CORRECT FOR BATCH EFFECT


source("LoadAndProcess.R")

a1 = kk.long
a2 = dcast(kk.long, ID+SUBROI~CHANNEL,
           value.var = "DENSITY", fun.aggregate = mean)

kk2 = kk


ggplot()

# Do AREAS differ?
ggplot(kk2) + aes(BATCH, AREAmm2, color=BATCH) + 
  geom_jitter() +
  facet_wrap(~SUBROI, scales="free")

# How does DAPI/Area look?
ggplot(kk) + aes(DAPI, AREAmm2, color=BATCH) + geom_jitter() + 
  facet_wrap(~SUBROI, scales="free") + 
  ggtitle("DAPI densities for each ROI, by batch")



kk2$nH2B = kk2$H2BGFP / kk$DAPI
kk2$nGFAP = kk2$GFAP / kk$DAPI
kk2$nArcIHC = kk2$ArcIHC / kk$DAPI
kk2$nOL = kk2$OL / kk2$DAPI
kk2$nNormOL = kk2$OL / kk2$DAPI


kk.longTOdapi = melt(kk2[,-(16:21)], id.vars=names(kk[c(1:15,22)]), variable.name = "CHANNEL", value.name="DENSITY")


kkl.collapseROI2 = kk.longTOdapi %>%
  group_by(CHANNEL, SUBROI, ID) %>%
  summarize(sem = std.error(DENSITY), mean_density = mean(DENSITY), N = length(DENSITY),
            ymin = mean_density-sem, ymax = mean_density+sem)
kkl.collapseROI2

ggplot(kkl.collapseROI) + aes(GROUP, mean_density, fill=EXPT, color = EXPT) + 
  facet_wrap(~CHANNEL, scales="free") +
  stat_summary(fun.y = mean, geom="point") +
  stat_summary(fun.data = mean_se, geom="linerange") +
  ggtitle("Comparing densities per channel ~ group/age, where n = # animals ")

