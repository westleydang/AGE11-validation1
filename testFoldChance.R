
# INVESTIGATING FOLD CHANCE PER GROUP

knew.molten4 = knew.molten2 %>%
  filter(UNITS == "density") %>%
  filter(BRIGHTNESS == 'total')

a = dcast(knew.molten4, EXPT + GROUP + VALENCE + CONTEXT + ID + SubROI_SubroiAGGR + Pop_roiAreaMM2 ~ variable, 
          value.var = "value", fun.aggregate=mean) %>%
  mutate(gfp.rate = Ch2TotalperMM2/Ch1TotalperMM2,
         arc.rate = Ch4TotalperMM2/Ch1TotalperMM2,
         ol.rate = OLTotalperMM2/Ch1TotalperMM2,
         nol.rate = ol.rate/(gfp.rate + arc.rate - ol.rate),
         chance = gfp.rate*arc.rate,
         fold.chance = NormOLTotalperMM2/chance,
         fold.chance2 = nol.rate/chance,
         ol.per.gfp = ol.rate/gfp.rate,
         ol.per.arc = ol.rate/arc.rate) %>%
  mutate(fold.chance2 = ifelse(is.na(fold.chance2), 0, fold.chance2)) %>%
  mutate(ol.per.gfp = ifelse(is.na(ol.per.gfp), 0, ol.per.gfp)) %>%
  mutate(fold.chance2 = ifelse(is.infinite(fold.chance2), 0, fold.chance2)) %>%
  ddply(., .(EXPT, GROUP, VALENCE, CONTEXT, SubROI_SubroiAGGR, Pop_roiAreaMM2), summarize,
        fc1 = weighted.mean(fold.chance, Pop_roiAreaMM2),
        fc2 = weighted.mean(fold.chance2, Pop_roiAreaMM2),
        gfp = weighted.mean(gfp.rate, Pop_roiAreaMM2),
        arc = weighted.mean(arc.rate, Pop_roiAreaMM2),
        ol = weighted.mean(ol.rate, Pop_roiAreaMM2),
        nol = weighted.mean(nol.rate, Pop_roiAreaMM2),
        ol.per.gfp = weighted.mean(ol.per.gfp, Pop_roiAreaMM2),
        ol.per.arc = weighted.mean(ol.per.arc, Pop_roiAreaMM2)) 

a %>% 
  ggplot() + aes(GROUP, nol, color=EXPT) + stat_summary(fun.y = "mean", geom="point") +
  stat_summary(fun.data="mean_se", geom="errorbar") 

