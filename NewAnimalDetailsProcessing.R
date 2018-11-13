write.csv(pre.ad, "_pre-ad.csv")

pre.ad = read.csv('animal_details4-pre.csv') 

freeze_details = read.csv('freeze_details.csv')

pre.ad = merge(pre.ad, group_details, by="GROUP")

# Note: changing EXPT to YOUNG and OLD now
pre.ad$EXPT = ifelse(pre.ad$AGE > 60, "OLD", "YOUNG")

pre.ad = merge(pre.ad, freeze_details, by="ID")
write.csv(pre.ad, "animal_details4.csv")

