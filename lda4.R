

# one big LDA for all the (channels x subroi)
# each (ch x subroi) is a concatenated variable

library(reshape2)



kk.long2 = kk.long
kk.long2$GRAIN = paste(kk.long2$GROUP, kk.long2$EXPT)
kk.long2$roi_channel = paste(kk.long2$SUBROI, kk.long2$CHANNEL)

# test just the arc channel

storage.kklong2 = kk.long2
library(dplyr)
library(stringr)
kk.long2 = kk.long2 %>% filter(str_detect(CHANNEL, "ArcIHC"))



lda.roi_channel = dcast(kk.long2, GRAIN + ID ~ roi_channel, value.var="DENSITY")

roi_channel = lda(formula = GRAIN ~ .,
                      data = lda.roi_channel)

prop.roi_channel = roi_channel$svd^2/sum(roi_channel$svd^2)
plda.roi_channel = predict(object = roi_channel, newdata = lda.roi_channel)
head(plda.roi_channel)
dataset.roi_channel = data.frame(GRAIN = lda.roi_channel$GRAIN, lda = plda.roi_channel$x)
ggplot(dataset.roi_channel) + aes(lda.LD1, lda.LD2, color=GRAIN, label=GRAIN) + 
  stat_ellipse() + geom_text() + 
  ggtitle("LDA for all")