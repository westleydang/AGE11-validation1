

# Purpose of this script:
# Import the batch details as a variable BATCH
# Then normalize within the batches
# Then do the LDA and see the differences




# modified from "lda4.R"
# one big LDA for all the (channels x subroi)
# each (ch x subroi) is a concatenated variable

library(reshape2)

kk.long2 = kk.long
kk.long2$GRAIN = paste(kk.long2$GROUP, kk.long2$EXPT)

  batch_details = read.csv("batch_details.csv")
  colnames(batch_details) = c("GRAIN", "BATCH")
  kk.long2 = merge(kk.long2, batch_details, by="GRAIN")

kk.long2$roi_channel = paste(kk.long2$SUBROI, kk.long2$CHANNEL)

lda.roi_channel = dcast(kk.long2, BATCH + ID ~ roi_channel, value.var="DENSITY")

roi_channel = lda(formula = BATCH ~ .,
                  data = lda.roi_channel)

prop.roi_channel = roi_channel$svd^2/sum(roi_channel$svd^2)
plda.roi_channel = predict(object = roi_channel, newdata = lda.roi_channel)
head(plda.roi_channel)
dataset.roi_channel = data.frame(BATCH = lda.roi_channel$BATCH, lda = plda.roi_channel$x)
ggplot(dataset.roi_channel) + aes(lda.LD1, lda.LD2, color=BATCH, label=BATCH) + 
  stat_ellipse() + geom_text() + 
  ggtitle("LDA for all")
