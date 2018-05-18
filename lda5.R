

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

lda.roi_channel.batch = dcast(kk.long2, BATCH + ID ~ roi_channel, value.var="DENSITY")

# shuffle batch
# temp = lda.roi_channel.batch
# lda.roi_channel.batch$BATCH = sample(lda.roi_channel.batch$BATCH)
# lda.roi_channel.batch = temp

roi_channel.batch = lda(formula = BATCH ~ ., data = lda.roi_channel)
prop.roi_channel.batch = roi_channel.batch$svd^2/sum(roi_channel.batch$svd^2)
plda.roi_channel.batch = predict(object = roi_channel.batch, newdata = lda.roi_channel.batch)
head(plda.roi_channel)
dataset.roi_channel.batch = data.frame(BATCH = lda.roi_channel.batch$BATCH, lda = plda.roi_channel.batch$x)
ggplot(dataset.roi_channel.batch) + aes(lda.LD1, lda.LD2, color=BATCH, label=BATCH) + 
  stat_ellipse() + geom_text() + 
  ggtitle("LDA for all by BATCH")


# but the question is whether they are different in the DAPI channel
# i wonder if i can make function that does this


# what inputs do you need? 1) df, 2) grouping, 3) variables 4) filter
make_lda = function(df, grouping, variables) {
  
  paste.grouping = paste(grouping, "+ ID")
  df = dcast(df, paste.grouping ~ variables, value.var="DENSITY")
  lda  = lda(formula = grouping ~., data = df)
  prop = lda$svd^2/sum(lda$svd^2)
  plda = predict(object = lda, newdata = df)
  dataset = data.frame(grouping = lda[grouping], lda = plda$x)
  
  ggplot(dataset) + aes(lda.LD1, lda.LD2, color = grouping, label = grouping) + stat_ellipse() + geom_text()
  
}
make_lda(kk.long2, "BATCH", "roi_channel")

# question if i shuffle across the batches will it also be separate like this?


