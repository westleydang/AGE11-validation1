# do an LDA for each channel, for each region

library(reshape2)

kk$GRAIN = paste(kk$GROUP, kk$EXPT)

# we have to cast the kk data into different dataframes
# and then widen the subroi variable
# split kk into different channels for lda
lda.dapi = dcast(kk, GRAIN + ID ~ SUBROI, value.var = "DAPI")
lda.gfp = dcast(kk, GRAIN + ID ~ SUBROI, value.var = "H2BGFP")
lda.gfap = dcast(kk, GRAIN + ID ~ SUBROI, value.var = "GFAP")
lda.arc = dcast(kk, GRAIN + ID ~ SUBROI, value.var = "ArcIHC")
lda.ol = dcast(kk, GRAIN + ID ~ SUBROI, value.var = "OL")
lda.olnorm = dcast(kk, GRAIN + ID ~ SUBROI, value.var = "NormOL")


# make the lda for dapi
dapi = lda(formula = GRAIN ~ ., data = lda.dapi)
prop.dapi = dapi$svd^2/sum(dapi$svd^2)
plda.dapi = predict(object = dapi, newdata = lda.dapi)
head(plda.dapi)
dataset.dapi = data.frame(GRAIN = lda.dapi$GRAIN, lda = plda.dapi$x)
ggplot(dataset.dapi) + aes(lda.LD1, lda.LD2, color=GRAIN, label=GRAIN) + 
  stat_ellipse() + geom_text() + 
  ggtitle("LDA for DAPI")

# make the lda for arc
arc = lda(formula = GRAIN ~., data = lda.arc)
prop.arc = arc$svd^2 / sum(arc$svd^2)
plda.arc = predict(object = arc, newdata = lda.arc)
head(plda.arc)
dataset.arc = data.frame(GRAIN = lda.arc$GRAIN, lda = plda.arc$x)
# visualize
ggplot(dataset.arc) + aes(lda.LD1, lda.LD2, color=GRAIN, label=GRAIN) + 
  stat_ellipse() + geom_text() + 
  ggtitle("LDA for ARC")

# make the lda for gfp
gfp = lda(formula = GRAIN ~., data = lda.gfp)
prop.gfp = gfp$svd^2 / sum(gfp$svd^2)
plda.gfp = predict(object = gfp, newdata = lda.gfp)
dataset.gfp = data.frame(GRAIN = lda.gfp$GRAIN, lda = plda.gfp$x)
# visualize
ggplot(dataset.gfp) + aes(lda.LD1, lda.LD2, color=GRAIN, label=GRAIN) + 
  stat_ellipse() + geom_text() + 
  ggtitle("LDA for H2BGFP")


# make the lda for gfap
gfap = lda(formula = GRAIN ~., data = lda.gfap)
prop.gfap = gfap$svd^2 / sum(gfap$svd^2)
plda.gfap = predict(object = gfap, newdata = lda.gfap)
dataset.gfap = data.frame(GRAIN = lda.gfap$GRAIN, lda = plda.gfap$x)
# visualize
ggplot(dataset.gfap) + aes(lda.LD1, lda.LD2, color=GRAIN, label=GRAIN) + 
  stat_ellipse() + geom_text() + 
  ggtitle("LDA for GFAP")


# make the lda for ol
ol = lda(formula = GRAIN ~., data = lda.ol)
prop.ol = ol$svd^2 / sum(ol$svd^2)
plda.ol = predict(object = ol, newdata = lda.ol)
dataset.ol = data.frame(GRAIN = lda.ol$GRAIN, lda = plda.ol$x)
# visualize
ggplot(dataset.ol) + aes(lda.LD1, lda.LD2, color=GRAIN, label=GRAIN) + 
  stat_ellipse() + geom_text() + 
  ggtitle("LDA for OL")

# make the lda for olnorm
olnorm = lda(formula = GRAIN ~., data = lda.olnorm)
prop.olnorm = olnorm$svd^2 / sum(olnorm$svd^2)
plda.olnorm = predict(object = olnorm, newdata = lda.olnorm)
dataset.olnorm = data.frame(GRAIN = lda.olnorm$GRAIN, lda = plda.olnorm$x)
# visualize
ggplot(dataset.olnorm) + aes(lda.LD1, lda.LD2, color=GRAIN, label=GRAIN) + 
  stat_ellipse() + geom_text() + 
  ggtitle("LDA for OL.norm")



