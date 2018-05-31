# LDA on the knew dataset
require(MASS)

lda.df0 = knew.molten %>%
  filter(NORMD=='yes') %>%
  filter(BRIGHTNESS=='total') %>%
  filter(CHANNEL=='ArcIHC') 
lda.df0 = lda.df0[c(1,10,20:21, 25:26)]


lda.df1 = dcast(lda.df0, GRAIN+ID ~ SubROI_SubroiAGGR, value.var="value", 
                fun.aggregate = mean)
lda.df1[,1] = as.factor(lda.df1[,1])
lda.df1[,2] = as.factor(lda.df1[,2 ])

r = lda(formula = GRAIN ~ .,
        data = lda.df1)
# r2 = lda(formula = grain ~ AREAmm2, DAPI, H2BGFP, GFAP, ArcIHC, OL, NormOL,
#          data = kk, CV=T)
prop = r$svd^2/sum(r$svd^2)

plda = predict(object = r, newdata = lda.df0)
head(plda)
dataset = data.frame(grain = raw$grain, 
                     subexpt = raw$SUBEXPT, # will facet by AA/AB
                     expt = raw$EXPT, 
                     group = raw$GROUP, # will facet by age
                     lda = plda$x)

library(ggplot2)

# compares young vs old
ggplot(dataset) + 
  aes(lda.LD1, lda.LD2, color = grain) +
  #geom_point() + 
  xlim(-4,4) + ylim(-3,3) +
  stat_ellipse() + facet_wrap(~group)


