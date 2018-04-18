# LDA on the raw dataset
require(MASS)

raw$grain = paste(raw$GROUP, raw$EXPT)
lda.df = raw[c(17:28, 34:53, 55)]



r = lda(formula = grain ~ .,
        data = lda.df)
# r2 = lda(formula = grain ~ AREAmm2, DAPI, H2BGFP, GFAP, ArcIHC, OL, NormOL,
#          data = kk, CV=T)
prop = r$svd^2/sum(r$svd^2)

plda = predict(object = r, newdata = lda.df)

head(plda)

dataset = data.frame(grain = raw$grain, 
                     subexpt = raw$SUBEXPT, # will facet by AA/AB
                     expt = raw$EXPT, 
                     group = raw$GROUP, # will facet by age
                     lda = plda$x)

library(ggplot2)
ggplot(dataset) + 
  aes(lda.LD1, lda.LD2, color = grain) +
  #geom_point() + 
  xlim(-4,4) + ylim(-3,3) +
  stat_ellipse() + facet_wrap(~group)

