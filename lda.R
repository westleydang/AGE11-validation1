# LDA

r = lda(formula = grain ~ AREAmm2, DAPI, H2BGFP, GFAP, ArcIHC, OL, NormOL,
        data = kk)
r2 = lda(formula = grain ~ AREAmm2, DAPI, H2BGFP, GFAP, ArcIHC, OL, NormOL,
         data = kk, CV=T)

prop = r$svd^2/sum(r$svd^2)

plda = predict(object = r, newdata = kk)

head(plda)

dataset = data.frame(grain = kk$grain, 
                     lda = plda$x)

ggplot(dataset) + 
  aes(lda.LD1, lda.LD2, color = group, alpha = 0.1) +
  #geom_point() + 
  xlim(-4,4) + ylim(-3,3) +
  stat_ellipse() +
  ggtitle("lda with only the groups, and channels are dimensions")

