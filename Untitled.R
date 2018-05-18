# LDA on the raw dataset

other.variables = (names(raw[c(2,17:28, 34:53)]))

lda.df = raw[c(55,17:28, 34:53)]
                   
raw$grain = paste(raw$GROUP, raw$EXPT)

r = lda(formula = grain ~ .,
        data = raw)
r2 = lda(formula = grain ~ AREAmm2, DAPI, H2BGFP, GFAP, ArcIHC, OL, NormOL,
         data = kk, CV=T)

prop = r$svd^2/sum(r$svd^2)

plda = predict(object = r, newdata = kk)

head(plda)

dataset = data.frame(grain = kk$grain, 
                     valence = kk$VALENCE,
                     context = kk$CONTEXT,
                     subexpt = kk$SUBEXPT,
                     group = kk$GROUP,
                     lda = plda$x)

ggplot(dataset) + 
  aes(lda.LD1, lda.LD2, color = grain, alpha = 0.1) +
  #geom_point() + 
  xlim(-4,4) + ylim(-3,3) +
  stat_ellipse()

