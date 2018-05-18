
# The purpose of this script
# is to find out what the expected OL increase is
# versus the actual OL increase
# because we think that the actual OL increase
# is greater than the theoretical increase
# based on the H2B and IHC increases. 


test = kk.long %>% 
  group_by(CHANNEL, GROUP, VALENCE, CONTEXT, EXPT) %>%
  summarize (mean = mean(DENSITY))
test

test2 = dcast(test, CHANNEL+GROUP+VALENCE+CONTEXT ~ EXPT, 
              value.var = "mean",
              fun.aggregate = mean)
test2$fold = test2$OLD/test2$`_YOUNG`

test3 = dcast(test2, GROUP+VALENCE+CONTEXT ~ CHANNEL, 
              value.var="fold", fun.aggregate = mean)
test3$expected = test3$H2BGFP * test3$ArcIHC
test3$fold_actual = test3$OL/test3$expected
test3$fold_actual_norm = test3$NormOL/test3$expected
test3
