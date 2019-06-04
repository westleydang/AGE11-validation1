# get_lda = function(df, dfname) {
  data.for.lda = nol %>%
    ddply(.(ID), transform, z = scale(weighted)) %>%
    dcast(ID ~ System, value.var="z", fun.aggregate = mean) 
  
  data.for.lda = merge(data.for.lda, animal_details %>% aged() %>%
                         mutate(GRAIN = paste(EXPT, VALENCE)) %>% dplyr::select(ID, GRAIN), by="ID")
  
  # remove spaces? 
  names(data.for.lda) = gsub(" ", "_", names(data.for.lda))
  names(data.for.lda) = gsub("-", ".", names(data.for.lda))
  
  # impute missing data
  data.for.lda = mice::complete(mice(data.for.lda, m=5, seed=25))
  
  # lda
  r <- lda(formula = GRAIN ~ ., 
           data = data.for.lda[,2:ncol(data.for.lda)], 
           prior = c(1,1,1,1)/4)
    
  prop = r$svd^2/sum(r$svd^2)
  
  plda = predict(object = r, # predictions
                 newdata = data.for.lda)
  print(plda$class)
  predict.table = matrix(plda$class, data.for.lda$GRAIN)
  print(predict.table)
  
  # change predict.table to percentage
  predict.table
  
  dataset = data.frame(GRAIN = data.for.lda[,"GRAIN"],
                       lda = plda$x) 
  
  ###
  
  a = as.data.frame(predict.table)
  a2 = prop.table(predict.table,1)
  melt(a2)
  ggplot(melt(a2)) + aes(x=Var1, y=Var2, fill=value) + geom_tile() + scale_fill_continuous(limits=c(0, 1)) 
  
  
  
  a %>% mutate_all
  