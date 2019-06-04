

# trying to do lda with new imputation method, per group instead


data.for.lda = arc %>%
  dcast(ID ~ System, value.var="weighted", fun.aggregate = mean, .drop=T) 

# remove spaces? 
names(data.for.lda) = gsub(" ", "_", names(data.for.lda))
names(data.for.lda) = gsub("-", ".", names(data.for.lda))

data.for.lda = mice::complete(mice(data.for.lda, m=5, seed=25))
data.for.lda$ID = as.factor(data.for.lda$ID)

data.for.lda = data.for.lda %>%
  melt(., id = 'ID', variable.name = "System", value.name = "weighted")

data.for.lda = merge(data.for.lda, animal_details %>% 
                       # filter(EXPT=='OLD') %>%
                       mutate(GRAIN = paste(EXPT, VALENCE)) %>% 
                       dplyr::select(ID, GRAIN), by="ID") %>%
  ddply(.(System), transform, z = scale(weighted)) %>%
  dcast(ID ~ System, value.var="z", fun.aggregate = mean, .drop=T) 

