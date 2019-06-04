
library(PredPsych)

# trying to do LDA with cross validation

data.for.lda = gfp %>%
  ddply(.(System), transform, z = scale(weighted)) %>%
  dcast(ID ~ System, value.var="z", fun.aggregate = mean, .drop=T) 

data.for.lda = merge(data.for.lda, animal_details %>% 
                       # filter(EXPT=='OLD') %>%
                       mutate(GRAIN = paste(EXPT, VALENCE)) %>% 
                       dplyr::select(ID, GRAIN), by="ID")

# remove spaces? 
names(data.for.lda) = gsub(" ", "_", names(data.for.lda))
names(data.for.lda) = gsub("-", ".", names(data.for.lda))

# impute data
data.for.lda = mice::complete(mice(data.for.lda, m=5, seed=25))

data.fl = data.for.lda %>%
  mutate(factor = case_when(
    GRAIN == 'AGE 10 SHOCK' ~ "y-shk",
    GRAIN == 'AGE 11 SHOCK' ~ "o-shk",
    GRAIN %in% c('AGE 10 CONTEXT', 'AGE 11 CONTEXT') ~ "else"
  ))

data.fl = data.for.lda %>%
  mutate(factor = GRAIN)


LDAModel <- LinearDA(Data = data.fl, classCol = 17, 
                     selectedCols = c(2:15,17), cvType="holdout",
                     cvFraction = 0.75)

PermutationResult <- ClassPerm(Data = data.fl, classCol = 17, 
                               selectedCols = c(2:17), nSims = 100, cvType = "holdout",
                               cvFraction = 0.80)


data.fl2 = data.for.lda %>%
  filter(GRAIN %in% c('AGE 10 SHOCK', 'AGE 10 CONTEXT'))
PermutationResult <- ClassPerm(Data = data.fl2, classCol = 16, 
                               selectedCols = c(2:16), nSims = 50, cvType = "holdout",
                               cvFraction = 0.75)

data.fl2 = data.for.lda %>%
  filter(GRAIN %in% c('AGE 11 SHOCK', 'AGE 11 CONTEXT'))
PermutationResult <- ClassPerm(Data = data.fl2, classCol = 16, 
                               selectedCols = c(2:16), nSims = 50, cvType = "holdout",
                               cvFraction = 0.75)

data.fl2 = data.for.lda %>%
  filter(GRAIN %in% c('AGE 11 SHOCK', 'AGE 10 SHOCK'))
PermutationResult <- ClassPerm(Data = data.fl2, classCol = 16, 
                               selectedCols = c(2:16), nSims = 50, cvType = "holdout",
                               cvFraction = 0.75)
