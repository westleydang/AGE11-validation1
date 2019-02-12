
library(PredPsych)

# trying to do LDA with cross validation

data.fl = data.for.lda %>%
  mutate(factor = case_when(
    GRAIN == 'AGE 10 SHOCK' ~ "y-shk",
    GRAIN == 'AGE 11 SHOCK' ~ "o-shk",
    GRAIN %in% c('AGE 10 CONTEXT', 'AGE 11 CONTEXT') ~ "else"
    
  ))

data.fl = data.for.lda %>%
  mutate(factor = GRAIN)
)

LDAModel <- LinearDA(Data = data.fl, classCol = 17, 
                     selectedCols = c(2:15,17), cvType="holdout",
                     cvFraction = 0.5)

PermutationResult <- ClassPerm(Data = data.fl, classCol = 17, 
                               selectedCols = c(2:15,17), nSims = 100, cvType = "holdout",
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
