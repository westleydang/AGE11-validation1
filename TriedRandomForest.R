


# let's try randomforest classifier

library(randomForest)

#training Sample with 300 observations
train=sample(1:nrow(a),40)


a
sys.weighted.cast = sys.weighted %>%
  merge(., animal_details %>% dplyr::select(FREEZE, ID), by='ID') %>%
  filter(VALENCE == 'SHOCK') %>%
  filter(!CHANNEL %in% c('DAPI')) %>%
  dcast(CHANNEL + ID + EXPT + VALENCE + CONTEXT + FREEZE ~ System, value.var="boxed")
arc.cast = sys.weighted.cast %>% filter(CHANNEL=='ArcIHC')
a = complete(mice(arc.cast, m=5, seed=50))

names(a) = gsub(" ", "_", names(a))
names(a) = gsub("-", ".", names(a))

a.rf = randomForest(FREEZE ~ ., a[,6:21], subset = train)
plot(a.rf)

oob.err=double(13)
test.err=double(13)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:13) 
{
  rf=randomForest(FREEZE ~ . , a[,6:21] , subset = train, mtry=mtry, ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,a[,6:21][-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(a[,6:21][-train,], mean( (FREEZE - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}
