

data.for.lda = arc %>%
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

# try this new imputed
init = mice(data.for.lda, maxit=0) 
meth = init$method
predM = init$predictorMatrix

# remove ID from imputing
predM[, c("ID")] = 0
mice.imputed = mice(data.for.lda, method=meth, predictorMatrix=predM, m=5)
data.for.lda = mice::complete(mice.imputed)


r <- lda(formula = GRAIN ~ ., 
         data = data.for.lda[,2:length(data.for.lda)], 
         prior = c(1,1,1,1)/4)

prop = r$svd^2/sum(r$svd^2)

plda = predict(object = r, # predictions
               newdata = data.for.lda)

dl <- data.frame(varnames=rownames(coef(r)), coef(r)) # lda scalings
dl$length <- with(dl, sqrt(LD1^2+LD2^2))


plda$class

dataset = data.frame(GRAIN = data.for.lda[,"GRAIN"],
                     lda = plda$x)


scaling_text = geom_text(data=dl,
                         aes(x=LD1, y=LD2,
                             label=varnames, 
                             linetype=NULL,
                             alpha=length, position="identity"),
                         size = 4, vjust=.5,
                         hjust=0, color="red")

scaling_arrows =   geom_segment(data=dl,
                                aes(x=0, y=0,
                                    xend=LD1, yend=LD2,
                                    linetype=NULL,
                                    alpha=length),
                                arrow=arrow(length=unit(0.1,"mm")),
                                color="red") 

p1 <- ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2, colour = GRAIN, shape = GRAIN), size = 2.5) + 
  labs(x = paste("LD1 (", (prop[1]), ")", sep=""),
       y = paste("LD2 (", (prop[2]), ")", sep="")) + 
  ggtitle("LDA for each age and conditioning") + theme_minimal() + 
  scaling_text + scaling_arrows + 
  stat_ellipse(aes(x=lda.LD1, y=lda.LD2, fill = GRAIN), alpha = 0.2, geom = "polygon")

p1


# GUIDE: https://towardsdatascience.com/linear-discriminant-analysis-lda-101-using-r-6a97217a55a6


sample.size.raw = floor(0.50 *nrow(data.for.lda[,2:17]))
train_ind_raw = sample(nrow(data.for.lda[,2:17]), size=sample.size.raw)
train_raw.df = data.for.lda[train_ind_raw,2:17]
test_raw.df = data.for.lda[-train_ind_raw,2:17]

# get formula
f <- paste(names(train_raw.df)[16], "~", paste(names(train_raw.df)[-16], collapse=" + "))

another.lda = lda(as.formula(paste(f)), data=train_raw.df)

another.lda.predict = predict(another.lda, newdata = test_raw.df)


### CONSTRUCTING ROC AUC PLOT:
# Get the posteriors as a dataframe.
another.lda.predict.posteriors <- as.data.frame(another.lda.predict$posterior)

# Evaluate the model
pred <- prediction(another.lda.predict.posteriors[,2], test_raw.df$GRAIN)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
# Plot
plot(roc.perf)
abline(a=0, b= 1)
text(x = .25, y = .65 ,paste("AUC = ", round(auc.train[[1]],3), sep = ""))



get_auc = function(data.for.lda) {
  sample.size.raw = floor(0.7 *nrow(data.for.lda[,2:17]))
  train_ind_raw = sample(nrow(data.for.lda[,2:17]), size=sample.size.raw)
  train_raw.df = data.for.lda[train_ind_raw,2:17]
  test_raw.df = data.for.lda[-train_ind_raw,2:17]
  
  # get formula
  f <- paste(names(train_raw.df)[16], "~", paste(names(train_raw.df)[-16], collapse=" + "))
  
  another.lda = lda(as.formula(paste(f)), data=train_raw.df)
  
  another.lda.predict = predict(another.lda, newdata = test_raw.df)
  
  
  ### CONSTRUCTING ROC AUC PLOT:
  # Get the posteriors as a dataframe.
  another.lda.predict.posteriors <- as.data.frame(another.lda.predict$posterior)
  
  # Evaluate the model
  pred <- prediction(another.lda.predict.posteriors[,2], test_raw.df$GRAIN)
  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  auc.train <- performance(pred, measure = "auc")
  auc.train <- auc.train@y.values
  round(auc.train[[1]],3)
}


get_auc(data.for.lda)
mean(replicate(100, get_auc(data.for.lda)))

