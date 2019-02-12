# make KNN classifier



decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}


data.for.lda = arc %>%
  ddply(.(System), transform, z = scale(weighted)) %>%
  dcast(ID ~ System, value.var="z", fun.aggregate = mean, .drop=T) 

data.for.lda = merge(data.for.lda, animal_details %>% 
                       # filter(EXPT=='OLD') %>%
                       mutate(GRAIN = paste(EXPT, GROUP)) %>% 
                       dplyr::select(ID, GRAIN), by="ID")
data.for.lda = complete(mice(data.for.lda, m=5, seed=25))
# remove spaces? 
names(data.for.lda) = gsub(" ", "_", names(data.for.lda))
# names(data.for.lda) = gsub("-", ".", names(data.for.lda))
library(e1071)
model <- naiveBayes(GRAIN ~ ., data=data.for.lda[,2:17])

decisionplot(model, data.for.lda[,2:17], class = "GRAIN", main = "naive Bayes")
