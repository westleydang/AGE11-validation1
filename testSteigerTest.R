

# trying to validate the steiger test

# make dummy matrices

make_symm = function(m1) {
  m1[lower.tri(m1)] = t(m1)[lower.tri(m1)]
  return(m1)
  }

m1 = matrix(c(1,7,8,3,5,6,2,2,8)/10, nrow=3, ncol=3)
m2 = matrix(c(1,2,2,3,5,6,7,8,8)/10, nrow=3, ncol=3)
make_symm(m1); make_symm(m2)
cortest.normal(m1, m2, n1=20, n2=20) # same R dist, but high chi2 because pattern diff
cortest.mat(m1, m2, n1=20, n2=20) # 

m1 = matrix(c(1,2,2,3,5,6,7,8,8)/10, nrow=3, ncol=3)
m2 = matrix(c(1,2,2,3,5,6,7,8,8)/-10.00, nrow=3, ncol=3)
make_symm(m1); make_symm(m2)
cortest.normal(m1, m2, n1=20, n2=20) # inversed R dist, but high chi2 because pattern diff
cortest.mat(m1, m2, n1=20, n2=20) # 

m1 = matrix(c(1,2,2,3,5,6,7,8,8)/10, nrow=3, ncol=3)
m2 = matrix(c(1,2,2,3,5,6,7,8,8)/20, nrow=3, ncol=3)

make_symm(m1); make_symm(m2)
cortest.normal(m1, m2, n1=20, n2=20) # diff R, low chi2 because same pattern
cortest.mat(m1, m2, n1=20, n2=20) # diff R, high chi2


m1 = matrix(c(1,2,2,3,5,6,7,8,8)/10, nrow=3, ncol=3)
m2 = matrix(rev(c(1,2,2,3,5,6,7,8,8)/10), nrow=3, ncol=3)
make_symm(m1); make_symm(m2)
cortest.normal(m1, m2, n1=20, n2=20) # same R but reversed, high chi2
cortest.mat(m1, m2, n1=20, n2=20) # same R but reversed, low chi2



"cortest.jennrich" <- 
  function(R1,R2,n1=NULL, n2=NULL) {
    p <- dim(R1)[2]
    if(dim(R1)[1] != p) { n1 <- dim(R1)[1]
    R1 <- cor(R1,use="pairwise")
    warning ("R1 matrix was not square, correlations found") 
    }
    if(dim(R2)[1] != dim(R2)[2] ) {n2 <- dim(R2)[1]
    R2 <- cor(R2,use="pairwise") 
    warning ("R2 matrix was not square, correlations found") }
    if(!is.matrix(R1) ) R1 <- as.matrix(R1)  #converts data.frames to matrices if needed
    if(!is.matrix(R2) ) R2 <- as.matrix(R2)
    
    if (dim(R1)[2] != dim(R2)[2]) stop("correlation matrices M and S must be of the same size!")
    if(is.null(n2)) n2 <- n1
    if(!is.null(n1) & !is.null(n2)) c <- n1*n2/(n1+n2) else c <- 1
    
    R <- (n1*R1+n2*R2)/(n1+n2)   #matrix of average values
    
    S <- R * R    #squared values of averaged correlations
    S.inv <- solve(S)
    R.inv <- solve(R)
    R.diff <- R1 - R2
    Z <- sqrt(c) * R.inv %*% R.diff
    chi2 <- tr(Z%*%t(Z))/2 - t(diag(Z)) %*% S.inv %*% diag(Z)
    chi2 <- chi2[1,1]
    p <- dim(R1)[1]
    df <- p*(p-1)/2 
    results <- list(chi2 =chi2,prob =pchisq(chi2,df,lower.tail=FALSE))
    return(results)
  }
