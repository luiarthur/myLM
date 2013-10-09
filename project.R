# DATA ENTRY:
  DATA <- matrix(c(3.05, 27, 25, 28, 24,
                   2.73, 19, 21, 10, 25,
                   3.55, 24, 31, 28, 31,
                   2.79, 23, 30, 30, 28,
                   3.76, 22, 25, 28, 30,
                   3.22, 28, 28, 30, 28,
                   3.89, 30, 31, 26, 31,
                   2.95, 19, 21, 10, 20,
                   3.80, 26, 29, 31, 32,
                   3.78, 27, 28, 33, 34,
                   2.88, 19, 24, 9,  21,
                   3.63, 22, 30, 29, 34,
                   3.25, 29, 26, 25, 30,
                   3.16, 21, 19, 21, 25,
                   3.23, 15, 22, 15, 26), byrow=T,ncol=5)

  #DATA <- read.table('bivNorm.dat',header=F)

# FUNCTION DEFINITION: my.lm
  my.lm <- function(Y, X) {
    n <- length(Y)
    k <- ifelse(length(dim(X)[2])==0, 1, dim(X)[2])
 
    Ys <- ( Y - mean(Y) ) / sd(Y)
    Xs <- cbind(1, t((t(X) - apply(X,2,mean)) / apply(X,2,sd)) )

    if ( det(t(Xs) %*% Xs) == 0 ){
      cat("ERROR: X'X is singular")
    } else {

      # STANDARDIZE to reduce computation error:

      #Solve for Beta:
      bHs <- solve(t(Xs) %*% Xs) %*% t(Xs) %*% Ys

      bH0 <- ( bHs[1,] - t(bHs[-1,]) %*% (apply(X,2,mean) / apply(X,2,sd)) ) * 
             sd(Y) + mean(Y)
      bHi <- sd(Y) * ( bHs[-1,] / apply(X, 2 ,sd) )
      bH <- c(bH0, bHi)

      # STILL NEED TO DO THIS FROM bHs!!!
      X <- cbind(1,X)
      ss <- as.numeric(1/(n-k-1) * t(Y - X%*%bH) %*% (Y-X%*%bH))
      se <- sqrt(diag(ss *  solve(t(X)%*%X)))
      t.stat <- bH/se
      p.val <-  1- abs(pt(t.stat,n-k-1)-pt(-t.stat,n-k-1))
            # try 2*(1-pt(abs(t.stat),n-k-1))
      #pt(t.stat,n-k-1)
      list('Coefficients'=cbind(bH,se,t.stat,p.val),'Sigma.Squared'=ss)
    }
  }

# FUNCTION DEFINITION: Ho.C.my.lm:
  Ho.C.my.lm <- function(C

############################################################################3
# TEST & EXECUTE FUNCTIONS: 
  summary(lm(DATA[,1] ~ DATA[,-1]))
  my.lm(DATA[,1],DATA[,-1])

