# DATA ENTRY:
  #DATA <- read.table('bivNorm.dat',header=F)
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

  colnames(DATA) <- c('GPA', 'ENG', 'MATH', 'READING', 'Natural.Science')
  
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
      p.val <- 2*(1-pt(abs(t.stat),n-k-1))
      #pt(t.stat,n-k-1)
 
      M <- cbind(bH,se,t.stat,p.val)
      if(length(rownames(M))==0) {
        rownames(M) <- paste('b',0:k,sep='') 
      } else {
        rownames(M) <- c('Intercept',rownames(M)[-1])
      }
      colnames(M) <- c('Estimates', 'Std.Error', 't.stat', 'p.value')
      
      return(list("Coefficients"=M,"s2"=ss,"Data:"=cbind(Y,X[,-1])))
      
    }
  }

# FUNCTION DEFINITION: Ho.C.my.lm:
  C1 <- cbind(0,diag(4))
  C2 <- c(0,0,1,0,0)

  Ho.C.my.lm <- function(C,model){
    mat <- is.matrix(C)
    if (!mat){C <- t(as.matrix(C))}

    bH <- model$Coefficients[,1]
    CB <- C %*% bH
    Y <- model$Data[,1]
    X <- cbind(1,model$Data[,-1])
    A <- solve (C %*% solve( t(X) %*% (X) ) %*% t(C) )
    Q <- dim(C)[1]
    n <- dim(X)[1]
    k <- dim(X)[2] - 1

    SSH <- t(CB) %*% A %*% (CB) 
    SSE <- t(Y) %*% ( diag(n) - X %*% solve(t(X) %*% X) %*% t(X) ) %*% Y
    
    #browser()
    if ( mat ){
      F.stat <- (SSH/Q) / (SSE/(n-k-1))
      p <- pf(F.stat,Q, n-k-1,lower.tail=F)
      c('F-Statistic'=F.stat, 'p-value'=p)
    } else {
      se <- sqrt(model$s2 * C %*% solve(t(X)%*%X) %*% t(C))
      t.stat <- CB / se
      p.val <- 2 * (1- pt(abs(t.stat), n-k-1))
      c('Estimator'=CB, 'Std.Err'=se, 't.stat'=t.stat, 'p.val'=p.val )
    }
  }

  predict.my.lm <- function(xo,model){
    outOfBound <- ifelse( all(apply(model$Data[,-1],2,min) <= xo) & 
                          all(xo <= apply(model$Data[,-1],2,max)),F,T)
    if (outOfBound) {
      print("ERROR: xo is out of bound.")
    } else {
      xo <- c(1,xo)
      beth  <- model$Coefficients[,1]
      ss <- model$s2
      X <- cbind(1,model$Data[,-1])
      n <- dim(X)[1]
      k <- dim(X)[2]-1
      yoh <- t(xo) %*% beth
      PI <- yoh + c(-1,1) * qt(.975, n-k-1) *
            sqrt(ss*( 1 + t(xo) %*% solve(t(X)%*%X) %*% xo))

      cat("\n")
      c("Prediction"=yoh,"LowerBound"=PI[1],"UpperBound"=PI[2])
    }
  }

  anova.my.lm <- function(type=c(1,3)[1],model){
    

    c("Source"=,"df"=,"SS"=)
  }

############################################################################3
# TEST & EXECUTE FUNCTIONS: 
  #summary(lm(DATA[,1] ~ DATA[,-1]))
  #mod <- my.lm(DATA[,1],DATA[,-1])
  #Ho.C.my.lm(C1,mod)
  #Ho.C.my.lm(C2,mod)
  
