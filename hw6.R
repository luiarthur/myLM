# my.lm(Y,X)
# Ho.C.my.lm(C,lm)

#2
source("./project.R")

#2a
xo <- c(20,28,18,22)
model <- my.lm(DATA[,1],DATA[,-1])
predict.my.lm(xo,model)

# Include a written response

#2b
C <- c(0,0,1,0,0)
F.math <- Ho.C.my.lm(C,out)

#2c
F.math2 <- Ho.C.my.lm()
