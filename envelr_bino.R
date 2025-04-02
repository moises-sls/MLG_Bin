X <- model.matrix(fit.model)
k <- nrow(X)
e <- matrix(0,k,100)
tot <- numeric(k)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
td <- sort(resid(fit.model, type="deviance")/sqrt(1-h))
#
for(i in 1:100){
  for(j in 1:k){
    dif <- runif(ntot[j]) - fitted(fit.model)[j]
    dif[dif>=0] <- 0
    dif[dif<0]  <- 1
    tot[j] <- sum(dif)}
  xmat <- cbind(tot,ntot-tot)
  fit <- glm(xmat ~ X, family=binomial)
  w <- fit$weights
  W <- diag(w)
  H <- solve(t(X)%*%W%*%X)
  H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
  h <- diag(H)
  e[,i] <- sort(resid(fit, type="deviance")/sqrt(1-h))}
#
e1 <- numeric(k)
e2 <- numeric(k)
#
for(i in 1:k){
  eo <- sort(e[i,])
  e1[i] <- (eo[2]+eo[3])/2
  e2[i] <- (eo[97]+eo[98])/2}
#
med <- apply(e,1,mean)
faixa <- range(td,e1,e2)
par(pty="s")
win.graph()
qqnorm(td,xlab="Percentil da N(0,1)",
       ylab="Componente do Desvio", ylim=faixa,pch=16, cex=0.8, main="")
#
par(new=TRUE)
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=TRUE)
qqnorm(med,axes=F,xlab="", ylab="", type="l", ylim=faixa, lty=2, main="")

