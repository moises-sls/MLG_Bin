X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
w <- fit.model$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)
ts <- resid(fit.model,type="pearson")/sqrt(1-h)
td <- resid(fit.model,type="deviance")/sqrt(1-h)
di <- (h/(1-h))*(ts^2)
a <- max(td)
b <- min(td)

win.graph()
plot(fitted(fit.model),h,xlab="Valor Ajustado", 
     ylab="Medida h", pch=16)
identify(fitted(fit.model), h, n=3)
#
win.graph()
plot(td,xlab="Índice", ylab="Resíduo Componente do Desvio",
     ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
identify(td, n=2)
#
win.graph()
plot(di,xlab="Índice", ylab="Distância de Cook",pch=16)
identify(di, n=2)
#
win.graph()
plot(fitted(fit.model), td,xlab="Valor Ajustado", 
     ylab="Resíduo Componente do Desvio", pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
identify(fitted(fit.model), td, n=1)



