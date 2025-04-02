grahani.dat = read.table("C:/Users/moise/Desktop/arquivos R/grahani_data.txt",
                         header=T, sep="", row.names = NULL)

# scan() lê linha por linha, ou seja, observação por observação

attach(grahani.dat)

suc = grahani.dat$suc
tot<-grahani.dat$tot
per<-grahani.dat$per
comp<-grahani.dat$comp
larg<-grahani.dat$larg
ocup<-grahani.dat$ocup

# no caso da familia binomial, a variavel resposta pode ser especifi-
# cada de 3 maneiras, uma delas sendo um vetor cuja primeira coluna
# é o número de sucessos e a segunda o número de fracassos.

Xmat<-cbind(suc,tot-suc)
per<-factor(per)
comp<-factor(comp)
larg<-factor(larg)
ocup<-factor(ocup)

# transformando os fatores em tratamento

per<-C(per,treatment)
comp<-C(comp,treatment)
larg<-C(larg,treatment)
ocup<-C(ocup,treatment)

fit.grahani = glm(Xmat ~ per + comp + larg + ocup, family = binomial())

summary(fit.grahani)

# sabemos que:
# 
# log( mu / [1-mu]) = eta
# => mu / [1-mu] = exp(eta)
# casela de referência = alfa, com beta = theta = delta = gamma = 0
# casela de referência = intercepto = eta
# logo, mu / [1-mu] = exp(intercepto) = exp(1.9447)
# 
# podemos calcular a probabilidade de encontramos um lagarto grahani
# com as caracteristicas da casela de referência:
# prob(grahani_(1111)) = exp(1.9447) / (1 + exp(1.9447)) = 0.8748676
# ou seja, encontrar um lagarto grahani no período da manhã, numa 
# madeira com o comprimento = curto, largura = estreita, e local de 
# ocupação = claro
# 
# odds ratio 
# 
# a chance de, nas condições da casela de referência, o lagarto ser 
# grahani é de 0.87 / 0.12, ou seja, quase de 9 para 1.
# podemos, então, fazer uma razão de chances com o intuito de comparar
# a diferença entre dois níveis.
# 
# iremos realizar a razão de chance entre o período da manhã e o 
# período da tarde:
# 
# temos que  mu_(2111) / (1-mu(2111)) = exp(alfa + per2) e
# mu_(1111) / (1-mu(1111)) = exp(alfa), logo a razao de chances é dada
# por:
# [mu_(2111) / (1-mu(2111))] / [mu_(1111) / (1-mu(1111))] = 
# exp(alfa + per2) / exp(alfa) = exp(per2) 
# => exp(per2) = exp(0.2271) = 1.254955. 
# mais provavel encontrar lagartos a tarde do que de manhã/meio-dia
# 
# razão de chances pro comprimento de madeira (comprida x curta):
# 
# exp(1.9447 + 1.13) / exp(1.9447) = 3.095657, ou seja, é mais provavel
# encontrar um grahani em madeiras com o comprimento maior.


# agrupando niveis do fator periodo do dia

levels(per)
# verificando quais são os níveis da variável

levels(per) = c("1", "1", "3")
# ao inves dos niveis serem "1", "2", "3", serão c("1", "1", "3"),
# ou seja, o período "meio dia" se uniu com o período manhã, nivel 1.

per = C(per, treatment)
# criando uma variável binária para os níveis 

contrasts(per)
# verificando a variável binária para os níveis

fit.model = glm(Xmat ~ per + comp + larg+ ocup, family=binomial())

summary(fit.model)

# considerando interações de primeira ordem entre as variáveis explicativas 
# (adicionar todas as interaçoes***)
fit3.grahani = glm(Xmat ~ per + comp + larg + ocup + per*ocup + comp*larg
                   + per*comp + per*larg,family=binomial())

summary(fit3.grahani)

anova(fit3.grahani, fit2.grahani)

# fazer anova comparando o modelo com as interaçoes de primeira 
# com o modelo sem as interaçoes


fitted(fit.grahani)
# index 1 é o lagarto numero 1 com suas caracteristicas, index 18 é o lagarto numero 18
# olhar tabela grahani.dat

ntot = grahani.dat$tot
