library(MASS)
library(tidyverse)


dados = read.table('canc3.txt', header=T)

dados$Sexo = factor(dados$Sexo)
dados$HL = factor(dados$HL)
dados$FF = factor(dados$FF)

levels(dados$Sexo) = c(0,1)
levels(dados$HL) = c(0,0,1,1)
levels(dados$FF) = c(0,0,1,1)

# Seja \pi(x) a probabilidade de sucesso dado os valores observados das variaveis
# explicativas x, 
# queremos modelar:
# log(\pi(x) / 1-\pi(x)) = \alfa + \betaIdade_j + \thetaSexo_k + 
#                                    + \gammaHL_l + \deltaFF_m





# selecionando as variaveis pelo metodo stepwise
fit.model = glm(Tipo ~ Idade+Sexo+HL+FF, family=binomial(),data=dados)

stepAIC(fit.model)

# selecionando as interações de primeira ordem para serem incluidas no modelo
fit.model = glm(Tipo ~ Idade+Sexo+HL+FF+Idade*Sexo+Idade*HL+Idade*FF+
                  Sexo*HL + Sexo*FF + HL*FF, family=binomial(), data=dados)

stepAIC(fit.model)


# modelo final
fit.model = glm(formula = Tipo ~ Idade + Sexo + HL + FF + Idade:HL + Sexo:FF + 
                  HL:FF, family = binomial(), data = dados)
summary(fit.model)


# desvio do modelo: no caso binomial, a função desvio ~ qui-quadrado, ou seja
# a função desvio é o desvio do modelo, logo a função desvio do modelo é dada
# por: 146.22  , 167 g.l.  p-valor = 0.5463432, indicando que é um ajuste ade-
# quado.


# Obseravando o gráfico da medida h, notamos que as observações #6 e #69 são 
# possiveis pontos de alavanca. Pelo gráfico da distância de Cook, podemos notar,
# novamente, a observação #69 e #172 como possiveis pontos influentes. No gráfico
# dos resíduos componente do desvio, as observações #21 e #172 se encontram fora
# dos limites [-2;2]. Pelo gráfico normal de probabilidades, não é encontrado
# nenhum indício de que o ajuste seja inadequado.

# fazer tabelas de contingencia


# Analisando o modelo sem as observações:


























