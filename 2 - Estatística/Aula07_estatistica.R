#### Aula 7 - Regressão Linear Simples/Multiplas
#### Emerson Adam
#instalando pacotes e utilizando no código
install.packages("see")
install.packages("performance")
require(dplyr)
require(see)
require(performance)

#se apropriando dos dados, olhando o dataset
?mtcars
dim(mtcars) #32 linhas e 11 colunas
str(mtcars) #todas numéricas = quantitativas

##Explorar as variaveis consumo(mpg), peso, potencia, hp, disp(cilindrada)
##Selecionando somente os dados que vou comparar e jogando pra variavel dados
dados <-  mtcars %>%
  dplyr::select(mpg, wt, hp, disp)
str(dados)


#verificando Histograma de cada variavel
hist(dados$wt)
hist(dados$hp)
hist(dados$disp)

#verificando linearidade estatistica dos dados. nota-se nos graficos de dispersão que há certa linearidade
par(mfrow=c(2,2))
plot(dados$wt,dados$mpg, main = "Peso")
plot(dados$hp,dados$mpg, main = "Potência")
plot(dados$disp,dados$mpg, main = "Cilindradas")
dev.off()

#verificando correlação entre os dados para identificar se há multicolinearidade 
cor(dados)
#nota-se que há forte correlação entre peso/cilindrada, indicando multicolinearidade, optando por somente uma variavel no modelo


##Verificando qual maior R², ajustando o modelo nos 3 casos abaixo, explicando o consumo(variavel Y)
#pelas 3 variaveis (X) peso, potência e cilindrada ou se utilizando 2 variaveis X resultam em um modelo melhor
m1 <- stats::lm(mpg ~ wt + hp + disp , data = dados)
summary(m1) # m1 o R² ajustado = 0.808 ou seja 80,8% do consumo é explicado pelas 3 variaveis 
m2 <- stats::lm(mpg ~ wt + hp , data = dados)
summary(m2) #m2 o R² ajustado = 0.814 ou seja 81,4% do consumo é explicado pelas 2 variaveis peso e potencia
m3 <- stats::lm(mpg ~ disp + hp , data = dados)
summary(m3) #m3 o R² ajustado = 0.730 ou seja 73,0% do consumo é explicado pelas 2 variaveis cilindrada e potencia
#nota-se que o modelo 2 é o melhor para trabalho visto que resulta um R² melhor 

#Avaliação e Diagnóstico do melhor modelo = M2
#explicar o ajuste, procurando por padrões de resíduos nos gráficos, para verificar se está bem ajustado ou não
par(mfrow=c(2,2))
plot(x = dados$mpg , y= stats::rstandard(m2))
abline(0,0)
plot(x = dados$wt , y= stats::rstandard(m2))
abline(0,0)
plot(x = dados$disp , y= stats::rstandard(m2))
abline(0,0)
dev.off()
#nota-se que, de forma geral, os gráficos estão bem ajustados, nao existe um padrão na distribuição nos dados,
#há homocedacidade nas variaveis, o que signifca que o modelo está correto!

plot(m2 , which = 2) #wich 2 mostra somente o grafico q-q
#o normal q-q plot apresenta uma distribuição normal dos dados, porém com 3 pontos atipicos deixam em duvida a normalidade dos resíduos, Fiat 128, Chrysler Imperial e Toyota Corolla


#verificando se os dados para a construção do modelo estao bem definidos(avaliação diagnostico de ajuste de modelo), utiliza pacotes performance e see
performance::check_model(m2)
#1º grafico mostra distribuição dos dados observados e dados preditos
#2º grafico mostra a linearidade dos dados, os dados estão dendo da faixa de confiança, sem violar de maneira importante
#3º grafico mostra a homogeneidade de variancia, os dados estão dendo da faixa de confiança, sem violar de maneira importante
#4º grafico mostra os pontos de alavanca, se os pontos estão fora da linha de confinça, estão dentro
#5º grafico mostra variancia e multicolinearidade
#6º grafico mostra a normalidade da distribuição dos resíduos.

#olhando todas estas etapas podemos garantir que o modelo m2 está bem ajustado, explicando consumo por peso e potência

#interpretar os dados, predizendo o modelo, entendendo os valores
summary(m2)
#resumo
#81.48% da variabilidade do consumo é explicado pelo modelo M2(peso e potência)
#o aumento de 1 unidade no consumo reduz em 3.87 unidades de peso, ou seja diminuindo o peso, diminui o consumo mpg
#o aumento de 1 unidade no consumo reduz em 0.03 unidades de potência, ou seja diminuindo a potencia, diminui o consumo mpg