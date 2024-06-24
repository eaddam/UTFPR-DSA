##########AULA 08 Regressao logistica - Dicotomica sim e nao
##logaritmo de chance de resposta positiva - Maxima verossimilhança
#Ajustar -Diagnosticar e interpretar Antes explorar e se apropriar
#aplicações, fraudes, cancelamentos de planos, churn, variavel resposta sim e não(dicotomia)
#avaliar fatores associados

#etapas -> EDA, Ajuste do modelo, avaliaçao do ajuste e interpretação


#instalando pacotes e usando os pacotes no código
install.packages("MASS")
install.packages("dplyr")
install.packages("stats")
require(MASS)
require(dplyr)
require(stats)
#analise da base birthwt, se apropriando do dataset e conhecendo seu contexto
?birthwt
dim(birthwt) #189 linhas e 10 colunas
str(birthwt) #tipos de variaveis inteiras


#selecionando variaveis, atribuindo, transformando em fator(transformando de numero pra categorica) low e smoke, e mudando o peso da mae em kg
dados <- birthwt %>%
  dplyr::select(low, age, lwt, smoke) %>%
  dplyr::mutate(peso_mae_kg = lwt*0.453,
                low_cat = factor(low),
                smoke_cat = factor(smoke))
str(dados)#agora temos fatores categorizando baixo peso e se fuma, sim ou não (dicotomia)


#interpretando os dados do modelo, analisando graficos e explicando os dados
summary(dados)

#grafico de distribuição de peso,
require(car)
car::qqPlot(dados$lwt, 
            id = FALSE,
            grid = FALSE,
            col.lines = "blue",
            col = "dark green",
            ylab = "Peso no nascimento em (gramas)",
            xlab = "Quantis normais",
            main = "Normal Q-Q Plot")
hist(dados$lwt, xlab = "Peso ao nascer", ylab = "quantidade") #nota-se que no dataset há em torno de 28 crianças com baixo peso
hist(dados$peso_mae_kg)#distribuição normal de peso


#olhando de forma global para as variaveis, pra saber se pode ser aplicado o teste t ou nao, a dist é normal?
boxplot(dados$age ~ dados$low_cat)
hist(dados$age)
shapiro.test(dados$age) #valor de P muito baixo, hipotese nula a distribuiçao normal
ks.test(dados$age, "pnorm")

#olhando de forma individual para baixo peso em cada situação e o teste T nao é uma boa opçao
boxplot(dados$age ~ dados$low_cat)
hist(dados$age[dados$low_cat == 0])
shapiro.test(dados$age[dados$low_cat == 0]) 
ks.test(dados$age, "pnorm") #hipotese nula distribuição é normal
wilcox.test(dados$age ~dados$low_cat)
#Nota-se que pelo valor de P 0.24 nao há evidencia de que mães que tiveram filhos com baixo peso tenham idade mediana diferente das mães que não tiveram filhos com baixo peso

#utilizando teste qui quadrado para identificar se os filhos com baixo pesos tem relação com as maes que fumaram
chisq.test(dados$low_cat, dados$smoke_cat) #nota-se que o valor de p é menor que 0,05 o que indica que há forte indicio de que o fumo é relevante para filhos com baixo peso.

#segunda etapa da analise exploratoria, ver IDADE peso fumo em relação ao baixo peso ao nascer, correlação
#boxplot com variavel numérica entendendo e explicando Qual teste confirma a diferença de idade, diferença de media, teste de comparação de média teste T teste wilcoxx se nao for dist normal
boxplot(dados$age ~ dados$low_cat) #mae que nao tiveram filhos com baixo tem uma idade mediana maior do que as maes que tiveram filhos com baixo peso
boxplot(dados$peso_mae_kg ~ dados$low_cat)
prop.table(table(dados$smoke_cat, dados$low_cat), margin =1)*100 #nota-se que maes que nao fumaram, tiveram menos filhos com baixo peso

#resposta ~variavel categorica -> p baixo, nao é (o P é o valor para definir aceitação se é verdãde ou nao H1 H0, significancia menor q 0,05 -> rejeita hipotese nula)
wilcox.test(dados$age ~dados$low_cat)


#Etapa 2 realizar ajuste do modelo 
#definir quais variaveis pelo Hosmer e LEMESHOW
#passo 1 ajustar covariavel cada x com a variavel y (simples)
#tem 3 covariaveis -> 3 modelos distintos
m_passo1_1 <- stats::glm(low_cat ~ age, data = dados, family = "binomial")#comparando peso e idade, mostra que valor P é pequeno, entao nao se tem tanta correlação
summary(m_passo1_1)
summary(m_passo1_1)$coefficients["age", "Pr(>|z|)"]

m_passo1_2 <- stats::glm(low_cat ~ peso_mae_kg, data = dados, family = "binomial")
summary(m_passo1_2)$coefficients["peso_mae_kg", "Pr(>|z|)"]

m_passo1_3 <- stats::glm(low_cat ~ smoke_cat, data = dados, family = "binomial")
summary(m_passo1_3)$coefficients["smoke_cat1", "Pr(>|z|)"]#fator smoke_cat

#segundo passo, considerar todas variaveis X que tiveram p menor que 0.25
m_passo2 <-  stats::glm(low_cat ~age + peso_mae_kg + smoke_cat,
                        data = dados, family = "binomial")
summary(m_passo2)

#passo 3  excluir(selecionar no modelo) todos os x com p > 0.05 -> modelo 3 é o melhor
m_passo3 <-  stats::glm(low_cat ~ peso_mae_kg + smoke_cat,
                        data = dados, family = "binomial")
summary(m_passo3)

#diagnostico de ajuste, podemos confiar neste modelo?
#adequação do modelo Teste de H&L - H0 o modelo está bem ajustado
require(glmtoolbox)
install.packages("glmtoolbox")
glmtoolbox::hltest(m_passo3) #mostra grupos criados, estatisticas de testes.
#de acordo com o teste hl, que de acordo com o valor de P em 0.20 a hipotese nula é valida, o modelo m_passo3 está bem ajustado!

#Olhar residuos de pearson e deviance
#avaliação residual
residuos_pearson <- data.frame(
  id = 1:nrow(dados),
  res_pearson = residuals(m_passo3, type = "pearson"))
head(residuos_pearson)
#olhar e nao encontrar padrão, quer dizer que é confiavel
plot(x = sample(residuos_pearson$id),
     y = residuos_pearson$res_pearson)
abline (0,0)
#nao encontramos padrões, ou seja, o modelo é confiavel, reforçando que o modelo está bem ajustado

# hpotese nula = qui-quadrado indicando que o modelo está bem ajustado
pchisq(sum(residuos_pearson$res_pearson^2), df = m_passo3$df.residual)
#a hipotese nula é que os residuos seguem uma distribuição normal, mais um indicio de que o modelo está bem ajustado

#olhar residuos deviance para confirmar mais um ponto sobre o modelo, está bem ajustado?
residuos_deviance <- data.frame(
  id = 1:nrow(dados),
  res_dev = residuals(m_passo3, type = "deviance"))
head(residuos_deviance)
#olhar e nao encontrar padrão, quer dizer qoe é confiavel
plot(x = sample(residuos_deviance$id),
     y = residuos_deviance$res_dev)
abline (0,0)
#nao encontramos padrões, ou seja, o modelo é confiavel, reforçando que o modelo está bem ajustado

#avaliando o deviance do modelo, utilizando um teste qui-quadrado novamente, comparando o residuo com as covariaveis
anova(m_passo3, test = "Chisq")

#interpretar os coeficientes(peso e smoke) pois o modelo está correto
#resumo do modelo
summary(m_passo3)

exp(coef(m_passo3)) #precisamos analisar os coeficientes, o aumento de X em Y (variavel quantitativa peso da mae)
#a cada kg no peso da mae reduz em 3% o bebe peso baixo (1-0,97)
#agora analisando a variavel categórica smoke, mães que fumam possuem 1,97% maior de ter um filho com baixo peso do que mães que nao fumaram durante a gravidez
#Intercept) peso_mae_kg  smoke_cat1
#1.8626437   0.9710148   1.9673220 
summary(m_passo3)
exp(10*coef(m_passo3)[2])#pra cada 10kg a mais no peso da mãe reduz em 25% no risco do bebe nascer com baixo peso(1-0.74)
