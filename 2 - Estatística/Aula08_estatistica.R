##########AULA 08 Regressao logistica - Dicotomica sim e nao
##logaritmo de chance de resposta positiva - Maxima verossimilhança
#Ajustar -Diagnosticar e interpretar Antes explorar e se apropriar
#aplicações, fraudes, cancelamentos de planos, churn, variavel resposta sim e não(dicotomia)
#avaliar fatores associados


#analise da base birthwt
require(MASS)
require(dplyr)
require(stats)
?birthwt

#selecionando variaveis, atribuindo, transformando em fator(transformando de numero pra categorica) low e smoke, e mudando o peso da mae em kg
dados <- birthwt %>%
  dplyr::select(low, age, lwt, smoke) %>%
  dplyr::mutate(peso_mae_kg = lwt*0.453,
                low_cat = factor(low),
                smoke_cat = factor(smoke))
str(dados)

#tarefa interpretar os dados do modelo, colocar graficos e explicar
summary(dados)

#segunda etapa da analise exploratoria, ver IDADE peso fumo em relação ao baixo peso ao nascer, correlação
#boxplot com variavel numérica entendendo e explicando Qual teste confirma a diferença de idade, diferença de media, teste de comparação de média teste T teste wilcoxx se nao for dist normal
boxplot(dados$age ~ dados$low_cat)
boxplot(dados$peso_mae_kg ~ dados$low_cat)
prop.table(table(dados$smoke_cat, dados$low_cat), margin =1)*100 #nota-se que maes que nao fumaram, tiveram menos filhos com baixo peso


#pra saber se pode ser aplicado o teste t ou nao, a dist é normal? vamos olhar
hist(dados$age)
shapiro.test(dados$age) # hipotese nula a distribuiçao normal
ks.test(dados$age,"pnorm")

#FAZER OS TESTES
#variavel
resposta ~variavel categorica -> p baixo, nao é (o P é o valor para definir aceitação se é verdãde ou nao H1 H0, significancia menor q 0,05 -> rejeita hipotese nula)
wilcox.test(dados$age ~dados$low_cat)

####verificar qual relação entre fumar e baixo peso, fazer teste qiquadrado

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

#segundo passo, considerar todas variaveis Xque tiveram p menor que 0.25
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
glmtoolbox::hltest(m_passo3) #mostra grupos criatos, estatisticas de testes 

#Olhar residuos de pearson e deviance
#avaliação residual
residuos_pearson <- data.frame(
  id = 1:nrow(dados),
  res_pearson = residuals(m_passo3, type = "pearson"))
head(residuos_pearson)
#olhar e nao encontrar padrão, quer dizer qoe é confiavel
plot(x = sample(residuos_pearson$id),
     y = residuos_pearson$res_pearson)
abline (0,0)

# hpotese nula = quiquadrado indicando que o modelo está bem ajustado
pchisq(sum(residuos_pearson$res_pearson^2), df = m_passo3$df.residual)


#olhar residuos deviance
residuos_deviance <- data.frame(
  id = 1:nrow(dados),
  res_dev = residuals(m_passo3, type = "deviance"))
head(residuos_deviance)
#olhar e nao encontrar padrão, quer dizer qoe é confiavel
plot(x = sample(residuos_deviance$id),
     y = residuos_deviance$res_dev)
abline (0,0)

anova(m_passo3, test = "Chisq")


#interpretar os coeficientes(peso e smoke) pois o modelo está correto

summary(m_passo3)
exp(coef(m_passo3))

#Intercept) peso_mae_kg  smoke_cat1 a cada kg no peso da mae reduz em 3% o bebe peso baixo (1-0,97)
#1.8626437   0.9710148   1.9673220 
summary(m_passo3)
exp(10*coef(m_passo3)[2])#pra cada 10kg a mais no peso da mãe reduz em 25% no risco do bebe nascer com baixo peso(1-0.74)

#