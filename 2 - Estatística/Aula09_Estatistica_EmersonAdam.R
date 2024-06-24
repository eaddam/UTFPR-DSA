#instalando pacotes e utilizando no código

require(survival)
install.packages("survminer")
install.packages("RColorBrewer")
require(survminer)
require(dplyr)
#EDA dataset lung analise de sobrevivência
?lung
dim(lung) #228 linhas e 10 colunas - variaveis
str(lung) #somente numéricas

#criando modelo somente com as variaveis tempo, status da doença, idade e sexo, categorizando em F ou M transformando em Factor o sexo
#declarando também o status como variavel categórica em 0 e 1 onde o é o evento censurado e 1 o óbito do paciente
dados <-  lung %>%
  dplyr::select(time, status, age, sex) %>%
  dplyr::mutate(sex_cat = factor(dplyr::case_when(sex == 1 ~ 'H',
                                        sex == 2 ~'M')),
                status_adj = dplyr::recode(status, `1` = 0,
                                          `2` = 1)) #0 = censura 1= causa mortis
?recode
str(dados)

#1 parte do exercicio - apresentar a curva de sobrevivencia e explanar sobre
#calculo/ajuste da funçao da sobrevivencia (etapa exploratoria do contexto) Kaplan-Meier somente olhando, sem nenhuma covariavel estimando S(t) 
km <- survival::survfit(formula = Surv(time, status_adj) ~1, data=dados)#par de variavel resposta (tempo e status ajustado), ~1 pois nao ha explicação, covariaveis, ainda
km #228 pacientes, 165 obitos, tempo mediano em 310 dias de sobrevivencia com 95% de confiança
summary(km)
#exemplos summary
#tempo 5 dias haviam 228 pacientes e houve 1 obito e a taxa de sobrevivencia é de 99%
#tempo 163 dias haviam 176 pacientes e houveram 3 obitos, onde a taxa de sobrevivencia é de 71%
plot(km, xlab = "dias de sobrevivencia", ylab= "S(t)" )
#grafico de escada com a probabilidade de sobrevivencia (0 a 1) de acordo com o tempo em dias
plot(km, xlab = "dias de sobrevivencia", ylab= "S(t)", conf.int = FALSE )
#removendo as linhas pontilhadas, intervalo de confiança do objeto KM
plot(km, xlab = "dias de sobrevivencia", ylab= "S(t)", mark.time = TRUE )
#marcar os pontos onde houveram censura no estudo


#2 parte testar/comparar a curva de sobrevivencia entre h e M com teste de logrank
table(dados$sex_cat)#qual é a distribuição entre H e M no modelo? 138h e 90 M
prop.table(table(dados$sex_cat)) #em proporção 60% homens e 40% mulheres
prop.table(table(dados$sex_cat, dados$status_adj), margin=2)#em proporção aos óbitos, 68% dos obitos foram em homens e 32% em mulheres
#somente olhando isto, esperamos ver uma taxa maior de obitos em homens.

#olhando a relação entre tempo até o obito ajustada pelo sexo
km_sex <-  survival::survfit(formula = Surv(time, status_adj) ~sex_cat, data=dados)
#descritiva em relação aos grupos, grande diferença no tempo médio de sobrevivencia entre os grupos, 270 para homens e 426 para mulheres
summary(km_sex) #a probabilidade de sobrevivencoa para homens no mesmo tempo que as mulheres é menor
#olhando a probabilidade de sobrevivencia em certos periodos de tempo
summary(km_sex, times = 365)#1 ano separando grupos
#a taxa de sobrevivencia em um ano para mulheres é 52% e 33% para homens, indicando que devemos rejeitar H0
summary(km, times = 365)#1 ano não separando os grupos
#a taxa de sobrevivencia em um ano para o grupo de estudo é de 41% com 95% de confiança


#criando curva de sobrevivencia, criando uma linha para cada grupo
plot(km_sex, conf.int = FALSE,
     xlab = "dias de sobrevivencia",
     ylab= "S(t)",
     lty = c(1,2))
  legend(legend = c('H','M'),
         lty = c(1,2),
         x = "topright",
         bty = 'n')#sem contorno
dev.off()

#realizando teste formal de LOG-Rank para testar se o risco é igual para H e M, ou seja, as curvas de sobrevivencia são iguais, esta é a H0 
log_rank <- survival:: survdiff(formula = Surv(time, status_adj) ~sex_cat, data=dados)
log_rank #pra cada grupo temos o numero de pessoas em risco por grupos, eventos observados(obitos), estatistica de teste(compara valores esperados e observados (o-e)^2/V)
#valor de P menor que 0,05 entao rejeitamos a h0(a curva de sobrevivencia é igual entre H e M), ou seja o tempo de sobrevivencia das mulheres é maior do que os homens e a diferença é signficativa do ponto de vista estátistico.


#3 parte - ajustar modelo avaliando os fatores de risco associados aos obitos
#modelo de cox, somente as covariaveis age e sex, poderia ser considerado também o peso
  
m_cox <- survival::coxph(formula = Surv(time, status_adj) ~sex_cat + age, data=dados)
m_cox
summary(m_cox) #228 observancias e 165 eventos
#cada covariavel existe um coeficiente estimado, variavel categorica referenciada
# o aumento de 1 ano de idade aumenta o risco de morte em 2%
# o sexo feminino possuem um risco 40% (1-0,6) menor quando comparamos com pacientes masculinos


#verificar se o modelo está bem ajustado, nao viola os pressupostos de validação do método
#avaliação do ajuste
#1 suposição de riscos proporcionais
survival::cox.zph(m_cox) #verificar se p possui valor menor de q 0,05 em cada variavel, no caso, nao rejeitamos h0 de que os riscos são proporcionais indicando que o modelo está bem ajustado
survminer::ggcoxzph(survival::cox.zph(m_cox))#de forma grafica
#nota-se que os residuos de Schoenfelde para cada categoria, onde cada valor(idade e sexo_cat) está dentro da faixa de confiança, ou seja, nao violando os riscos proporcionais

#2 verificar residuos deviance, olhando pontos de alavanca, se existem e impactam no modelo
survminer::ggcoxdiagnostics(m_cox,type = "deviance")
#nota-se que nao ha nenhum padrão ou pontos de alavanca, pontos longe da variancia, indicando que o modelo está bem ajustado.

#3 verificar a concordância do modelo, literatura indica como aceitavel valores maiores que 0,6
summary(m_cox)
#nota-se que o modelo está correto visto que a concordância tem valor de 0.63.

#então é valido e correto dizer que a taxa de sobrevivencia entre os grupos é diferente, sendo que a maior taxa é do grupo das mulheres.
