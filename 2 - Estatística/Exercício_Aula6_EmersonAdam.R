#####################
#  Exerciser Aula 6 #
#  Emerson Adam     #
#####################

#importando bibliotecas
require(dplyr)
require(ggplot2)


####Questão 1 - Analise do dataset, contextualizando

#importando Dataset
CO2_Emissions_Canada <- read.csv("C:/Users/emerson.adam/OneDrive - Grupo Laguna/Área de Trabalho/CDA/Estatística/Bases/Veiculos/CO2_Emissions_Canada.csv", header=TRUE)

#análise inicial do dataset, se a importação e variaveis estão corretos
head(CO2_Emissions_Canada)
str(CO2_Emissions_Canada)
dim(CO2_Emissions_Canada)
summary(CO2_Emissions_Canada)

#declarando nova variavel recebendo os dados originais
veiculos <- CO2_Emissions_Canada

#Renomeando as colunas na nova variavel
veiculos <- veiculos %>%
  dplyr::rename(marca = Make,
                modelo = Model,
                classe = Vehicle.Class,
                tam_motor = Engine.Size.L.,
                cilindros = Cylinders,
                transmissao = Transmission,
                combustivel = Fuel.Type,
                consumo_cidade = Fuel.Consumption.City..L.100.km.,
                consumo_estrada = Fuel.Consumption.Hwy..L.100.km.,
                consumo_misto = Fuel.Consumption.Comb..L.100.km.,
                emissao_CO2 = CO2.Emissions.g.km.)

#limpeza da coluna que expressa consumo combinado em milhas por galão (imperial) pois vamos trabalhar somente L/100KM
veiculos <-  select(veiculos, -Fuel.Consumption.Comb..mpg.)
View(veiculos)

#troca de nomenclatura dos combustiveis
veiculos <- veiculos %>%
  mutate(combustivel = case_when(combustivel == "X" ~ "GAS_Comum",
                                 combustivel == "Z" ~ "GAS_Aditivada",
                                 combustivel == "D" ~ "Diesel",
                                 combustivel == "E" ~ "Etanol",
                                 combustivel == "N" ~ "GNV"))

#Quantas marcas unicas existem no dataset = 42
length(unique(veiculos$marca))
#Quantos modelos unicos existem no dataset = 2053
length(unique(veiculos$modelo))
#Quantas transmissões unicas existem no dataset = 27
length(unique(veiculos$transmissao))
#Quantas classses unicass existem no dataset = 16
length(unique(veiculos$classe))
#Quantos combustíveis unicos existem no dataset = 5
length(unique(veiculos$combustivel))

#grafico de dispersão entre CO2 x Cilindradas
plot(x = veiculos$emissao_CO2,
     y = veiculos$tam_motor,
     xlab = "Emissão CO2",
     ylab = "Cilindradas",
     main = 'Grafico Dispersão Emissão CO2 x Cilindradas')

#correlação entre tamanho do motor e emissão de Co2
cor(veiculos$tam_motor, veiculos$emissao_CO2, method = 'pearson')
cor(veiculos$tam_motor, veiculos$emissao_CO2, method = 'spearman')

#Analise entre combustivel e emissão de CO2, nota-se que o Etanol é o que mais emite Co2/KM        
tapply(veiculos$emissao_CO2, veiculos$combustivel, summary)

#boxplot entre combustivel e emissão de CO2
boxplot(veiculos$emissao_CO2 ~ veiculos$combustivel,
        xlab = "Combustivel",
        ylab = "Emissão CO2",
        main = 'BoxPlot Emissão CO2 x Combustivel')

#### Questão 2 Existe diferença na proporção de carros automáticos entre Ford e Chevrolet?

#Filtrando as marcas e criando nova classificação dos tipos de trasmissão em automático ou manual
veiculos2 <- veiculos %>%
  dplyr::filter(marca %in% c('FORD','CHEVROLET')) %>%
  dplyr::mutate(manual = factor(dplyr::case_when(transmissao  %in% c('M5','M6','M7') ~'SIM',
                                                 TRUE ~ 'NAO'), levels = c('SIM','NAO')))

#Quantos modelos unicos existem na variavel veiculos2 = 230
length(unique(veiculos2$modelo))
summary(veiculos2)

#Hipótese Há diferença de proporção dos carros automáticos entre Ford e Chevrolet? ####Teste Bilateral####
#H0 a proporção é = entre as 2 marcas
#H1 a proporção é != entre as 2 marcas
tab_vei_auto <- table(veiculos2$marca, veiculos2$manual)
View(tab_vei_auto)
#Teste de hipotese entre a proporção das 2 marcas onde os carros são automáticos, utilizando o nuvel de confiança 95%
prop.test(x = c(sum(tab_vei_auto["FORD", "NAO"]), sum(tab_vei_auto["CHEVROLET", "NAO"])),
          n = c(sum(tab_vei_auto["FORD", ]), sum(tab_vei_auto["CHEVROLET", ])),
          alternative = "less", conf.level = 0.95)

#Realizando teste de Wilcox
wilcox.test(tab_vei_auto["FORD", "NAO"], tab_vei_auto["CHEVROLET", "NAO"])
#Realizando teste fisher
fisher.test(tab_vei_auto)


##### Questão 3: Existe correlação entre consumo de combustível (rodovia) e emissão de CO2 considerando os veículos do tipo COMPACT?

#correlação somente do grupo compacto com emissão de CO2 - Alta pois todos emitem CO2
#filtrando somente os veiculos compactos
veiculos_compactos <- veiculos %>%
  filter(classe == "COMPACT")

#Verificando a correlação entre consumo e emissão de CO2
cor_vei_compactos <-  veiculos_compactos %>%
  select(consumo_estrada, emissao_CO2) %>%
  cor(method = 'pearson')

cor_vei_compactos

#categorizando os outros tipos de veiculos para correlacionar CO2
veiculos_outros <- veiculos %>%
  filter(classe != "COMPACT")


# Verificando normalidade dos dados
hist(veiculos_compactos$consumo_estrada)
shapiro.test(veiculos_compactos$consumo_estrada)
hist(veiculos_compactos$emissao_CO2)
shapiro.test(veiculos_compactos$emissao_CO2)

# Verificando relação linear entre as variáveis
plot(veiculos_compactos$consumo_estrada, veiculos_compactos$emissao_CO2)

# Teste de correlação de Pearson
cor.test(veiculos_compactos$consumo_estrada, veiculos_compactos$emissao_CO2, method = "pearson")

# Teste de correlação de Spearman
cor.test(veiculos_compactos$consumo_estrada, veiculos_compactos$emissao_CO2, method = "spearman")

#Correlação entre o grupo compacto com os demais, baixa, pois todos possuem motores emissores de CO2, porém há diferença de emissão por serem compactos, menor peso, menor consumo.
# Filtro para selecionar apenas os veículos do tipo compacto
veiculos_compactos <- veiculos %>% 
  filter(classe == "COMPACT")

# Separação dos veículos compactos dos demais
veiculos_nao_compactos <- veiculos %>% 
  filter(classe != "COMPACT")

# Cálculo das médias e desvios-padrão de consumo e emissão de CO2 em cada grupo
media_consumo_compactos <- mean(veiculos_compactos$consumo_estrada)
media_emissao_compactos <- mean(veiculos_compactos$emissao_CO2)
desvio_consumo_compactos <- sd(veiculos_compactos$consumo_estrada)
desvio_emissao_compactos <- sd(veiculos_compactos$emissao_CO2)

media_consumo_nao_compactos <- mean(veiculos_nao_compactos$consumo_estrada)
media_emissao_nao_compactos <- mean(veiculos_nao_compactos$emissao_CO2)
desvio_consumo_nao_compactos <- sd(veiculos_nao_compactos$consumo_estrada)
desvio_emissao_nao_compactos <- sd(veiculos_nao_compactos$emissao_CO2)

# Teste t independente para comparar as médias
t.test(veiculos_compactos$consumo_estrada, veiculos_nao_compactos$consumo_estrada, var.equal = TRUE)
t.test(veiculos_compactos$emissao_CO2, veiculos_nao_compactos$emissao_CO2, var.equal = TRUE)

# Cria um data frame com as médias de emissão de CO2 para cada tipo de carro
df_medias <- data.frame(
  tipo = c("Compacto", "Não compacto"),
  media_emissao = c(media_emissao_compactos, media_emissao_nao_compactos)
)

# Cria o gráfico de barras
ggplot(df_medias, aes(x = tipo, y = media_emissao, fill = tipo)) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.8) +
  labs(x = "Tipo de carro", y = "Média de emissão de CO2", 
       title = "Média de emissão de CO2 por tipo de carro") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_bw()



#### Questão 4: Existe diferença na quantidade média de emissão de CO2 nas SUV-Small das marcas KIA, SUBARU e TOYOTA?

# Filtro para selecionar apenas os veículos SUV-Small das marcas KIA, SUBARU e TOYOTA
veiculos_suv <- veiculos %>% 
  filter(classe == "SUV - SMALL", marca %in% c("KIA", "SUBARU", "TOYOTA"))

# Agrupamento dos veículos pela marca e cálculo da média de emissão de CO2
media_emissao_suv <- veiculos_suv %>% 
  group_by(marca) %>% 
  summarize(media_emissao = mean(emissao_CO2))  

# Teste ANOVA(3 ou mais grupos) para verificar se existe diferença significativa na média de emissão de CO2 entre os grupos
anova_emissao_suv <- aov(emissao_CO2 ~ marca, data = veiculos_suv)
summary(anova_emissao_suv)

# Realização do teste Tukey
tukey_emissao <- TukeyHSD(anova_emissao_suv)
tukey_emissao



ggplot(veiculos_suv %>% filter(marca %in% c("KIA", "SUBARU", "TOYOTA")),
       aes(x = marca, y = emissao_CO2, fill = marca)) +
  geom_bar(stat = "summary", fun = "mean", color = "black") +
  labs(title = "Média de emissão de CO2 nas SUV-Small",
       x = "Marca",
       y = "Média de emissão de CO2") +
  theme_bw()





