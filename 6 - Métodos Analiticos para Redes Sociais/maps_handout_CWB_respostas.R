#Rede CWB Transporte

#1. Qual o ponto de ônibus mais “carregado”? 
#--> estamos falando de centralidade de grau. Quantas arestas estão conectadas a um nó específico
#--> utilizado o exemplo1 para responder a pergunta
# Calcule a centralidade de grau para cada nó no grafo
degree_centrality <- degree(curitiba_l, mode = "all")
# Encontre o nó com a maior centralidade de grau (o ponto mais carregado)
max_degree_node <- which.max(degree_centrality)
# Imprima o resultado
cat("O ponto mais carregado é o nó:", max_degree_node, "com centralidade de grau:", degree_centrality[max_degree_node], "\n")


#2. Em média, os pontos de ônibus servem quantas linhas?
#degree_distribution do pacote igraph
#--> utilizado o exemplo1 para responder a pergunta
# Calcule a distribuição de graus dos nós no grafo
degree_dist <- degree_distribution(curitiba_l, mode = "all")
# Crie um vetor que armazena o número de pontos servidos por linhas
num_points_served_by_lines <- 0:(length(degree_dist) - 1)
# Calcule a média ponderada de pontos servidos por linhas
average_points_served <- sum(num_points_served_by_lines * degree_dist) / sum(degree_dist)
# Imprima o resultado
cat("A média de pontos servidos por linhas é:", average_points_served, "\n")


#3. Qual a distância média dos caminhos existentes (em KM)? --> mean_distance(g_random)
#shortest.paths do pacote igraph 
#resolvido com o grafo do exemplo 2
# Calcule as distâncias entre todos os pares de nós no grafo
all_distances <- shortest.paths(curitiba_l, v = V(curitiba_l), to = V(curitiba_l))
# Converta as distâncias de metros para quilômetros (1 metro = 0.001 km)
all_distances_km <- all_distances * 0.001
# Calcule a média das distâncias (em quilômetros)
average_distance_km <- mean(all_distances_km[is.finite(all_distances_km)], na.rm = TRUE)
#average_distance_km <- mean(all_distances_km, na.rm = TRUE)
# Imprima o resultado
cat("A distância média dos caminhos é:",average_distance_km, "km\n")




#4. Localize graficamente o maior caminho existente (KM) 
# precisa ser com o objet l_curitiba do ex2, pois no ex3 é filtrado somente para a linha de ônibus 020
# Calcule as distâncias entre todos os pares de nós no grafo (em quilômetros)
all_distances_km <- shortest.paths(curitiba_l, v = V(curitiba_l), to = V(curitiba_l)) * 0.001
# Encontre a maior distância (em quilômetros) no grafo
max_distance_km <- max(all_distances_km[is.finite(all_distances_km)], na.rm = TRUE)
# Crie um gráfico de barras mostrando a maior distância
data <- data.frame(Nodes = c("Maior Distância (km)"), Distance = c(max_distance_km))
ggplot(data, aes(x = Nodes, y = Distance, fill = Nodes)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_minimal() +
  labs(title = "Maior Distância em Quilômetros")


#a. Diâmetro da rede ponderada --> diameter(g_random)
# Crie um grafo ponderado representando a rede
# Suponha que seu grafo já esteja definido aqui como 'weighted_graph'
# Calcule o diâmetro ponderado
weighted_diameter <- diameter(weighted_graph, directed = FALSE, weights = E(weighted_graph)$weight)
# Imprima o resultado
cat("O diâmetro ponderado da rede é:", weighted_diameter, "\n")


#5. Em média, os caminhos existentes têm quantos pontos? 
# Calcule a média do número de pontos nos caminhos existentes
average_points_on_paths <- average.path.length(transport_graph, directed = FALSE)
# Imprima o resultado
cat("A média do número de pontos nos caminhos existentes é:", average_points_on_paths, "\n")




#6. Quais os 10 pontos de ônibus mais críticos do ponto de vista de caminhos?
#medida a ser utilizada: centralidade de intermediação (betweenness centrality)
#a. Betweenness
# Calcule a centralidade de intermediação (betweenness centrality) para cada nó
betweenness_centrality <- betweenness(transport_graph)
# Classifique os nós com base na centralidade de intermediação (do maior para o menor)
top_10_critical_nodes <- order(-betweenness_centrality)[1:10]
# Imprima os 10 pontos mais críticos
cat("Os 10 pontos mais críticos (baseados na centralidade de intermediação) são:\n")
for (i in 1:10) {
  cat("Nó:", top_10_critical_nodes[i], " - Centralidade de Intermediação:", betweenness_centrality[top_10_critical_nodes[i]], "\n")
}



#7. Quais os 10 pontos mais centrais do sistema de transporte?
#a. Closeness
# Calcule a centralidade de proximidade para cada nó
closeness_centrality <- closeness(transport_graph, mode = "all")
# Classifique os nós com base na centralidade de proximidade (do maior para o menor)
top_10_closeness_nodes <- order(-closeness_centrality)[1:10]
# Imprima os 10 pontos mais centrais
cat("Os 10 pontos mais centrais (baseados na centralidade de proximidade) são:\n")
for (i in 1:10) {
  cat("Nó:", top_10_closeness_nodes[i], " - Centralidade de Proximidade:", closeness_centrality[top_10_closeness_nodes[i]], "\n")
}





#8. Como você classificaria a rede?
#a. Aleatória, mundo pequeno, escala-livre
#Rede Livre de Escala (Scale-Free Network):
# Se a rede de transporte público exibir uma distribuição de grau em que alguns nós (estações ou paradas) têm um grande número de conexões (linhas de transporte) enquanto a maioria dos nós tem apenas algumas conexões, ela pode ser classificada como uma rede livre de escala. Isso significa que alguns pontos-chave (grandes hubs de transporte) desempenham um papel crucial na rede.




#9. Usando a representação P-Space, os caminhos existentes exigem quantas trocas de ônibus?



#10. Plote um mapa da cidade contendo apenas as linhas de ônibus “expresso”
#--> resposta no exemplo 4, trocar string por EXPRESSO


#11. Faça o mesmo para a linha de ônibus “alimentadores”
#--> resposta no exemplo 4, trocar String por ALIMENTADOR
