

#Questionário com perguntas a serem feitas a respeito de uma rede complexa (redes genéricas e sistema de transporte público)

####################################################################################################################

#1- Instalar bibliotecas
#Igraph
#install.packages('igraph')
#ggplot2
#install.packages('ggplot2')
#ggmap
#install.packages('ggmap')

####################################################################################################################

#2- Puxar as bibliotecas instaladas
#Igraph
require(igraph)
#ggplot2
require(ggplot2)
#ggmap
require(ggmap)


####################################################################################################################

#3- Definir o diretório de trabalho
setwd("D:/Pessoal/Estudo/UTFPR/CDA/redes")

####################################################################################################################

#4- Carregar arquivos de arestas
##4.1 - Rede complexa de transcrição das interações entre operões no organismo E. coli
e_coli <- read_graph('e_coli.net', 'ncol', directed = TRUE)


##4.2 - Rede complexa de interações de blogs sobre AIDS
aids <- read_graph('AIDSBlog.txt', 'edgelist')

##4.3 - Rede complexa de conexões entre roteadores da Internet 
internet <- read_graph('router_INET.txt','edgelist')

##4.4 - Rede complexa do sistema elétrico do oeste dos EUA
power <- read_graph('power.gml',format='gml')

##4.5 - Rede complexa do sistema de transporte urbano de Curitiba
bus_curitiba <-  load('bus_curitiba.R')

##4.6 - Rede complexa do clube de karatekas Zachary
zachary <- make_graph("Zachary")

####################################################################################################################

#5- Respostas dos questionários


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
##5.1 - E.Coli
grafo <- e_coli

###5.1.1 -  Qual a representação adotada?

# O arquivo disponibilizado possui as informações das arestas com seus pesos

###5.1.2 - Qual a melhor forma de visualizar a rede complexa?

#Representação Circular
  # Criar um layout para o grafo
  layout_circular <- layout.circle(grafo)

  #Plotar com o layout
  plot(grafo, layout = layout_circular)

  
#Representação Hierarquica (Arvore)
  # Criar um layout para o grafo
  layout_hierarquia <- layout_as_tree(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_hierarquia)

  
#Representação bidimensional
  # Criar um layout para o grafo
  layout_bidimensional <- layout_nicely(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_bidimensional)


#Representação em estrela
  # Criar um layout para o grafo
  layout_estrela <- layout_as_star(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_estrela)


#Representação em grade
  # Criar um layout para o grafo
  layout_grade <- layout_on_grid(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_grade)


#Representação em esfera
  # Criar um layout para o grafo
  layout_esfera <- layout_on_sphere(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_esfera) 
  
  
#Representação Graph Optimal Placement  
  # Criar um layout para o grafo
  layout_optimal <- layout.graphopt(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_optimal)
  
    
#Representação aleatória
  # Criar um layout para o grafo
  layout_random <- layout.random(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_random)

  
#Representação automatica
  # Criar um layout para o grafo
  layout_auto <- layout.auto(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_auto)


  #Neste caso a melhor visualização é a bidimensional ou a automatica.
  
  
  
  ###5.1.3. Qual a distribuição de valores das seguintes métricas:
  #         a. Grau/grau ponderado

  
  # Calcular o grau para cada nó
  grau <- degree(grafo)
  
  # Calcular o grau ponderado para cada nó (se o grafo tiver pesos nas arestas)
  grau_ponderado <- strength(grafo)
  
  # Visualizar a distribuição de grau
  histograma <- hist(grau, main = "Distribuição de Grau", 
                     xlab = "Grau",
                     ylab = "Frequência",
                     breaks = seq(0, max(grau) + 5, by = 5))
  # Adicionar rótulos automáticos às barras
  text(histograma$mids, histograma$counts, labels = histograma$counts, pos = 3)
  # Personalizar os rótulos no eixo x
  axis(1, at = seq(0, max(grau) + 5, by = 5), labels = seq(0, max(grau) + 5, by = 5))
  
  
  # Visualizar a distribuição de grau ponderado
  histograma <- hist(grau_ponderado, main = "Distribuição de Grau Ponderado",
                     xlab = "Grau Ponderado",
                     ylab = "Frequência",
                     breaks = seq(0, max(grau_ponderado) + 5, by = 5))
  # Adicionar rótulos automáticos às barras
  text(histograma$mids, histograma$counts, labels = histograma$counts, pos = 3)
  # Personalizar os rótulos no eixo x
  axis(1, at = seq(0, max(grau_ponderado) + 5, by = 5), labels = seq(0, max(grau_ponderado) + 5, by = 5))
  
  

  #         b. Caminho/caminho ponderado
  # Calcular o caminho mais curto entre dois nós (caminho não ponderado)
  caminho <- shortest.paths(grafo, mode = "all")
  
  # Calcular o caminho mais curto ponderado entre dois nós (se o  grafo tiver pesos nas arestas)
  caminho_ponderado <- shortest.paths(grafo, mode = "all", weights = E(grafo)$weight)
  
  # Visualizar a distribuição de caminho
  hist(caminho, main = "Distribuição de Caminho", xlab = "Caminho", ylab = "Frequência")
  
  # Visualizar a distribuição de caminho ponderado
  hist(caminho_ponderado, main = "Distribuição de Caminho Ponderado", xlab = "Caminho Ponderado", ylab = "Frequência")
  
  
  
  
  
  
  #         c. Coeficiente de clusterização (transitividade) (com ou sem peso)
  # Calcular o coeficiente de clusterização para cada nó (com pesos nas arestas)
  coeficiente_clusterizacao <- transitivity(grafo, type = "weighted") 
  
  # Visualizar a distribuição do coeficiente de clusterização
  hist(coeficiente_clusterizacao, main = "Distribuição do Coeficiente de Clusterização", xlab = "Coeficiente de Clusterização", ylab = "Frequência")

  # Detectar comunidades usando o algoritmo de Louvain
  clusters <- cluster_louvain(grafo_nd)
  
  # Avaliação a qualidade das comunidades usando a métrica de modularidade
  modularidade <- modularity(clusters)
  
  # Definção do tamanho dos vértices como 5
  V(grafo)$size <- 5
  
  # Plotar o resultado
  plot(clusters, grafo, vertex.label = NA)
  
  
  #Método Edge Betweeness
  
  
  # Calcular a centralidade de betweenness das arestas
  cluster_ecoli_betw <- edge_betweenness(grafo_nd)
  
  #remover arestar com centralidade maxima
  grafo_edge <- delete_edges(grafo_nd,which.max(cluster_ecoli_betw))
  
  #aplicasção fast greedy
  cluster_edge <- cluster_fast_greedy(grafo_edge)
  
  # Avaliação da qualidade das comunidades usando a métrica de modularidade
  modularidade_edge <- modularity(cluster_edge)
  
  # Definção do tamanho dos vértices como 5
  V(grafo)$size <- 5
  
  # Plotar o resultado
  plot(cluster_edge, grafo, vertex.label = NA)
  
  #Walktrap modularidade = 0,7709  
  #Edge Between modularidade = 0,7721
  
  #Edgebetween é melhor
  
  
  ###5.1.5. Qual o nó mais importante e por quê?
  # Centralidade de Grau (Degree Centrality)
  
  # Cálculo da centralidade de grau para todos os nós
  centrality_degree <- degree(grafo)
  
  # Encontrar o nó com o maior grau
  no_maior_grau <- which.max(centrality_degree)
  
  ###5.1.6. Qual a aresta mais importante e por quê?
  # Centralidade de Intermediação (Betweenness Centrality)
  
  # Calcular a centralidade de intermediação (Betweenness) das arestas
  betweenness_values <- edge_betweenness(grafo)
  
  # Encontrar o índice da aresta mais importante
  indice_aresta_mais_importante <- which.max(betweenness_values)
  
  # Obter informações da aresta mais importante
  aresta_mais_importante <- E(grafo)[indice_aresta_mais_importante]
  
  
  ###5.1.7. Qual o diâmetro da rede?
  # Calcular o diâmetro da rede
  diametro <- diameter(grafo, directed = TRUE)
  
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  ##5.2 - AIDS
  grafo <- aids
  grafo_nd <- as.undirected(aids)
  
  ###5.1.1 -  Qual a representação adotada?
  
  # O arquivo disponibilizado possui as informações das arestas que ligam genes. Existem 3 tipos de ligações.
  
  ###5.1.2 - Qual a melhor forma de visualizar a rede complexa?
  
  # Definção do tamanho dos vértices como 5
  V(grafo)$size <- 5
  
  #Representação Circular
  # Criar um layout para o grafo
  layout_circular <- layout.circle(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_circular, vertex.label = NA, main='Layout Circular')
  
  
  #Representação Hierarquica (Arvore)
  # Criar um layout para o grafo
  layout_hierarquia <- layout_as_tree(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_hierarquia,vertex.label = NA, main='layout hierarquia')
  
  
  #Representação bidimensional
  # Criar um layout para o grafo
  layout_bidimensional <- layout_nicely(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_bidimensional,vertex.label = NA, main='Layout bidimensional')
  
  
  #Representação em estrela
  # Criar um layout para o grafo
  layout_estrela <- layout_as_star(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_estrela,vertex.label = NA, main='Layout estrela')
  
  
  #Representação em grade
  # Criar um layout para o grafo
  layout_grade <- layout_on_grid(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_grade,vertex.label = NA, main='Layout em grade')
  
  
  #Representação em esfera
  # Criar um layout para o grafo
  layout_esfera <- layout_on_sphere(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_esfera,vertex.label = NA, main='Layout em esfera') 
  
  
  #Representação Graph Optimal Placement  
  # Criar um layout para o grafo
  layout_optimal <- layout.graphopt(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_optimal,vertex.label = NA, main='Layout Graph Optimal Placement')
  
  
  #Representação aleatória
  # Criar um layout para o grafo
  layout_random <- layout.random(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_random,vertex.label = NA, main='Layout aleatório')
  
  
  #Representação automatica
  # Criar um layout para o grafo
  layout_auto <- layout.auto(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_auto,vertex.label = NA, main='Layout automatico')
  
  
  #Neste caso a melhor visualização é a bidimensional ou a automatica.
  
  
  
  ###5.1.3. Qual a distribuição de valores das seguintes métricas:
  #         a. Grau/grau ponderado
  
  
  # Calcular o grau para cada nó
  grau <- degree(grafo)
  
  # Visualizar a distribuição de grau
  histograma_grau <- hist(grau, main = "Distribuição de Grau", 
                          xlab = "Grau",
                          ylab = "Frequência",
                          breaks = seq(0, max(grau) + 5, by = 5))
  # Adicionar rótulos automáticos às barras
  text(histograma_grau$mids, histograma_grau$counts, labels = histograma_grau$counts, pos = 3)
  # Personalizar os rótulos no eixo x
  axis(1, at = seq(0, max(grau) + 5, by = 5), labels = seq(0, max(grau) + 5, by = 5))
  
  
  #         b. Caminho/caminho ponderado
  # Calcular o caminho mais curto entre dois nós (caminho não ponderado)
  caminho <- shortest.paths(grafo, mode = "all")
  
  # Visualizar a distribuição de caminho
  histograma_caminho <-hist(caminho, main = "Distribuição de Caminho", xlab = "Caminho", ylab = "Frequência")
  # Adicionar rótulos automáticos às barras
  text(histograma_caminho$mids, histograma_caminho$counts, labels = histograma_caminho$counts, pos = 3)
  # Personalizar os rótulos no eixo x
  axis(1, at = seq(0, max(grau) + 5, by = 5), labels = seq(0, max(grau) + 5, by = 5))
  
  
  #         c. Coeficiente de clusterização (transitividade) (com ou sem peso)
  # Calcular o coeficiente de clusterização para cada nó (com pesos nas arestas)
  coeficiente_clusterizacao <- transitivity(grafo, type = "global") 
  
  
  ###5.1.4. Qual o tipo de agrupamento (formação de comunidades) melhor se encaixa para a rede complexa
  
  # Método de Walktrap
  
  # Detectar comunidades usando o algoritmo de Louvain
  clusters <- cluster_louvain(grafo_nd)
  
  # Avaliação a qualidade das comunidades usando a métrica de modularidade
  modularidade <- modularity(clusters)
  
  # Definção do tamanho dos vértices como 5
  V(grafo_nd)$size <- 5
  
  # Plotar o resultado
  plot(clusters, grafo_nd, vertex.label = NA)
  
  
  #Método Edge Betweeness
  
  
  # Calcular a centralidade de betweenness das arestas
  cluster_betw <- edge_betweenness(grafo_nd)
  
  #remover arestar com centralidade maxima
  grafo_edge <- delete_edges(grafo_nd,which.max(cluster_ecoli_betw))
  
  #aplicasção fast greedy
  cluster_edge <- cluster_fast_greedy(grafo_edge)
  
  # Avaliação da qualidade das comunidades usando a métrica de modularidade
  modularidade_edge <- modularity(cluster_edge)
  
  # Definção do tamanho dos vértices como 5
  V(grafo)$size <- 5
  
  # Plotar o resultado
  plot(cluster_edge, grafo, vertex.label = NA)
  
  #Walktrap modularidade = 0,6431  
  #Edge Between modularidade = 0,6524
  
  #Edgebetween é melhor
  
  
  ###5.1.5. Qual o nó mais importante e por quê?
  # Centralidade de Grau (Degree Centrality)
  
  # Cálculo da centralidade de grau para todos os nós
  centrality_degree <- degree(grafo)
  
  # Encontrar o nó com o maior grau
  no_maior_grau <- which.max(centrality_degree)
  
  ###5.1.6. Qual a aresta mais importante e por quê?
  # Centralidade de Intermediação (Betweenness Centrality)
  
  # Calcular a centralidade de intermediação (Betweenness) das arestas
  betweenness_values <- edge_betweenness(grafo)
  
  # Encontrar o índice da aresta mais importante
  indice_aresta_mais_importante <- which.max(betweenness_values)
  
  # Obter informações da aresta mais importante
  aresta_mais_importante <- E(grafo)[indice_aresta_mais_importante]
  
  
  ###5.1.7. Qual o diâmetro da rede?
  # Calcular o diâmetro da rede
  diametro <- diameter(grafo, directed = TRUE)
  
  
  #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
  ##5.3 - Internet
  grafo <- internet
  grafo_nd <- as.undirected(grafo)
  
  ###5.1.1 -  Qual a representação adotada?
  
  # O arquivo disponibilizado possui as informações das arestas que ligam genes. Existem 3 tipos de ligações.
  
  ###5.1.2 - Qual a melhor forma de visualizar a rede complexa?
  
  # Definção do tamanho dos vértices como 5
  V(grafo)$size <- 5
  
  #Representação Circular
  # Criar um layout para o grafo
  layout_circular <- layout.circle(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_circular, vertex.label = NA, main='Layout Circular')
  
  
  #Representação Hierarquica (Arvore)
  # Criar um layout para o grafo
  layout_hierarquia <- layout_as_tree(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_hierarquia,vertex.label = NA, main='layout hierarquia')
  
  
  #Representação bidimensional
  # Criar um layout para o grafo
  layout_bidimensional <- layout_nicely(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_bidimensional,vertex.label = NA, main='Layout bidimensional')
  
  
  #Representação em estrela
  # Criar um layout para o grafo
  layout_estrela <- layout_as_star(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_estrela,vertex.label = NA, main='Layout estrela')
  
  
  #Representação em grade
  # Criar um layout para o grafo
  layout_grade <- layout_on_grid(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_grade,vertex.label = NA, main='Layout em grade')
  
  
  #Representação em esfera
  # Criar um layout para o grafo
  layout_esfera <- layout_on_sphere(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_esfera,vertex.label = NA, main='Layout em esfera') 
  
  
  #Representação Graph Optimal Placement  
  # Criar um layout para o grafo
  layout_optimal <- layout.graphopt(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_optimal,vertex.label = NA, main='Layout Graph Optimal Placement')
  
  
  #Representação aleatória
  # Criar um layout para o grafo
  layout_random <- layout.random(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_random,vertex.label = NA, main='Layout aleatório')
  
  
  #Representação automatica
  # Criar um layout para o grafo
  layout_auto <- layout.auto(grafo)
  
  #Plotar com o layout
  plot(grafo, layout = layout_auto,vertex.label = NA, main='Layout automatico')
  
  
  #Neste caso a melhor visualização é a bidimensional ou a automatica.
  
  
  
  ###5.1.3. Qual a distribuição de valores das seguintes métricas:
  #         a. Grau/grau ponderado
  
  
  # Calcular o grau para cada nó
  grau <- degree(grafo)
  
  # Visualizar a distribuição de grau
  histograma_grau <- hist(grau, main = "Distribuição de Grau", 
                          xlab = "Grau",
                          ylab = "Frequência",
                          breaks = seq(0, max(grau) + 5, by = 5))
  # Adicionar rótulos automáticos às barras
  text(histograma_grau$mids, histograma_grau$counts, labels = histograma_grau$counts, pos = 3)
  # Personalizar os rótulos no eixo x
  axis(1, at = seq(0, max(grau) + 5, by = 5), labels = seq(0, max(grau) + 5, by = 5))
  
  
  #         b. Caminho/caminho ponderado
  # Calcular o caminho mais curto entre dois nós (caminho não ponderado)
  caminho <- shortest.paths(grafo, mode = "all")
  
  # Visualizar a distribuição de caminho
  histograma_caminho <-hist(caminho, main = "Distribuição de Caminho", xlab = "Caminho", ylab = "Frequência")
  # Adicionar rótulos automáticos às barras
  text(histograma_caminho$mids, histograma_caminho$counts, labels = histograma_caminho$counts, pos = 3)
  # Personalizar os rótulos no eixo x
  axis(1, at = seq(0, max(grau) + 5, by = 5), labels = seq(0, max(grau) + 5, by = 5))
  
  
  #         c. Coeficiente de clusterização (transitividade) (com ou sem peso)
  # Calcular o coeficiente de clusterização para cada nó (com pesos nas arestas)
  coeficiente_clusterizacao <- transitivity(grafo, type = "global") 
  
  
  ###5.1.4. Qual o tipo de agrupamento (formação de comunidades) melhor se encaixa para a rede complexa
  
  # Método de Walktrap
  
  # Detectar comunidades usando o algoritmo de Louvain
  clusters <- cluster_louvain(grafo_nd)
  
  # Avaliação a qualidade das comunidades usando a métrica de modularidade
  modularidade <- modularity(clusters)
  
  # Definção do tamanho dos vértices como 5
  V(grafo_nd)$size <- 5
  
  # Plotar o resultado
  plot(clusters, grafo_nd, vertex.label = NA)
  
  
  #Método Edge Betweeness
  
  
  # Calcular a centralidade de betweenness das arestas
  cluster_betw <- edge_betweenness(grafo_nd)
  
  #remover arestar com centralidade maxima
  grafo_edge <- delete_edges(grafo_nd,which.max(cluster_ecoli_betw))
  
  #aplicasção fast greedy
  cluster_edge <- cluster_fast_greedy(grafo_edge)
  
  # Avaliação da qualidade das comunidades usando a métrica de modularidade
  modularidade_edge <- modularity(cluster_edge)
  
  # Definção do tamanho dos vértices como 5
  V(grafo)$size <- 5
  
  # Plotar o resultado
  plot(cluster_edge, grafo, vertex.label = NA)
  
  #Walktrap modularidade = 0,6431  
  #Edge Between modularidade = 0,6524
  
  #Edgebetween é melhor
  
  
  ###5.1.5. Qual o nó mais importante e por quê?
  # Centralidade de Grau (Degree Centrality)
  
  # Cálculo da centralidade de grau para todos os nós
  centrality_degree <- degree(grafo)
  
  # Encontrar o nó com o maior grau
  no_maior_grau <- which.max(centrality_degree)
  
  ###5.1.6. Qual a aresta mais importante e por quê?
  # Centralidade de Intermediação (Betweenness Centrality)
  
  # Calcular a centralidade de intermediação (Betweenness) das arestas
  betweenness_values <- edge_betweenness(grafo)
  
  # Encontrar o índice da aresta mais importante
  indice_aresta_mais_importante <- which.max(betweenness_values)
  
  # Obter informações da aresta mais importante
  aresta_mais_importante <- E(grafo)[indice_aresta_mais_importante]
  
  
  ###5.1.7. Qual o diâmetro da rede?
  # Calcular o diâmetro da rede
  diametro <- diameter(grafo, directed = TRUE)   
  
  
  





