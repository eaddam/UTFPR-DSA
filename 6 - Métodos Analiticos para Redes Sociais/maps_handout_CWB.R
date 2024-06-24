require(ggmap)
require(ggplot2)
require(igraph)
require(geosphere)

#setwd("C:/Users/paulo/OneDrive/Documentos/paulo_workspace/Estudos/MasterAbschluss/UTFPR_2022_EspecializacaoCienciaDeDados/Aula_06_RedesSociais/exercicio_final")
setwd("C:/Users/c803657/Documents/Estudos/data-science-IA/UTFPR_pos_2022/aula_06_metodosAnaliticos/entrega_exercício")



load("CWB/all_bus_info.R")
#nesta primeira etapa o mapa da cidade será desenhado com os pontos de ônibus

# get bus line information
df_lines <- data.frame(index = numeric(length(all_bus_info)),
                       code = character(length(all_bus_info)),
                       name = character(length(all_bus_info)),
                       cat = character(length(all_bus_info)),
                       stringsAsFactors = FALSE)
for (i in 1:length(all_bus_info)) {
  df_lines$index[i] <- i
  df_lines$code[i] <- all_bus_info[[i]][[1]]
  df_lines$name[i] <- all_bus_info[[i]][[2]]
  df_lines$cat[i] <- all_bus_info[[i]][[3]]
}

# get bus stop information
df_all_stops <- data.frame(num = character(0),
                           lat = numeric(0),
                           lon = numeric(0),
                           group = character(0),
                           cat = character(0),
                           stringsAsFactors = FALSE)
for (i in 1:length(all_bus_info)) {
  num_of_directions <- length(all_bus_info[[i]][[4]])
  if (num_of_directions==0)
    next
  a_line_code <- all_bus_info[[i]][[1]]
  for (j in 1:num_of_directions) {
    total_stops <- nrow(all_bus_info[[i]][[4]][[j]][[2]])
    df_sub <- data.frame(num = character(total_stops),
                         lat = numeric(total_stops),
                         lon = numeric(total_stops),
                         group = character(total_stops),
                         cat = character(total_stops),
                         stringsAsFactors = FALSE)
    df_sub$num <- all_bus_info[[i]][[4]][[j]][[2]]$NUM
    df_sub$lat <- all_bus_info[[i]][[4]][[j]][[2]]$LAT
    df_sub$lon <- all_bus_info[[i]][[4]][[j]][[2]]$LON
    df_sub$cat <- df_lines[df_lines$code==a_line_code,]$cat
    df_all_stops <- rbind(df_all_stops, df_sub)
  }
}

# removing repetition
df_all_stops <- df_all_stops[!duplicated(df_all_stops$num), ]

# plotting
a_delta = 0.001
city_region <- c(
  left = min(df_all_stops$lon)-a_delta,
  top = max(df_all_stops$lat)+a_delta,
  right = max(df_all_stops$lon)+a_delta,
  bottom = min(df_all_stops$lat)-a_delta)
my_curitiba_v11 <- get_map(
  location = city_region, zoom = 12, source='stamen')
ggmap(my_curitiba_v11, extent = 'device') +
  geom_point(aes(x = lon, y = lat, color = factor(cat)),
             data = df_all_stops)


#install.packages("C:/Users/c803657/Documents/Estudos/Rprogramming/geosphere_1.5-18.zip", repos = NULL, type = "win.binary")

#nesta etapa é criado o grafo
#exemplo 1
curitiba_l <- make_empty_graph(directed = FALSE)
df_empty <- data.frame(from = character(0),
                       to = character(0))
for (i in 1:length(all_bus_info)) {
  num_of_directions <- length(all_bus_info[[i]][[4]])
  if (num_of_directions==0)
    next
  df_stops_per_line <- df_empty
  for (j in 1:num_of_directions) {
    paste(i)
    a_df <- all_bus_info[[i]][[4]][[j]][[2]]
    total_stops <- nrow(a_df)
    df_sub <- data.frame(from = a_df$NUM[1:nrow(a_df) - 1],
                         to = a_df$NUM[-1])
    df_stops_per_line <- rbind(df_stops_per_line, df_sub)
  }
  curitiba_l <- curitiba_l +
    graph_from_data_frame(df_stops_per_line, directed = FALSE)
}
# feeding information of each vertex/bus stop
V(curitiba_l)[order(V(curitiba_l)$name)]$lat <- df_all_stops[order(df_all_stops$num),]$lat
V(curitiba_l)[order(V(curitiba_l)$name)]$lon <- df_all_stops[order(df_all_stops$num),]$lon
V(curitiba_l)[order(V(curitiba_l)$name)]$group <- df_all_stops[order(df_all_stops$num),]$group
# feeding distance (in meters) between edges
# cria uma lista de arestas (edges) contendo os pares de vértices que formam elas
bus_stop_connection <- as_edgelist(curitiba_l) #teremos uma lista contendo as conexões dos pontos de ônibus
# a função vapply irá aplicar a função fornecida como parâmetro no objeto especificado, no caso o bus_stop_connections
# a função which retornará uma lista de índices que contenham a condição especificada, no caso num==key
idx_1 <- vapply(bus_stop_connection[,1],
                function(key) { which(df_all_stops$num==key) },
                0)
idx_2 <- vapply(bus_stop_connection[,2],
                function(key) { which(df_all_stops$num==key) },
                0)
# determina o peso das arestas com a função distHaversine
# o distHaversine calcula a distância entre os pares de coordenadas idx1 e idx2
# ao final, o cbind cria uma matriz a partir das coordenadas
E(curitiba_l)$weight <- distHaversine(
  cbind(df_all_stops$lon[idx_1], df_all_stops$lat[idx_1]),
  cbind(df_all_stops$lon[idx_2], df_all_stops$lat[idx_2]))

#exemplo 2
curitiba_l <- make_empty_graph(directed = FALSE)
df_empty <- data.frame(from = character(0),
                       to = character(0))

?nrow
?cbind
for (i in 1:length(all_bus_info)) {
  num_of_directions <- length(all_bus_info[[i]][[4]])
  if (num_of_directions==0)
    next
  df_stops_per_line <- df_empty
  for (j in 1:num_of_directions) {
    a_df <- all_bus_info[[i]][[4]][[j]][[2]]
    total_stops <- nrow(a_df)
    df_sub <- data.frame(
      from = a_df$NUM[1:nrow(a_df) - 1],
      to = a_df$NUM[-1],
      weight = distHaversine(
        cbind(a_df$LON[1:nrow(a_df) - 1],
              a_df$LAT[1:nrow(a_df) - 1]),
        cbind(a_df$LON[-1],
              a_df$LAT[-1]))
    )
    df_stops_per_line <- rbind(df_stops_per_line, df_sub)
  }
  curitiba_l <- curitiba_l +
    graph_from_data_frame(df_stops_per_line, directed = FALSE)
  if(length(grep('_[[:digit:]]\\b',
                 edge_attr_names(curitiba_l)))) {
    E(curitiba_l)$weight <-
      apply(cbind(E(curitiba_l)$weight_1,
                  E(curitiba_l)$weight_2),
            1, mean, na.rm = TRUE)
    curitiba_l <- delete_edge_attr(curitiba_l, 'weight_1')
    curitiba_l <- delete_edge_attr(curitiba_l, 'weight_2')
  }
}
# feeding information of each vertex/bus stop
V(curitiba_l)[order(V(curitiba_l)$name)]$lat <-
  df_all_stops[order(df_all_stops$num),]$lat
V(curitiba_l)[order(V(curitiba_l)$name)]$lon <-
  df_all_stops[order(df_all_stops$num),]$lon
V(curitiba_l)[order(V(curitiba_l)$name)]$group <-
  df_all_stops[order(df_all_stops$num),]$group



#exemplo 3 para pegar o grafo de uma linha específica, no caso, a 020
wanted_line_code = '020'
bus_line_graph_l <- make_empty_graph(directed = FALSE)
df_empty <- data.frame(from = character(0),
                       to = character(0))
for (i in 1:length(all_bus_info)) {
  num_of_directions <- length(all_bus_info[[i]][[4]])
  if (num_of_directions==0)
    next
  a_line_code <- all_bus_info[[i]][[1]]
  if (a_line_code==wanted_line_code)
  {
    df_stops_per_line <- df_empty
    for (j in 1:num_of_directions) {
      a_df <- all_bus_info[[i]][[4]][[j]][[2]]
      total_stops <- nrow(a_df)
      df_sub <- data.frame(from = a_df$NUM[1:nrow(a_df) - 1],
                           to = a_df$NUM[-1])
      df_stops_per_line <- rbind(df_stops_per_line, df_sub)
    }
    bus_line_graph_l <-
      graph_from_data_frame(df_stops_per_line, directed = FALSE)
    break;
  }
}
# feeding information of each vertex/bus stop
idx_1 <- vapply(V(bus_line_graph_l)$name,
                function(key) { which(df_all_stops$num==key) },
                0)
V(bus_line_graph_l)$lat <- df_all_stops$lat[idx_1]
V(bus_line_graph_l)$lon <- df_all_stops$lon[idx_1]
V(bus_line_graph_l)$group <- df_all_stops$group[idx_1]
# feeding distance (in meters) between edges
bus_stop_connection <- as_edgelist(bus_line_graph_l)
idx_1 <- vapply(bus_stop_connection[,1],
                function(key) { which(df_all_stops$num==key) },
                0)
idx_2 <- vapply(bus_stop_connection[,2],
                function(key) { which(df_all_stops$num==key) },
                0)
E(bus_line_graph_l)$weight <- distHaversine(
  cbind(df_all_stops$lon[idx_1], df_all_stops$lat[idx_1]),
  cbind(df_all_stops$lon[idx_2], df_all_stops$lat[idx_2]))
# plotting such a bus line
df_wanted_line_1 <- data.frame(lat = V(bus_line_graph_l)$lat,
                               lon = V(bus_line_graph_l)$lon)
df_wanted_line_2 <- data.frame(x = df_all_stops$lon[idx_1],
                               y = df_all_stops$lat[idx_1],
                               xend = df_all_stops$lon[idx_2],
                               yend = df_all_stops$lat[idx_2])

a_delta = 0.001
city_region <- c(
  left = min(df_all_stops$lon)-a_delta,
  top = max(df_all_stops$lat)+a_delta,
  right = max(df_all_stops$lon)+a_delta,
  bottom = min(df_all_stops$lat)-a_delta)
my_curitiba_v12 <- get_map(
  location = city_region, zoom = 13, source = "stamen")
ggmap(my_curitiba_v12, extent = 'device') +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               data = df_wanted_line_2) +
  geom_point(aes(x = lon, y = lat),
             data = df_wanted_line_1,
             color = 'red')


#exemplo 4
wanted_category = 'ALIMENTADOR'
bus_line_graph_l <- make_empty_graph(directed = FALSE)
df_empty <- data.frame(from = character(0),
                       to = character(0))
for (i in 1:length(all_bus_info)) {
  num_of_directions <- length(all_bus_info[[i]][[4]])
  if (num_of_directions==0)
    next
  a_wanted_category <- all_bus_info[[i]][[3]]
  if (a_wanted_category==wanted_category)
  {
    df_stops_per_line <- df_empty
    for (j in 1:num_of_directions) {
      a_df <- all_bus_info[[i]][[4]][[j]][[2]]
      total_stops <- nrow(a_df)
      df_sub <- data.frame(from = a_df$NUM[1:nrow(a_df) - 1],
                           to = a_df$NUM[-1])
      df_stops_per_line <- rbind(df_stops_per_line, df_sub)
    }
    bus_line_graph_l <- bus_line_graph_l +
      graph_from_data_frame(df_stops_per_line, directed = FALSE)
  }
}
# feeding information of each vertex/bus stop
idx_1 <- vapply(V(bus_line_graph_l)$name,
                function(key) { which(df_all_stops$num==key) },
                0)
V(bus_line_graph_l)$lat <- df_all_stops$lat[idx_1]
V(bus_line_graph_l)$lon <- df_all_stops$lon[idx_1]
V(bus_line_graph_l)$group <- df_all_stops$group[idx_1]
# feeding distance (in meters) between edges
bus_stop_connection <- as_edgelist(bus_line_graph_l)
idx_1 <- vapply(bus_stop_connection[,1],
                function(key) { which(df_all_stops$num==key) },
                0)
idx_2 <- vapply(bus_stop_connection[,2],
                function(key) { which(df_all_stops$num==key) },
                0)
E(bus_line_graph_l)$weight <- distHaversine(
  cbind(df_all_stops$lon[idx_1], df_all_stops$lat[idx_1]),
  cbind(df_all_stops$lon[idx_2], df_all_stops$lat[idx_2]))
# plotting such a bus line
df_wanted_line_1 <- data.frame(lat = V(bus_line_graph_l)$lat,
                               lon = V(bus_line_graph_l)$lon)
df_wanted_line_2 <- data.frame(x = df_all_stops$lon[idx_1],
                               y = df_all_stops$lat[idx_1],
                               xend = df_all_stops$lon[idx_2],
                               yend = df_all_stops$lat[idx_2])
ggmap(my_curitiba_v12, extent = 'device') +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
               data = df_wanted_line_2) +
  geom_point(aes(x = lon, y = lat),
             data = df_wanted_line_1,
             color = 'red')

#4.3.3
df_bus_stops <- data.frame(lat = V(curitiba_l)$lat,lon = V(curitiba_l)$lon)
# remove 'simplify' to see what's happen!
ctba_degree <- degree(simplify(curitiba_l))
df_metrics <- df_bus_stops
df_metrics$value <- ctba_degree
my_break_function <- function(x) {
  seq(min(x), max(x), length.out = 6)
}
ggmap(my_curitiba_v11, extent = 'device') +
  geom_point(data = df_metrics,
             aes(x = lon, y = lat,
                 size = value, color = value)) +
  scale_size_continuous(range = c(0, 3),
                        breaks = my_break_function) +
  scale_color_gradient(breaks = my_break_function) +
  guides(size = guide_legend(title = NULL),
         color = guide_legend(title = NULL))

df_bus_stops <- data.frame(lat = V(curitiba_l)$lat,
                           lon = V(curitiba_l)$lon)
# choose: NA->unweighted NULL->weighted
ctba_betweeness <- betweenness(simplify(curitiba_l),
                               weight = NULL,
                               normalized = TRUE)
df_metrics <- df_bus_stops
df_metrics$value <- ctba_betweeness
ggmap(my_curitiba_v11, extent = 'device') +
  geom_point(data = df_metrics,
             aes(x = lon, y = lat,
                 size = value, color = value)) +
  scale_size_continuous(range = c(0, 3),
                        breaks = my_break_function) +
  scale_color_gradient(breaks = my_break_function) +
  guides(size = guide_legend(title = NULL), color = guide_legend(title = NULL))

require(MASS)
# Copied from MASS::kde2d and modified Ort Christoph
# https://stat.ethz.ch/pipermail/r-help/2006-June/107405.html
kde2d.weighted <- function (x, y, w, h, n = 25,
                            lims = c(range(x), range(y))) {
  nx <- length(x)
  if (length(y) != nx)
    stop("data vectors must be the same length")
  gx <- seq(lims[1], lims[2], length = n) # gridpoints x
  gy <- seq(lims[3], lims[4], length = n) # gridpoints y
  if (missing(h))
    h <- c(bandwidth.nrd(x), bandwidth.nrd(y));
  if (missing(w))
    w <- numeric(nx)+1;
  h <- h/4
  # distance of each point to each grid point in x-direction
  ax <- outer(gx, x, "-")/h[1]
  # distance of each point to each grid point in y-direction
  ay <- outer(gy, y, "-")/h[2]
  # z is the density
  z <- (matrix(rep(w,n), nrow=n, ncol=nx, byrow=TRUE) *
          matrix(dnorm(ax), n, nx)) %*%
    t(matrix(dnorm(ay), n, nx))/(sum(w) * h[1] * h[2])
  return(list(x = gx, y = gy, z = z))
}
df_bus_stops <- data.frame(lat = V(curitiba_l)$lat,
                           lon = V(curitiba_l)$lon)
# to correct a problem with our DATA

# (different bus stops with same lat/lon)
df_bus_stops$lat <- jitter(df_bus_stops$lat)
# choose: NA->unweighted NULL->weighted
ctba_betweeness <- betweenness(simplify(curitiba_l),
                               weight = NULL,
                               normalized = TRUE)
df_metrics <- df_bus_stops
df_metrics$value <- ctba_betweeness
a_density <- kde2d.weighted(df_metrics$lon,
                            df_metrics$lat,
                            df_metrics$value,
                            n=100)
df_density <- data.frame(
  expand.grid(x = a_density$x,
              y = a_density$y),
  z = as.vector(a_density$z))
ggmap(my_curitiba_v11, extent = 'device') +
  geom_contour(data = df_density,
               aes(x = x, y = y, z = z), binwidth = 5) +
  stat_contour(data = df_density,
               aes(x = x, y = y, z = z,
                   fill = ..level..,
                   alpha = ..level..),
               geom = 'polygon') +
  scale_fill_gradient(low = 'green',
                      high = 'red',
                      guide = FALSE) +
  scale_alpha(range=c(0, 0.30), guide = FALSE)

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
# Calcule as distâncias entre todos os pares de nós no grafo
all_distances <- shortest.paths(curitiba_l, v = V(curitiba_l), to = V(curitiba_l))
# Converta as distâncias de metros para quilômetros (1 metro = 0.001 km)
all_distances_km <- all_distances * 0.001
# Calcule a média das distâncias (em quilômetros)
average_distance_km <- mean(all_distances_km, na.rm = TRUE)
# Imprima o resultado
cat("A distância média dos caminhos é:", is.infinite(average_distance_km), "km\n")




#4. Localize graficamente o maior caminho existente (KM) --> distances(all_lines)[1:10, 1:10]
# Calcule as distâncias entre todos os pares de nós no grafo (em quilômetros)
all_distances_km <- shortest.paths(curitiba_l, v = V(curitiba_l), to = V(curitiba_l)) * 0.001
# Encontre a maior distância (em quilômetros) no grafo
max_distance_km <- max(all_distances_km, na.rm = TRUE)
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
