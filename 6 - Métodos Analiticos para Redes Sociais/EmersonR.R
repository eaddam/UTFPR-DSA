require(ggmap)
require(ggplot2)
require(igraph)

setwd("D:/Pessoal/Estudo/UTFPR/CDA/redes")

#aids

#grafo direcionado
#vértices são os blogs e as arestas ligam os blogs
#é um gráfico denso, ou seja, com muitas arestas

aids <- read_graph('D:/Pessoal/Estudo/UTFPR/CDA/redes/AIDSBlog.txt', 'edgelist')
#representação
layout_aids <- layout.circle(aids)
layout_aids_rein <- layout.fruchterman.reingold(aids)
layout_aids_grid <- layout.grid(aids)
layout_aids_auto <- layout.auto(aids)
#layout
plot.igraph(aids, layout = layout_aids_auto)
#grau ponderado
grau_pond_in <- degree(aids, mode = "in", loops = FALSE)
summary(grau_pond_in)
grau_pond_out <- degree(aids, mode = "out", loops = FALSE)
summary(grau_pond_out)
grau_aids <- degree(aids)
hist(degree_distribution(aids, cumulative = TRUE ))
#caminho ponderado
menor_caminho <- shortest_paths(aids, from = "122", to = "38")
print(menor_caminho)
escala <- farthest_vertices(aids)
#coeficiente de clusterização
coef_cluster_global <- transitivity(aids, type="global")
print(coef_cluster_global)
#agrupamento / clusterização
cluster_aids_betw <- edge_betweenness(aids, directed = TRUE)
#vértice mais importante
centralidade_aids <- closeness(aids)
pager_aids <- page.rank(aids)
#aresta mais importante
#notar que não existem pesos nas arestas
#nos restam os métodos de centralidade e centralidade ponderada para medir
central_aresta <- edge_betweenness(aids, directed = TRUE)
aresa_central_mais <- which.max(central_aresta)
print(aresa_central_mais)
#diâmetro do grafo
diam_aids <- diameter(aids)
#classificação:aleatória , escala-livre , mundo pequeno
#temos uma mistura de rede aleatória com escala livre




#operações organismo
#tipo de grafo
#não há pesos nas arestas
#é um grafo direcionado "src node dest node type"
e_coli <- read_graph('e_coli.net', 'ncol',directed = TRUE)
e_coli_nodes <- read.table('e_coli_nodes.txt')
# para ler os pesos (weight)data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABwAAAAcCAYAAAByDd+UAAAC10lEQVR42u2W20vTYRjHva/+jQkSEzrQRdB9VwXdVEvYxAJnUSDzwlDotBZqTrYfLdq0aefVDMNp22JbOxVGB6OMrY3pclP7CSUTB7/s7f1OFmv+jtpV9MLD7/x8fu/zfp/neWtq/g86ahsd22p11gMqndVI7b5Kx/ioeVQ6ixP38AzvbBpUp2e2UodNO1rsYb0tkLjqmWIHItmVWy/nV/ujuR+93vRyh+s9q+n1J+qb7WG8u2Ew/nr7CVuobTCaevR6gXO/Zcndia9kID5ProfzhAnliBUWzJG+QI5cHs9wmr5Auq7JFsK3CkPIaPYZhuK3Y9nCk8lFcmdigdgopOfZLDE9zZKLY1lyYWymdDTSa5M3S654v5Su21yfCrtPO+PwIQumarTs39vqjD18lS8+frdI7NE50u1fc3beMyNo+AEYztuHk8Vdp/pj8CUKUzd0bUEYneHMEmAIHWZUdgQLJr6JgsvWeu/DEsILHQjPTsscN9yMphBGR2yuFKpqRxhyoUfMwRR8Ckp/Z4s94qYCwZr1+Gf/mFklUC60cyTDqZtvRHiVq9JaDuptwQTUeO15XnDNKocc6KEubwK+ecRivdTt+ciWZkfVKOSgekhBTzrfsCgO64G0gtCkLiLPIBS5QClouztZhG8eIONDBYEyxVJAaAhBz41Or8I33wxHHbRcoYLwiUUKGEp+Vwpkhsy+9DLKldIZCsHEQ0pF0+GaZFEbjQrWUAwmLhoqXY3Zn0Ah5kt4PqAUTDQtkJz1NElN49McCrHQOiqBiSZ+ubQd7Qumyt1ACCgHJlnayg0XBdfwYKog5EQuDG1KsniXG++eM4Mvzg5/LspxzKtM+i18yG7EKq21AU0Uf6kU9rsB6yzHFG8xEJLD5kC6cyTNSQskzeHdDW0xKpWLjZGabpAgb+QUTeQVWj1+wnCOe3im3uwmah2Y5lLVNtG3dk7v0Wd/BfRPjF/sOXqT33GGYwAAAABJRU5ErkJggg==
# usando match() e sapply()
f <- function(x) e_coli_nodes$V2[match(x, e_coli_nodes$V1)]
res <- sapply(V(e_coli), f)
V(e_coli)$name <- res

#representação
layout_auto_ecoli <- layout_with_fr(e_coli)
unname(e_coli)
#layout
plot.igraph(e_coli, layout = layout_auto_ecoli)
#grau ponderado
grau_pond_in_ecoli <- degree(e_coli, mode = "in", loops = FALSE)
summary(grau_pond_in_ecoli)
grau_pond_out_ecoli <- degree(e_coli, mode = "out", loops = FALSE)
summary(grau_pond_out_ecoli)
#caminho ponderado
menor_caminho_ecoli <- shortest_paths(e_coli, from = "purHD", to = "yhfA")
print(menor_caminho_ecoli)
escala_ecoli <- farthest_vertices(e_coli)
#coeficiente de clusterização
coef_cluster_global_ecoli <- transitivity(e_coli, type="global")
print(coef_cluster_global_ecoli)
#agrupamento / clusterização
cluster_ecoli_betw <- edge_betweenness(e_coli, directed = TRUE)
#vértice mais importante
centralidade_ecoli <- closeness(e_coli)
pager_ecoli <- page.rank(e_coli)
#aresta mais importante
#notar que não existem pesos nas arestas
#nos restam os métodos de centralidade e centralidade ponderada para medir
central_aresta_ecoli <- edge_betweenness(e_coli, directed = TRUE)
aresa_central_mais_ecoli <- which.max(central_aresta_ecoli)
print(aresa_central_mais_ecoli)
#diâmetro do grafo
diam_ecoli <- diameter(e_coli)
#classificação:aleatória , escala-livre , mundo pequeno


#roteadores internet

#router_inet <- read.graph('roteadores/router_INET.txt', 'edgelist', directed = FALSE)
rot <- read_graph('router_INET.txt', 'ncol')
#representação
layout_auto_rot <- layout.auto(rot)
#layout
plot.igraph(rot, layout = layout_auto_rot)
#grau ponderado
grau_pond_in_rot <- degree(rot, mode = "in", loops = FALSE)
summary(grau_pond_in_rot)
grau_pond_out_rot <- degree(rot, mode = "out", loops = FALSE)
summary(grau_pond_out_rot)
#caminho ponderado
menor_caminho_rot <- shortest_paths(rot, from = "1", to = "104")
print(menor_caminho_rot)
escala_rot <- farthest_vertices(rot)
#coeficiente de clusterização
coef_cluster_global_rot <- transitivity(rot, type="global")
print(coef_cluster_global_rot)
#agrupamento / clusterização
cluster_rot_betw <- edge_betweenness(rot, directed = TRUE)
#vértice mais importante
centralidade_rot <- closeness(rot)
pager_rot <- page.rank(rot)
#aresta mais importante
central_aresta_rot <- edge_betweenness(rot, directed = TRUE)
aresa_central_mais_rot <- which.max(central_aresta_rot)
print(aresa_central_mais_rot)
#diâmetro do grafo
diam_rot <- diameter(rot)
#classificação:aleatória , escala-livre , mundo pequeno




#karatecas


zachary <- make_graph("Zachary")
zachary <- read.graph('zachary.txt', 'ncol', directed = FALSE)
#representação
layout_zachary <- layout.circle(zachary)
layout_zachary_auto <- layout.auto(zachary)
#layout
plot.igraph(zachary, layout = layout_zachary_auto)
#grau ponderado
grau_pond_in <- degree(zachary, mode = "in", loops = FALSE)
summary(grau_pond_in)
grau_pond_out <- degree(zachary, mode = "out", loops = FALSE)
summary(grau_pond_out)
grau_zachary <- degree(zachary)
hist(degree_distribution(zachary, cumulative = TRUE ))
#caminho ponderado
menor_caminho <- shortest_paths(zachary, from = "122", to = "38")
print(menor_caminho)
escala <- farthest_vertices(zachary)
#coeficiente de clusterização
coef_cluster_global <- transitivity(zachary, type="global")
print(coef_cluster_global)
#agrupamento / clusterização
cluster_zachary_betw <- edge_betweenness(zachary, directed = TRUE)
#vértice mais importante
centralidade_zachary <- closeness(zachary)
pager_zachary <- page.rank(zachary)
#aresta mais importante
#notar que não existem pesos nas arestas
#nos restam os métodos de centralidade e centralidade ponderada para medir
central_aresta <- edge_betweenness(zachary, directed = TRUE)
aresa_central_mais <- which.max(central_aresta)
print(aresa_central_mais)
#diâmetro do grafo
diam_zachary <- diameter(zachary)
#classificação:aleatória , escala-livre , mundo pequeno




#rede elétrica

power <- read.graph('power.gml', format = "gml")
#representação
layout_power <- layout.circle(power)
layout_power_auto <- layout.auto(power)
#layout
plot.igraph(power, layout = layout_power_auto)
#grau ponderado
grau_pond_in <- degree(power, mode = "in", loops = FALSE)
summary(grau_pond_in)
grau_pond_out <- degree(power, mode = "out", loops = FALSE)
summary(grau_pond_out)
grau_power <- degree(power)
hist(degree_distribution(power, cumulative = TRUE ))
#caminho ponderado
menor_caminho <- shortest_paths(power, from = "122", to = "38")
print(menor_caminho)
escala <- farthest_vertices(power)
#coeficiente de clusterização
coef_cluster_global <- transitivity(power, type="global")
print(coef_cluster_global)
#agrupamento / clusterização
cluster_power_betw <- edge_betweenness(power, directed = TRUE)
#vértice mais importante
centralidade_power <- closeness(power)
pager_power <- page.rank(power)
#aresta mais importante
#notar que não existem pesos nas arestas
#nos restam os métodos de centralidade e centralidade ponderada para medir
central_aresta <- edge_betweenness(power, directed = TRUE)
aresa_central_mais <- which.max(central_aresta)
print(aresa_central_mais)
#diâmetro do grafo
diam_power <- diameter(power)
#classificação:aleatória , escala-livre , mundo pequeno
