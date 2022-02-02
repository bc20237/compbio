library(igraph)
library(dplyr)
test<-read.csv("SS5.csv")
test$microbe1<-sub(".*f__", "", test$feature1) 
test$microbe2<-sub(".*f__", "", test$feature2)
test$microbe1<-ifelse(test$microbe1=="", test$feature1,test$microbe1)
test$microbe2<-ifelse(test$microbe2=="", test$feature2,test$microbe2)
#test[test == ""] <- NA

#to filter by r value, optional
test_r<-subset(test, r > 0.5 | r < (-0.5), select=3:5)
#cleaned edge list
#columns will be -1 and -2 from total number of variables, will change depending on how you filtered
test_edge<-test_r[c(2:3)]
#manually chose the rows to keep by p value- will work once pariwise comparisons
#test_top<-test_edge[r(x:y)]
#separate one with missing data values if you want
#test_NA<-test
#test[test == ""] <- NA
#change to graph
#export bc 
#write.csv(test_edge, "edgetest", row.names = FALSE)
test_graph<-graph.data.frame(test_edge, directed = FALSE)

plot(test_graph)
plot(test_graph, edge.arrow.size=0.4,edge.curved=.1)
#attributes for fun
#getting starting node
#in theory this should make vib and everything attached to it gold but its not working, has something to do with the indexing where i have microbe1
#not throwing a tech error but when i evaluate V(test_graph)$node=="Vibrionaceae" or V(test_graph)$microbe1=="Vibrionaceae" 
#i have a logical expression with 0=true
edge.start <- ends(test_graph, es=E(test_graph), names=F)[,1]
edge.col <- V(test_graph)$color[edge.start]
plot(test_graph, edge.color=edge.col, edge.curved=.1)

plot(test_graph, vertex.color=vcol, edge.color=ecol)
l <- layout_on_grid(test_graph)
inc.edges <- incident(test_graph, V(test_graph)[microbe1=="Vibrionaceae"], mode="all")
# Set colors to plot the selected edges.
ecol <- rep("gray80", ecount(test_graph))
ecol[inc.edges] <- "orange"
vcol <- rep("grey40", vcount(test_graph))
vcol[V(test_graph)$node=="Vibrionaceae"] <- "gold"
plot(test_graph, layout=l, vertex.color=vcol, edge.color=ecol)
#this makes a weird cool window pop up that i havent figured out what to do with her yet
tkid <- tkplot(net) #tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
tk_close(tkid, window.close = T)
plot(net, layout=l)
#colorization based on diameter
diam <- get_diameter(test_graph, directed=T)
as.vector(diam)
vcol <- rep("gray40", vcount(test_graph))
vcol[diam] <- "gold"
ecol <- rep("gray80", ecount(test_graph))
ecol[E(test_graph, path=diam)] <- "orange"
# E(net, path=diam) finds edges along a path, here 'diam'
plot(test_graph, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)

# quant measures of networks
# number of ties
degree(test_graph, mode="in")
centr_degree(test_graph, mode="in", normalized=T)
#Closeness (centrality based on distance to others in the graph)
#Inverse of the node’s average geodesic distance to others in the network.
closeness(test_graph, mode="all", weights=NA)
centr_clo(test_graph, mode="all", normalized=T)
#Betweenness (centrality based on a broker position connecting others)
#Number of geodesics that pass through the node or the edge.
betweenness(test_graph, directed=T, weights=NA)
edge_betweenness(test_graph, directed=T, weights=NA)
centr_betw(test_graph, directed=T, normalized=T)
mean_distance(test_graph, directed=F)
distances(test_graph) # with edge weights
distances(test_graph, weights=NA) # ignore weights
# (Newman-Girvan) community detection
ceb<-cluster_edge_betweenness(test_graph)
dendPlot(ceb, mode="auto")
class(ceb)
#amount of communities
length(ceb)
#who is in each
membership(ceb)
# measurement, high modularity= density within community, less connected xc communities
modularity(ceb)
#add variable telling which community theyre in, makes graphing that easier
#cfg<-cluster_optimal(test_graph)
#plot(cfg, test_graph)
V(test_graph)$community <- ceb$membership
#would need as many colors as communities
colrs <- adjustcolor( c("blue violet","coral","cyan", "tomato", "gold", "yellowgreen", "darkorange","firebrick","darkseagreen","dodgerblue","deeppink","darkslategray4","lavenderblush3","lightblue1","mediumpurple4","mediumspringgreen", "navajowhite","maroon","olivedrab3","saddlebrown","royalblue1", "seagreen1","tan1","snow2","wheat3", "turquoise2","palegoldenrod","red2","seagreen","gray50"), alpha=.6)
test_graph_simple<-simplify(
  test_graph,
  remove.multiple = TRUE,
  remove.loops = TRUE,
  edge.attr.comb = igraph_opt("edge.attr.comb")
)
plot(test_graph_simple, vertex.color=colrs[V(test_graph)$community],edge.label=test_r$r)
#edge.label=test$r