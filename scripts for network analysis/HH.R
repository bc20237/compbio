library(igraph)
library(dplyr)
library(ggplot2)
library(gt)
test<-read.csv("HH5.csv")
#get just differentially abundant ones
#make a new category if they are diff abundant (=1) or not (=0)
test$microbe1<-sub(".*f__", "", test$feature1) 
test$microbe2<-sub(".*f__", "", test$feature2)
test$microbe1<-ifelse(test$microbe1=="", test$feature1,test$microbe1)
test$microbe2<-ifelse(test$microbe2=="", test$feature2,test$microbe2)
test<-subset(test, r >= 0.3 | r <= (-0.3), select=3:5)
#data cleaning specific cases bc of the fucky classifier
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;__;__;__;__", "unknown bacteria a", test$microbe1)

test$microbe2<-ifelse(test$microbe2=="k__Bacteria;__;__;__;__", "unknown bacteria a", test$microbe2)

test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Spirochaetes;c__Spirochaetes;__;__","Class Spirochaetes" ,test$microbe1)

test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Spirochaetes;c__Spirochaetes;__;__","Class Spirochaetes" ,test$microbe2)

test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Spirochaetes;c__Spirochaetes;o__;f__","Class Spirochaetes" ,test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Spirochaetes;c__Spirochaetes;o__;f__","Class Spirochaetes" ,test$microbe2)
#bacteriodales
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Bacteroidetes;c__Bacteroidia;o__Bacteroidales;f__","Order Bacteriodales " ,test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Bacteroidetes;c__Bacteroidia;o__Bacteroidales;f__","Order Bacteriodales " ,test$microbe2)
#rickets
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;o__Rickettsiales;f__","Order Rickettsiales " ,test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;o__Rickettsiales;f__","Order Rickettsiales " ,test$microbe2)
#unknown b
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__;c__;o__;f__","unknown bacteria b", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__;c__;o__;f__","unknown bacteria b", test$microbe2)

#	k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;__;__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;__;__","class Gammaproteobacteria ", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;__;__","class Gammaproteobacteria ", test$microbe2)
#k__Bacteria;p__GN02;c__;o__;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__GN02;c__;o__;f__","Phylum GN02 ", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__GN02;c__;o__;f__","Phylum GN02 ", test$microbe2)
#k__Bacteria;p__GN02;c__BD1-5;o__;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__GN02;c__BD1-5;o__;f__","class BD1-5  ", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__GN02;c__BD1-5;o__;f__","class BD1-5  ", test$microbe2)
#k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;__;__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;__;__","class Alphaproteobacteria  ", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;__;__","class Alphaproteobacteria  ", test$microbe2)
#k__Bacteria;p__Proteobacteria;c__Deltaproteobacteria;o__GMD14H09;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Deltaproteobacteria;o__GMD14H09;f__","order GMD14H09  ", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Deltaproteobacteria;o__GMD14H09;f__","order GMD14H09  ", test$microbe2)
#k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Alteromonadales;__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Alteromonadales;__","order Altermonadales ", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Alteromonadales;__","order Altermonadales ",test$microbe2)
#k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Chromatiales;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Chromatiales;f__","order Chromatiales  ", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Chromatiales;f__","order Chromatiales",test$microbe2)
#k__Bacteria;p__TM6;c__F38;o__;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__TM6;c__F38;o__;f__","class F38", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__TM6;c__F38;o__;f__","class F38",test$microbe2)
#k__Bacteria;p__Fibrobacteres;c__Fibrobacteria;o__Fibrobacterales;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Fibrobacteres;c__Fibrobacteria;o__Fibrobacterales;f__","order Fibrobacterales", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Fibrobacteres;c__Fibrobacteria;o__Fibrobacterales;f__","order Fibrobacterales",test$microbe2)
#k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;o__Kiloniellales;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;o__Kiloniellales;f__","order Kiloniellales", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;o__Kiloniellales;f__","order Kiloniellales",test$microbe2)
#k__Bacteria;p__Proteobacteria;c__Deltaproteobacteria;o__;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Deltaproteobacteria;o__;f__","class Deltaproteobacteria", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Deltaproteobacteria;o__;f__","class Deltaproteobacteria",test$microbe2)
#	k__Bacteria;p__Cyanobacteria;c__Chloroplast;o__CAB-I;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Cyanobacteria;c__Chloroplast;o__CAB-I;f__","order CAB-I", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Deltaproteobacteria;o__;f__","order CAB-I",test$microbe2)
#k__Bacteria;p__Bacteroidetes;c__BME43;o__;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Bacteroidetes;c__BME43;o__;f__","class BME43", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Bacteroidetes;c__BME43;o__;f__","class BME43",test$microbe2)

#cleaned edge list
#columns will be -1 and -2 from total number of variables, will change depending on how you filtered
test_edge<-test[c(2:3)]
test_graph<-graph.data.frame(test_edge, directed = FALSE)

plot(test_graph)
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


commtab<-as.data.frame(ceb$membership,ceb$names)
commtab['names'] <- ceb$names
commtab['community']<-ceb$membership
commtab=commtab[c("names","community")]
gtable<-commtab %>% 
  group_by(community) %>% 
  summarize(names)

#make a variable that contains which microbes are in each community

gtable2<-gt(gtable,rownames_to_stub = FALSE)

#measurement, high modularity= density within community, less connected xc communities
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
plot(test_graph_simple, vertex.color=colrs[V(test_graph)$community],edge.label=test$r)
#edge.label=test$r
clp<-cluster_label_prop(test_graph_simple)
HH<-plot(clp,test_graph_simple,vertex.color=colrs[V(test_graph)$community],edge.label=test$r,main="A")


