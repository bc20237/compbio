
library(igraph)
library(dplyr)
library(ggplot2)
library(gt)
test<-read.csv("SS5.csv")

test$microbe1<-sub(".*f__", "", test$feature1) 
test$microbe2<-sub(".*f__", "", test$feature2)
test$microbe1<-ifelse(test$microbe1=="", test$feature1,test$microbe1)
test$microbe2<-ifelse(test$microbe2=="", test$feature2,test$microbe2)
test<-subset(test, r >= 0.5 | r <= (-0.5), select=3:5)
#data cleaning specific cases bc of the fucky classifier
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;__;__;__;__", "unknown bacteria a", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;__;__;__;__", "unknown bacteria a", test$microbe2)

#k__Bacteria;p__Spirochaetes;c__Spirochaetes;__;__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Spirochaetes;c__Spirochaetes;__;__","Class Spirochaetes" ,test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Spirochaetes;c__Spirochaetes;__;__","Class Spirochaetes" ,test$microbe2)

#k__Bacteria;p__Spirochaetes;c__Spirochaetes;o__;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Spirochaetes;c__Spirochaetes;o__;f__","Class Spirochaetes" ,test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Spirochaetes;c__Spirochaetes;o__;f__","Class Spirochaetes" ,test$microbe2)

#k__Bacteria;p__Bacteroidetes;c__Bacteroidia;o__Bacteroidales;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Bacteroidetes;c__Bacteroidia;o__Bacteroidales;f__","Order Bacteriodales " ,test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Bacteroidetes;c__Bacteroidia;o__Bacteroidales;f__","Order Bacteriodales " ,test$microbe2)

#k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;o__Rickettsiales;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;o__Rickettsiales;f__","Order Rickettsiales " ,test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;o__Rickettsiales;f__","Order Rickettsiales " ,test$microbe2)

#k__Bacteria;p__;c__;o__;f__ <- unknown bacteria b
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__;c__;o__;f__","unknown bacteria b", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__;c__;o__;f__","unknown bacteria b", test$microbe2)

#	k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;__;__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;__;__","class Gammaproteobacteria ", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;__;__","class Gammaproteobacteria ", test$microbe2)

#k__Bacteria;p__GN02;c__;o__;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__GN02;c__;o__;f__","phylum GN02 ", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__GN02;c__;o__;f__","phylum GN02 ", test$microbe2)

#k__Bacteria;p__GN02;c__BD1-5;o__;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__GN02;c__BD1-5;o__;f__","class BD1-5   ", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__GN02;c__BD1-5;o__;f__","class BD1-5   ", test$microbe2)

#k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;__;__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;__;__","class Alphaproteobacteria   ", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;__;__","class Alphaproteobacteria   ", test$microbe2)

#k__Bacteria;p__Proteobacteria;c__Deltaproteobacteria;o__GMD14H09;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Deltaproteobacteria;o__GMD14H09;f__","order GMD14H09   ", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Deltaproteobacteria;o__GMD14H09;f__","order GMD14H09   ", test$microbe2)

#k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Alteromonadales;__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Alteromonadales;__","order Altermonadales ", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Alteromonadales;__","order Altermonadales ",test$microbe2)

#k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Chromatiales;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Chromatiales;f__","order Chromatiales   ", test$microbe1)
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

#k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;__","Order Clostridiales", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;__","Order Clostridiales", test$microbe2)

#[Acidaminobacteraceae]
test$microbe1<-ifelse(test$microbe1=="[Acidaminobacteraceae]","Acidaminobacteraceae", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="[Acidaminobacteraceae]","Acidaminobacteraceae", test$microbe2)

# k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__","Order Clostridiales", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Firmicutes;c__Clostridia;o__Clostridiales;f__","Order Clostridiales", test$microbe2)

#k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;o__;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;o__;f__","Class Alphaproteobacteria", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;o__;f__","Class Alphaproteobacteria", test$microbe2)

#k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Vibrionales;__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Vibrionales;__","Vibrionaceae", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Vibrionales;__","Vibrionaceae", test$microbe2)

#k__Bacteria;p__Bacteroidetes;__;__;__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Bacteroidetes;__;__;__","phylum Bacteroidetes", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Bacteroidetes;__;__;__","phylum Bacteroidetes", test$microbe2)

#k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__;f__","Class Gammaproteobacteria", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__;f__","Class Gammaproteobacteria", test$microbe2)

#[Weeksellaceae]
test$microbe1<-ifelse(test$microbe1=="[Weeksellaceae]","Weeksellaceae", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="[Weeksellaceae]","Weeksellaceae", test$microbe2)

#k__Bacteria;p__Proteobacteria;c__;o__;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__;o__;f__","phylum Proteobacteria", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__;o__;f__","phylum Proteobacteria", test$microbe2)

#k__Bacteria;p__Cyanobacteria;c__4C0d-2;o__MLE1-12;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Cyanobacteria;c__4C0d-2;o__MLE1-12;f__","Order MLE1", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Cyanobacteria;c__4C0d-2;o__MLE1-12;f__","Order MLE1", test$microbe2)

#k__Bacteria;p__Cyanobacteria;c__Chloroplast;o__Stramenopiles;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Cyanobacteria;c__Chloroplast;o__Stramenopiles;f__","Order Stramenopiles", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Cyanobacteria;c__Chloroplast;o__Stramenopiles;f__","Order Stramenopiles", test$microbe2)

#	k__Bacteria;p__H-178;c__;o__;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__H-178;c__;o__;f__","phylum H-178", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__H-178;c__;o__;f__","phylum H-178", test$microbe2)

#k__Bacteria;p__Lentisphaerae;c__[Lentisphaeria];o__Lentisphaerales;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Lentisphaerae;c__[Lentisphaeria];o__Lentisphaerales;f__","Order Lentisphaerales", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Lentisphaerae;c__[Lentisphaeria];o__Lentisphaerales;f__","Order Lentisphaerales", test$microbe2)

#k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Thiotrichales;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Thiotrichales;f__","Order Thiotrichales", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Thiotrichales;f__","Order Thiotrichales", test$microbe2)

#k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Thiohalorhabdales;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Thiohalorhabdales;f__","Order Thiohalorhabdales", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Thiohalorhabdales;f__","Order Thiohalorhabdales", test$microbe2)

#k__Bacteria;p__Bacteroidetes;c__Flavobacteriia;o__Flavobacteriales;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Bacteroidetes;c__Flavobacteriia;o__Flavobacteriales;f__","Order Flavobacteriales", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Bacteroidetes;c__Flavobacteriia;o__Flavobacteriales;f__","Order Flavobacteriales", test$microbe2)

#k__Bacteria;p__Bacteroidetes;c__[Saprospirae];o__[Saprospirales];f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Bacteroidetes;c__[Saprospirae];o__[Saprospirales];f__","Order Saprospirales", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Bacteroidetes;c__[Saprospirae];o__[Saprospirales];f__","Order Saprospirales", test$microbe2)

#k__Bacteria;p__Proteobacteria;c__Epsilonproteobacteria;o__Campylobacterales;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Epsilonproteobacteria;o__Campylobacterales;f__","Order Campylobacterales", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Epsilonproteobacteria;o__Campylobacterales;f__","Order Campylobacterales", test$microbe2)

#k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__34P16;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__34P16;f__","Order 34P16", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__34P16;f__","Order 34P16", test$microbe2)

#k__Bacteria;p__Bacteroidetes;c__Cytophagia;o__Cytophagales;__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Bacteroidetes;c__Cytophagia;o__Cytophagales;__","Order Cytophagales", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__34P16;f__","Order Cytophagales", test$microbe2)

#k__Bacteria;p__TM6;c__SBRH58;o__;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__TM6;c__SBRH58;o__;f__","Class SBRH58", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__TM6;c__SBRH58;o__;f__","Class SBRH58", test$microbe2)

#k__Bacteria;p__Proteobacteria;c__Epsilonproteobacteria;o__Campylobacterales;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Proteobacteria;c__Epsilonproteobacteria;o__Campylobacterales;f__","Order Campylobacterales", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Proteobacteria;c__Epsilonproteobacteria;o__Campylobacterales;f__","Order Campylobacterales", test$microbe2)

#k__Bacteria;p__Cyanobacteria;c__Chloroplast;o__UA01;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Cyanobacteria;c__Chloroplast;o__UA01;f__","Order UA01", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Cyanobacteria;c__Chloroplast;o__UA01;f__","Order UA01", test$microbe2)

#k__Bacteria;p__Cyanobacteria;c__;o__;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Cyanobacteria;c__;o__;f__","Cyanobacteria", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Cyanobacteria;c__;o__;f__","Cyanobacteria", test$microbe2)

#k__Bacteria;p__Cyanobacteria;c__Chloroplast;o__Streptophyta;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__Cyanobacteria;c__Chloroplast;o__Streptophyta;f__","Streptophyta", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__Cyanobacteria;c__Chloroplast;o__Streptophyta;f__","Streptophyta", test$microbe2)

#k__Bacteria;p__OP8;c__OP8_2;o__;f__
test$microbe1<-ifelse(test$microbe1=="k__Bacteria;p__OP8;c__OP8_2;o__;f__","Class OP8_2", test$microbe1)
test$microbe2<-ifelse(test$microbe2=="k__Bacteria;p__OP8;c__OP8_2;o__;f__","Class OP8_2", test$microbe2)

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
membership(ceb)
# measurement, high modularity= density within community, less connected xc communities
modularity(ceb)
#add variable telling which community theyre in, makes graphing that easier
#cfg<-cluster_optimal(test_graph)
#plot(cfg, test_graph)
commtab<-as.data.frame(ceb$membership,ceb$names)
commtab['names'] <- ceb$names
commtab['community']<-ceb$membership
commtab=commtab[c("names","community")]
gtable<-commtab %>% 
  group_by(community) %>% 
  summarize(names)

#make a variable that contains which microbes are in each community

gtable2<-gt(gtable,rownames_to_stub = FALSE)
gtable2
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
plot(clp,test_graph_simple,vertex.color=colrs[V(test_graph)$community], vertex.label.cex=1.5,edge.label=test$r,edge.label.cex=1,main="C")


