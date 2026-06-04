
######################################################################## BS Net
set.seed(11+2)
cols = plvs_vltra("mystic_mausoleum", rev=FALSE, elements=NULL, show=FALSE)
X = ifelse(A_Friend_BS + t(A_Friend_BS)>1,1,0)
diag(X)=rep(0,nrow(X))
net = graph_from_adjacency_matrix(X, mode = c("undirected"))
Isolated = which(igraph::degree(net)==0)
net = igraph::delete.vertices(net, Isolated)

g = net

bb = layout_as_backbone(g, keep = 0.55)
igraph::E(g)$col = FALSE
igraph::E(g)$col[bb$backbone] = TRUE

V(g)$size <- igraph::degree(g) + 2

V(g)$clu <- as.character(membership(cluster_spinglass(g,spin=9)))

n1 = ggraph(g, layout = "manual", x = bb$xy[, 1], y = bb$xy[, 2]) +
  geom_edge_link0(aes(edge_colour = col), edge_linewidth = 0.1) +
  geom_node_point(aes(size=size, color=clu), shape = 20) +
  scale_color_manual(values=sample(cols)) +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3), rgb(0, 0, 0, 0.3))) +
  theme_graph() +
  theme(legend.position = "none")

  ggsave("BS_net.pdf", n1, width=10, height=8)

######################################################################## SC Net
set.seed(13+2)
X = ifelse(A_Friend_SC + t(A_Friend_SC)>1,1,0)
diag(X)=rep(0,nrow(X))
net = graph_from_adjacency_matrix(X, mode = c("undirected"))
Isolated = which(igraph::degree(net)==0)
net = igraph::delete.vertices(net, Isolated)

g = net

bb <- layout_as_backbone(g, keep = 0.55)
igraph::E(g)$col <- FALSE
igraph::E(g)$col[bb$backbone] <- TRUE

V(g)$size <- igraph::degree(g)

V(g)$clu <- as.character(membership(cluster_spinglass(g,spin=9)))

n2 = ggraph(g, layout = "manual", x = bb$xy[, 1], y = bb$xy[, 2]) +
  geom_edge_link0(aes(edge_colour = col), edge_linewidth = 0.1) +
  geom_node_point(aes(size=size, color=clu), shape = 20) +
  scale_color_manual(values=sample(cols)) +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3), rgb(0, 0, 0, 0.3))) +
  theme_graph() +
  theme(legend.position = "none")

  ggsave("SC_net.pdf", n2, width=10, height=8)

######################################################################## SU Net
set.seed(46+2)
X = ifelse(A_Friend_SU + t(A_Friend_SU)>1,1,0)
diag(X)=rep(0,nrow(X))
net = graph_from_adjacency_matrix(X, mode = c("undirected"))
Isolated = which(igraph::degree(net)==0)
net = igraph::delete.vertices(net, Isolated)

V(net)$comp = igraph::components(net)$membership

net = igraph::induced_subgraph(net,V(net)$comp==1)

g = net

bb <- layout_as_backbone(g, keep = 0.75)
igraph::E(g)$col <- FALSE
igraph::E(g)$col[bb$backbone] <- TRUE

V(g)$size <- igraph::degree(g)

V(g)$clu <- as.character(membership(cluster_spinglass(g,spin=7)))

n3 = ggraph(g, layout = "manual", x = bb$xy[, 1], y = bb$xy[, 2]) +
  geom_edge_link0(aes(edge_colour = col), edge_linewidth = 0.1) +
  geom_node_point(aes(size=size, color=clu), shape = 20) +
  scale_color_manual(values=cols) +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3), rgb(0, 0, 0, 0.3))) +
  theme_graph() +
  theme(legend.position = "none")

  ggsave("SU_net.pdf", n3, width=10, height=8)

######################################################################## TB Net
set.seed(46+2)
X = ifelse(A_Friend_TB + t(A_Friend_TB)>1,1,0)
diag(X)=rep(0,nrow(X))
net = graph_from_adjacency_matrix(X, mode = c("undirected"))
Isolated = which(igraph::degree(net)==0)
net = igraph::delete.vertices(net, Isolated)

g = net

bb <- layout_as_backbone(g, keep = 0.95)
igraph::E(g)$col <- FALSE
igraph::E(g)$col[bb$backbone] <- TRUE

V(g)$size <- igraph::degree(g)

V(g)$clu <- as.character(membership(cluster_spinglass(g,spin=4)))

n4 = ggraph(g, layout = "manual", x = bb$xy[, 1], y = bb$xy[, 2]) +
  geom_edge_link0(aes(edge_colour = col), edge_linewidth = 0.1) +
  geom_node_point(aes(size=size, color=clu), shape = 20) +
  scale_color_manual(values=cols) +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3), rgb(0, 0, 0, 0.3))) +
  theme_graph() +
  theme(legend.position = "none")

 ggsave("TB_net.pdf", n4, width=10, height=8)


######################################################################## Multiplex



####################################################################### SU Net Prestige + Friends
set.seed(46+2)
X = ifelse(A_Friend_SU + t(A_Friend_SU)>1,1,0) # and
diag(X)=rep(0,nrow(X))
net0 = net = graph_from_adjacency_matrix(X, mode = c("undirected"))

V(net)$comp = igraph::components(net)$membership
net = igraph::induced_subgraph(net,V(net)$comp==1)

g = net

bb = layout_as_backbone(g, keep = 0.75)
igraph::E(g)$col = FALSE
igraph::E(g)$col[bb$backbone] = TRUE

V(g)$size = igraph::degree(g)

V(g)$clu = as.character(membership(cluster_louvain(g)))

ggraph(g, layout = "manual", x = bb$xy[, 1], y = bb$xy[, 2]) +
  geom_edge_link0(aes(edge_colour = col), edge_linewidth = 0.1) +
  geom_node_point(aes(size=size, color=clu), shape = 20) +
  scale_color_manual(values=cols) +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3), rgb(0, 0, 0, 0.3))) +
  theme_graph() +
  theme(legend.position = "none")


################################## SU Net
X2 = ifelse(A_Respect_SU + t(A_Respect_SU)>0,1,0) # or
diag(X2)=rep(0,nrow(X2))
net2 = graph_from_adjacency_matrix(X2, mode = c("undirected"))

V(net2)$comp = igraph::components(net0)$membership

net2 = igraph::induced_subgraph(net2,V(net2)$comp==1)
 net3 = net2+net
g = net3

edgelist_g1 = apply(as_edgelist(net), 1, function(x) paste(sort(x), collapse = "-"))
edgelist_g2 = apply(as_edgelist(net3), 1, function(x) paste(sort(x), collapse = "-"))
# Match edges
edges_in_g2 = edgelist_g2 %in% edgelist_g1

igraph::E(g)$col = edges_in_g2


V(g)$size = igraph::degree(g)

V(g)$clu = as.character(membership(cluster_louvain(g)))

n4b = ggraph(g, layout = "manual", x = bb$xy[, 1], y = bb$xy[, 2]) +
  geom_edge_link0(aes(edge_colour = col, edge_linewidth = col)) +
  geom_node_point(aes(size=size, color=clu), shape = 20) +
  scale_color_manual(values=cols) +
  scale_edge_color_manual(values = c(rgb(0.0, 0, 0, 0.28), rgb(0.019, 0.019, 0.019, 0.8))) +
  scale_edge_width_manual(values=c(0.1, 1.0)) +
  theme_graph() +
  theme(legend.position = "none")

 ggsave("SU_net_prestige.pdf", n4b, width=10, height=8)

###### Just backbone
 n4b2 = ggraph(g, layout = "manual", x = bb$xy[, 1], y = bb$xy[, 2]) +
  geom_edge_link0(aes(edge_colour = col, edge_linewidth = col)) +
  geom_node_point(aes(size=size, color=clu), shape = 20) +
  scale_color_manual(values=cols) +
  scale_edge_color_manual(values = c(rgb(0.0, 0, 0, 0.0), rgb(0.019, 0.019, 0.019, 0.8))) +
  scale_edge_width_manual(values=c(0.1, 1.0)) +
  theme_graph() +
  theme(legend.position = "none")

 ggsave("SU_net_backbone.pdf", n4b2, width=10, height=8)

####################################################################### SU Net Dominant + Friends
set.seed(46+2)
X = ifelse(A_Friend_SU + t(A_Friend_SU)>1,1,0) # and
diag(X)=rep(0,nrow(X))
net0 = net = graph_from_adjacency_matrix(X, mode = c("undirected"))

V(net)$comp = igraph::components(net)$membership
net = igraph::induced_subgraph(net,V(net)$comp==1)

g = net

bb = layout_as_backbone(g, keep = 0.75)
igraph::E(g)$col = FALSE
igraph::E(g)$col[bb$backbone] = TRUE

V(g)$size = igraph::degree(g)

V(g)$clu = as.character(membership(cluster_louvain(g)))

ggraph(g, layout = "manual", x = bb$xy[, 1], y = bb$xy[, 2]) +
  geom_edge_link0(aes(edge_colour = col), edge_linewidth = 0.1) +
  geom_node_point(aes(size=size, color=clu), shape = 20) +
  scale_color_manual(values=cols) +
  scale_edge_color_manual(values = c(rgb(0, 0, 0, 0.3), rgb(0, 0, 0, 0.3))) +
  theme_graph() +
  theme(legend.position = "none")


################################## SU Net
X2 = ifelse(A_Dominant_SU + t(A_Dominant_SU)>0,1,0) # or
diag(X2)=rep(0,nrow(X2))
net2 = graph_from_adjacency_matrix(X2, mode = c("undirected"))

V(net2)$comp = igraph::components(net0)$membership

net2 = igraph::induced_subgraph(net2,V(net2)$comp==1)
 net3 = net2+net
g = net3

edgelist_g1 = apply(as_edgelist(net), 1, function(x) paste(sort(x), collapse = "-"))
edgelist_g2 = apply(as_edgelist(net3), 1, function(x) paste(sort(x), collapse = "-"))
# Match edges
edges_in_g2 = edgelist_g2 %in% edgelist_g1

igraph::E(g)$col = edges_in_g2


V(g)$size = igraph::degree(g)

V(g)$clu = as.character(membership(cluster_louvain(g)))

n4c = ggraph(g, layout = "manual", x = bb$xy[, 1], y = bb$xy[, 2]) +
  geom_edge_link0(aes(edge_colour = col, edge_linewidth = col)) +
  geom_node_point(aes(size=size, color=clu), shape = 20) +
  scale_color_manual(values=cols) +
  scale_edge_color_manual(values = c(rgb(0.0, 0, 0, 0.28), rgb(0.019, 0.019, 0.019, 0.8))) +
  scale_edge_width_manual(values=c(0.1, 1.0)) +
  theme_graph() +
  theme(legend.position = "none")


 ggsave("SU_net_dominant.pdf", n4c, width=10, height=8)

 

############################## Path lengths
# SC
X = ifelse(A_Friend_SC + t(A_Friend_SC) == 2, 1, 0) # and
diag(X)=rep(0,nrow(X))
g = graph_from_adjacency_matrix(X, mode = c("undirected"))
D = igraph::distances(g)

PL = c(D)
RR = c(A_Respect_SC)
DD = c(A_Dominant_SC)
m = 12
PL[is.infinite(PL)] = m + 1 # Add maximally distant ties as their own col, most to the right

# Build dataset with different distributions
data_SC = data.frame(
  type = c(rep("Dominance", length(PL[which(DD==1)])), rep("Prestige", length(PL[which(RR==1)]))),
  value = c(PL[which(DD==1)], PL[which(RR==1)]),
  site = "Lowland"
)

# SU
X = ifelse(A_Friend_SU + t(A_Friend_SU) == 2, 1, 0) # and
diag(X)=rep(0,nrow(X))
g = graph_from_adjacency_matrix(X, mode = c("undirected"))
D = igraph::distances(g)

PL = c(D)
RR = c(A_Respect_SU)
DD = c(A_Dominant_SU)
PL[is.infinite(PL)] = m + 1

# Build dataset with different distributions
data_SU = data.frame(
  type = c(rep("Dominance", length(PL[which(DD==1)])), rep("Prestige", length(PL[which(RR==1)]))),
  value = c(PL[which(DD==1)], PL[which(RR==1)]),
  site = "Altiplano"
)

# BS
X = ifelse(A_Friend_BS + t(A_Friend_BS) == 2, 1, 0) # and
diag(X)=rep(0,nrow(X))
g = graph_from_adjacency_matrix(X, mode = c("undirected"))
D = igraph::distances(g)

PL = c(D)
RR = c(A_Respect_BS)
DD = c(A_Dominant_BS)
PL[is.infinite(PL)] = m + 1

# Build dataset with different distributions
data_BS = data.frame(
  type = c(rep("Dominance", length(PL[which(DD==1)])), rep("Prestige", length(PL[which(RR==1)]))),
  value = c(PL[which(DD==1)], PL[which(RR==1)]),
  site = "Coast"
)

# TB
X = ifelse(A_Friend_TB + t(A_Friend_TB) == 2, 1, 0) # and
diag(X)=rep(0,nrow(X))
g = graph_from_adjacency_matrix(X, mode = c("undirected"))
D = igraph::distances(g)

PL = c(D)
RR = c(A_Respect_TB)
DD = c(A_Dominant_TB)
PL[is.infinite(PL)] = m + 1

# Build dataset with different distributions
data_TB = data.frame(
  type = c(rep("Dominance", length(PL[which(DD==1)])), rep("Prestige", length(PL[which(RR==1)]))),
  value = c(PL[which(DD==1)], PL[which(RR==1)]),
  site = "Highland"
)

# Merge
data = rbind(data_TB, data_BS, data_SU, data_SC)
data = data[which(data$value>0),]

cols = plvs_vltra("mystic_mausoleum", rev=FALSE, elements=NULL, show=FALSE)

# Represent it
p1 = data %>%
  ggplot( aes(x=value, fill=site)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values = c("Lowland"=cols[5], "Coast"=cols[1], "Highland"=cols[3], "Altiplano"=cols[7])) +
    facet_grid(vars(site), vars(type), scales = "free_y" )+
    labs(x="Path length in social network", y="Count") + theme(legend.position = "none")


ggsave("pl_both.pdf", p1, width=8, height=6)


################# Cross tabs
prop.table(table(c(ifelse(A_Respect_BS==0,"N","R")), c(ifelse(A_Trusted_BS==0,"N","T"))), margin = 1)[2,2]
prop.table(table(c(ifelse(A_Respect_SC==0,"N","R")), c(ifelse(A_Trusted_SC==0,"N","T"))), margin = 1)[2,2]
prop.table(table(c(ifelse(A_Respect_TB==0,"N","R")), c(ifelse(A_Trusted_TB==0,"N","T"))), margin = 1)[2,2]
prop.table(table(c(ifelse(A_Respect_SU==0,"N","R")), c(ifelse(A_Trusted_SU==0,"N","T"))), margin = 1)[2,2]


prop.table(table(c(ifelse(A_Respect_BS==0,"N","R")), c(ifelse(A_Like_BS==0,"N","Y"))), margin = 1)[2,2]
prop.table(table(c(ifelse(A_Respect_SC==0,"N","R")), c(ifelse(A_Like_SC==0,"N","Y"))), margin = 1)[2,2]
prop.table(table(c(ifelse(A_Respect_TB==0,"N","R")), c(ifelse(A_Like_TB==0,"N","Y"))), margin = 1)[2,2]
prop.table(table(c(ifelse(A_Respect_SU==0,"N","R")), c(ifelse(A_Like_SU==0,"N","Y"))), margin = 1)[2,2]




