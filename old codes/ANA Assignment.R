#Title: EU Procurement - Network Analysis
#Author: Lukas Lehmann
#Date: November 9 , 2023

library(igraph)
library(tidyverse)

#load in initial dataset
graph <- read.graph("network.gml", format = "gml")

#This code didn't work bc it was too big
#plot(graph, layout = layout_with_fr(graph), vertex.size = 1, vertex.label = NA)

#limit to Belgium
be_vertices <- V(graph)[V(graph)$country == "BE"]
graph_be <- induced_subgraph(graph, vids = be_vertices)

#PREPARING FIRST GRAPH
membership <- bipartite_mapping(graph_be)$type
if (any(is.na(membership))) {
  graph_be <- delete_vertices(graph_be, which(is.na(membership)))
  membership <- bipartite_mapping(graph_be)$type
  if (any(is.na(membership))) {
    stop("NA values found in vertex types after cleaning, cannot proceed.")
  }
}

membership <- as.logical(membership)
V(graph_be)$type <- membership
vertex_colors <- ifelse(V(graph_be)$type, "blue", "red")
layout <- layout_as_bipartite(graph_be, types = V(graph_be)$type)
plot(graph_be, layout = layout, vertex.color = vertex_colors, vertex.label = NA,
     vertex.size = 5, edge.arrow.size = 0.5, edge.curved = 0.2,
     main = "Bipartite Network of BE Nodes")

#limit to 2012
edges_be_2012 <- E(graph_be)[grepl("2012", E(graph_be)$date_dispatched)]
vertices_ids_be_2012 <- unique(unlist(lapply(edges_be_2012, ends, graph = graph_be)))
graph_be_2012 <- induced_subgraph(graph_be, vids = vertices_ids_be_2012)

#getting ready for second graph
membership1 <- bipartite_mapping(graph_be_2012)$type
membership1 <- as.logical(membership1)

if (any(is.na(membership1))) {
  graph_be_2012 <- delete_vertices(graph_be_2012, which(is.na(membership1)))
  membership1 <- bipartite_mapping(graph_be_2012)$type
  if (any(is.na(membership1))) {
    stop("NA values found in vertex types after cleaning, cannot proceed.")
  }
}

V(graph_be_2012)$type <- membership1
vertex_colors <- ifelse(V(graph_be_2012)$type, "blue", "red")
layout <- layout_as_bipartite(graph_be_2012, types = V(graph_be_2012)$type)

# belgium 2012 plot
plot(graph_be_2012, layout = layout, vertex.color = vertex_colors, vertex.label = NA,
     vertex.size = 5, edge.arrow.size = 0.5, edge.curved = 0.2,
     main = "Bipartite Network of BE Nodes from 2012")

############### Local Clustering Coefficients

local_clustering_coefficients <- transitivity(graph_be_2012, type = "local")
print(local_clustering_coefficients)

local_clust_df <- data.frame(local_clustering_coefficient = local_clustering_coefficients)


ggplot(local_clust_df, aes(x = local_clustering_coefficient)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribution of Local Clustering Coefficients",
       x = "Local Clustering Coefficient",
       y = "Count") +
  scale_x_discrete(labels = c("0" = "0"))

zero_count_df <- data.frame(local_clustering_coefficient = 0, count = nrow(local_clust_df))


ggplot(zero_count_df, aes(x = factor(local_clustering_coefficient), y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Distribution of Local Clustering Coefficients",
       x = "Local Clustering Coefficient",
       y = "Count") +
  theme_minimal() +
  scale_x_discrete(labels = c("0" = "0")) 

############### Degree Distribution

#get the distribution of degrees
degree_dist <- degree_distribution(graph)

ggplot(data = data.frame(degree = degree_dist), aes(x = degree)) +
  geom_histogram(fill = "blue") +
  labs(title = "Degree Distribution - All EU Procurement Data", x = "Degree", y = "Frequency") +
  scale_x_log10() +
  ylim(0, 150)

max(degree_dist)
min(degree_dist)


############### Community Analysis

communities3 <- cluster_infomap(graph_be_2012)
print(communities3)

#FIRST COMMUNITIES GRAPH
plot(communities3, graph_be_2012, vertex.size = 1, vertex.label = NA)

#SECOND COMMUNITIES GRAPH
layout <- layout_with_fr(graph_be_2012, repulserad = vertex_count(graph_be_2012)^2)
community_sizes <- sizes(communities3)
V(graph_be_2012)$size <- community_sizes[communities3$membership] / max(community_sizes) * 10
plot(communities3, graph_be_2012, layout = layout, vertex.size = V(graph_be_2012)$size, vertex.label = NA)

#3, 4, and 5 Communities

# Fruchterman-Reingold algorithm
layout_fr <- layout_with_fr(graph_be_2012, niter = 1000)
plot(communities3, graph_be_2012, layout = layout_fr, vertex.size = 1, vertex.label = NA)

# Kamada-Kawai algorithm
layout_kk <- layout_with_kk(graph_be_2012)
plot(communities3, graph_be_2012, layout = layout_kk, vertex.size = 1, vertex.label = NA)

# Large Graph Layout algorithm
layout_lgl <- layout_with_lgl(graph_be_2012)
plot(communities3, graph_be_2012, layout = layout_lgl, vertex.size = 1, vertex.label = NA)

# Size of communities
community_sizes <- sizes(communities3)
community_sizes_df <- data.frame(community = names(community_sizes), size = community_sizes) 
community_sizes_df <- community_sizes_df %>%
  mutate(comm_size = size.Community.sizes) %>%
  mutate(freq = size.Freq) %>%
  select(-size.Community.sizes, -size.Freq)

community_sizes_df$comm_size <- as.numeric(as.character(community_sizes_df$comm_size))

community_sizes_df$binned <- cut(community_sizes_df$comm_size, breaks=seq(0, max(community_sizes_df$comm_size), by=50), include.lowest=TRUE)

binned_freq <- aggregate(freq ~ binned, community_sizes_df, sum)

ggplot(binned_freq, aes(x=binned, y=freq)) +
  geom_bar(stat="identity", position=position_dodge(), fill="blue") +
  theme_minimal() +
  labs(x="Community Size", 
       y="Frequency",
       title = "Community Size Distribution - Belgium 2012 Procurement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################## Assortativity

assortativity_coefficient <- assortativity_degree(graph_be_2012)
print(assortativity_coefficient)

################## Density

density <- graph.density(graph_be_2012)
density

###END
