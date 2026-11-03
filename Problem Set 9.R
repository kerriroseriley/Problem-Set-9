#Title: Problem Set 9
#Author: Kerri Rose Riley
#Requires: data set names
#Output: Graph and Quarto Document typeset as a .pdf


#Load library
library("igraph")

#Load nodelist and edgelist as objects

nodes <- read.csv("https://jacklreilly.github.io/networkscc/supreme_nodes.csv", header=T, as.is=T)
links <- read.csv("https://jacklreilly.github.io/networkscc/supreme_edgelist.csv", header=T, as.is=T)

#Convert to an igraph network. "d" identifies the edgelist; "vertices" identifies the nodelist
net <- graph_from_data_frame(d=links, vertices=nodes, directed=F)
net


# Lock in a layout
nice_layout <- layout_with_fr(net,
                              weights=E(net)$weight)

# Size the nodes by centrality
V(net)$eigen<-eigen_centrality(net)$vector

# Plot the graph
plot.igraph(net,
            edge.width=E(net)$weight*5,
            vertex.label=V(net)$abbrev,
            vertex.label.family="Helvetica",
            vertex.label.font=2,
            vertex.label.color="black",
            vertex.size=V(net)$eigen*50,
            vertex.frame.width=V(net)$width,
            layout=nice_layout)