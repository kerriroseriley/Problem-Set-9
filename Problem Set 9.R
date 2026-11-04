#Title: Problem Set 9
#Author: Kerri Rose Riley
#Requires: species.csv
#Output: Graph and Quarto Document typeset as a .pdf

#Load library
library(tidyverse)
library(igraph)

# Set the working directory
setwd("~/Desktop/Current Classes/R/Problem Set 9")

# Load ONLY the species csv file
species_df <- read.csv("~/Desktop/Current Classes/R/Problem Set 9/Data/species.csv", header = TRUE, as.is = TRUE)

# Create nodes
# Extract unique park names straight from the species file
nodes <- data.frame(
  name = unique(species_df$Park.Name),
  stringsAsFactors = FALSE
)

# Clean the labels to remove "National Park", "and Preserve", etc. for a cleaner look
nodes$full_label <- nodes$name %>%
  gsub(" National Park and Preserve", "", .) %>%
  gsub(" National Park", "", .) %>%
  gsub(" National Parks", "", .)

# Create links based on shared mammals
mammals_df <- species_df %>% 
  filter(Category == "Mammal", Occurrence == "Present")

# Create a Park x Species incidence matrix
incidence_matrix <- table(mammals_df$Park.Name, mammals_df$Common.Names)
binary_matrix <- as.matrix(incidence_matrix > 0) + 0

# Calculate shared species matrix (Adjacency)
shared_matrix <- binary_matrix %*% t(binary_matrix)
# Remove self-loops
diag(shared_matrix) <- 0

# Convert adjacency matrix into an Edge List (links) data frame
links <- as.data.frame(as.table(shared_matrix)) %>%
  rename(from = Var1, to = Var2, weight = Freq) %>%
  filter(weight > 10) # Filter to keep only meaningful shared connections (e.g., > 10 shared species)

# Filter nodes to match only the connected parks in our filtered links
nodes_filtered <- nodes %>%
  filter(name %in% unique(c(links$from, links$to)))

# Ensure character format to prevent any factor mismatch errors
links$from <- as.character(links$from)
links$to <- as.character(links$to)
nodes_filtered$name <- as.character(nodes_filtered$name)

# Convert this to an igraph network
net <- graph_from_data_frame(d = links, vertices = nodes_filtered, directed = FALSE)

# Lock in a layout weighted by the number of shared species
nice_layout <- layout_with_kk(net) # Kamada-Kawai layout spaces out full names better

# Size the nodes by eigenvector centrality
V(net)$eigen <- eigen_centrality(net)$vector
V(net)$width <- 1  

# Plot with full names of National Parks
par(mar = c(0, 0, 2, 0), bg = "white")


png("network.png", width = 2400, height = 1800, res = 300)

plot.igraph(net,
            vertex.size = V(net)$eigen * 10 + 3,
            vertex.color = "gold",
            vertex.frame.color = "darkorange",
            
            # Use full names as labels, with a smaller font size so they fit
            vertex.label = V(net)$full_label,          
            # Small font size to prevent massive text overlap
            vertex.label.cex = 0.45,
            vertex.label.family = "sans",
            vertex.label.font = 2,
            vertex.label.color = "black",
            
            # Faint edge lines
            edge.width = E(net)$weight * 0.03,
            edge.color = rgb(0.2, 0.2, 0.2, alpha = 0.2),
            
            layout = nice_layout,
            main = "National Parks Shared Species Network")

dev.off()
