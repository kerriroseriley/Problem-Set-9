#Title: Problem Set 9
#Author: Kerri Rose Riley
#Requires: species.csv
#Output: Graph and Quarto Document typeset as a .pdf

#Load library
library(tidyverse)
library(igraph)

# Set the working directory
setwd("~/Desktop/Current Classes/R/Problem Set 9")

# Load only the species csv file
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

# Calculate total documented species per park for node sizing
species_counts <- species_df %>%
  group_by(Park.Name) %>%
  summarize(total_species = n())

# Merge total species count into your nodes data frame
nodes <- nodes %>%
  left_join(species_counts, by = c("name" = "Park.Name"))

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
  # Filter to keep only meaningful shared connections (> 10 shared species)
  filter(weight > 10) 

# Filter nodes to match only the connected parks in our filtered links
nodes_filtered <- nodes %>%
  filter(name %in% unique(c(links$from, links$to)))

# Ensure character format to prevent any factor mismatch errors
links$from <- as.character(links$from)
links$to <- as.character(links$to)
nodes_filtered$name <- as.character(nodes_filtered$name)

# Convert this to an igraph network
net <- graph_from_data_frame(d = links, vertices = nodes_filtered, directed = FALSE)

# Lock in a layout weighted by the number of shared species (using Fruchterman-Reingold with weights)
nice_layout <- layout_with_fr(net, weights = E(net)$weight)

write.csv(nice_layout, "layout.csv", row.names = FALSE)
nice_layout <- as.matrix(read.csv("layout.csv"))
nice_layout

# Size the nodes by total documented species count (scaled for visual clarity)
V(net)$size <- (V(net)$total_species / 250) + 3  
V(net)$width <- 1  


# test
# Rescale weights so they map cleanly between 0.5 and 4.5 pixels wide
min_w <- min(E(net)$weight)
max_w <- max(E(net)$weight)
edge_widths <- 0.5 + 4 * (E(net)$weight - min_w) / (max_w - min_w)
# end test

# Plot with full names of National Parks
par(mar = c(0, 0, 2, 0), bg = "white")

png("network.png", width = 2400, height = 1800, res = 300)

plot.igraph(net,
            vertex.size = V(net)$size,
            vertex.color = "#C8A265",
            
            # Use full names as labels, with a smaller font size so they fit
            vertex.label = V(net)$full_label,          
            # Small font size to prevent massive text overlap
            vertex.label.cex = 0.45,
            vertex.label.family = "Helvetica",
            vertex.label.font = 2,
            vertex.label.color = "black",
            
            # Faint edge lines
            edge.width = edge_widths,
            edge.color = rgb(0.2, 0.2, 0.2, alpha = 0.4),
            
            layout = nice_layout,
            # adds title
            main = "National Parks Shared Species Network")

dev.off()
