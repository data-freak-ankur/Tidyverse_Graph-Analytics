#Import Data

data <- read.csv("small_trains.csv")

#Reviewing the Data
head(data)
str(data)

#Analyze how the stations are connected
# Also, we converetd teh column names as the tidygraph requires 2 variables namely from & to

library(tidyverse)

routes <- data %>%
  group_by(departure_station, arrival_station) %>%
  summarise(journey_time = mean(journey_time_avg)) %>%
  ungroup() %>%
  mutate(from = departure_station, 
         to = arrival_station) %>%
  select(from, to, journey_time)

routes

# The next step is to covert this dataset into graph table in the form of Nodes & Edges
# From Station and to Station will now become Nodes and relationship between them will become edge
# We will use as_tbl_graph function of tidygraph library 

#The as_tbl_graph() function splits the routes table into two:
  
#1)Node Data - Contains all of the unique values found in the from and to variables. 
  #In this case, it is a table with a single column containing the names of all of the stations.

#2)Edge Data - Is a table of all relationships between from and to. A peculiarity of tidygraph is 
   #that it uses the row position of the node as the identifier for from and to, instead of its 
   #original name.

install.packages("tidygraph")
library(tidygraph)

graph_routes <- as_tbl_graph(routes)
graph_routes

# Visualizing the Graph Routes
install.packages("ggraph")
library(ggraph) 

graph_routes %>%
  ggraph(layout = "kk") +
  geom_node_point() +
  geom_edge_diagonal() 

# We got an efficient connection graph
# Getting the Graph with Node Names

graph_routes %>%
  ggraph(layout = "kk") +
  geom_node_text(aes(label = name, color = name), size = 3) +
  geom_edge_diagonal(color = "gray", alpha = 0.4) 

# Additional Information

# If We are required to add more properties to our nodes. Now how does tidygraph knows that we need to add 
# properties to Nodes or edges
# We have activate() function present to let tidygraph knows where we want to add changes

graph_routes <- graph_routes %>%
  activate(nodes)

# Example

graph_routes <- graph_routes %>%
  activate(nodes) %>%
  mutate(
    title = str_to_title(name),
    label = str_replace_all(title, " ", "\n")
  )

graph_routes

# Visualizing the Graph Routes

graph_routes %>%
  ggraph(layout = "kk") +
  geom_node_text(aes(label = name, color = name), size = 3, show.legend = F) +
  geom_edge_diagonal(color = "gray", alpha = 0.4) 


stations <- graph_routes %>%
  activate(nodes) %>%
  pull(title)


from <- which(stations == "Arras")
to <-  which(stations == "Nancy")

shortest <- graph_routes %>%
  morph(to_shortest_path, from, to, weights = journey_time)

# The results can be previewed, or committed back to the original 
#R variable using unmorph()

#While it was morphed, only the few nodes that make up the connections 
#between the Arras and Nancy stations were selected. A simple mutate() 
#adds a new variable called selected_node, which tags those nodes with 
#TRUE. 
#The new variable and value is retained once the rest of the nodes are 
#restored via the unmorph() command.

shortest_path <- shortest %>%
  activate(nodes) %>%
  mutate(selected_node = TRUE) %>%
  activate(edges) %>%
  mutate(selected_edge = TRUE) %>%
  unmorph()

#The next step is to coerce each NA into a 1, and the shortest route into a 2. This will allow us 
#to easily re-arrange the order that the edges are drawn in the plot, ensuring that the route will be 
#drawn at the top.

shortest_path <- shortest_path %>%
  activate(nodes) %>%
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
  activate(edges) %>%
  mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
  arrange(selected_edge)

# Visualization

shortest_path %>%
  ggraph(layout = "kk") +
  geom_node_text(aes(label = name, color = name, alpha = selected_node), size = 3, show.legend = F) +
  geom_edge_diagonal(aes(alpha = selected_edge),color = "gray",show.legend = F) 


# Reusing the code and putting it in a Function

shortest_path_func <- function(from_station,to_station){
  stations <- graph_routes %>%
    activate(nodes) %>%
    pull(title)
  
  
  from <- which(stations == from_station)
  to <-  which(stations == to_station)
  
  shortest <- graph_routes %>%
    morph(to_shortest_path, from, to, weights = journey_time)
  
  shortest_path <- shortest %>%
    activate(nodes) %>%
    mutate(selected_node = TRUE) %>%
    activate(edges) %>%
    mutate(selected_edge = TRUE) %>%
    unmorph()
  
  shortest_path <- shortest_path %>%
    activate(nodes) %>%
    mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
    activate(edges) %>%
    mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
    arrange(selected_edge)
  
  path_graph <- shortest_path %>%
    ggraph(layout = "kk") +
    geom_node_text(aes(label = name, color = name, alpha = selected_node), size = 3, show.legend = F) +
    geom_edge_diagonal(aes(alpha = selected_edge),color = "gray",show.legend = F) 
  
  return(path_graph)
  
}


shortest_path_func("Arras","Nancy")
