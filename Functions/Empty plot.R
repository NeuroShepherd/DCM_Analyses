


blank_polar_graph <- function(dataframe, node1, node2, graph_title, coloring_var=NULL){
  node1 <- enquo(node1)
  node2 <- enquo(node2)
  coloring_var <- enquo(coloring_var)
  
  
  nodes_of_interest <- dataframe %>%
    select(!!node1, !!node2) 
  
  oink <- useful::cart2pol(nodes_of_interest[[1]],nodes_of_interest[[2]], degrees = TRUE)
  polar_coords <- dataframe %>%
    select(!!coloring_var) %>%
    bind_cols(oink)
  print(polar_coords, n=88)
  
  ggplot() +
    theme_bw() +
    scale_x_continuous(limits = c(0, 360), breaks = seq(0, 359.99, by = 45), 
                       labels=c("C","B","A","H","G","F","E","D")) +
    scale_y_continuous(position = "left", labels = c(0.4, 0.3, 0.2, 0.1), breaks=c(0.1,0.2,0.3,0.4)) +
    coord_polar(start = pi*3/2, direction = -1) +
    geom_point(data = polar_coords, color="#FFFFFF", aes(x=theta,y=r)) +
    ggtitle(graph_title) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          #axis.text.y = element_text(vjust=12),
          #plot.margin = margin(2, 2, 2, 2, "cm"),
          axis.title.y = element_text(angle=0, vjust=0.5)
    ) +
    annotate(label=c("0.1","0.2","0.3","0.4"),x=202.5, y=c(0.1, 0.2 ,0.3, 0.4),
             geom="text", size=3, color="darkgrey")
  
  # Option--put labels directly on the graph:
  #  annotate(label=c("0.1","0.2","0.3","0.4"),x=202.5, y=c(0.1, 0.2 ,0.3, 0.4),
  #          geom="text", size=3, color="darkgrey")
  
}





empty_plot <- tibble(oink=c(0,0.1), meow=c(0,0.45))


blank_polar_graph(empty_plot,
                  node1 = oink,
                  node2 = meow,
                  graph_title = "test")


