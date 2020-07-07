library(useful)
library(tidyverse)


dcm_data <- readxl::read_excel("./Data tables/NC_thalamy_2PC_3clus.xls") %>%
  mutate(ClusChar = as.factor(ClusChar))


polar_graph_with_avg <- function(dataframe, node1, node2, graph_title, coloring_var=ClusChar){
  node1 <- enquo(node1)
  node2 <- enquo(node2)
  coloring_var <- enquo(coloring_var)
  
  # Caclulate r/theta for all datapoints in the columns
    nodes_of_interest <- dataframe %>%
      select(!!node1, !!node2) 
    oink <- useful::cart2pol(nodes_of_interest[[1]],nodes_of_interest[[2]], degrees = TRUE)
    polar_coords <- dataframe %>%
      select(!!coloring_var) %>%
      bind_cols(oink) 
  
  # Create cartesian averages, and then calculate r/theta for those averages
    averages <- dataframe %>%
      select(!!node1, !!node2, !!coloring_var) %>%
      group_by(!!coloring_var) %>%
      summarize(node1_avg = mean(!!node1), node2_avg = mean(!!node2))
    oink_avg <- useful::cart2pol(averages[[2]],averages[[3]], degrees = TRUE) %>%
      bind_cols(averages %>% select(!!coloring_var))
    
    print(oink_avg)
    
    ggplot() +
      scale_x_continuous(limits = c(0, 360), breaks = seq(0, 359.99, by = 45), 
                         labels=c("C","B","A","H","G","F","E","D")) +
      coord_polar(start = pi*3/2, direction = -1) +
      geom_point(data = polar_coords, aes(x=theta,y=r,color=!!coloring_var)) +
      geom_point(data = oink_avg, shape=17, size=5, aes(x=theta,y=r,color=!!coloring_var)) +
      ggtitle(graph_title) +
      theme_bw()
}


polar_graph_with_avg(dcm_data, R_dmThal2_R_dlPAG, R_dlPAG_R_dmThal2, "A Title", ClusChar)


















