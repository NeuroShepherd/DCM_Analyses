
library(tidyverse)
library(useful)


# Example data to read in (Myrthe, the csv listed is the same you sent to me)
dcm_data <- read_csv(file = "Data tables/dcm_data_from_myrthe.csv") %>%
  select(PIDN,Dx,R_dmThal2_R_dmThal2:L_Amy_L_Amy) %>%
  select(-c(R_dmThal2_R_dmThal2, L_dmThal2_L_dmThal2, R_Hypo_R_Hypo, L_Hypo_L_Hypo, R_Amy_R_Amy, L_Amy_L_Amy, ACC_ACC, R_dlPAG_R_dlPAG, R_vAI_R_vAI, L_vAI_L_vAI))


# Function to convert cart2pol, then graph the results
make_polar_graphs_color <- function(dataframe, node1, node2, graph_title){
  node1 <- enquo(node1)
  node2 <- enquo(node2)
  
  Dx <- dataframe$Dx
  
  nodes_of_interest <- dataframe %>%
    select(!!node1, !!node2) 
  
  oink <- useful::cart2pol(nodes_of_interest[[1]],nodes_of_interest[[2]], degrees = TRUE)
  polar_coords <- bind_cols(data.frame(Dx), oink)
  
  ggplot(polar_coords, aes(theta,r, color = Dx)) +
    coord_polar() +
    geom_point() +
    scale_x_continuous(limits = c(0, 360), breaks = seq(0, 359.99, by = 45), 
                       labels=c("A","B","C","D","E","F","G","H")) +
    ggtitle(graph_title) +
    theme_bw()
  
}

# Example run of the fxn using nodes1 and nodes2 discussed with Kate this afternoon
make_polar_graphs_color(dcm_data, node1 = R_vAI_L_vAI, node2 = L_vAI_R_vAI, graph_title = "Oink")
