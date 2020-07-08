


dcm_data <- read_csv(file = "Data tables/dcm_data_from_myrthe.csv") %>%
  select(PIDN,Dx,R_dmThal2_R_dmThal2:L_Amy_L_Amy) %>%
  select(-c(R_dmThal2_R_dmThal2, L_dmThal2_L_dmThal2, R_Hypo_R_Hypo, L_Hypo_L_Hypo, R_Amy_R_Amy, L_Amy_L_Amy, ACC_ACC, R_dlPAG_R_dlPAG, R_vAI_R_vAI, L_vAI_L_vAI))

#E31919,  #46A619 and #1F7AC4 
# red, green, and blue

make_polar_graphs_custom_color <- function(dataframe, node1, node2, graph_title, coloring_var=NULL){
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
    scale_x_continuous(limits = c(0, 360), breaks = seq(0, 359.99, by = 45), 
                       labels=c("C","B","A","H","G","F","E","D")) +
    scale_y_continuous(position = "left", labels = c(0.4, 0.3, 0.2, 0.1), breaks=c(0.1,0.2,0.3,0.4)) +
    coord_polar(start = pi*3/2, direction = -1) +
    geom_point(data = polar_coords, aes(x=theta,y=r,color=!!coloring_var)) +
    ggtitle(graph_title) +
    scale_color_manual(values = c("#E31919",  "#46A619",  "#1F7AC4"))  +
    theme(#axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(vjust=12),
          #plot.margin = margin(2, 2, 2, 2, "cm"),
          axis.title.y = element_text(angle=0, vjust=0.5)
          )

  # Option--put labels directly on the graph:
  #  annotate(label=c("0.1","0.2","0.3","0.4"),x=202.5, y=c(0.1, 0.2 ,0.3, 0.4),
   #          geom="text", size=3, color="darkgrey")
  
} 
# http://www.sthda.com/english/wiki/ggplot2-texts-add-text-annotations-to-a-graph-in-r-software


# Example run of the fxn using nodes1 and nodes2 discussed with Kate this afternoon
make_polar_graphs_custom_color(dcm_data, 
                        node1 = R_vAI_L_vAI, 
                        node2 = L_vAI_R_vAI, 
                        graph_title = "Oink",
                        coloring_var = Dx)
