
library(tidyverse)
library(useful)

# Select useful columns
dcm_data <- read_csv(file = "dcm_data_from_myrthe.csv") %>%
  dplyr::filter(Dx == "ONC") %>%
  select(PIDN,R_dmThal2_R_dmThal2:L_Amy_L_Amy) %>%
  select(-c(R_dmThal2_R_dmThal2, L_dmThal2_L_dmThal2, R_Hypo_R_Hypo, L_Hypo_L_Hypo, R_Amy_R_Amy, L_Amy_L_Amy, ACC_ACC, R_dlPAG_R_dlPAG, R_vAI_R_vAI, L_vAI_L_vAI))


# Example use of cart2pol()
# useful::cart2pol(dcm_data$R_dmThal2_L_Hypo, dcm_data$L_dmThal2_R_Hypo)

parsed_data <- dcm_data %>%
  select(PIDN,R_vAI_L_vAI, L_vAI_R_vAI)

test_data <- cart2pol(parsed_data$R_vAI_L_vAI, parsed_data$L_vAI_R_vAI, degrees=TRUE)

ggplot(test_data, aes(theta,r)) +
  coord_polar() +
  geom_point() +
  scale_x_continuous(limits = c(0, 360), breaks = seq(0, 359.99, by = 45), 
                     labels=c("A","B","C","D","E","F","G","H")) +
  ggtitle("Hello") +
  theme_bw()

# Successful test! Try to set this up for all now


testing_results <- dcm_data %>%
  gather(node_label,value,-PIDN) %>% 
  dplyr::filter(node_label %in% c("R_vAI_L_vAI","L_vAI_R_vAI")) %>%
  spread(key = node_label, value = value)



write_csv(oink, path="node_pairs_step_1.csv")












