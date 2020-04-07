library(magrittr)
library(tidyverse)
library(useful)


# Example data to read in (Myrthe, the csv listed is the same you sent to me)
dcm_data <- read_csv(file = "Data tables/dcm_data_from_myrthe.csv") %>%
  dplyr::filter(Dx == "ONC") %>%
  select(PIDN,Dx,R_dmThal2_R_dmThal2:L_Amy_L_Amy) %>%
  select(-c(R_dmThal2_R_dmThal2, L_dmThal2_L_dmThal2, R_Hypo_R_Hypo, L_Hypo_L_Hypo, R_Amy_R_Amy, L_Amy_L_Amy, ACC_ACC, R_dlPAG_R_dlPAG, R_vAI_R_vAI, L_vAI_L_vAI))

polar_results <- cart2pol(dcm_data[["R_vAI_L_vAI"]], dcm_data[["L_vAI_R_vAI"]], degrees = TRUE)


plotrix::polar.plot(lengths = polar_results$r, 
                    polar.pos = polar_results$theta,
                    rp.type = "s")




ggplot(polar_results, aes(theta,r, color = NULL)) +
  geom_point() +
  ggtitle("Ello") +
  theme_bw() + 
  scale_x_continuous(limits = c(0, 360), breaks = seq(0, 359.99, by = 45)) +
  coord_polar(start = 0)


ggplot() +
  scale_x_continuous(limits = c(0, 360), breaks = seq(0, 359.99, by = 45), 
                     labels=c("C","B","A","H","G","F","E","D")) +
  coord_polar(start = pi*3/2, direction = -1) +
  geom_point(data = polar_results, aes(x=theta,y=r))



