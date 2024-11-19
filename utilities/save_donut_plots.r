# this skript produces the exploded donut plots
# I need to save the plot as png otherwhise the plot gets distorted when rendered directly to the app
# presaving it saves computational time, ggplot is slow :/

data <- data.frame(
  category=c("Cluster 5", "Cluster 4", "Cluster 3", "Cluster 2", "Cluster 1","Cluster 6"),
  count=c(1, 1, 1, 1, 1, 1))


for(i in c(1:6)){
p <- PieDonut(data, aes(category, count=count), explode = i,showPieName=FALSE, showRatioPie = F,
labelposition = F, addPieLabel = F, pieLabelSize = 12)+
theme_void()+
scale_fill_viridis_d()+
ylim(-1.1,1.1)+
xlim(-1.1,1.1)

ggsave(paste0("supplementary/figures/Cluster",i, ".svg"), width = 10, height = 10, device = "svg")
}

# cluster 0 neads to be done seperately because it is not exploded
p <- PieDonut(data, aes(category, count=count), showPieName=FALSE, showRatioPie = F,
labelposition = F, addPieLabel = F, pieLabelSize = 12)+
theme_void()+
scale_fill_viridis_d()+
ylim(-1.1,1.1)+
xlim(-1.1,1.1)


ggsave(paste0("supplementary/figures/Cluster",0, ".svg"), width = 10, height = 10, device = "svg")
