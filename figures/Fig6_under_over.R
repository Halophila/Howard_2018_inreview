library(tidyverse)
library(png)
library(grid)
library(cowplot)
library(png)

df <- read_csv("./Howard_2019_ESCO_Data.csv") %>% 
  drop_na(sedimentscore_name) %>% 
  mutate(sedimentscore_name = factor(sedimentscore_name, 
                                     levels = c("Mud", "Sandy mud", 
                                                "Muddy sand", "Sand", "Gravel")))

colors <- colorRampPalette(c("#000000", "#808080"))
color_scale <- colors(5)

img <- readPNG("./figures/Fig6_legend.png")
g <- rasterGrob(img, interpolate=TRUE)
imagexy <- c(0.55,0.37)
imagewidth <- c(0.20, 0.127)


over_over_plot <-   df %>% 
  ggplot(aes(bottom_K, surface_K, 
             size = sedimentscore_cata, 
             color = sedimentscore_name))+
  geom_point(alpha = 0.8)+
  scale_color_manual(values = color_scale,
                     labels = c("Mud", "Sandy\n mud", 
                                "Muddy\n sand", "Sand", "Gravel"))+
  scale_radius(range = c(1,15), 
               labels = c("Mud", "Sandy\n mud", 
                          "Muddy\n sand", "Sand", "Gravel"))+
  annotation_custom(grob = g, 
                    xmin=imagexy[1], xmax=imagexy[1] + imagewidth[1], 
                    ymin =imagexy[2], ymax=imagexy[2] + imagewidth[2])+
  theme(text = element_text(size=14),
        legend.position="none",
        plot.margin=unit(c(0.5,2.2,0.5,0.5),"cm"))+####
  geom_abline(slope = 1, col = "grey70", linetype = 2)+
  scale_x_continuous(expand = c(0.005, 0.0), limits = c(0,0.63), breaks = seq(0, 0.6, by = 0.2))+
  scale_y_continuous(expand = c(0.005, 0.0), limits = c(0,0.63), breaks = seq(0, 0.6, by = 0.2))+
  labs(y = expression(paste("Surface OM breakdown rates, ", italic(k), " (yr"^{-1},')')),
       x = expression(paste("Buried OM breakdown rates, ", italic(k), " (yr"^{-1},')')))+
  annotate("text", 0.125, 0.1, label = "1:1", col = "grey70",angle = 43)

over_over_plot

g <- ggplotGrob(over_over_plot)
g$layout$clip[g$layout$name=="panel"] <- "off"
grid.draw(g)

ggsave("Fig6_under_over.pdf",g, width = 5, height = 4.8)
