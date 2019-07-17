library(gridExtra)
library(grid)
library(tidyverse)
library(cowplot)
library(plotrix)

sedorder=c("Mud", "Sandy mud", "Muddy sand", "Sand", "Gravel")

df = read_csv("./Howard_2019_ESCO_Data.csv") %>% 
  mutate(Cdensity = trad_LOI * density/100*1000) %>% 
  select(sedimentscore_name,trad_LOI, mud_percent, Cdensity, density) %>% 
  mutate(sedimentscore_name <- factor(df$sedimentscore_name,
                                levels = sedorder))

sedsummary <- df %>% 
  group_by(sedimentscore_name) %>% 
  summarise_all(funs(mean, std.error), na.rm = TRUE) %>% 
  ungroup()

for (col in 1:4){
  sedsummary[5,(5+col)]  <- NA
}


mudplot = ggplot(sedsummary, aes(x=as.factor(sedimentscore_name), y=mud_percent_mean)) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=mud_percent_mean-mud_percent_std.error, ymax=mud_percent_mean+mud_percent_std.error), 
                width=.2, position=position_dodge(.9)) +
  scale_y_continuous(expand = c(0, 0), breaks=c(0,20,40,60))+
  coord_cartesian(ylim=c(0,65)) +
  labs(y = "Mud content (%)")+
  annotate("text", x=5, y=60, label= "a)",size = 7)+ #####
annotate("text", x=1, y=3, label= "a",size = 4, color = "white")+ #####
annotate("text", x=2, y=3, label= "a",size = 4, color = "white")+ #####
annotate("text", x=3, y=3, label= "ab",size = 4, color = "white")+ #####
annotate("text", x=4, y=3, label= "b",size = 4, color = "white")+ #####
theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      text = element_text(size=13.5))

model=aov(df$mud_percent~df$sedimentscore_name)
summary(model)
TukeyHSD(model)



densityplot = ggplot(sedsummary, 
                     aes(x=as.factor(sedimentscore_name), y=density_mean)) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=density_mean-density_std.error, ymax=density_mean+density_std.error), width=.2,position=position_dodge(.9)) +
  scale_y_continuous(expand = c(0, 0), breaks=c(0,.5,1,1.5,2))+
  coord_cartesian(ylim=c(0,1.65)) +
  labs(y = expression(paste("Dry bulk density (g cm"^{-3},')')))+
  annotate("text", x=5, y=1.4, label= "b)",size = 7)+ #####
annotate("text", x=1, y=.12, label= "a",size = 4, color = "white")+ #####
annotate("text", x=2, y=.12, label= "a",size = 4, color = "white")+ #####
annotate("text", x=3, y=.12, label= "a",size = 4, color = "white")+ #####
annotate("text", x=4, y=.12, label= "b",size = 4, color = "white")+ #####
theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      text = element_text(size=12))

model=aov(df$density~df$sedimentscore_name)
summary(model)
TukeyHSD(model)


contentplot=ggplot(sedsummary, aes(x=as.factor(sedimentscore_name), y=trad_LOI_mean))+
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=trad_LOI_mean-trad_LOI_std.error, ymax=trad_LOI_mean+trad_LOI_std.error), width=.2,position=position_dodge(.9)) +
  scale_y_continuous(expand = c(0, 0),breaks=c(0,1,2,3,4,5,6))+
  coord_cartesian(ylim=c(0,6.4)) +
  annotate("text", x=5, y=5.5, label= "c)",size = 7)+ #####
annotate("text", x=1, y=.27, label= "a",size = 4, color = "white")+ #####
annotate("text", x=2, y=.27, label= "ab",size = 4, color = "white")+ #####
annotate("text", x=3, y=.27, label= "bc",size = 4, color = "white")+ #####
annotate("text", x=4, y=.27, label= "c",size = 4, color = "white")+ #####
labs(y = expression(paste("C"[org] ~" content (% dry wt.)")))+
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size=13))

model=aov(df$trad_LOI~df$sedimentscore_name)
summary(model)
TukeyHSD(model)


Cdensityplot=ggplot(sedsummary, aes(x=as.factor(sedimentscore_name), y=Cdensity_mean)) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=Cdensity_mean-Cdensity_std.error, ymax=Cdensity_mean+Cdensity_std.error), width=.2,position=position_dodge(.9)) +
  scale_y_continuous(expand = c(0, 0),breaks=c(0,5,10,15,20,25))+
  coord_cartesian(ylim=c(0,27)) +
  annotate("text", x=5, y=23.5, label= "d)",size = 7)+ #####
annotate("text", x=1, y=1.5, label= "a",size = 4, color = "white")+ #####
annotate("text", x=2, y=1.5, label= "b",size = 4, color = "white")+ #####
annotate("text", x=3, y=1.5, label= "c",size = 4, color = "white")+ #####
annotate("text", x=4, y=1.5, label= "d",size = 4, color = "white")+ #####
labs(x = "", 
     y = expression(paste("Soil C"[org] ~" density (mg cm"^-3~ ")"))) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        text = element_text(size=13.5),
        axis.text.x=element_text(angle=30,hjust=1),
        axis.title.x = element_text(margin = margin(t = 20, r = 20, b = 0, l = 0)))

model=aov(df$Cdensity~df$sedimentscore_name)
summary(model)
TukeyHSD(model)



sed_effect <- plot_grid(mudplot,densityplot, contentplot, Cdensityplot, 
                        align = "v", nrow = 4, rel_heights = c(0.25,.25, .25, .37))


ggsave("Fig4_sed_effect_stacked_plot.pdf",sed_effect, width = 4.8, height = 10)
