#C histograms


#online plot1 
library(ggthemes)
library(tidyverse)
library(cowplot)
library(plotrix)

df=read_csv("../Howard_2019_ESCO_Data.csv")%>% 
  filter(!is.na(longitude))

df$Cdensity=df$trad_LOI*df$density/100*1000


## Corg content trad.LOI...org
global_mean = c(1.8)
variable_of_interest <- df$trad_LOI
ylimits = c(0,.5)
xlimits = c(0,10)

Corg_plot <- df %>% 
  ggplot(aes(trad_LOI)) + 
 # geom_histogram(aes(y = ..density..),binwidth=density(bci$trad.LOI...org)$bw) 
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 breaks=seq(0, 10, by = 1), 
                 col="grey20", 
                 fill="grey50", 
                 alpha = .2) +
  annotate("text", x=xlimits[2]*0.9, y=ylimits[2]*0.9, label= "a)",size = 7)+ #####
  geom_vline(xintercept = global_mean, col = "grey50", linetype = "longdash")+
  annotate("text", x = global_mean+1.9, y = ylimits[2]*0.90, label = "Global median",
           col = "grey50", size = 4)+
#  geom_vline(xintercept = median, col = "black", linetype = "dashed")+
 # annotate("text", x = median+1.4, y = ylimits[2]*0.85, label = "Median", col = "black", size = 4.5)+
  scale_y_continuous(expand = c(0.000, 0.0), limits = ylimits)+
  scale_x_continuous(expand = c(0.000, 0.0), limits = xlimits)+
  #limits = c(0,0.61), breaks = seq(0, 0.6, by = 0.2))+
  labs(x = expression(paste("Soil C"[org] ~" content (% dry wt.)")), 
       y="Frequency")+
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))+
  annotate("text", x = xlimits[2]*0.8, y = ylimits[2]*0.6, 
           label = paste0("n = ", length(variable_of_interest),
                          "\nMean = ", round(mean(variable_of_interest),1), " ± ", round(std.error(variable_of_interest), 1),
                          "\nMedian = ", round(median(variable_of_interest),1),
                          "\nMin = ", round(min(variable_of_interest),1),
                          "\nMax = ", round(max(variable_of_interest),1)),
           size = 2.9)



## DBD
global_mean = c(0.92)
variable_of_interest <- df$density
ylimits = c(0,0.5)
xlimits = c(0,1.6)

DBD_plot <- df %>% 
  ggplot(aes(density)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                breaks=seq(0, 1.7, by = .15), 
                 col="grey20", 
                 fill="grey50", 
                 alpha = .2) +
  annotate("text", x=xlimits[2]*0.9, y=ylimits[2]*0.9, label= "b)",size = 7)+ #####
  geom_vline(xintercept = global_mean, col = "grey50", linetype = "longdash")+
  annotate("text", x = global_mean-0.29, y = ylimits[2]*0.9, label = "Global median",
           col = "grey50", size = 4)+
  scale_y_continuous(expand = c(0.000, 0.0), limits = ylimits)+
  scale_x_continuous(expand = c(0.000, 0.0), limits = xlimits)+
  labs(x = expression(paste("Dry bulk density (g cm"^-3~ ")")), 
       y="")+
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))+
  annotate("text", x = xlimits[2]*0.8, y = ylimits[2]*0.6, 
           label = paste0("n = ", length(variable_of_interest),
                          "\nMean = ", round(mean(variable_of_interest),1), " ± ", round(std.error(variable_of_interest), 1),
                          "\nMedian = ", round(median(variable_of_interest),1),
                          "\nMin = ", round(min(variable_of_interest),1),
                          "\nMax = ", round(max(variable_of_interest),1)),
           size = 2.9)

## C density
global_mean = c(16.56)
variable_of_interest <- df$Cdensity
ylimits = c(0,0.5)
xlimits = c(0,30)

Cdensity <- df %>% 
  ggplot(aes(Cdensity)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)),
                 breaks=seq(0, 30, by = 2), 
                 col="grey20", 
                 fill="grey50", 
                 alpha = .2) +
  annotate("text", x=xlimits[2]*0.9, y=ylimits[2]*0.9, label= "c)",size = 7)+ #####
  geom_vline(xintercept = global_mean, col = "grey50", linetype = "longdash")+
  annotate("text", x = global_mean-5.8, y = ylimits[2]*0.9, label = "Global median",
           col = "grey50", size = 4)+
  scale_y_continuous(expand = c(0.000, 0.0), limits = ylimits)+
  scale_x_continuous(expand = c(0.000, 0.0), limits = xlimits)+
  labs(x=expression(paste("Soil C"[org] ~" density (mg cm"^-3~ ")")), 
       y="")+
  theme(plot.margin = unit(c(0.2, 0.4, 0.2, 0.2), "cm"))+
  annotate("text", x = xlimits[2]*0.8, y = ylimits[2]*0.6, 
           label = paste0("n = ", length(variable_of_interest),
                          "\nMean = ", round(mean(variable_of_interest),1), " ± ", round(std.error(variable_of_interest), 1),
                          "\nMedian = ", round(median(variable_of_interest),1),
                          "\nMin = ", round(min(variable_of_interest),1),
                          "\nMax = ", round(max(variable_of_interest),1)),
           size = 2.9)


big_plot <- plot_grid(Corg_plot, DBD_plot, Cdensity, ncol = 3)


ggsave("Fig1_histo.pdf",big_plot, width = 10.8, height = 3.8)

       