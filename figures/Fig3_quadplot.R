##4square_ seagrass and C chararistics


library(tidyverse)
library(cowplot)
library(personalFunctions) ### from Halophia github
library(gvlma)
df=read.csv("../Howard_2019_ESCO_Data.csv")

df$Cdensity=df$trad_LOI*df$density/100*1000

#coverage vs stock

##linear model
line=(lm(df$Cdensity~df$total_cover))
gvlma(line)
eqn1 <- equationPrinter(line)[1]
eqn2 <- equationPrinter(line)[2]

##plot
coverage_vs_stock <- df %>% 
  ggplot(aes(total_cover, Cdensity))+
  geom_point()+
  geom_smooth(method ="lm", se = FALSE, fullrange = TRUE, color = "grey50")+
  theme_few()+  
  theme(text = element_text(size=14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  scale_x_continuous(expand = c(0.005, 0.0), limits = c(0,81), breaks = seq(0, 80, by = 20))+
  scale_y_continuous(expand = c(0.005, 0.0), limits = c(0,31), breaks = seq(0,30,by = 5))+
  labs(y = expression(paste("Soil C"[org] ~" density (mg cm"^-3~ ")")))+
  annotate("text", label = eqn1, x = 65, y = 4,parse = TRUE)+
  annotate("text", label = eqn2, x = 65, y = 2,parse = TRUE)

coverage_vs_stock

###
##height vs stock

##linear model
line=(lm(df$Cdensity~df$canopy_ht))
gvlma(line)
eqn1 <- equationPrinter(line)[1]
eqn2 <- equationPrinter(line)[2]

##plot

canopy_vs_stock <- df %>% 
  ggplot(aes(canopy_ht,Cdensity))+
  geom_point()+
  geom_smooth(method ="lm", se = FALSE, fullrange = TRUE, color = "grey50")+
  theme_few()+  
  theme(text = element_text(size=14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  scale_x_continuous(expand = c(0.005, 0.0), limits = c(0,45), breaks = seq(0, 40, by = 10))+
  scale_y_continuous(expand = c(0.005, 0.0), limits = c(0,31), breaks = seq(0,30,by = 5))+
  annotate("text", label = eqn1, x = 35, y = 4,parse = TRUE)+
  annotate("text", label = eqn2, x = 35, y = 2,parse = TRUE)

canopy_vs_stock



#seagrass vs mud

##linear model
line=(lm(df$mud~df$total_cover))
gvlma(line)
eqn1 <- equationPrinter(line)[1]
eqn2 <- equationPrinter(line)[2]


##plot
coverage_vs_mud <- df %>% 
  ggplot(aes(total_cover, mud_percent))+
  geom_point()+
  geom_smooth(method ="lm", se = FALSE, fullrange = TRUE, color = "grey50")+
  theme_few()+  
  theme(text = element_text(size=14),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  scale_x_continuous(expand = c(0.005, 0.0), limits = c(0,81), breaks = seq(0, 80, by = 20))+
  scale_y_continuous(expand = c(0.005, 0.0), limits = c(0,91), breaks = seq(0,90,by = 10))+
  labs(y = "Mud content (%)", 
       x = "Seagrass coverage (%)")+
  annotate("text", label = eqn1, x = 65, y = 12,parse = TRUE)+
  annotate("text", label = eqn2, x = 65, y = 6,parse = TRUE)

coverage_vs_mud

#height vs mud

##linear model
line=(lm(df$mud~df$canopy_ht))
gvlma(line)
eqn1 <- equationPrinter(line)[1]
eqn2 <- equationPrinter(line)[2]


##plot
canopy_vs_mud <- df %>% 
  ggplot(aes(canopy_ht, mud_percent))+
  geom_point()+
  geom_smooth(method ="lm", se = FALSE, fullrange = TRUE, color = "grey50")+
  theme_few()+  
  theme(text = element_text(size=14),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  scale_x_continuous(expand = c(0.005, 0.0), limits = c(0,45), breaks = seq(0, 40, by = 10))+
  scale_y_continuous(expand = c(0.005, 0.0), limits = c(0,91), breaks = seq(0,90,by = 10))+
  labs(x = "Canopy height (cm)")+
  annotate("text", label = eqn1, x = 35, y = 12, parse = TRUE)+
  annotate("text", label = eqn2, x = 35, y = 6, parse = TRUE)

####
### top graphes
left_plots <- plot_grid(coverage_vs_stock, coverage_vs_mud, ncol = 1,
                        align = 'v', axis = 'l',
                        rel_heights=c(0.47,0.5))
right_plots <- plot_grid(canopy_vs_stock, canopy_vs_mud, ncol = 1,
                        align = 'v', axis = 'l',
                        rel_heights=c(0.47,0.5))



big_plot <- plot_grid(left_plots, right_plots, ncol = 2,
                      rel_widths=c(0.5,0.45))


big_plot
ggsave("Fig3_quadplot.pdf",big_plot, width = 9.8, height = 6.6)
