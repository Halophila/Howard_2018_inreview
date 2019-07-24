##depth affects plot

library(tidyverse)
library(cowplot)
library(gvlma)
library(personalFunctions) ### from my "Halophia" github

df <- read_csv("./Howard_2019_ESCO_Data.csv")

df$Cdensity <- df$trad_LOI*df$density/100*1000



#soil Corg

##no linear model

##plot
CorgContent <- df %>% 
  ggplot(aes(depth,trad_LOI))+
  geom_point()+
  theme(text = element_text(size=14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))+
  scale_x_continuous(expand = c(-0.0, 0.0), limits = c(0,10.5), breaks = c(0,2,4,6,8,10))+
  scale_y_continuous(expand = c(-0.0, 0.0), limits = c(0,8.8), breaks = c(0,2,4,6,8))+
  labs(y = expression(paste("Soil C"[org] ~" content (% dry wt.)")))

###
##soil density

##linear model
line=(lm(df$density~df$depth))
gvlma(line)

eqn1 <- equationPrinter(line)[1]
eqn2 <- equationPrinter(line)[2]


##plot

density <- df %>% 
  ggplot(aes(depth, density))+
  geom_point()+
  geom_smooth(method ="lm", se = FALSE, fullrange = TRUE, color = "grey50")+
  theme(text = element_text(size=14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        plot.margin = unit(c(0, 0.1, 0.1, 0.1), "cm"))+
  scale_x_continuous(expand = c(-0.0, 0.0), limits = c(0,10.5), breaks = c(0,2,4,6,8,10))+
  scale_y_continuous(expand = c(-0.0, 0.0), limits = c(0,1.7), breaks = c(0,0.5,1,1.5))+
  labs(y = expression(paste("Dry bulk density (g cm"^-3~ ")")))+
  annotate("text", label = eqn1, x = 8, y = .3,parse = TRUE)+
  annotate("text", label = eqn2, x = 8, y = .2,parse = TRUE)



###
##Cdensity

##linear model
line <- lm(df$Cdensity~df$depth)
gvlma(line)

eqn1 <- equationPrinter(line)[1]
eqn2 <- equationPrinter(line)[2]


##plot
Cdensity <-  df %>% 
  ggplot(aes(depth,Cdensity))+
  geom_point()+
  geom_smooth(method ="lm", se = FALSE, fullrange = TRUE, color = "grey50")+
  theme(text = element_text(size=14),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))+
  scale_x_continuous(expand = c(-0.0, 0.0), limits = c(0,10.5), breaks = c(0,2,4,6,8,10))+
  scale_y_continuous(expand = c(-0.0, 0.0), limits = c(0,30), breaks = c(0,5,10,15,20,25))+
  labs(y = expression(paste("Soil C"[org] ~" density (mg cm"^-3 ~ ")")),
       x = "Depth (m)")+
  annotate("text", label = eqn1, x = 8, y = 26.3,parse = TRUE)+
  annotate("text", label = eqn2, x = 8, y = 24,parse = TRUE)





big_plot <- plot_grid(CorgContent, density, Cdensity, ncol = 1, 
                      align = 'v', axis = 'l') # aligning vertically along the left axis


ggsave("Fig2_depth_influance.pdf",big_plot, width = 4.8, height = 9.6)