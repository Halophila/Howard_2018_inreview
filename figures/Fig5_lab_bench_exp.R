library(tidyverse)
library(cowplot)

df <- read_csv("./lab_bench_exp_Data.csv") %>% 
  select(`tensile strength`,`% weight loss`) %>% 
  drop_na()

model=lm(df$`% weight loss` ~ log10(df$`tensile strength`))

b=(summary(model)$coefficients[1,1])
m=(summary(model)$coefficients[2,1])

eqn1 <- expression(paste(italic("y"),' = 29.5 - 12.4 log'[10], italic(" x")))
eqn2 <- expression(paste('r'^2~'= 0.77'))


lab_bench_plot <- df %>% 
  ggplot(aes(x = `tensile strength`, y = `% weight loss`)) +
  geom_point()+
  labs(x = "Tensile Strength (N)",
       y = "Weight Loss (%)") +  
  scale_x_continuous(expand = c(0.01, 0.0), 
                     limits = c(0,300), 
                     breaks = seq(0, 300, by = 50))+
  scale_y_continuous(expand = c(0.015, 0.0), 
                     limits = c(0,30), 
                     breaks = seq(0, 30, by = 10))+
  geom_smooth(method = "lm", 
              formula =  y ~ log10(x), color = 'grey50', 
              se = FALSE, fullrange = TRUE)+
  annotate("text", x = 200, y = 25,  label = eqn1)+
  annotate("text", x = 200, y = 23,  label = eqn2)

ggsave("Fig5_lab_bench_exp.pdf",lab_bench_plot, width = 4.8, height = 4.6)
