library(tidyverse)
library(plotrix)

df=read_csv("../Howard_2019_ESCO_Data.csv") 

df_trimmed <- data.frame("sedimentscore_name" = df$sedimentscore_name, 
                      "buried" = df$`bottom percent loss day`,
                      "surface" = df$`surface percent loss day`)

sedtype_long <- df_trimmed %>% 
  gather(key = "location",value = "rate", buried:surface) %>% 
  drop_na(sedimentscore_name)

summarized =sedtype_long %>%
  group_by(sedimentscore_name, location) %>% 
  summarise(mean = mean(rate,na.rm = TRUE),
            SE = st.err(rate,na.rm = TRUE)) %>% 
  ungroup()

sedorder=c("Mud", "Sandy mud", "Muddy sand", "Sand", "Gravel")
summarized$sedimentscore_name = factor(summarized$sedimentscore_name,sedorder)


#barchart

p <- ggplot(summarized, aes(x=as.factor(sedimentscore_name), y=mean, fill=location)) +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.2,position=position_dodge(.9)) +
  scale_y_continuous(expand = c(0, 0))+
  coord_cartesian(ylim=c(0,0.161)) +
  scale_fill_manual(values=c("tan","lightblue"))+
  labs(y = bquote('Breakdown rate (% day' ^ '-1'~')'))+
  theme(legend.position = c(0.06, 0.9),
        text = element_text(size=14),
        axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 0.7, vjust = 0.8))

ggsave("Fig7_breakdown_specs.pdf",p, width = 5.8, height = 5.8)
 