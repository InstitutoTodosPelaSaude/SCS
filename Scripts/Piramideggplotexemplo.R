library(ggplot2)
SCSage <- SCS %>% 
  filter.(febre == 1) %>% 
  summarise.(feb = sum(febre), .by = c(cs_sexo, groupage)) %>% 
  mutate.(feb = if_else(cs_sexo == "M", feb*(-1), feb*1)) %>% 
  ggplot(aes(x = groupage,y = feb, fill=cs_sexo)) +
  geom_bar(stat = "identity") +
  coord_flip()+
  labs(title = "Pirâmide etária de atendimentos - S. Caetano do Sul (2022)", x = "Idade")+
  
