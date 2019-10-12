library(tidyverse)
zhadina <- read_csv("/home/agricolamz/for_work/HSE/students/2019_Ferapontov_zhadina/zhadina_full_final_data - Manual_data.csv")

olivier <- read_csv("/home/agricolamz/work/materials/2019.08.08-09.19_Olivier_salad/data/questionary.csv")
olivier %>% 
  select(age) %>% 
  mutate(type = "Оливье")->
  olivier

zhadina %>% 
  select(age) %>% 
  mutate(type = "Жадина") %>% 
  rbind(olivier) %>% 
  count(type, age) %>% 
  mutate(n = ifelse(type == "Оливье", -n, n)) %>% 
  ggplot((aes(age, n, fill = type)))+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(y = "количество участников опроса",
       x = "возраст",
       caption = "Г. А. Мороз, 2019. Создано при помощи пакета ggplot2.")+
  theme(legend.title = element_blank(),
        plot.caption = element_text(color = rgb(1,250/255,250/255), size = 12))+
  scale_y_continuous(breaks = c(-500, 0, 500, 1000, 2000, 3000),
                     labels = c(500, 0, 500, 1000, 2000, 3000))+
  scale_x_continuous(breaks = 1:10*10)

zhadina %>% 
  select(age) %>% 
  mutate(type = "Жадина") %>% 
  rbind(olivier) %>% 
  ggplot((aes(age, fill = type)))+
  geom_density(alpha = 0.3)+
  theme_bw()+
  labs(y = "плотность распределения",
       x = "возраст",
       caption = "Г. А. Мороз, 2019. Создано при помощи пакета ggplot2.")+
  theme(legend.title = element_blank(),
        plot.caption = element_text(color = rgb(1,250/255,250/255), size = 12))+
  scale_y_continuous(breaks = 0:3*2/100,
                     labels = paste0(0:3*2, "%"))+
  scale_x_continuous(breaks = 1:10*10)
  
olivier %>% 
  group_by(gender) %>% 
  summarise(median = median(age))

olivier %>% 
  group_by(gender) %>% 
  mutate(median = median(age)) %>% 
  ggplot(aes(age)) +
  geom_histogram(fill = 'lightblue')+
  geom_vline(aes(xintercept = median), linetype = 2)+
  facet_wrap(~gender, scales = "free_y")+
  theme_bw()+
  labs(x = "возраст", y = "количество",
       caption = "Г. А. Мороз, 2019. Создано при помощи пакета ggplot2.")+
  theme(plot.caption = element_text(color = rgb(1,250/255,250/255), size = 12))



olivier %>% 
  group_by(gender, country) %>% 
  summarise(median = median(age),
            n_observations = n()) %>% 
  as.data.frame() %>% 
  arrange(country, gender, -n_observations)

olivier %>% 
  count(country) %>% 
  mutate(ratio = n/sum(n)*100,
         country = reorder(country, ratio)) %>% 
  ggplot(aes(country, ratio, label = paste0(round(ratio, 2), "%")))+
  geom_col(fill = "lightblue")+
  geom_text(nudge_y = 3)+
  coord_flip()+
  scale_y_continuous(breaks = 0:3*25, labels = paste0(0:3*25, "%"))+
  labs(x = "", y = "",
       caption = "Г. А. Мороз, 2019. Создано при помощи пакета ggplot2.")+
  theme_bw()+
  theme(plot.caption = element_text(color = rgb(1,250/255,250/255), size = 12))
  
olivier %>% 
  count(country) %>% 
  mutate(country = reorder(country, n)) %>% 
  ggplot(aes(country, n, label = n))+
  geom_col(fill = "lightblue")+
  geom_text(nudge_y = 100)+
  coord_flip()+
  labs(x = "", y = "",
       caption = "Г. А. Мороз, 2019. Создано при помощи пакета ggplot2.")+
  theme_bw()+
  theme(plot.caption = element_text(color = rgb(1,250/255,250/255), size = 12))


