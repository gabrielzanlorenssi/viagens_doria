# Libraries

library(dplyr)
library(ggplot2)
library(readxl)
library(ggthemes)
library(lubridate)

# Gerar a imagem

doria <- read_excel("./doria.xlsx") %>% 
  mutate(day=day(date),
         month=month(date),
         weekday=weekdays(date), 
         week=week(date)) %>% 
  filter(month!=12) %>% 
  mutate(weekday2=case_when(weekday=="Domingo" ~ 1,
                        weekday=="Segunda Feira" ~ 2,
                        weekday=="Terça Feira" ~ 3,
                        weekday=="Quarta Feira" ~ 4,
                        weekday=="Quinta Feira" ~ 5, 
                        weekday=="Sexta Feira" ~ 6, 
                        weekday=="Sábado" ~ 7, 
                        TRUE ~ NA)) %>% 
  ggplot(aes(x=reorder(substr(weekday, 1, 3),
                       weekday2), y=reorder(week, -week))) +
  geom_tile(aes(fill=as.factor(travel)), size=2) +
  geom_text(aes(label=as.character(day))) +
  facet_wrap(~month, scales="free") +
  theme_tufte() +
  scale_fill_manual(values=c("#bdbdbd", "#e41a1c")) +
  labs(x="", y="")

# Salvar como png 

ggsave("doria.png", plot=doria, width=8.4, height = 6.4, dpi=300)





