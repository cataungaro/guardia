library(readxl)
library(tidyverse)
library (ggplot2)
library(lubridate)

carpa_julio <- read_excel("data/carpa_julio.xls", 
                          col_types = c("text", "text", "skip", 
                                        "text", "text", "skip", "numeric"))
View(carpa_julio)

head(carpa_julio)
names(carpa_julio)

carpa_julio$Fecha <- dmy(carpa_julio$Fecha)
carpa_julio$Hora <- hm(carpa_julio$Hora)

# Construir tabla pacientes por hora
carpa_julio <- carpa_julio %>% mutate(hora_red = hour(Hora))

pacientes_hora <- carpa_julio %>% filter(Nombre != "NA") %>%
  group_by(hour(Hora))%>%
  summarise(pacientes = n())

pacientes_hora %>%
  ggplot(aes(`hour(Hora)`, pacientes))+
  geom_point(alpha=0.5)

pacientes_hora %>%
  ggplot(aes(`hour(Hora)`, pacientes/22))+
  geom_bar(stat ="identity", fill="coral4")+
  xlab("Hora") + ylab("Pacientes (promedio horario)")+
  ggtitle("Pacientes atendidos en Carpa por hora")+
  scale_x_continuous(breaks=seq(7,23,1))+
  scale_y_continuous(breaks=seq(0.00,10, 1))

ggsave("figs/pacientes_hora_bar.png")


pacientes_hora <- pacientes_hora %>%
  mutate(media = pacientes/22)
  
ph <- data.frame(pacientes_hora)

writexl::write_xlsx(ph,"data/pacientes_hora.xlsx")
