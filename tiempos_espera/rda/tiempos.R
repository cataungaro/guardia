library(readxl)
library(tidyverse)
library(lubridate)

tiempo_espera <- read_excel("tiempos_espera/data/tiempo_espera.xlsx")
View(tiempo_espera)

names(tiempo_espera)
head(tiempo_espera)

julio <- read_excel("tiempos_espera/data/julio.xlsx", 
                    col_types = c("date", "text", "text", 
                                  "date", "date", "date", "numeric", 
                                  "skip"))

julio <- julio %>% mutate(hora_ingreso = hour(Ingreso))
julio <- julio %>% mutate(espera = `Espera (min)`)
julio <- julio %>% mutate(mes = month(`Fecha Ing.`))

julio$`Fecha Ing.` <- ymd(julio$`Fecha Ing.`)

view(julio)

names(julio)
names(tiempo_espera)

totales <- rbind(tiempo_espera, julio)
view(totales)

## Tiempos de espera por hora (media y mediana)

tiempo_espera <- tiempo_espera %>%
  mutate(espera = `Espera (min)`)

tiempo_espera$Ingreso <- hm(tiempo_espera$Ingreso)

tiempo_espera <- tiempo_espera %>%
  mutate(mes = month(`Fecha Ing.`, label=TRUE), hora_ingreso = hour(Ingreso))%>%
  filter(Especialidad == "Clinica")


tiempo_espera$`Fecha Ing.`<- dmy(tiempo_espera$`Fecha Ing.`)
tiempo_espera <- tiempo_espera %>%
  mutate(dia = weekdays(`Fecha Ing.`))

resumen_espera <- tiempo_espera %>%
  group_by(mes, hora_ingreso)%>%
  summarize (mean(espera), sd(espera), 
             median(espera), IQR(espera))


resumen_espera_dia <- tiempo_espera %>%
  group_by(dia, hora_ingreso)%>%
  summarize (mean(espera), sd(espera), 
             median(espera), IQR(espera))

resumen_espera <- data.frame(resumen_espera)
resumen_espera_dia <- data.frame(resumen_espera_dia)

resumen_espera

writexl::write_xlsx(resumen_espera, "data/espera_mes.xls")
writexl::write_xlsx(resumen_espera_dia, "data/espera_dia.xls")

## Figuras

resumen_espera_dia$dia <- 
  factor(resumen_espera_dia$dia , 
         levels=c("lunes", "martes", "miércoles",
                  "jueves", "viernes", "sábado", "domingo"))

resumen_espera %>%
  ggplot(aes(hora_ingreso, mean.espera., fill=mes))+
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=seq(0,23,1))+
  facet_grid(rows=vars(mes))+
  labs(x="Hora de Ingreso", y="Tiempo de Espera (promedio mensual)",
title="Tiempos de Espera por Hora")

ggsave("tiempos_espera/figs/espera_mes_bar.png")  

resumen_espera_dia %>%
  ggplot(aes(dia, mean.espera.,fill=dia))+
  geom_boxplot(alpha=0.5)+
  theme(legend.position="none") +
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(breaks=seq(0,60,10), limits=c(0,60))+
  labs(x="Día de la Semana", y="Tiempo de Espera (mediana)",
       title="Tiempos de Espera por Día")
  
  
ggsave("tiempos_espera/figs/espera_dia_box.png")  
  
## Pacientes por día y por hora

library(readxl)
library(tidyverse)

totales_julio <- read_excel("data/totales_julio.xls")
View(totales_julio)

names(totales_julio)
head(totales_julio)

totales_julio$FECHA <- ymd(totales_julio$FECHA)
totales_julio <- totales_julio %>% mutate(hora_ingreso = hour(ADMISION))


table(totales_julio$DIA)
table(totales_julio$hora_ingreso)
