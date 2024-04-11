# Grafico intermitencia solar y eólica ---------------------------------------------------

rm(list = ls())

library(tidyverse)
library(ggtext)

fotovoltaica <- read.csv2("rmd/bases/fotovoltaica.csv")

anotacion1 <- glue::glue("Capacidad Efectiva Neta <span style = 'color:#CA4008;'>**278.66**</span> MW <br> Factor de Planta <span style = 'color:#CA4008;'>**0.250**</span>")

anotacion2 <- glue::glue("Capacidad Efectiva Neta <span style = 'color:#6C08CA;'>**18.42**</span> MW <br> Factor de Planta <span style = 'color:#6C08CA;'>**0.208**</span>")


graph10 <- fotovoltaica %>% 
  ggplot(mapping = aes(x = Hora)) +
  geom_col(aes(y = Media_Eolica*20), col = "#6C08CA", fill = "#6C08CA", alpha = 0.6, linewidth = 1) +
  geom_area(aes(y = Media_Solar), col = "#CA4008", fill = "#CA4008", linewidth = 1.2, alpha = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~./20, name = "Eólica - Megavatios (MW)",
                                         breaks = 
                                           seq(from = 0, to = 10, by = 1)), n.breaks = 10) +
  scale_x_continuous(breaks = 1:24) +
  labs(title = "<span style = 'color:#4B5253;'>Generación de electricidad por hora</span>",
       subtitle = "<span style = 'color:#4B5253;'>Plantas</span> <span style = 'color:#CA4008;'>**Solares**</span> <span style = 'color:#4B5253;'>y</span> <span style = 'color:#6C08CA;'>**Eólicas**</span> <span style = 'color:#4B5253;'>en Colombia durante el 2022</span>",
       y = "Solar - Megavatios (MW)",
       x = "Hora del día",
       caption = "Fuente: Elaboración propia en base a XM y UPME<br>**#30DayChartChallenge #Day10** @GEstebanGomez") +
  annotate(geom = "richtext",
           x = c(4, 21),
           y = c(200, 200),
           label = c(anotacion1, anotacion2),
           size = 3) +
  theme_test() +
  theme(plot.title = element_markdown(hjust = 0.5, size = 14),
        plot.subtitle = element_markdown(hjust = 0.5, size = 12),
        plot.caption = element_markdown(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 8, angle = 0),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 10, color = "#CA4008"),
        axis.title.y.right = element_text(size = 10, color = "#5A00C1"),
        axis.title.x = element_text(size = 9),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


ggsave(plot = graph10, 
       filename = "../rmd/resultados/graficos/30DayChartChallenge/10Day_physical.png",
       units = "in",
       dpi = 500,
       width = 10.6,
       height = 5.27
       )
