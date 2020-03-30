
library(sf)
library(ggplot2)
library(patchwork)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

Esp = readxl::read_excel("CoronaVirus.xlsx")

esp_sf = readRDS("gadm36_ESP_1_sf.rds")

esp_sf_ne = rnaturalearth::ne_states("spain", returnclass = "sf")

mesp1 = ggplot() +
  geom_sf(data = esp_sf, fill = NA, inherit.aes = F) +
  geom_point(data = Esp,
             aes(
               x = Longitud,
               y = Latitud,
               size = Contagios16,
               color = Tasa16
             )) +
  scale_size_continuous(breaks = c(30, 300, 3000), range = c(1, 15), name = "Total") +
  scale_color_viridis_c(option = "C", name = "Tasas") +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "#d8cfbd"),
    plot.background = element_rect(fill = "#d8cfbd"),
    legend.background = element_rect(fill = "#d8cfbd"),
    legend.key = element_rect(fill = "#d8cfbd"),
    legend.position =  c(0.95, 0.01),
    legend.justification = c(1, 0),
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  coord_sf(xlim = c(-13.1, 3), ylim = c(33, 43.5))

mesp2 = ggplot() +
  geom_sf(data = esp_sf, fill = NA, inherit.aes = F) +
  geom_point(data = Esp,
             aes(
               x = Longitud,
               y = Latitud,
               size = Contagios16,
               color = Tasa16
             ),
             show.legend = F) +
  scale_size_continuous(range = c(1, 15)) +
  scale_color_viridis_c(option = "C") +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "#d8cfbd"),
    plot.background = element_rect(fill = "#d8cfbd"),
    legend.background = element_rect(fill = "#d8cfbd"),
    legend.key = element_rect(fill = "#d8cfbd")
  ) +
  coord_sf(xlim = c(-19.5, -13), ylim = c(26, 30))

layout <- c(
  area(t = 1, l = 1, b = 5, r = 6),
  area(t = 4, l = 1, b = 5, r = 2)
)

mesp1 + mesp2 +
  plot_layout(design = layout) +
  plot_annotation(
  title = "Incidencia de contagios de COV-19 en España al 16 de marzo de 2020",
  subtitle = "Tasa por cada cien mil habitantes",
  caption = "Fuente: El País; INE - Padrón municipal 1 de enero de 2019; GADM.",
  theme = theme(
    panel.background = element_rect(fill = "#d8cfbd"),
    plot.background = element_rect(fill = "#d8cfbd"),
    legend.background = element_rect(fill = "#d8cfbd"),
    legend.key = element_rect(fill = "#d8cfbd")
  )
)

