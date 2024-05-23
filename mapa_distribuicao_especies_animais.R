
# Mapa de distribuição de espécies ---------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 21/05/24 ---------------------------------------------------------------------------------------------------------------------------
# Espécies de animais ameaçados de extinção ------------------------------------------------------------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(rgbif)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(cols4all)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

# Buscar dados de ocorrência no GBIF para espécies específicas

species_list <- c("Leontopithecus rosalia", "Cyanopsitta spixii", 
                  "Myrmecophaga tridactyla", "Panthera onca", 
                  "Chrysocyon brachyurus")

# Função para buscar dados de uma espécie

get_occ_data <- function(species_name) {
  occ_search(scientificName = species_name, limit = 500)$data %>%
    filter(!is.na(decimalLongitude) & !is.na(decimalLatitude)) %>%
    select(decimalLongitude, decimalLatitude) %>%
    mutate(species = species_name)
}

# Buscar dados para todas as espécies

all_occ_data <- bind_rows(lapply(species_list, get_occ_data))
View(all_occ_data)

# Converter para objeto sf

coords_sf_all <- st_as_sf(all_occ_data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
View(coords_sf_all)

# Obter dados das fronteiras dos países

world <- ne_countries(scale = "medium", returnclass = "sf")
View(world)

# Filtrar para um país específico (por exemplo, Brasil)

brazil <- world %>% filter(name == "Brazil") 

# Filtrar ocorrências para aquelas dentro do Brasil

coords_sf_brazil <- st_intersection(coords_sf_all, brazil)

# Ajustar os limites do mapa para focar na América do Norte

xlim <- c(-85, -36)
ylim <- c(-37, 6.7)   

# Visualizar mapa --------------------------------------------------------------------------------------------------------------------------

# Definir cores

cols4all::c4a_table(type = "cat", n = 5)
c4a_gui()

# Criar mapa básico com ggplot2

ggplot() +
 geom_sf(data = brazil) +  
geom_sf(data = coords_sf_brazil, aes(color = species), size = 1.7, 
        shape = 15, alpha = 0.9) +  # Ocorrências das espécies no Brasil
  scale_color_manual(
    values = c(
      "Leontopithecus rosalia" = "#A40000",
      "Cyanopsitta spixii" = "#16317D",
      "Myrmecophaga tridactyla" = "#007E2F",
      "Panthera onca" = "#FFCD12",
      "Chrysocyon brachyurus" = "#B86092"
    ),
    labels = c(
      "Leontopithecus rosalia" = "Mico-leão-dourado"~italic("(Leontopithecus rosalia)"),
      "Cyanopsitta spixii" = "Ararinha-azul"~italic("Cyanopsitta spixii"),
      "Myrmecophaga tridactyla" = "Tamanduá-bandeira"~italic("Myrmecophaga tridactyla"),
      "Panthera onca" = "Onça-pintada"~italic("Panthera onca"),
      "Chrysocyon brachyurus" = "Lobo-guará"~italic("Chrysocyon brachyurus")
    )) +
  coord_sf(xlim = xlim, ylim = ylim) +
  labs(title = "Distribuição de Espécies Animais Ameaçados de Extinção no Brasil",
       x = "Longitude",
       y = "Latitude",
       colour = "") +
  theme_light() +
 theme(legend.position = c(0.28, 0.28),
       axis.text = element_text(color = "black",size = 8),
        axis.title = element_text(size = 10, hjust = 1),
        legend.text = element_text(size = 10),
        text = element_text(size = 9),
        legend.text.align = 0) 

# Salvar mapa ------------------------------------------------------------------------------------------------------------------------------

ggsave("m.jpg", dpi = 300,
       width = 35, height = 15, 
       units = "cm", m)

ggsave("m.pdf", dpi = 300,
       width = 35, height = 15, 
       units = "cm", m) 
