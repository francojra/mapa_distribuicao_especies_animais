
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

xlim <- c(-81, -36)
ylim <- c(-37, 6.7)   

# Visualizar mapa --------------------------------------------------------------------------------------------------------------------------

# Definir cores

cols4all::c4a_table(type = "cat", n = 5)
c4a_gui()

# Criar mapa básico com ggplot2

ggplot() +
 geom_sf(data = brazil, fill = "#000000") +  
geom_sf(data = coords_sf_brazil, aes(color = species), size = 2.5, 
        shape = 18, alpha = 0.5) +  # Ocorrências das espécies no Brasil
  scale_color_manual(
    values = c(
      "Leontopithecus rosalia" = "#CC6677",
      "Cyanopsitta spixii" = "#332288",
      "Myrmecophaga tridactyla" = "#DDCC77",
      "Bertholletia excelsa" = "#117733",
      "Myracrodruon urundeuva" = "#88CCEE"
    ),
    labels = c(
      "Leontopithecus rosalia" = expression(italic("Leontopithecus rosalia")),
      "Cyanopsitta spixii" = expression(italic("Cyanopsitta spixii")),
      "Myrmecophaga tridactyla" = expression(italic("Myrmecophaga tridactyla")),
      "Bertholletia excelsa" = expression(italic("Bertholletia excelsa")),
      "Myracrodruon urundeuva" = expression(italic("Myracrodruon urundeuva"))
    )) +
  coord_sf(xlim = xlim, ylim = ylim) +
  labs(title = "Distribuição de Espécies Vegetais Ameaçadas\n de Extinção no Brasil",
       x = "Longitude",
       y = "Latitude",
       colour = "") +
  theme_minimal() +
  theme(legend.position = c(0.28, 0.28),
        axis.text = element_text(color = "black",size = 8),
        axis.title = element_text(size = 10, hjust = 1),
        legend.text = element_text(size = 10),
        text = element_text(size = 10),
        legend.text.align = 0) 

# Salvar mapa ------------------------------------------------------------------------------------------------------------------------------

ggsave("m.jpg", dpi = 300,
       width = 35, height = 15, 
       units = "cm", m)

ggsave("m.pdf", dpi = 300,
       width = 35, height = 15, 
       units = "cm", m) 
