# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggview)

library(betapart)

library(betapart)

# Dados ----

## Importar ----

comp <- readxl::read_xlsx("Base de dados dispersão.xlsx",
                          sheet = 2)

## Visualizar ----

comp

comp |> dplyr::glimpse()

## Tratar ----

comp <- comp |>
  dplyr::mutate(Tratamento = c(rep("Bloco 1", 5),
                               rep("Bloco 2", 4)))

comp

comp |> dplyr::glimpse()

# Testar composição ----

## Matriz de composição ----

comp_bray <- comp |>
  tibble::column_to_rownames(var = "Amostra") |>
  dplyr::select(dplyr::where(is.numeric)) |>
  vegan::vegdist(method = "bray")

comp_bray

## Permanova ----

vegan::adonis2(comp_bray ~ Tratamento,
               data = comp,
               permutations = 1000)

## NMDS ----

### Calcular ----

nmds <- comp_bray |>
  vegan::metaMDS()

nmds

### Visualizar ----

nmds_df <- nmds |>
  vegan::scores() |>
  dplyr::bind_cols(comp |> dplyr::select(1, 9))

nmds_df

nmds_df |>
  ggplot(aes(NMDS1, NMDS2, fill = Tratamento, label = Amostra)) +
  geom_label(size = 7.5) +
  scale_color_manual(values=c("orange", "royalblue")) +
  scale_fill_manual(values=c("orange", "royalblue")) +
  theme_classic() +
  theme(axis.text = element_text(size = 17.5),
        axis.title = element_text(size = 20),
        panel.grid = element_line(linetype = "dashed",
                                  color = "gray",
                                  linewidth = 1),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 20),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "nmds.png",
       height = 10, width = 12)
