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

# Índices de diversidade beta ----

## Índices ----

indices <- comp |>
  tibble::column_to_rownames(var = "Amostra") |>
  dplyr::select(dplyr::where(is.numeric)) |>
  betapart::beta.multi(index.family = "jaccard")

indices

## Par-a-par ----

### Calcular ----

df_beta <- purrr::map2(1:3,
                       c("Substituição", "Aninhamento", "Jaccard"),
                       \(id, indice){

  matriz <- comp |>
    tibble::column_to_rownames(var = "Amostra") |>
    dplyr::select(dplyr::where(is.numeric)) |>
    betapart::beta.pair(index.family = "jaccard") %>%
    .[[id]] |>
    as.matrix()

  matriz[upper.tri(matriz)] <- NA

  matriz |>
    reshape2::melt() |>
    tidyr::drop_na() |>
    dplyr::filter(Var2 != Var1) |>
    dplyr::rename("Índice" = value) |>
    dplyr::mutate(Índice = Índice |> round(2),
                  tipo = paste0(indice,
                                " = ",
                                indices[[id]] |> round(2)))

  }) |>
  dplyr::bind_rows() |>
  dplyr::mutate(tipo = tipo |> forcats::fct_relevel(c("Jaccard = 0.89",
                                                      "Substituição = 0.87",
                                                      "Aninhamento = 0.02")))

df_beta

### Gráfico ----

df_beta |>
  ggplot(aes(Var1, Var2, fill = Índice, label = Índice)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(size = 4.5) +
  coord_equal() +
  facet_wrap(~tipo) +
  labs(x = NULL,
       y = NULL) +
  scale_fill_viridis_c(limits = c(0, 1),
                       guide = guide_colourbar(title.position = "top",
                                               title.hjust = 0.5,
                                               barwidth = 25,
                                               frame.colour = "black",
                                               frame.linewidth = 1,
                                               ticks.colour = "black",
                                               ticks.linewidth = 1)) +
  theme_classic() +
  theme(axis.text = element_text(size = 17.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 20),
        panel.grid = element_line(linetype = "dashed",
                                  color = "gray",
                                  linewidth = 1),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 17.5),
        legend.title = element_text(size = 20),
        legend.position = "bottom",
        strip.text = element_text(size = 25)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "indices_diversidade.png",
       height = 10, width = 12)
