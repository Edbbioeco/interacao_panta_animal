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
