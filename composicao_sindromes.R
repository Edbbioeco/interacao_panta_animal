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
