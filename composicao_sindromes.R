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

### Visualizar ----

comp

comp |> dplyr::glimpse()
