source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

#Carregando a pasta

infosclientes <- read_xlsx("relatorio_old_town_road.xlsx",
                             sheet = "infos_clientes")

#Transformando peso e altura
infosclientes$Weight_lbs <- infosclientes$Weight_lbs * 0.453592
infosclientes$Height_dm <- infosclientes$Height_dm * 10

#Grafico dispersao
dispersao <- ggplot(infosclientes, aes(x = Height_dm, y = Weight_lbs)) +
  geom_point(
    colour = "#A11D21",
    size = 3,
    alpha = 0.3
  ) +
  labs(
    x = "Altura (cm)",
    y = "Peso (Kg)"
  ) +
  theme_estat()
