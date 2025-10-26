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

#Carregando as pastas
cidade <- read_excel("relatorio_old_town_road.xlsx",
                  sheet = "infos_cidades")
cliente <- read_excel("relatorio_old_town_road.xlsx",
                       sheet = "infos_clientes")
loja <- read_excel("relatorio_old_town_road.xlsx",
                   sheet = "infos_lojas")
relatoriovendas <- read_xlsx("relatorio_old_town_road.xlsx",
                             sheet = "relatorio_vendas")

#Renomeando variaveis
cidade <- cidade %>%
  rename(CityID = C1tyID)
cliente <- cliente %>%
  rename(ClientID = Cli3ntID)
loja <- loja %>%
  rename(StoreID = Stor3ID)

#Juntando sheets
ageambar <- inner_join(cidade, loja, by = "CityID")
ageambar <- inner_join(ageambar, relatoriovendas, by = "StoreID")
ageambar <- inner_join(ageambar, cliente, by = "ClientID")

#filtrando e eliminando repetidos
ageambar <- ageambar %>%
  filter(NameCity == "Âmbar Seco") %>%
  distinct(ClientID, .keep_all = TRUE)

#Fazendo gráfico
bp <- ggplot(ageambar) +
  aes(x = reorder(NameStore, Age, FUN = median), y = Age) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Loja", y = "Idade") +
  theme_estat()+
  coord_flip()

#Criando tabela
ageambar$NameStore <- as.factor(ageambar$NameStore)
ageambar %>%
  group_by(StoreID) %>%
  print_quadro_resumo(var_name = Age)
