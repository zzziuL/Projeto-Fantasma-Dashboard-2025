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

relatoriovendas <- read_xlsx("relatorio_old_town_road.xlsx",
                   sheet = "relatorio_vendas")
infosvendas <- read_xlsx("relatorio_old_town_road.xlsx",
                   sheet = "infos_vendas")
produtos <- read_xlsx("relatorio_old_town_road.xlsx",
                      sheet = "infos_produtos")

##ENTREGA1

#Renomeando os items Sale e Item
infosvendas <- infosvendas %>%
  rename(SaleID = Sal3ID)
produtos <- produtos %>%
  rename(ItemID = Ite3ID)

#Juntando os itens
dados <- inner_join(relatoriovendas, infosvendas, by = "SaleID")
dados1 <- inner_join(dados, produtos, by = "ItemID")

#Criando coluna Ano
dados1 <- dados1 %>%
  mutate(Ano = year(Date))

#Filtrando e criando a receita
dados1 <- dados1 %>%
  filter(Ano >= 1880 & Ano <= 1889) %>%
  mutate(Receita = Quantity * UnityPrice)

#Calculando a média
media <- dados1 %>%
  group_by(Ano) %>%
  summarise(media_anual = sum(Receita)/18)

#Transformando em Real
media$media_anual <- media$media_anual * 5.31

#Corrigindo a ordem para o grafico
media <- media %>%
  arrange(Ano)

#Criando graficos
linha <- ggplot(media) +
  aes(x=as.factor(Ano), y=media_anual, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",
                                                  size=2) +
  labs(x="Ano", y="Media Anual") +
  theme_estat()

#Quadro
print_quadro_resumo(media, media_anual)










