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
relatoriovendas <- read_excel("relatorio_old_town_road.xlsx",
                   sheet = "relatorio_vendas")
infosvendas <- read_excel("relatorio_old_town_road.xlsx",
                   sheet = "infos_vendas")
produtos <- read_excel("relatorio_old_town_road.xlsx",
                      sheet = "infos_produtos")
cliente <- read_excel("relatorio_old_town_road.xlsx",
                           sheet = "infos_clientes")
cidade <- read_excel("relatorio_old_town_road.xlsx",
                     sheet = "infos_cidades")
loja <- read_excel("relatorio_old_town_road.xlsx",
                   sheet = "infos_lojas")

#-----------------ANALISE_1----------------------------------------------------#
infosvendas <- infosvendas %>%
  rename(SaleID = Sal3ID)
produtos <- produtos %>%
  rename(ItemID = Ite3ID)

#Juntando os itens
dados1 <- inner_join(relatoriovendas, infosvendas, by = "SaleID")
dados1 <- inner_join(dados1, produtos, by = "ItemID")

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
  labs(x="Ano", y="Receita média") +
  theme_estat()
linha

#-----------------ANALISE_2----------------------------------------------------#

#Transformando peso e altura
cliente$Weight_lbs <- cliente$Weight_lbs * 0.453592
cliente$Height_dm <- cliente$Height_dm * 10

#Grafico dispersao
dispersao <- ggplot(cliente, aes(x = Height_dm, y = Weight_lbs)) +
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
dispersao

#Quadro
cliente %>%
  print_quadro_resumo(var_name = Weight_lbs)
cliente %>%
  print_quadro_resumo(var_name = Height_dm)

#Correlacao de Pearson
cor(cliente$Weight_lbs, cliente$Height_dm, method = "pearson")

#-----------------ANALISE_3----------------------------------------------------#

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
  theme_estat()
bp

#Criando tabela
ageambar$NameStore <- as.factor(ageambar$NameStore)
ageambar %>%
  group_by(StoreID) %>%
  print_quadro_resumo(var_name = Age)

#-----------------ANALISE_4----------------------------------------------------#

#Juntando pastas
final <- inner_join(relatoriovendas, infosvendas, by = "SaleID")
final <- inner_join(final, produtos, by = "ItemID")
final <- inner_join(final, loja, by = "StoreID")

#filtrando
final_89 <- final %>%
  mutate(Ano = substr(Date, 1, 4),
         Receita = UnityPrice * Quantity) %>%
  filter(Ano == 1889)

#descobrindo lojas
top3lojas <- final_89 %>%
  group_by(NameStore) %>%
  summarise(Receita_total = sum(Receita, na.rm = TRUE)) %>%
  arrange(desc(Receita_total)) %>%
  head(3)

filtrolojas <- final_89 %>%
  filter(NameStore %in% top3lojas$NameStore)

#produtos
top3produtos <- filtrolojas %>%
  group_by(NameStore, NameProduct) %>%
  summarise(Quantidade = sum(Quantity, na.rm = TRUE),
            .groups = 'drop') %>%
  group_by(NameStore) %>%
  arrange(desc(Quantidade)) %>%
  slice_head(n = 3) %>%
  ungroup()

#juntando dados
teste <- filtrolojas %>%
  inner_join(top3produtos %>% select(NameStore, NameProduct), 
             by = c("NameStore", "NameProduct")) %>%
  group_by(NameStore, NameProduct) %>%
  summarise(Quantidade_total = sum(Quantity, na.rm = TRUE),
            .groups = 'drop')

#calculando por loja
totaloja <- teste %>%
  group_by(NameStore) %>%
  summarise(Total = sum(Quantidade_total))

# Preparando labels
teste <- teste %>%
  left_join(totaloja, by = "NameStore") %>%
  mutate(
    porcentagem = round(Quantidade_total / Total * 100, 1),
    legendas = paste0(Quantidade_total, " (", porcentagem, "%)")
  )

teste
#Criando grafico
coluna <- ggplot(teste) +
  aes(
    x = NameStore,
    y = Quantidade_total,
    fill = NameProduct,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 2.3
  ) +
  labs(x = "Loja", y = "Quantidade Vendida") +
  theme_estat() +
  labs(fill = "Produto")
coluna

