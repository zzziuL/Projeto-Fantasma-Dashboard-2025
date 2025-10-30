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
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "#003366"
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

#Descobrindo as lojas de maior receita
final <- final %>%
  mutate(Ano = substr(Date, 1, 4),
         Receita = UnityPrice*Quantity)

final <- filter(final, Ano == 1889)

top3lojas <- final %>%
  group_by(NameStore) %>%
  summarise(Receita_total = sum(Receita, na.rm = TRUE)) %>%
  arrange(desc(Receita_total)) %>%
  head(3)

#Criando frequencia
teste <- final %>%
  mutate(NameStore = case_when(
    NameStore %>% str_detect("Loja Ouro Fino") ~ "Loja Ouro Fino",
    NameStore %>% str_detect("Loja TendTudo") ~ "Loja TendTudo",
    NameStore %>% str_detect("Ferraria Apache") ~ "Ferraria Apache"
  )) %>%
  filter(!is.na(NameStore)) %>%
  group_by(NameStore, NameProduct) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100, 1)
    )
porcentagens <- str_c(teste$freq_relativa, "%") %>% str_replace("
\\.", ",")
legendas <- str_squish(str_c(teste$freq, " (", porcentagens, ")")
)

#Filtrando produtos
top3apache <- teste %>%
  filter(NameStore == "Ferraria Apache") %>%
  arrange(desc(freq)) %>%
  head(3)

top3ouro <- teste %>%
  filter(NameStore == "Loja Ouro Fino") %>%
  arrange(desc(freq)) %>%
  head(3)

top3tend <- teste %>%
  filter(NameStore == "Loja TendTudo") %>%
  arrange(desc(freq)) %>%
  head(3)

teste <- rbind(top3apache, top3ouro, top3tend)

porcentagens <- str_c(teste$freq_relativa, "%") %>% str_replace("
\\.", ",")
legendas <- str_squish(str_c(teste$freq, " (", porcentagens, ")")
)

#Criando grafico
coluna <- ggplot(teste) +
  aes(
    x = fct_reorder(NameStore, freq, .desc = T),
    y = freq,
    fill = NameProduct,
    label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Loja", y = "Frequência") +
  theme_estat() +
  labs(fill = "Produto") +
  ylim(c(0,25))
coluna
