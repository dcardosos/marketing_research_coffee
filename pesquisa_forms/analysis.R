library(GGally)
library(ggplot2)
library(hrbrthemes)
library(ggplot2)
library(ggalluvial)
library(magrittr)

replace_domestico <- function(tb) {
  tb %>%
    dplyr::mutate(Ocupação = dplyr::if_else(
      condition = stringr::str_detect(Ocupação, 'doméstico'), 
      true = 'Doméstico', 
      false = Ocupação))
}

df_sim <- readr::read_csv("dados/respostas_sim_cafe.csv") %>% 
  dplyr::rename("Sexo"= Gênero) %>%
  replace_domestico()

df_nao <- readr::read_csv("dados/respostas_nao_cafe.csv") %>% 
  dplyr::rename("Sexo"= Gênero) %>%
  replace_domestico()

df_geral <- readr::read_csv("dados/perfil_respostas.csv") %>% 
  dplyr::rename("Sexo"= Gênero) %>%
  replace_domestico()

# df_geral[df_geral == "grau_de_escolaridade"] <- "escolaridade"
# df_nao[df_nao == "grau_de_escolaridade"] <- "escolaridade"
# df_sim[df_sim == "grau_de_escolaridade"] <- "escolaridade"
dim(df_geral) # 111 respostas

names(df_geral)

df_geral <- 
  df_geral %>%
  dplyr::mutate(Idade = factor(Idade, levels = c(
    "Menor de 18 anos",
    "Entre 18 e 24" ,
    "Entre 25 e 30" ,   
    "Entre 31 e 40" ,
    "Entre 41 e 50")))
  
# Características das pessoas que responderam o forms -------------------------

# Consumo por gênero
df_geral %>%  
  ggplot(aes(Sexo,fill=consumo_café)) + 
    geom_bar(position = "dodge") + 
    geom_text( 
      stat='count', 
      aes(label=..count..),
      color='white', 
      size = 5,
      vjust = 1.5, 
      position = position_dodge(.9)) +
    theme_classic() +
  labs(
    fill = "Consome ou não café",
    y = 'Total',
    title = 'Consumo - Homens x mulheres')

# Consumo por idade
df_geral %>%  
  ggplot(aes(Idade,fill=consumo_café)) +
    geom_bar(position = 'dodge') + 
    geom_text(
      stat='count', 
      aes(label=..count..),
      color='black',
      size = 4,
      vjust = 1.5,
      position = position_dodge(.9)) +
    theme_classic() +
  labs(
    fill = "Consumo de café",
    x = 'Idade',
    y = 'Total',
    title = 'Consumo - Idade')

# Ocupação
df_geral %>%  
  ggplot(aes(Ocupação,fill=consumo_café)) +
  geom_bar() +  
  theme_classic() + 
  #theme(axis.text.x = element_text(angle=45)) +
  labs(
    fill = "Consumo de café",
    x = 'Ocupação',
    y = 'Total',
    title = 'Consumo - Ocupação') +
  coord_flip()

# faixa etária
df_geral %>%  
  ggplot(aes(x=Idade,fill=Sexo)) + 
  geom_bar(data = subset(df_geral,Sexo == "Feminino")) + 
  geom_bar(data = subset(df_geral,Sexo == "Masculino"), aes(y = ..count..*(-1))) + 
  scale_y_continuous(breaks = seq(-40,40,10), labels = abs(seq(-40, 40, 10))) + 
  labs(x = NULL, y = 'Total') +
  coord_flip() + 
  hrbrthemes::theme_ipsum()

  #labs(fill= "Sexo",x='Total',y='Idade',title = 'Faixa etária') + 
  #theme_classic()


# escolaridade
df_geral %>%  
  ggplot(aes(escolaridade, fill = consumo_café)) +
  geom_bar() +
  theme_classic() +
  labs(
    fill = 'Consumo de café', 
    x = 'Escolaridade', 
    y = 'Total', 
    title = 'Consumo - Idade') +
  coord_flip()

# Características das pessoas que não consomem café ---------------------------

names(df_nao)

# Razões pelas quais não consomem café

contagem_razoes <- df_nao %>% 
  dplyr::select(starts_with("razões_")) %>% 
  dplyr::summarize_all(sum) %>% 
  tidyr::pivot_longer(
    everything(), 
    names_to = 'razoes',
    values_to = "Total") |> 
  dplyr::mutate(
    razoes = dplyr::case_when(
      razoes == 'razões_hábito' ~ 'Hábito',
      razoes == 'razões_sabor_residual' ~ 'Sabor residual',
      razoes == 'razões_sabor_aroma' ~ 'Sabor aroma',
      razoes == 'razões_azia' ~ 'Azia',
      razoes == 'razões_dor_cabeça' ~ 'Dor de cabeça',
      razoes == 'razões_saude' ~ 'Saúde',
      razoes == 'razões_restr_med' ~ 'Restrição médica',
      razoes == 'razões_outros' ~ 'Outros',
      TRUE ~ razoes),
    prop = round(Total / sum(Total) * 100, 2)) 


#contagem_razoes$prop <- round((contagem_razoes$Total / dim(df_nao)[1]) * 100,2)

contagem_razoes %>% 
  ggplot(aes(x = razoes, y = prop)) + 
  geom_col(fill = 'dark orange') +
  geom_text(
    aes(x = razoes, y = prop, label = prop),
    position = position_dodge(width = 1),
    color = 'white',
    vjust = 1.5, 
    size = 5) +
  theme_classic() +
  labs(
    x = 'Razões para não tomar café', 
    y = 'Porporção (%)',
    title = 'Razões para não tomar café')
  
contagem_razoes %>% 
  ggplot(aes(x = razoes, y = Total)) + 
  geom_col(fill = 'brown') +
  geom_text(
    aes(x = razoes, y = Total, label = Total),
    position = position_dodge(width = 1),
    vjust = 1.5, 
    size = 4,
    color = 'white') +
  theme_classic() +
  labs(
    x = 'Razões para não tomar café',
    y = 'Total',
    title = 'Razões para não tomar café')
  
# não consigo pivotar essa tabela 
df_nao %>% 
  dplyr::select(Sexo, dplyr::starts_with('razões'), -razões) %>%
  tidyr::pivot_longer(
    cols = -Sexo,
    names_to = 'razoes') %>%
  dplyr::group_by(Sexo, razoes) %>% 
  dplyr::summarize(Total = sum(value), .groups = 'drop') |> 
  dplyr::mutate(
    razoes = dplyr::case_when(
      razoes == 'razões_hábito' ~ 'Hábito',
      razoes == 'razões_sabor_residual' ~ 'Sabor residual',
      razoes == 'razões_sabor_aroma' ~ 'Sabor aroma',
      razoes == 'razões_azia' ~ 'Azia',
      razoes == 'razões_dor_cabeça' ~ 'Dor de cabeça',
      razoes == 'razões_saude' ~ 'Saúde',
      razoes == 'razões_restr_med' ~ 'Restrição médica',
      razoes == 'razões_outros' ~ 'Outros',
      TRUE ~ razoes)) |> 
  ggplot(aes(x = razoes, y = Total, fill = Sexo)) +
  geom_col(position = 'fill') +
  labs(x = NULL, y = NULL) +
  theme_minimal()
  

# df_nao %>% 
#   dplyr::group_by(Sexo) %>% 
#   dplyr::select("razões_hábito","razões_sabor_residual","razões_sabor_aroma", "razões_azia","razões_dor_cabeça","razões_saude") %>% 
#   dplyr::summarize_all(sum)  %>% 
#   tidyr::pivot_longer(cols = Sexo, names_to = 'razoes',values_to = "Total")


#----------------------------------------------------------------------------------------
#Características das pessoas que consomem café

names(df_sim)


df_sim %>% 
  ggplot(aes(forma_preferida_consumo)) + 
  geom_bar(fill='brown') +
  geom_text(
    stat='count', 
    aes(label=..count..),
    color='black',
    size = 4,
    vjust = -0.50) + 
  theme_classic() +
  labs(
    x= NULL,
    y = 'Total',
    title = 'Formas preferidas de se beber café')


# Alluvium plot  Ocupação x entendimento x Tipo de varejo
df_sim %>%  
  ggplot(aes(
    axis1 = Ocupação,
    axis2 = conhecimento_avançado,
    axis3 = tipo_varejo )) +
  geom_alluvium(aes(fill = conhecimento_avançado)) +
  geom_stratum() +
  geom_text(
    stat = "stratum", 
    size = 2,
    aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Ocupação", "Tipo de varejo"), expand = c(.1, .1)) +
  theme_minimal() +
  labs(
    title = "Tipo de varejo estratificado por Ocupação e grau de conhecimento",
    y = "Frequency")
  

# Grau 
graus <- df_sim %>% 
  dplyr::select(starts_with("grau_")) |> 
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = 'grau') |> 
  dplyr::mutate(
    value_int = dplyr::case_when(
      value == "Muito fraco" ~ -2,
      value == "Moderadamente fraco" ~ -1,
      value == "Neutro" ~ 0,
      value == "Moderadamente forte" ~ 1,
      value == "Muito forte" ~ 2),
    grau = dplyr::case_when(
      grau == 'grau_aroma' ~ 'Aroma',
      grau == 'grau_sabor' ~ 'Sabor',
      grau == 'grau_cor' ~ 'Cor',
      grau == 'grau_suavidade' ~ 'Suavidade',
      grau == 'grau_doçura' ~ 'Doçura'))

# ???
graus %>%  
  ggplot(aes(x = value_int, fill = grau)) +
  geom_histogram() + 
  theme_classic() +
  labs(x="Nível do Aroma", y="Quantidade") +
  facet_grid(~grau)


# Parallel coordinates --- um pouco confuso de ver

change <- function(value ) {
  if (value == "Muito fraco") {
    new_value <- -2
  } else if (value == "Moderadamente fraco") {
    new_value <- -1
  } else if (value == "Neutro") {
    new_value <- 0 
  } else if (value == "Moderadamente forte") {
    new_value <- 1
  } else if (value == "Muito forte") {
    new_value <- 2
  } else {
    new_value <- 999
  }
  
  new_value
}


df_sim |> 
  dplyr::mutate(dplyr::across(
    .cols = dplyr::starts_with('grau'),
    .fns = ~ purrr::map_dbl(.x, .f = ~ change(.x)))) |> 
  dplyr::select(dplyr::starts_with('grau_'), 'Sexo') |> 
  ggparcoord(
    columns = 1:5,
    groupColumn = 6,
    showPoints = TRUE,
    alphaLines = 0.3)  +
  theme_classic() +
  facet_grid(~ Sexo)

# media dos niveis para cada forma de consumo
df_sim |> 
  dplyr::mutate(dplyr::across(
    .cols = dplyr::starts_with('grau'),
    .fns = ~ purrr::map_dbl(.x, .f = ~ change(.x)))) %>%
  tidyr::pivot_longer(dplyr::starts_with('grau'), names_to = 'grau') |> 
  dplyr::group_by(forma_preferida_consumo, grau) %>% 
  dplyr::summarise(media = mean(value)) |> 
  tidyr::pivot_wider(names_from = grau, values_from = media)

# Correlação entre níveis dos atributos

df_sim |> 
  dplyr::mutate(dplyr::across(
    .cols = dplyr::starts_with('grau'),
    .fns = ~ purrr::map_dbl(.x, .f = ~ change(.x)))) %>%
  dplyr::select(dplyr::starts_with('grau')) %>% 
  dplyr::rename_all(~stringr::str_replace(.,"^grau_","")) %>% 
  GGally::ggcorr(method = c("everything", "pearson"),label = TRUE)

# Relação Ocupação x Periodo
df_sim %>%
  dplyr::group_by(Ocupação) %>% 
  dplyr::select(starts_with("periodos_")) %>% 
  dplyr::rename_all(~stringr::str_replace(.,"^periodos_consumo_","")) %>% 
  dplyr::summarise_all(sum)


#Relação Ocupação e Número de xícaras
df_sim %>% 
  ggplot(aes(x=Ocupação,fill=qnt_xícaras)) +
  geom_bar(position = 'fill')+
  scale_y_continuous(labels = scales::percent) +
  labs("Número de xícaras consumidas para cada Ocupação ", y='Proporção') + 
  theme_classic()
  

# Local de consumo

df_sim %>% 
  ggplot(aes(x=local_consumo)) +
  geom_bar()


df_sim %>%  
  ggplot(aes(axis1 = local_consumo,
             axis2 = Ocupação)) +
  geom_alluvium(aes(fill = local_consumo)) +
  geom_stratum() +
  geom_text(size = 3, stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Local de Consumo", "Ocupação"), expand = c(.1, .1)) +
  labs(
    title = "Local de consumo estratificado por Ocupação e grau de conhecimento",
    y = "Proporção") +
  theme_minimal()


# Atributos 

## Agrupado por sexo
df_sim %>% 
  dplyr::group_by(Sexo) %>% 
  dplyr::select(starts_with("atributos_compra_")) %>% 
  dplyr::rename_all(~stringr::str_replace(.,"^atributos_compra_","")) %>% 
  dplyr::summarise_all(sum) 

## Agrupado por Ocupação
df_sim %>% 
  dplyr::group_by(Ocupação) %>% 
  dplyr::select(starts_with("atributos_compra_")) %>% 
  dplyr::rename_all(~stringr::str_replace(.,"^atributos_compra_","")) %>% 
  dplyr::summarise_all(sum) 

## Agrupado por atributos_compra
df_sim %>% 
  dplyr::select(starts_with("atributos_compra_")) %>% 
  dplyr::rename_all(~stringr::str_replace(.,"^atributos_compra_","")) %>% 
  dplyr::summarise_all(sum) 

# Perguntas diretas

#Geral 
  ### lembrar : quanto mais perto de 1 é concordo totalmente e mais perto de 2 é discordo totalmente
  ### Mediana nesse caso parece ser a métrica mais correta

df_sim %>% 
  dplyr::select(starts_with("café_")) %>% 
  dplyr::rename_all(~stringr::str_replace(.,"^café_","")) %>% 
  summary()

#----------------------------------------------------------------------------------------
# Data Science
readr::read_csv('dados/respostas_sim_cafe.csv', col_types = 'c')
