library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)


df_sim <- read.csv("Respostas_sim_café.csv")
df_nao <- read.csv("Respostas_não_café.csv")
df_geral <- read.csv("Perfil_respostas.csv")

dim(df_geral) # 111 respostas

names(df_geral)

df_geral$Idade <- factor(df_geral$Idade,
                         levels = c("Menor de 18 anos" ,"Entre 18 e 24" ,
                                    "Entre 25 e 30" ,   "Entre 31 e 40" ,
                                    "Entre 41 e 50"))
#----------------------------------------------------------------------------------------

# Características das pessoas que responderam o forms

#Consumo por gênero
df_geral %>%  
  ggplot(aes(Gênero,fill=consumo_café)) + 
    labs(fill= "Consome ou não café",y='Total',title = 'Consumo - Homens x mulheres') + 
    geom_bar(position = "dodge") + 
    geom_text(
      stat='count', aes(label=..count..),
      color='black', size = 5,vjust = 1.5, 
      position = position_dodge(.9)) +
    
    theme_classic()

#Consumo por idade
df_geral %>%  
  ggplot(aes(Idade,fill=consumo_café)) +
    labs(fill= "Consumo de café",x='Idade',y='Total',title = 'Consumo - Idade')+
    geom_bar() + 
    geom_text(
      stat='count', aes(label=..count..),
      color='black',size = 4,vjust = -1) +
    theme_classic()

#Ocupação
df_geral %>%  
  ggplot(aes(Ocupação,fill=consumo_café)) +
  labs(fill= "Consumo de café",x='Ocupação',y='Total',title = 'Consumo - Idade')+
  geom_bar() +  
  theme_ipsum()
  theme_classic() + theme(axis.text.x = element_text(angle=45))

#faixa etária
df_geral %>%  
  ggplot(aes(x=Idade,fill=Gênero)) + 
  geom_bar(data = subset(df_geral,Gênero == "Feminino")) + 
  geom_bar(data = subset(df_geral,Gênero == "Masculino"),aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip() + 
  theme_ipsum()
  #labs(fill= "Gênero",x='Total',y='Idade',title = 'Faixa etária') + 
  #theme_classic()


#escolaridade
df_geral %>%  
  ggplot(aes(grau_de_escolaridade,fill=consumo_café)) +
  labs(fill= "Consumo de café",x='Escolaridade',y='Total',title = 'Consumo - Idade')+
  geom_bar() +  
  theme_classic()

#----------------------------------------------------------------------------------------
#Características das pessoas que não consomem café

names(df_nao)

#Razões pelas quais não consomem café
df_nao %>%  
  ggplot(aes(razões)) + 
  geom_bar() + coord_flip()

