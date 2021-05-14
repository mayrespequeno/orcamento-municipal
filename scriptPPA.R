##### Análise das prioridades de recursos destinados as políticas públicas no PPA ############
######  2018-2021 do Município de Maceió

#---------Banco de Informações e Pacotes---------------------------------------------------------------

install.packages("dplyr")
install.packages("readxl")

library(gghighlight)
library(readxl)
library(dplyr)


programas <- read_excel("PROGRAMA_FUNCAO_SUBFUNCAO.xlsx")

acao <- read_excel("PROGRAMA_ACAO.xlsx") 


porprograma <- programas%>%
  select(PROGRAMA, REPASSE2018, REPASSE2018A2021)%>%
  group_by(PROGRAMA)%>%
  summarise(PROVREPA2018 = sum(REPASSE2018),
            PROVREPA2018A2021 = sum(REPASSE2018A2021))

porfuncao <- programas%>%
  select(FUNCAO, REPASSE2018, REPASSE2018A2021)%>%
  group_by(FUNCAO)%>%
  summarise(PROVREPA2018 = sum(REPASSE2018),
            PROVREPA2018A2021 = sum(REPASSE2018A2021))

porsubfuncao <- programas%>%
  select(SUBFUNCAO, REPASSE2018, REPASSE2018A2021)%>%
  group_by(SUBFUNCAO)%>%
  summarise(PROVREPA2018 = sum(REPASSE2018),
            PROVREPA2018A2021 = sum(REPASSE2018A2021))


#-----------------------POR PROGRAMA--------------------------------------------------

options(scipen = 999)

#REPASSE2018

programas2018 <- porprograma%>%
  ggplot(aes(y= PROVREPA2018, x= reorder(PROGRAMA, -PROVREPA2018)))+
  geom_bar(stat = "identity", color = "#F8766D", fill = "#F8766D")+ 
  theme_classic()+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  geom_hline(yintercept = mean(porprograma$PROVREPA2018),linetype = 'longdash', 
             color = '#FFFF00', size = 1)+
  geom_hline(yintercept = median(porprograma$PROVREPA2018),linetype = 'longdash', 
             color = "#696969", size = 1)+
  labs(x = NULL , y = NULL)+
  geom_label(aes(label = PROVREPA2018), colour = "black", fontface = "bold", hjust = - 0.1, nudge_x = 0.05)+
  theme(axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"))

ggsave("programas2018.png", width = 60, height = 40, units = "cm")

#REPASSE2018A2021

programas2018A2021 <- porprograma%>%
  ggplot(aes(y= PROVREPA2018A2021, x= reorder(PROGRAMA, -PROVREPA2018A2021)))+
  geom_bar(stat = "identity", color = "#F8766D", fill = "#F8766D")+ 
  theme_classic()+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  geom_hline(yintercept = mean(porprograma$PROVREPA2018A2021),linetype = 'longdash', 
             color = '#FFFF00', size = 1)+
  labs(x = NULL , y = NULL)+
  geom_label(aes(label = PROVREPA2018), colour = "black", fontface = "bold", hjust = - 0.1, nudge_x = 0.05)+
  theme(axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"))

ggsave("programas2018A2021.png", width = 60, height = 40, units = "cm")

#-----------------------POR FUNÇÃO---------------------------------------------------

porfuncao%>%
  ggplot(aes(y= PROVREPA2018, x= reorder(FUNCAO, -PROVREPA2018)))+
  geom_bar(stat = "identity", color = "#F8766D", fill = "#F8766D")+
  theme_classic()+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  geom_hline(yintercept = mean(porprograma$PROVREPA2018A2021),linetype = 'longdash', 
             color = '#FFFF00', size = 1)+
  geom_hline(yintercept = median(porprograma$PROVREPA2018),linetype = 'longdash', 
             color = "#696969", size = 1)+
  labs(x = NULL , y = NULL)+
  geom_label(aes(label = PROVREPA2018), colour = "black", fontface = "bold", hjust = - 0.1, nudge_x = 0.05)+
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"))


funcao2018a2021 <- porfuncao%>%
  ggplot(aes(y= PROVREPA2018A2021, x= reorder(FUNCAO, -PROVREPA2018A2021)))+
  geom_bar(stat = "identity", color = "#F8766D", fill = "#F8766D")+
  theme_classic()+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  geom_hline(yintercept = mean(porprograma$PROVREPA2018A2021),linetype = 'longdash', 
             color = '#FFFF00', size = 1)+
  labs(x = NULL , y = NULL)+
  geom_label(aes(label = PROVREPA2018), colour = "black", fontface = "bold", hjust = - 0.1, nudge_x = 0.05)+
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"))

ggsave("funcao2018a2021.png", width = 60, height = 40, units = "cm")


#selcionando variaveis para evidência no gráfico
pautas <- c("181 - POLICIAMENTO",
            "244 - ASSISTÊNCIA COMUNITÁRIA",
            "542 - CONTROLE AMBIENTAL",
            "813 - LAZER",
            "365 - EDUCAÇÃO INFANTIL",
            "366 - EDUCAÇÃO DE JOVENS E ADULTOS",
            "367 - EDUCAÇÃO ESPECIAL",
            "368 - EDUCAÇÃO BÁSICA",
            "301 - ATENÇÃO BÁSICA",
            "302 - ASSISTÊNCIA HOSPITALAR E AMBULATORIAL",
            "812 - DESPORTO COMUNITÁRIO",
            "241 - ASSISTÊNCIA AO IDOSO",
            "242 - ASSISTÊNCIA AO PORTADOR DE DEFICIÊNCIA",
            "243 - ASSISTÊNCIA A CRIANÇA E AO ADOLESCENTE",
            "244 - ASSISTÊNCIA COMUNITÁRIA",
            "422 - DIREITOS INDIVIDUAIS, COLETIVOS E DIFUSOS",
            "392 - DIFUSÃO CULTURAL")

filtropautas <- porsubfuncao%>%
  filter(SUBFUNCAO %in% pautas)

subfuncaorepasse2018 <- filtropautas$PROVREPA2018
subfuncaorepasse2018A2021 <- filtropautas$PROVREPA2018A2021

#-----------------------PORSUBFUNCAO-------------------------------------------------------

porsubfuncao%>%
  ggplot(aes(y= PROVREPA2018, x= reorder(SUBFUNCAO, -PROVREPA2018)))+
  geom_bar(stat = "identity", color = "#F8766D", fill = "#F8766D")+
  theme_classic()+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  geom_hline(yintercept = mean(porprograma$PROVREPA2018A2021),linetype = 'longdash', 
             color = '#FFFF00', size = 1)+
  labs(x = NULL , y = NULL)+
  geom_label(aes(label = PROVREPA2018), colour = "black", fontface = "bold", hjust = - 0.1, nudge_x = 0.05)+
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"))+
  gghighlight(SUBFUNCAO %in% pautas, unhighlighted_colour = "black")


subfuncao2018A2021 <- porsubfuncao%>%
  ggplot(aes(y= PROVREPA2018A2021, x= reorder(SUBFUNCAO, -PROVREPA2018A2021)))+
  geom_bar(stat = "identity", color = "#F8766D", fill = "#F8766D")+
  theme_classic()+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  geom_hline(yintercept = mean(porprograma$PROVREPA2018A2021),linetype = 'longdash', 
             color = '#FFFF00', size = 1)+
  labs(x = NULL , y = NULL)+
  geom_label(aes(label = PROVREPA2018), colour = "black", fontface = "bold", hjust = - 0.1, nudge_x = 0.05)+
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"))+
  gghighlight(SUBFUNCAO %in% pautas, unhighlighted_colour = "black")

ggsave("subfuncao2018A2021.png", width = 60, height = 40, units = "cm")


#-----------------------SUBFUNÇÃO COM DIREÇÃO DA AÇÃO E PAUTAS MANDATO--------------------------------------------

graficopautas <- filtropautas%>%
  ggplot(aes(y= PROVREPA2018A2021, x= reorder(SUBFUNCAO, -PROVREPA2018A2021)))+
  geom_bar(stat = "identity", color = "#F8766D", fill = "#F8766D")+
  theme_classic()+
  coord_flip()+
  scale_y_continuous(expand = c(0,0))+
  geom_hline(yintercept = mean(porprograma$PROVREPA2018A2021),linetype = 'longdash', 
             color = '#FFFF00', size = 1)+
  geom_hline(yintercept = median(porprograma$PROVREPA2018),linetype = 'longdash', 
             color = "#696969", size = 1)+
  labs(x = NULL , y = NULL)+
  geom_label(aes(label = PROVREPA2018), colour = "black", fontface = "bold", hjust = - 0.1, nudge_x = 0.05)+
  theme(axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"))+
  gghighlight(SUBFUNCAO %in% pautas, unhighlighted_colour = "black")

ggsave("graficopautas.png", width = 60, height = 40, units = "cm")




