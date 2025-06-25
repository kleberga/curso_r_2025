library(ggplot2)
library(readr)
library(readxl) 
library(RJSONIO)
library(tidyr)
library(writexl)
library(stringr)
#_______________________________________________________________________________________________________________________
# criar um array
#_______________________________________________________________________________________________________________________
# series do SGS
seriesSGS <- c(20545, # Saldo - Pessoas jurídicas - Desconto de cheques
               20552, # Saldo - Pessoas jurídicas - Cheque especial
               20553, # Saldo - Pessoas jurídicas - Aquisição de veículos
               20591, # Saldo - Pessoas físicas - Desconto de cheques
               20573, # Saldo - Pessoas físicas - Cheque especial
               20581, # Saldo - Pessoas físicas - Aquisição de veículos
               20637, # Concessões - Pessoas jurídicas - Desconto de cheques
               20644, # Concessões - Pessoas jurídicas - Cheque especial
               20645, # Concessões - Pessoas jurídicas - Aquisição de veículos
               20683, # Concessões - Pessoas físicas - Desconto de cheques
               20665, # Concessões - Pessoas físicas - Cheque especial
               20673  # Concessões - Pessoas físicas - Aquisição de veículos
               )
# vetor de tipo de pessoa
pessoa <- c("PJ","PJ","PJ","PF","PF","PF","PJ","PJ","PJ","PF","PF","PF")
operacao <- c("Desconto de cheques","Cheque especial","Aquisição de veículos",
              "Desconto de cheques","Cheque especial","Aquisição de veículos",
              "Desconto de cheques","Cheque especial","Aquisição de veículos",
              "Desconto de cheques","Cheque especial","Aquisição de veículos")
# vetor de tipo de operacao
tipo_op <- c("saldo","saldo","saldo","saldo","saldo","saldo","conc","conc","conc","conc","conc","conc")
# criar tabela conjunta
dadosCred <- data.frame(seriesSGS = seriesSGS, pessoa = pessoa, operacao = operacao, tipo_op = tipo_op)
# carregar os dados
saldoConc <- read_excel("C:/Users/Kleber/Documents/Curso_R/base_dados/saldoConc.xlsx")
# transformar para o formato long
saldoConc_2 <- saldoConc %>% gather("variable", "value", -data)
# extrair o numero das series
saldoConc_2$codigo1 <- as.numeric(str_sub(saldoConc_2$variable, 7, 11))
# unir a tabela de valores com a tabela de descricao das series
saldoConc_2 <- merge(saldoConc_2, dadosCred, by.x="codigo1", by.y="seriesSGS", all.x=T)
# manter apenas as colunas de interesse
saldoConc_2 <- saldoConc_2[,c("data","pessoa","operacao","tipo_op","value")]
# renomear as colunas
colnames(saldoConc_2) <- c("data","pessoa","tipoOper","tipoValor","valor")
# gerar o array
array_teste <- acast(saldoConc_2, data~pessoa~tipoOper~tipoValor, value.var="valor")
# renomear as dimensoes
dimnames(array_teste) <- list(data = dimnames(array_teste)[[1]],pessoa=dimnames(array_teste)[[2]],
                              tipoOper = dimnames(array_teste)[[3]], valor = dimnames(array_teste)[[4]])
#_______________________________________________________________________________________________________________________

pibIpca <- read.csv("C:/Users/Kleber/Documents/Curso_R/base_dados/pib_ipca.csv", sep=";", dec=",")


library(readr)
pibIpca <- read_delim("Curso_R/base_dados/pib_ipca.csv", delim = ";",
                      escape_double = FALSE,
                      locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE,
                      show_col_types = FALSE)


library(readr)
pibIpca <- read_delim("Curso_R/base_dados/pib_ipca.csv", delim = ";",
                      escape_double = FALSE,
                      locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE,
                      show_col_types = FALSE)
export <- read_delim("Curso_R/base_dados/exportacoes.csv",
                     delim = ";", escape_double = FALSE,
                     locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE,
                     show_col_types = FALSE)

plot(y=pibIpca$pibReal, x=pibIpca$ano, type="l", ylab="%", xlab=NA,
     main="PIB real")

library(ggplot2)
ggplot(pibIpca, aes(as.factor(ano), pibReal)) +
  geom_line(color="red", group=1) +
  labs(x = NULL, y = "%", title="PIB real (%)")

ggplot(pibIpca) +
  geom_line(aes(as.factor(ano), pibReal, color="PIB real"), group=1) +
  geom_line(aes(as.factor(ano), ipca, color="IPCA"), group=1) +
  labs(x = NULL, y = "%", title="PIB real e IPCA") +
  scale_color_manual(values=c("PIB real" = "blue","IPCA" = "red"))+
  theme(
    legend.title = element_blank(), legend.key=element_blank() )

ggplot(pibIpca, aes(as.factor(ano), pibReal)) +
  geom_bar(stat="identity", fill="lightblue") +
  labs(x = NULL, y = "%", title="PIB real") +
  scale_y_continuous(n.breaks=8, limits=c(-4,10)) + geom_text(aes(label =
                                                                    format(round(pibReal,1), decimal.mark = ",", scientific = FALSE)), vjust =
                                                                -1) +
  theme( panel.background = element_rect(fill='transparent'),
         panel.grid.major.y = element_line(color = "gray88", size = 0.5,
                                           linetype = 1), panel.grid.major.x = element_blank(),
         plot.title = element_text(face = "bold"))


importPIB <- read_delim("Curso_R/base_dados/importacoes_pib.csv", delim =
                          ";",
                        escape_double = FALSE,locale = locale(decimal_mark = ",",
                                                              grouping_mark = "."), trim_ws = TRUE, show_col_types = FALSE)
importPIB$import <- importPIB$import/1000
ggplot(importPIB) +
  geom_bar(aes(x=as.factor(ano), y=pibReal, fill="PIB real"),
           stat="identity") +
  geom_line(aes(x=as.factor(ano), y=import/50, group = 1, color =
                  "Importações")) +
  labs(x = NULL, y = "PIB real (%)", title="PIB real e Importações") +
  scale_y_continuous(n.breaks=7, limits=c(-5,10),
                     labels = function(x) format(x, decimal.mark = ",",
                                                 scientific = FALSE),
                     sec.axis = sec_axis(~.*50, name="Importações (US$ bilhões)")) +
  geom_text(aes(x = as.factor(ano), y = pibReal,
                label = format(round(pibReal,1), decimal.mark = ",",
                               scientific = FALSE)), vjust = -1, color = "lightblue4", size=3) +
  geom_text(aes(x = as.factor(ano), y = import/50,
                label = format(round(import,1), decimal.mark = ",",
                               scientific = FALSE)), vjust = -1, color = "red3", size=3,
            position = position_nudge(y = c(0,-1.5,0,0,0,0,0,0,0,0,0,-1.5,0))) +
  scale_colour_manual(values=c("Importações" = "red3")) +
  scale_fill_manual(values=c("PIB real" = "lightblue"))+
  theme(panel.background = element_rect(fill='transparent'),
        panel.grid.major.y = element_line(color = "gray88", size = 0.5,
                                          linetype = 1), panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.key=element_blank(),
        axis.title.y = element_text(size = 8),
        legend.title=element_blank(),
        legend.position = "bottom")

library(readxl)
# url do arquivo do IBC-Br
urlIBCBR <- "https://www.bcb.gov.br/content/indeco/
indicadoresselecionados/ie-01.xlsx"
# arquivo de destino
pastaDest <- "C:/Users/Kleber/Documents/Curso_R/base_dados/ibcbr.xlsx"
# baixar o arquivo
download.file(urlIBCBR, pastaDest, mode="wb")
# carregar o arquivo para dentro do R
ibcbr <- read_excel(pastaDest, range="A11:D35", col_names = F)
# alterar o nome das colunas
colnames(ibcbr) <- c("ano","mes","obs","dessaz")



# carregar o package zoo
library(zoo)
# preencher os NA's com o ultimo valor observado do ano
ibcbr$ano <- na.locf(ibcbr$ano)
# eliminar a linha que nao possui mes
ibcbr <- ibcbr[complete.cases(ibcbr),]
# criar coluna apenas com os dois ultimos numeros de ano
ibcbr$ano2 <- substr(ibcbr$ano, 3, 4)
# criar nova coluna de mes
ibcbr$mes <- paste(ibcbr$mes, ibcbr$ano2, sep="/")
# excluir a coluna ano2
ibcbr$ano2 <- NULL

# pib <- read.csv("C:/Users/Kleber/Documents/Curso_R/base_dados/pib.csv", sep=";", dec=",")
# pibIpca <- read.csv("C:/Users/Kleber/Documents/Curso_R/base_dados/pib_ipca.csv", sep=";", dec=",")
# plot(y=pib$pibReal, x=pib$ano, type="l", xlab="Anos", ylab="PIB real (%)", main="Variação do PIB real (%)")
# 
# library(ggplot2)
# 
# library(readr)
# pibIpca2 <- read_delim("Curso_R/base_dados/pib_ipca.csv", delim = ";", escape_double = FALSE,
#                        locale = locale(decimal_mark = ",",  grouping_mark = "."), trim_ws = TRUE,
#                        show_col_types = FALSE)
# 
# 
# teste <- read.csv("C:/Users/Kleber/Documents/Curso_R/base_dados/exportacoes.csv", sep=";", dec=",", grouping_mark=".")
# 
# ggplot(pib, aes(ano, pibReal)) +   
#  geom_line(color="red") +  
#  labs(x = "Anos", y = "PIB real (%)", title="Variação do PIB real (%)") +   
#  scale_x_continuous(n.breaks=12)  
# 
# 
# ggplot(pib, aes(as.factor(ano), pibReal)) +   
#   geom_bar(stat="identity", fill="lightblue") +  
#   labs(x = "Anos", y = "PIB real (%)", title="Variação do PIB real (%)") +   
#   scale_y_continuous(n.breaks=7, limits=c(-4,10), 
#                      labels = function(x) format(x, decimal.mark = ",", scientific = FALSE)) +
#   geom_text(aes(label = format(round(pibReal,1), decimal.mark = ",", scientific = FALSE)), vjust = -1) +  
#   theme(
#     panel.background = element_rect(fill='transparent'),
#     panel.grid.major.y = element_line(color = "gray88", size = 0.5, linetype = 1),
#     panel.grid.major.x = element_blank(),
#     plot.title = element_text(face = "bold")
#     )
# 
# ggplot(pibIpca) +
#   geom_bar(aes(x=as.factor(ano), y=pibReal, fill="PIB"), stat="identity", fill="lightblue") +
#   geom_line(aes(x=as.factor(ano), y=ipca, group = 1, color = "IPCA"), stat="identity", color="red") +
#   labs(x = "Anos", y = "PIB real (%)", title="IPCA e PIB real - (%)") +
#   scale_y_continuous(n.breaks=7, limits=c(-4,12.5),
#                      labels = function(x) format(x, decimal.mark = ",", scientific = FALSE)) +
#   geom_text(aes(x = as.factor(ano), y = pibReal, label = format(round(pibReal,1), decimal.mark = ",",
#                                scientific = FALSE)), vjust = -1, color = "lightblue4") +
#   geom_text(aes(x = as.factor(ano), y = ipca, label = format(round(ipca,1), decimal.mark = ",",
#                                scientific = FALSE)), vjust = -1, color = "red3") +
#   scale_colour_manual(" ", values=c("PIB" = "lightblue", "IPCA" = "red"))+
#   theme(panel.background = element_rect(fill='transparent'),
#         panel.grid.major.y = element_line(color = "gray88", size = 0.5, linetype = 1), panel.grid.major.x = element_blank(),
#         plot.title = element_text(face = "bold"),
#         legend.box="horizontal")
#   
# 
# ggplot(pibIpca) +
#   geom_bar(aes(x=as.factor(ano), y=pibReal, fill="PIB"), 
#            stat="identity", fill="lightblue") +
#   geom_line(aes(x=as.factor(ano), y=ipca, group = 1, color = "IPCA")) +
#   labs(x = "Anos", y = "PIB real / IPCA (%)", title="IPCA e PIB real (%)") +
#   scale_y_continuous(n.breaks=7, limits=c(-4,12.5),
#                      labels = function(x) format(x, decimal.mark = ",", 
#                                                  scientific = FALSE)) +
#   geom_text(aes(x = as.factor(ano), y = pibReal, 
#                 label = format(round(pibReal,1), decimal.mark = ",",
#                                scientific = FALSE)), vjust = -1, color = "lightblue4") +
#   geom_text(aes(x = as.factor(ano), y = ipca, 
#                 label = format(round(ipca,1), decimal.mark = ",",
#                                scientific = FALSE)), vjust = -1, color = "red3") +
#   scale_colour_manual(" ", values=c("PIB" = "lightblue", "IPCA" = "red"))+
#   theme(panel.background = element_rect(fill='transparent'),
#         panel.grid.major.y = element_line(color = "gray88", size = 0.5, 
#                                           linetype = 1), 
#         panel.grid.major.x = element_blank(),
#         plot.title = element_text(face = "bold"),
#         legend.box="horizontal")
# 
# 
# ggplot(pibIpca) +
#   geom_bar(aes(x=as.factor(ano), y=pibReal, fill="PIB"),
#            stat="identity", fill="lightblue") +
#   geom_line(aes(x=as.factor(ano), y=ipca, group = 1, color = "IPCA")) +
#   labs(x = "Anos", y = "PIB real / IPCA (%)", title="IPCA e PIB real (%)") +
#   scale_y_continuous(n.breaks=7, limits=c(-4,12.5),
#                      labels = function(x) format(x, decimal.mark = ",",
#                                                  scientific = FALSE)) +
#   geom_text(aes(x = as.factor(ano), y = pibReal,
#                 label = format(round(pibReal,1), decimal.mark = ",",
#                                scientific = FALSE)), vjust = -1, color = "lightblue4") +
#   geom_text(aes(x = as.factor(ano), y = ipca,
#                 label = format(round(ipca,1), decimal.mark = ",",
#                                scientific = FALSE)), vjust = -1, color = "red3") +
#   scale_colour_manual(" ", values=c("PIB" = "lightblue", "IPCA" = "red")) +
#   theme(panel.background = element_rect(fill='transparent'),
#         panel.grid.major.y = element_line(color = "gray88", size = 0.5,
#                                           linetype = 1),
#         panel.grid.major.x = element_blank(),
#         plot.title = element_text(face = "bold"),
#         legend.key=element_blank(),
#         legend.box="horizontal")
# 
# 
# ggplot(pibIpca, aes(as.factor(ano), pibReal)) +
#   geom_bar(stat="identity", fill="lightblue") +
#   labs(x = "Anos", y = "PIB real (%)", title="Variação do PIB real (%)") +
#   scale_y_continuous(n.breaks=7, limits=c(-4,10),
#                      labels = function(x) format(x, decimal.mark = ",", scientific = FALSE)) +
#   geom_text(aes(label = format(round(pibReal,1), decimal.mark = ",",
#                                scientific = FALSE)), vjust = -1) +
#   theme( panel.background = element_rect(fill='transparent'),
#          panel.grid.major.y = element_line(color = "gray88", size = 0.5, linetype =
#                                              1), panel.grid.major.x = element_blank(),
#          plot.title = element_text(face = "bold"))
# 
# 
# 
# ggplot(pibIpca) +
#   geom_bar(aes(x=as.factor(ano), y=pibReal),
#            stat="identity", fill="lightblue") +
#   geom_line(aes(x=as.factor(ano), y=ipca, group = 1, color = "IPCA")) +
#   labs(x = "Anos", y = "PIB real / IPCA (%)", title="PIB real e IPCA (%)") +
#   scale_y_continuous(n.breaks=8, limits=c(-5,12.5),
#                      labels = function(x) format(x, decimal.mark = ",",
#                                                  scientific = FALSE)) +
#   geom_text(aes(x = as.factor(ano), y = pibReal,
#                 label = format(round(pibReal,1), decimal.mark = ",",
#                                scientific = FALSE)), vjust = -1, color = "lightblue4") +
#   geom_text(aes(x = as.factor(ano), y = ipca,
#                 label = format(round(ipca,1), decimal.mark = ",",
#                                scientific = FALSE)), vjust = -1, color = "red3") +
#   scale_colour_manual(" ", values=c("PIB" = "lightblue", "IPCA" = "red")) +
#   theme(panel.background = element_rect(fill='transparent'),
#         panel.grid.major.y = element_line(color = "gray88", size = 0.5,
#                                           linetype = 1),
#         panel.grid.major.x = element_blank(),
#         plot.title = element_text(face = "bold"),
#         legend.key=element_blank())
# 
# 
# importPIB <- read_delim("Curso_R/base_dados/importacoes_pib.csv", delim = ";", escape_double = FALSE,
#                        locale = locale(decimal_mark = ",",  grouping_mark = "."), trim_ws = TRUE,
#                        show_col_types = FALSE)
# importPIB$import <- importPIB$import/1000
# 
# 
# 
# ggplot(importPIB) +
#   geom_bar(aes(x=as.factor(ano), y=pibReal),
#            stat="identity", fill="lightblue") +
#   geom_line(aes(x=as.factor(ano), y=import/50, group = 1, color = "Importações")) +
#   labs(x = "Anos", y = "PIB real (%)", title="PIB real e Importações (%)") +
#   scale_y_continuous(n.breaks=7, limits=c(-5,10),
#                      labels = function(x) format(x, decimal.mark = ",",
#                                                  scientific = FALSE),
#                      sec.axis = sec_axis(~.*50, name="Importações (US$ bilhões)")) +
#   geom_text(aes(x = as.factor(ano), y = pibReal,
#                 label = format(round(pibReal,1), decimal.mark = ",",
#                                scientific = FALSE)), vjust = -1, color = "lightblue4", size=3) +
#   geom_text(aes(x = as.factor(ano), y = import/50,
#                 label = format(round(import,1), decimal.mark = ",",
#                                scientific = FALSE)), vjust = -1, color = "red3", size=3 ,
#             position = position_nudge(y = c(0,-1.5,0,0,0,0,0,0,0,0,0,-1.5,0))) +
#   scale_colour_manual(" ", values=c("PIB" = "lightblue", "Importações" = "red")) +
#   theme(panel.background = element_rect(fill='transparent'),
#         panel.grid.major.y = element_line(color = "gray88", size = 0.5,
#                                           linetype = 1),
#         panel.grid.major.x = element_blank(),
#         plot.title = element_text(face = "bold"),
#         legend.key=element_blank(),
#         axis.title.x = element_text(size = 8),
#         axis.title.y = element_text(size = 8),
#         legend.position = c("bottom"))



# ggplot(importPIB) +
#   geom_bar(aes(x=as.factor(ano), y=pibReal, fill="PIB real"),
#            stat="identity") +
#   geom_line(aes(x=as.factor(ano), y=import/50, group = 1, color = "Importações")) +
#   labs(x = NULL, y = "PIB real (%)", title="PIB real e Importações") +
#   scale_y_continuous(n.breaks=7, limits=c(-5,10),
#                      labels = function(x) format(x, decimal.mark = ",",
#                                                  scientific = FALSE),
#                      sec.axis = sec_axis(~.*50, name="Importações (US$ bilhões)")) +
#   geom_text(aes(x = as.factor(ano), y = pibReal,
#                 label = format(round(pibReal,1), decimal.mark = ",",
#                                scientific = FALSE)), vjust = -1, color = "lightblue4", size=3) +
#   geom_text(aes(x = as.factor(ano), y = import/50,
#                 label = format(round(import,1), decimal.mark = ",",
#                                scientific = FALSE)), vjust = -1, color = "red3", size=3 ,
#             position = position_nudge(y = c(0,-1.5,0,0,0,0,0,0,0,0,0,-1.5,0))) +
#   scale_colour_manual(values=c("Importações" = "red3")) +
#   scale_fill_manual(values=c("PIB real" = "lightblue"))+
#   theme(panel.background = element_rect(fill='transparent'),
#         panel.grid.major.y = element_line(color = "gray88", size = 0.5,
#                                           linetype = 1),
#         panel.grid.major.x = element_blank(),
#         plot.title = element_text(face = "bold"),
#         legend.key=element_blank(),
#         axis.title.y = element_text(size = 8),
#         legend.title=element_blank(),
#         legend.position = "bottom")
# 
# library(readr)
# pibIpca <- read_delim("Curso_R/base_dados/pib_ipca.csv", delim = ";",
#                       escape_double = FALSE,
#                       locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE,
#                       show_col_types = FALSE, file = "UTF-8")
# 
# 
# ggplot(pibIpca) +
#   geom_line(aes(as.factor(ano), pibReal, color="PIB real"), group=1) +
#   geom_line(aes(as.factor(ano), ipca, color="IPCA"), group=1) +
#   labs(x = NULL, y = "PIB real (%)", title="Variação do PIB real (%)") + 
#   scale_color_manual(values=c("PIB real" = "blue","IPCA" = "red"))+
#   theme(
#     legend.title = element_blank(),
#     legend.key=element_blank()
#   )
#_______________________________________________________________________________________________________________________
# grafico com facets
#_______________________________________________________________________________________________________________________
library(readr)
library(ggplot2)
ipcaGrupos <- read_delim("Curso_R/base_dados/ipca_grupos.csv", delim = ";",
                      escape_double = FALSE,
                      locale = locale(decimal_mark = ",", grouping_mark = ".", encoding="windows-1252"), trim_ws = TRUE,
                      show_col_types = FALSE)

ipcaGrupos$data <- factor(ipcaGrupos$data, levels = c("jan/21","fev/21","mar/21","abr/21","mai/21","jun/21","jul/21","ago/21","set/21","out/21",
                                                      "nov/21","dez/21","jan/22","fev/22","mar/22","abr/22","mai/22","jun/22","jul/22","ago/22",
                                                      "set/22","out/22","nov/22","dez/22","jan/23"))

ggplot(ipcaGrupos, aes(x=data, y=ipca, colour = grupo, group = grupo, fill=grupo)) + 
  geom_bar(stat = "identity", colour="white") + 
  facet_wrap(~grupo) + 
  scale_x_discrete(breaks=c("jan/21","jun/21",
                            "jan/22","jun/22","jan/23"))+
  theme(
    panel.spacing = unit(1.4, "lines"),
    legend.title = element_blank()
    ) + 
  scale_fill_manual(values = c( "paleturquoise2", "paleturquoise3", "turquoise4" ))+
  labs(x=NULL, y="%", title="IPCA - grupos selecionados")
#_______________________________________________________________________________________________________________________
# baixar arquivo do IBC-Br
#_______________________________________________________________________________________________________________________
# url do arquivo do IBC-Br
urlIBCBR <- "https://www.bcb.gov.br/content/indeco/indicadoresselecionados/ie-01.xlsx"
# arquivo de destino
pastaDest <- "C:/Users/Kleber/Documents/Curso_R/base_dados/ibcbr.xlsx"
# baixar o arquivo
download.file(urlIBCBR, pastaDest, mode="wb")
# carregar o arquivo para dentro do R
ibcbr <- read_excel(pastaDest, range="A11:D35", col_names = F)
# alterar o nome das colunas
colnames(ibcbr) <- c("ano","mes","obs","dessaz")
# carregar o package zoo
library(zoo)
# preencher os NA's com o ultimo valor observado do ano
ibcbr$ano <- na.locf(ibcbr$ano)
# eliminar a linha que nao possui mes
ibcbr <- ibcbr[complete.cases(ibcbr),]
# criar coluna apenas com os dois ultimos numeros de ano
ibcbr$ano2 <- substr(ibcbr$ano, 3, 4)
# criar nova coluna de mes
ibcbr$mes <- paste(ibcbr$mes, ibcbr$ano2, sep="/")
# excluir a coluna ano2
ibcbr$ano2 <- NULL
#_______________________________________________________________________________________________________________________
# baixar arquivo do SGS
#_______________________________________________________________________________________________________________________
# url da serie do SGS
urlSGS <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados/ultimos/10?formato=csv"
# arquivo de destino
nomeArq <- paste0(tempfile("sgsArq"), ".csv")
# baixar o arquivo
download.file(urlSGS, nomeArq, mode="wb")
# carregar o arquivo
ipca <- read.csv(nomeArq, dec = ",", sep=";")
#_______________________________________________________________________________________________________________________
# baixar arquivo do fOCUS
#_______________________________________________________________________________________________________________________
# url da série do Focus
urlFocus <- "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativaMercadoMensais?$top=24&
$filter=Indicador%20eq%20'C%C3%A2mbio'%20and%20baseCalculo%20eq%200&$orderby=Data%20desc&$format=json&$select=Indicador,
Data,DataReferencia,Mediana,baseCalculo"
# carregar o arquivo
cambio <- fromJSON(urlFocus)
# extrair o arquivo de dentro da lista
cambio <- cambio$value
#_______________________________________________________________________________________________________________________
# baixar serie do SIDRA - tabela de IPCA
#_______________________________________________________________________________________________________________________
library(sidrar)
# retornar informacoes de uma tabela
dadosTab <- info_sidra(7060, wb = FALSE)
# retornar os dados do SIDRA da tabela 7060, para tomate (categoria 7212)
ipcaTomate <- get_sidra(7060, 69, classific="c315", category = list(7212), period = "all")
#_______________________________________________________________________________________________________________________
# baixar serie do SIDRA - tabela de estoques
#_______________________________________________________________________________________________________________________
# retornar informacoes de uma tabela
dadosTab <- info_sidra(254, wb = FALSE)
# retornar os dados do SIDRA da tabela 254, variavel Quantidade estocada (Toneladas) (150), para milho e soja 
# (3045,3047), no servico de armazenagem (114629) da iniciativa privada (3030)
estoques <- get_sidra(254, 150, classific=c("c162","c163","c161"), category = list(c(3045,3047), 114629, 3030), 
                      period = "202201", geo = "State")
#_______________________________________________________________________________________________________________________
# baixar serie do IPCA do SGS em json
#_______________________________________________________________________________________________________________________
library(jsonlite)
# url da consulta
urlIPCA <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados/ultimos/10?formato=json"
# baixar os dados
sgsIPCA <- fromJSON(urlIPCA)
#_______________________________________________________________________________________________________________________
# baixar serie do Focus da Selic em json
#_______________________________________________________________________________________________________________________
# definir a URL da consulta
urlSelic <- "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoSelic?$top=16&$
filter=Indicador%20eq%20'Selic'%20and%20%20baseCalculo%20eq%200&$orderby=Data%20desc&$format=json&$select=Indicador,Data,Reuniao,Mediana"
# baixar os dados
focusSelic <-  jsonlite::fromJSON(urlSelic)
# extrair os dados da lista para um data frame
focusSelic <- focusSelic[["value"]]
#_______________________________________________________________________________________________________________________
# mostrar o merge
#_______________________________________________________________________________________________________________________
# tabela de estudante e nota
estud = data.frame(estudante = c(1:6), nota = c("70", "84", "90", "93", "80", "76"))
# tabela de cidade e nota
cidade = data.frame(estudante = c(2, 4, 6, 7, 8), cidade = c("Brasília", "Natal", "Manaus", "Curitiba", "São Paulo")) 
# uniao das duas tabelas
baseEst = merge(estud, cidade, by = "estudante", all = T)
#_______________________________________________________________________________________________________________________
# mostrar o merge com Reduce
#_______________________________________________________________________________________________________________________
library(jsonlite)
# url da serie 4382
urlPIB <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.4382/dados/ultimos/24?formato=json"
# baixar os dados
pibSGS <- fromJSON(urlSGS)
# url da serie 24363
urlIBCBR <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.24363/dados/ultimos/24?formato=json"
# baixar os dados
ibcbr <- fromJSON(urlIBCBR)
# url da serie 24364
urlIBCBRDessaz <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.24364/dados/ultimos/24?formato=json"
# baixar os dados
ibcbrDessaz <- fromJSON(urlIBCBRDessaz)
# renomear as colunas
colnames(ibcbr) <- c("data","ibcbr")
colnames(ibcbrDessaz) <- c("data","ibcbrDessaz")
colnames(pibSGS) <- c("data","pibAcum12")
# unir as tabelas
baseAtiv <- Reduce(function(x, y) merge(x, y, by="data"), list(ibcbr, ibcbrDessaz, pibSGS))
#_______________________________________________________________________________________________________________________
# mostrar o merge com Reduce
#_______________________________________________________________________________________________________________________

grep("a", c("vidro","casa"))

grepl("a", c("vidro","casa"))

grepl("a", c("vidro","casa"))

sub("a", "w", c("vidro","casa"))

gsub("a", "w", c("vidro","casa", "casal", "vidente"))


grepl("cas[a-z]*", c("vidro","casa", "casal", "vidente"))
grepl("cas[a-z]?", c("vidro","casa", "casal", "vidente"))


strings <- c("a", "ab", "acb", "accb", "acccb", "accccb")

grep("ac?b", strings, value = TRUE)
grep("ac*b", strings, value = TRUE)
grep("ac+b", strings, value = TRUE)
grep("ac{2}b", strings, value = TRUE)
grep("ac{2,}b", strings, value = TRUE)
grep("ac{2,3}b", strings, value = TRUE)

vetorNomes <-  c( "PIB", "PIB serviços serviços","PIB agropecuária", "PIB indústria","IPCA",
                  "IPCA transportes", "IPCA alimentos")

grep("PIB?", vetorNomes, value=T)

grep("IPCA?", vetorNomes, value=T)

gsub(" serviços", "", vetorNomes)

strings <- c("abcd", "cdab", "cabd", "c abd")

grep("\\Bab", strings, value = TRUE)
  
vetor2 <- c("PIB?", "IPCA+", "atividade*")

gsub("\\?","", vetor2)
gsub("\\+","", vetor2)

dados <- c("rstudio", "software r", "carro", "erroPerroB")

dados <- gsub("^rs", "RS", dados)
dados <- gsub("r$", "R", dados)
dados <- gsub("^c", "C", dados)
dados <- gsub("^erro", "",  dados)
dados <- gsub("erro", "I",  dados)



gsub("[0-9]", "",  c("1ipca2", "PIB2030", "Receita998"))

gsub("[A-z]", "",  c("1ipca2", "PIB2030", "Receita998"))


gsub("(?<=\\()\\d+(?=\\))", "\\1", c("sfd(100.00)sdf", "(9.20)", "(1210.05)"), perl=T)

strings <- c("^ab", "ab", "abc", "abd", "abe", "ab 12")

grep("ab.", strings, value = TRUE)
grep("abc|abd", strings, value = TRUE)

which(iris$Species=="setosa")

(6 == 6)&(6 > 5)

iris[which(iris$Species=="setosa"),]

seq(1, 16, by=3)
seq(20, 2, by=-2)
seq(0, 20, length.out = 11)


seq(0, 1, length.out = 11)

summary(iris$Sepal.Length, digits = 3)

iris$Sepal.Length1 <- lag(as.vector(iris$Sepal.Length))

library(jsonlite)
# url da serie 4382
urlSelic <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.4189/dados/ultimos/24?formato=json"
# baixar os dados
selic <- fromJSON(urlSelic)
selic$valor <- as.numeric(selic$valor)


selic$valor1 <- stats::lag(selic$valor, 1)
#_______________________________________________________________________________________________________________________
# usar o lag
#_______________________________________________________________________________________________________________________
library(jsonlite)
library(dplyr)
# url da serie 4189
urlSelic <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.4189/dados/ultimos/24?formato=json"
# baixar os dados
selic <- fromJSON(urlSelic)
# transformar a coluna da selic para numero
selic$valor <- as.numeric(selic$valor)
# ordenar por data
selic <- selic[order(selic$data),]
# criar o lag 1 da selic
selic$valor1 <- lag(selic$valor)
# criar o lag 2 da selic
selic$valor2 <- lag(selic$valor, n=2)
#_______________________________________________________________________________________________________________________
# usar o diff
#_______________________________________________________________________________________________________________________
library(jsonlite)
library(dplyr)
# url da serie 4189
urlSelic <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.4189/dados/ultimos/24?formato=json"
# baixar os dados
selic <- fromJSON(urlSelic)
# transformar a coluna da selic para numero
selic$valor <- as.numeric(selic$valor)
# ordenar por data
selic <- selic[order(selic$data),]
# criar o lag 1 da selic
selic$valorDiff1 <- c(NA,diff(selic$valor))

selic$valorDiff1Outro <- selic$valor-selic$valor1
#_______________________________________________________________________________________________________________________
# usar if
#_______________________________________________________________________________________________________________________
x <- -1
y <- 2
if(c(0,-4)>x){
  print("x é maior que y")
} else if(x<0) {
  print("x é menor que y e menor que 0")
} else {
  print("x é menor que y")
}
#_______________________________________________________________________________________________________________________
# usar o ifelse
#_______________________________________________________________________________________________________________________
library(jsonlite)
# url da serie 7326
urlPIB <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.7326/dados/ultimos/24?formato=json"
# baixar os dados
pib <- fromJSON(urlPIB)
# transformar a coluna do pib para numero
pib$valor <- as.numeric(pib$valor)
# ordenar por data
pib <- pib[order(pib$data),]
# criar uma coluna vazia de avaliacao do PIB
pib$aval <- NA
# preencher a coluna de avaliacao
pib$aval <- ifelse(pib$valor>0, "crescimento", pib$aval)
pib$aval <- ifelse(pib$valor=0, "estabilidade", pib$aval)
pib$aval <- ifelse(pib$valor<0, "redução", pib$aval)
#_______________________________________________________________________________________________________________________
# usar o for
#_______________________________________________________________________________________________________________________
# criar um vetor
pib <- c(1.1, 0.5, 2.5, 2)
# imprimir cada valor no console
for (x in pib) {
  valorImp <- paste0("Valor: ", x)
  print(valorImp)
}

library(jsonlite)
# url da serie 188
urlINPC <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.188/dados/ultimos/24?formato=json"
# baixar os dados
inpc <- fromJSON(urlINPC)
# transformar a coluna do inpc para numero
inpc$valor <- as.numeric(inpc$valor)
# ordenar por data
inpc <- inpc[order(inpc$data),]
# fazer a diferenca primeira do inpc
for(w in c(2:nrow(inpc))){
  inpc[w,"valor1"] <- inpc[w,"valor"]-inpc[w-1,"valor"]
}
#_______________________________________________________________________________________________________________________
# usar o while
#_______________________________________________________________________________________________________________________
x <- 0
while(x <= 10){
  print(x)
  x <- x + 1
}

ipca1 <- 5
ipca2 <- 4
meta <-  ipca1-ipca2
while(meta > 0){
  meta <-  ipca1-ipca2
  ipca1 <- ipca1-0.1
}
#_______________________________________________________________________________________________________________________
# criar funções
#_______________________________________________________________________________________________________________________

soma <- function(numero1 , numero2){
  resultado <- numero1 + numero2
}
w <- soma(2, 3)


library(jsonlite)
# url da serie 20540
url20540 <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.20540/dados/ultimos/12?formato=json"
# url da serie 20541
url20541 <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.20541/dados/ultimos/12?formato=json"
# criar a funcao para somar as colunas
uneSaldos <- function(url1 , url2){
  
  browser()
  
  # baixar os dados
  saldoPJ <- fromJSON(url1)
  # transformar a coluna do saldo para numero
  saldoPJ$valor <- as.numeric(saldoPJ$valor)
  # renomear as colunas
  colnames(saldoPJ) <- c("data","saldoPJ")
  # baixar os dados
  saldoPF <- fromJSON(url2)
  # transformar a coluna do saldo para numero
  saldoPF$valor <- as.numeric(saldoPF$valor)
  # renomear as colunas
  colnames(saldoPF) <- c("data","saldoPF")
  # une as tabelas
  tab <- merge(saldoPJ, saldoPF, by = "data")
  # criar a coluna de saldo total
  tab$saldoTotal <- tab$saldoPJ + tab$saldoPF
  # retornar o objeto
  return(tab)
}
# executar a função
saldos <- uneSaldos(url20540, url20541)

#_______________________________________________________________________________________________________________________
# criar ellipsis
#_______________________________________________________________________________________________________________________
ellipisExemp <- function(...) {
  print("Eu possuo os seguintes argumentos:")
  print(list(...))
}
ellipisExemp(x = 3, y = "ipca", z = FALSE)


soma <- function(numero1 , numero2, ...){
  valor <- list(...)
  valor <- unlist(valor)
  valor <- sum(valor)
  resultado <- numero1 + numero2 + valor
  return(resultado)
}
soma(2, 3, 10, 7, 1)
#_______________________________________________________________________________________________________________________
# funcao apply
#_______________________________________________________________________________________________________________________
# url da serie 189
url189 <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.189/dados/ultimos/12?formato=json"
# baixar os dados
igpm <- fromJSON(url189)
# transformar a coluna do saldo para numero
igpm$valor <- as.numeric(igpm$valor)
# renomear as colunas
colnames(igpm) <- c("data","igpm")
# url da serie 433
url433 <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados/ultimos/12?formato=json"
# baixar os dados
ipca <- fromJSON(url433)
# transformar a coluna do saldo para numero
ipca$valor <- as.numeric(ipca$valor)
# renomear as colunas
colnames(ipca) <- c("data","ipca")
# unir as bases
indPrecos <- merge(igpm, ipca, by="data", sort=F)
# calcular a media de cada indice
apply(indPrecos[,c("igpm","ipca")], 2, mean)
# calcular a media em cada coluna
apply(indPrecos[,c("igpm","ipca")], 1, mean)
# criar um funcao para gerar a media movel
mediaMovel <- function(x){
  for(i in c(3:length(x))){
    x[i] <- mean(x[c(i:(i-3))])
  }
  return(x)
}
# executar a funcao
indPrecos[,c("igpmMean","ipcaMean")] <- apply(indPrecos[,c("igpm","ipca")], 2, mediaMovel)
#_______________________________________________________________________________________________________________________
# funcao split
#_______________________________________________________________________________________________________________________
# url da serie 189
url189 <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.189/dados/ultimos/12?formato=json"
# baixar os dados
igpm <- fromJSON(url189)
# transformar a coluna do saldo para numero
igpm$valor <- as.numeric(igpm$valor)
# criar a coluna de tipo
igpm$tipo <- "igpm"
# url da serie 433
url433 <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados/ultimos/12?formato=json"
# baixar os dados
ipca <- fromJSON(url433)
# transformar a coluna do saldo para numero
ipca$valor <- as.numeric(ipca$valor)
# criar a coluna de tipo
ipca$tipo <- "ipca"
# unir as tabelas pelas linhas
tabPrecos <- rbind(igpm, ipca)
# transformar em lista
tabPrecosLista <- split(tabPrecos, f=tabPrecos$tipo)
#_______________________________________________________________________________________________________________________
# funcao sapply
#_______________________________________________________________________________________________________________________
mediaCalc <- function(x){
  y <- mean(x$valor)
  return(y)
}
w <- lapply(tabPrecosLista, mediaCalc)

mediaCalc <- function(x){
  x$valor <- x$valor*2
  return(x)
}
w <- lapply(tabPrecosLista, mediaCalc)


q <- by(tabPrecos[,"valor"], tabPrecos[,"tipo"], summary)

teste <- aggregate(state.x77, list(Region = state.region), mean)

w <- do.call("rbind",w)
rownames(w) <- NULL
#_______________________________________________________________________________________________________________________
# package BETS
#_______________________________________________________________________________________________________________________
library(BETS)

dados <- BETSsearch()

BETSget(20633)


library(rbcb)
# obter todas as moedas em uma data
get_all_currencies("2023-03-09")
# obter as expectativas anuais do IGP-M
get_annual_market_expectations("IGP-M", start_date = "2023-03-01")
# obter os dados do IPCA mensal
get_series(433, start_date = "2022-11-01")


library(siconfir)
teste <- get_annex()

get_budget(2022, 6, 1, sphere="U")

codigosIBGE <- br_cods

codigosIBGE[codigosIBGE$esfera=="U",]
#_______________________________________________________________________________________________________________________
# R markdown
#_______________________________________________________________________________________________________________________
library(jsonlite)
library(ggplot2)
# url da serie 433
url433 <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.433/dados/ultimos/12?formato=json"
# baixar os dados
ipca <- fromJSON(url433)
# transformar a coluna do saldo para numero
ipca$valor <- as.numeric(ipca$valor)
# criar coluna de data
ipca$data <- as.Date(ipca$data, "%d/%m/%Y")
# ordenar por data
ipca <- ipca[order(ipca$data),]
# formatar a data
ipca$data <- format(ipca$data, "%m/%Y")
# transformar em fator
ipca$data <- factor(ipca$data, levels=ipca$data)
# gerar um grafico
g <- ggplot(ipca, aes(data, valor)) + 
  geom_line(color="blue", group=1) +
  labs(x = NULL, y = "%", title="IPCA (%)")


