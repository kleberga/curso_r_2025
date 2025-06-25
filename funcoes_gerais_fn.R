#_______________________________________________________________________________________________________________________
#
# Descricao
# Funcoes de uso geral, ou seja, nao sao especificas de projecao e podem ser utilizadas em qualquer situacao
# 
#_______________________________________________________________________________________________________________________
#
#_______________________________________________________________________________________________________________________
# funcao para copiar direto para o excel ----
#_______________________________________________________________________________________________________________________
ctrlc <- function(base)
# INPUT:
# base: objeto que se deseja copiar - data.frame ou matriz
# OUTPUT:
# o objeto serah copiado para o clipboard do windows
{
  write.table(base, file="clipboard-128", row.names = F, dec=",")
}
#_______________________________________________________________________________________________________________________
# funcao para gerar um unico arquivo csv com todos os objetos informados no argumento ----
# o usuario pode escolher se deseja usar ponto ou virgula como separador decimal no arquivo .csv
#_______________________________________________________________________________________________________________________
gera_csv <- function(database, dirout, objetos, nome_arquivo, ponto_sep_dec = F, codificacao = "UTF-8", cria_dir = T)
# INPUTS:
# database: data-base do teste no formato AAAAMM - numerico
# dirout: diretorio em que se deseja salvar o csv - caracter
# objetos: lista contendo os objetos que se deseja incluir no csv - lista
# nome_arquivo: nome que se deseja colocar no csv. Se nulo, o nome do csv serah formado pela expressao
#               "csv", juntamente com data-base, nome do arquivo RDS e dia e horario da geracao do mesmo)
# ponto_sep_dec: utilizar ponto como separador decimal (TRUE) ou como virgula (FALSE) - logico
# codificacao: tipo de codificacao do arquivo - character
# cria_dir: indica se deve ser criado diretorio com o nome da data-base dentro do diretorio informado em "dirout", a fim
#           de que o arquivo gerado seja armazenado no mesmo (TRUE) ou nao deve ser criado o diretorio e o arquivo serah
#           salvo diretamente na pasta informada no parametro "dirout" (FALSE) - logic
# OUTPUT:
# grava arquivo csv contendo todos os objetos.
{
  if(cria_dir==T){
    # verifica se o diretorio da data-base existe na paste "output" e, caso negativo, cria a mesma
    ifelse(!dir.exists(paste0(dirout, as.character(database))),
           dir.create(paste0(dirout, as.character(database))), FALSE)
    # cria um arquivo csv e estabelece conexao com o mesmo
    sink(paste0(dirout, as.character(database),"/", database, "_", nome_arquivo, " (",
                format(Sys.time(), "%Y-%m-%d %Hh%Mm%Ss"),")", ".csv"))
  } else {
    # cria um arquivo csv e estabelece conexao com o mesmo
    sink(paste0(dirout,"/", database, "_", nome_arquivo, " (",
                format(Sys.time(), "%Y-%m-%d %Hh%Mm%Ss"),")", ".csv"))
  }
  # colocar os data.frames e matrizes no arquivo csv criado, na ordem que aparecem no argumento "objetos"
  for(i in 1:length(objetos)){
    cat(names(objetos[i]))
    cat('\n')
    if(ponto_sep_dec==T){
      write.table(objetos[[i]], sep = ";", row.names = F, fileEncoding = codificacao, na = "")
    } else {
      write.csv2(objetos[[i]], row.names = F, fileEncoding = codificacao, na = "")
    }
    cat('\n')
    cat('\n')
  }
  # fechar a conexao com o arquivo csv
  sink()
}
#_______________________________________________________________________________________________________________________
# funcao que converte o numero do mes para o nome abreviado do mes ----
#_______________________________________________________________________________________________________________________
convert_mes <- function(num_mes)
# INPUT:
# num_mes: numero do mes - data.frame[(n,1)]
# OUTPUT:
# nome_mes: nome do mes abreviado - vetor caracter
{
  # guardar o nome da coluna
  nm_coluna <- colnames(num_mes)
  # tabela de de-para
  de_para <- matrix(c(1,  "Jan",
                      2,  "Fev",
                      3,  "Mar",
                      4,  "Abr",
                      5,  "Maio",
                      6,  "Jun",
                      7,  "Jul",
                      8,  "Ago",
                      9,  "Set",
                      10, "Out",
                      11, "Nov",
                      12, "Dez"), ncol = 2, byrow=T, dimnames = list(NULL, c("nr_mes","nm_mes")))
  # criar coluna com o numero da linha, para ser usada no ordenamento apos o merge
  num_mes$nr_linha <- as.numeric(rownames(num_mes))
  # realizar a substituicao
  num_mes <- merge(num_mes, de_para, by.x = nm_coluna, by.y = "nr_mes", all.x = T)
  # ordenar pelo nr da linha
  num_mes <- num_mes[order(num_mes$nr_linha),]
  # apagar os rownames para que a sequencia do numero das linhas fique correta
  rownames(num_mes) <- NULL
  # manter apenas a coluna de nome do mes
  num_mes <- num_mes[,"nm_mes"]
  # retorna
  return(num_mes)
}
#_______________________________________________________________________________________________________________________
# funcao que converte o numero do mes para o nome completo do mes ----
#_______________________________________________________________________________________________________________________
convert_mes_2 <- function(num_mes)
# INPUT:
# num_mes: numero do mes - data.frame[(n,1)]
# OUTPUT:
# nome_mes: nome do mes completo - vetor caracter
{
  # guardar o nome da coluna
  nm_coluna <- colnames(num_mes)
  # tabela de de-para
  de_para <- matrix(c(1,  "Janeiro",
                      2,  "Fevereiro",
                      3,  "Março",
                      4,  "Abril",
                      5,  "Maio",
                      6,  "Junho",
                      7,  "Julho",
                      8,  "Agosto",
                      9,  "Setembro",
                      10, "Outubro",
                      11, "Novembro",
                      12, "Dezembro"), ncol = 2, byrow=T, dimnames = list(NULL, c("nr_mes","nm_mes")))
  # criar coluna com o numero da linha, para ser usada no ordenamento apos o merge
  num_mes$nr_linha <- as.numeric(rownames(num_mes))
  # realizar a substituicao
  num_mes <- merge(num_mes, de_para, by.x = nm_coluna, by.y = "nr_mes", all.x = T)
  # ordenar pelo nr da linha
  num_mes <- num_mes[order(num_mes$nr_linha),]
  # apagar os rownames para que a sequencia do numero das linhas fique correta
  rownames(num_mes) <- NULL
  # manter apenas a coluna de nome do mes
  num_mes <- num_mes[,"nm_mes"]
  # retorna
  return(num_mes)
}
#_______________________________________________________________________________________________________________________
# funcao para corrigir um unico valor por uma taxa projetada ----
# o valor do periodo t eh igual ao valor do periodo t-1 corrigido pela taxa projetada para o periodo t
#_______________________________________________________________________________________________________________________
aplic_taxa <- function(valor, taxa)
# INPUTS:
# valor: objeto contendo o valor a ser corrigido pela taxa. Os demais valores da serie devem ser "NA". A primeira
#        coluna da tabela deve conter a data - data.frame(data,valor)
# taxa: taxa a ser aplicada, em decimais. A primeira coluna da tabela deve conter a data - data.frame(data,taxa)
# OUTPUT:
# valor_2: objeto contendo a data e o valor corrigido pela taxa projetada - data.frame(data,valor_corrigido)
{
  # guardar o nome das valors
  nm_valor <- colnames(valor)
  # renomear a primeira coluna da tabela de taxa para "data"
  colnames(taxa) <- c("data","taxa")
  # renomear a primeira coluna da tabela de valors para "data"
  colnames(valor) <- c("data","valor")
  # unir as bases das valores e de taxa
  valor <- merge(valor, taxa, by = "data", all.x = T)
  # ordenar por data
  valor <- valor[order(valor[,"data"]),]
  # identificar as datas em que a valor nao possui valores de NA
  data_na <- valor[!is.na(valor[,"valor"]),"data"]
  # atribuir "0" para o valor da taxa nos anos em que a seria nao possui NA
  valor[valor$data%in%data_na,"taxa"] <- 0
  # preencher os valores de NA com os valores mais recentes
  valor[,"valor"] <- na.locf(valor[,"valor",drop=F], na.rm = F)
  # eliminar as linhas com NA
  valor_2 <- valor[complete.cases(valor),]
  # calcular o valor acumulado da taxa
  valor_2$taxa_acum <- cumprod(1 + valor_2$taxa)
  # multiplicar o valor a ser corrigido pela taxa acumulada
  valor_2[,"valor"] <- valor_2[,"valor"] * valor_2[,"taxa_acum"]
  # filtrar as variaveis
  valor_2 <- valor_2[,c("data","valor")]
  # renomear as colunas
  colnames(valor_2) <- nm_valor
  # retorna
  return(valor_2)
}
#_______________________________________________________________________________________________________________________
# funcao para corrigir uma serie de valores por uma taxa projetada ----
# o valor do periodo t eh igual ao valor do periodo t-1 corrigido pela taxa projetada para o periodo t
# utiliza a funcao "aplic_taxa"
# quando ha mais de uma valor, os valores sao acumulados
#_______________________________________________________________________________________________________________________
aplic_taxa_2 <- function(serie, taxa, inclui_ult = T)
# INPUTS:
# serie: objeto contendo a data e a serie a ser atualizada. A primeira coluna deve ser a data - data.frame(data,serie)
# taxa: objeto contendo a data e a taxa projetada. A primeira coluna deve ser a data - data.frame(data,taxa)
# inclui_ult: indica se o ultimo valor da serie deve ser atualizado (TRUE) ou nao (FALSE) - logico
# OUTPUT:
# valor_3: objeto contendo a data e a serie corrigida pela taxa - data.frame(data,serie)
{
  # guardar os nomes das colunas
  nm_col <- colnames(serie)
  # renomear as colunas
  colnames(serie) <- c("data","valor_serie")
  # renomear as colunas de taxa
  colnames(taxa) <- c("data", "valor_taxa")
  # extrair coluna de data
  data_tb <- serie[,"data",drop=F]
  # ordenar por data
  serie <- serie[order(serie$data),]
  # identificar as datas que possuem valores diferentes de zero
  datas_maior <- serie[serie$valor_serie!=0,"data"]
  if(length(datas_maior)>0){
    # identificar a data mais recente
    data_rec <- max(datas_maior)
    # inicializar variavel para acumular os resultados
    valor_2 <- list()
    for(i in datas_maior){
      # gerar base com o valor desejado
      valor <- serie[serie$data>=i,]
      # zerar os valores a partir da data filtrada
      valor[valor$data>i,"valor_serie"] <- NA
      # atualizar o valor pelo IPCA
      valor <- aplic_taxa(valor[,c("data","valor_serie")], taxa)
      # fazer merge com o vetor de datas
      valor <- merge(data_tb, valor, by = "data", all.x = T)
      # preencher os NA's com zero
      valor[is.na(valor)] <- 0
      # renomear as colunas
      colnames(valor) <- c("data", paste("serie", i, sep = "_"))
      # guardar em uma lista
      valor_2[[as.character(i)]] <- valor
    }
    # gerar coluna com os valores corrigidos pela taxa
    valor_3 <- Reduce(function(x,y) merge(x, y, by = "data", all = T), valor_2)
    # somar as colunas
    valor_3$serie <- rowSums(valor_3[,-1,drop=F])
    # remover as colunas desnecessarias
    valor_3[,which(!colnames(valor_3)%in%c("data","serie"))] <- NULL
    # renomear as colunas
    colnames(valor_3) <- nm_col
    # retorna
    return(valor_3)
  } else {
    # renomear as colunas
    colnames(serie) <- nm_col
    # retorna
    return(serie)
  }
}
#_______________________________________________________________________________________________________________________
# funcao para calcular a taxa acumulada a partir de uma data ----
#_______________________________________________________________________________________________________________________
taxa_acum <- function(taxa, data_ini = NULL)
# INPUTS:
# taxa: objeto contendo apenas duas colunas, nesta ordem: data e taxa (os nomes das colunas nao afetam o calculo) - 
#       data.frame
# data_ini: data inicial para calculo da taxa acumulada. Ou seja, a taxa serah acumulada a apenas para datas posteriores
#           (nao inclui esta data) .Se nao informado, o calculo serah feito a partir da data mais antiga - vetor 
#           numerico
# OUTPUT:
# valor_acum: taxa acumulada - vetor numerico
{
  # renomear o arquivo
  colnames(taxa) <- c("data","valor")
  # ordenar por data
  taxa <- taxa[order(taxa$data),]
  # definir a data inicial
  if(is.null(data_ini)){
    data_ini <- min(taxa$data)
  }
  # zerar as datas anteriores aa data inicial
  taxa[taxa$data<data_ini,"valor"] <- 0
  # calcular o valor acumulado
  valor_acum <- cumprod(1 + taxa$valor) - 1
  # retorna apenas a taxa
  return(valor_acum)
}
#_______________________________________________________________________________________________________________________
# funcao para carregar series de fiscal do BCLink na periodicidade mensal ----
# a funcao extrai as series da planilha FinancasPublicas_FontesExternas.xlsx
#_______________________________________________________________________________________________________________________
carga_bclink_mes <- function(series, data_ini = NULL, data_fim = NULL, aba = "Série STN",
                             local_arq = "//home.intranet.bcb.gov.br@SSL/portal/depec/Pacotes/CONAP/Fiscal/FinancasPublicas_FontesExternas.xlsx")
# INPUT:
# series: codigo do BCLInk das series a serem extraidas. Exemplo: BCL.114096 - vetor character
# data_ini: data inicial da serie no formato "AAAA-MM-01" (colocar entre aspas). Se informado "NULL" serao extraidas
#           as series desde a data mais antiga existente na base - character
# data_fim: data final da serie no formato "AAAA-MM-01" (colocar entre aspas). Se informado "NULL" serao extraidas
#           as series ateh a data mais atual existente na base - character
# aba: aba da planilha do BCLink onde se encontram as series a serem extraidas - character
# local_arq: local onde se encontra o arquivo excel do BCLink - data.frame
# OUTPUT:
# base_2: series do BCLink, juntamente com as coluna de data (AAAA-MM-DD) e ano (AAAA) - data.frame
{
  # carregar a aba "Serie STN" da tabela que alimenta o BCLink
  base <- as.data.frame(read_excel(local_arq, sheet = aba))
  # filtrar apenas as series escolhidas
  base <- base[base$`Código BCL`%in%series,]
  # remover colunas desnecessarias
  base <- base[,-c(1,3:16)]
  # transpor de linhas para colunas
  base <- reshape2::dcast(reshape2::melt(base, id.vars = "Código BCL"), variable ~ `Código BCL`)
  # substituir o termo "n.d." (Nao disponivel) por NA 
  base_2 <- as.data.frame(apply(base, 2, function(x) gsub("n.d.|n.a.", NA, x)))
  # substituir a virgula pelo ponto como separador de decimal e converter para numero
  base_2 <- as.data.frame(apply(base_2, 2, function(x) as.numeric(gsub(",", ".", x))))
  # converter a data para formato AAAAMM
  base_2$variable <- as.Date(base_2$variable, origin = "1899-12-30")
  # alterar o nome da coluna "variable" para "data"
  colnames(base_2)[which(colnames(base_2)=="variable")] <- "data"
  # criar coluna de ano
  base_2$ano <- as.numeric(format(base_2$data, "%Y"))
  # filtrar o periodo
  if(!is.null(data_ini)){
    base_2 <- base_2[base_2$data>=data_ini,]
  }
  if(!is.null(data_fim)){
    base_2 <- base_2[base_2$data<=data_fim,]
  }
  # retorna os dados
  return(base_2)
}
#_______________________________________________________________________________________________________________________
# funcao para carregar series de fiscal do BCLink totalizadas por ano ----
# a funcao extrai as series da planilha FinancasPublicas_FontesExternas.xlsx
#_______________________________________________________________________________________________________________________
carga_bclink_ano <- function(series = NULL, data_ini = NULL, data_fim = NULL, aba = "Série STN",
                             local_arq = "//home.intranet.bcb.gov.br@SSL/portal/depec/Pacotes/CONAP/Fiscal/FinancasPublicas_FontesExternas.xlsx")
# INPUT:
# series: codigo do BCLInk das series a serem extraidas. Exemplo: "BCL.114096" (informar entre aspas). 
#         Se informado "NULL', serao extraidas todas as series - vetor character
# data_ini: ano inicial da serie no formato AAAA. Se informado "NULL" serao extraidas as series desde a data mais
#           antiga existente na base - numeric
# data_fim: ano final da serie no formato AAAA. Se informado "NULL" serao extraidas as series ateh a data mais recente
#           existente na base - numeric
# aba: aba da planilha do BCLink onde se encontram as series a serem extraidas - character
# local_arq: local onde se encontra o arquivo excel do BCLink - data.frame
# OUTPUT:
# base_2: series do BCLink, juntamente com as coluna de data (AAAA-MM-DD) e ano (AAAA) - data.frame
{
  # carregar a aba "Serie STN" da tabela que alimenta o BCLink
  base <- as.data.frame(read_excel(local_arq, sheet = aba))
  # filtrar apenas as series escolhidas
  if(!is.null(series)){
    base <- base[base$`Código BCL`%in%series,]
  }
  # remover colunas desnecessarias
  base <- base[,-c(1,3:16)]
  # transpor de linhas para colunas
  base <- reshape2::dcast(reshape2::melt(base, id.vars = "Código BCL"), variable ~ `Código BCL`)
  # substituir o termo "n.d." (Nao disponivel) por NA 
  base_2 <- as.data.frame(apply(base, 2, function(x) gsub("n.d.|n.a.", NA, x)))
  # substituir a virgula por ponto como separador de decimal e converter para numero
  base_2 <- as.data.frame(apply(base_2, 2, function(x) as.numeric(gsub(",", ".", x))))
  # converter a data para formato AAAAMM
  base_2$variable <- as.Date(base_2$variable, origin = "1899-12-30")
  # alterar o nome da coluna "variable" para "data"
  colnames(base_2)[which(colnames(base_2)=="variable")] <- "data"
  # atribuir zero para os valores de NA
  base_2[is.na(base_2)] <- 0
  # criar coluna de ano
  base_2$ano <- as.numeric(format(base_2$data, "%Y"))
  # eliminar a coluna de data
  base_2$data <- NULL
  # agrupar por ano
  base_2 <- aggregate(. ~ ano, data = base_2, FUN = "sum")
  # filtrar o periodo
  if(!is.null(data_ini)){
    base_2 <- base_2[base_2$ano>=data_ini,]
  }
  if(!is.null(data_fim)){
    base_2 <- base_2[base_2$ano<=data_fim,]
  }
  # retorna os dados
  return(base_2)
}
#_______________________________________________________________________________________________________________________
# funcao para corrigir os valores de uma serie pela taxa informada e projetar os valores que estao faltando ----
#_______________________________________________________________________________________________________________________
aplic_taxa_3 <- function(base, infl, tipo_proj_aje, ano_ini)
  # INPUTS:
  # base: objeto contendo apenas a data (na primeira coluna) e a serie a ser corrigida - data.frame
  # infl: objeto contendo apenas a data (na primeira coluna) e a taxa de inflacao - data.frame
  # tipo_proj_aje: indicador do tipo de correcao a ser realizada (vetor numerico):
  #                1 - preencher os valores que estao faltando com o ultimo valor disponivel corrigido pela taxa
  #                    projetada informada pelo usuario;
  #                2 - corrigir acumulando os valores e projetar os valores que estao faltando aplicando a taxa
  #                    no ultimo valor disponivel;
  #                3 - aplicar a taxa informada de forma cumulativa sem projetar os valores que estao faltando. Pode ser
  #                    utilizado o argumento "ano_ini" para informar o ano de inicio da correcao
  # ano_ini: ano de inicio da correcao, no formato AAAA. Se nao informado, os dados serao corrigidos a partir do ano
#          inicial - numeric
# OUTPUT:
# serie: serie corrigida pela taxa - vector
{
  # guardar o nome das colunas
  nm_colunas <- colnames(base)
  # renomear as colunas da base contendo a serie a ser ajustada
  colnames(base) <- c("data","serie")
  # renomear as colunas da base contendo a taxa a ser aplicada
  colnames(infl) <- c("data","taxa")
  # ordenar por data
  base <- base[order(base$data),]
  # indicar a linha de inicio da projecao
  vl_ini <- which(base$data==ano_ini)
  if(vl_ini==1){
    vl_ini <- vl_ini + 1
  }
  # projetar as variaveis
  if(tipo_proj_aje==1){
    # incluir o IPCA na base
    base <- merge(base, infl, by = "data", all.x = T)
    # realizar as projecoes
    for(i in c(vl_ini:nrow(base))){
      if(is.na(base[i,"serie"])|base[i,"serie"]==0){
        base[i,"serie"] <- base[i-1,"serie"] * (1 + base[i,"taxa"])
      }
    }
    # remover o ipca
    base$taxa <- NULL
  } else if(tipo_proj_aje==2){
    base <- aplic_taxa_2(base, infl)
  } else if(tipo_proj_aje==3){
    # incluir o IPCA na base
    base <- merge(base, infl, by = "data", all.x = T)
    # calcular a taxa acumulada
    base$taxa_acum <- taxa_acum(base[,c("data","taxa")], ano_ini)
    # aplicar na serie
    base$serie <- base$serie * (1 + base$taxa_acum)
    # remover as colunas desnecessarias
    base[,c("taxa","taxa_acum")] <- NULL
  }
  # transformar em serie de dados
  serie <- base[,"serie"]
  # retorna
  return(serie)
}
#_______________________________________________________________________________________________________________________
# funcao para extrair as projecoes anuais do focus para o ipca e inpc ----
#_______________________________________________________________________________________________________________________
inpc_ipca_focus_ano <- function(data_focus = NA)
# INPUT:
# data_focus: data a que se refere a pesquisa do focus, no formato 'AAAA-MM-DD' (colocar as aspas simples ao redor da
#             data). Caso nao seja informada, serao extraidas as informacoes do ultimo focus disponivel - character
# OUTPUT:
# base_2: base contendo as projecoes do inpc e do ipca - data.frame
{
  # extrair as projecoes do Focus
  if(is.na(data_focus)){
    i <- 0
    while(i <= 15){
      # tenta fazer o download
      base <- try(fromJSON("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=100&$filter=Indicador%20eq%20'INPC'%20or%20Indicador%20eq%20'IPCA'%20and%20baseCalculo%20eq%200&$orderby=Data%20desc&$format=json&$select=Indicador,Data,DataReferencia,Mediana"),
                  silent = T)
      i <- i + 1
      # se conseguiu baixar (nao gerou um try-error), para o loop
      if(!is(base, 'try-error')) break
      # mensagem que vai baixar de novo
      message(paste0("A tentativa de donwload nr. ", i, " falhou! Tentando novamente!"))
    }
  } else {
    i <- 0
    while(i <= 15){
      # tenta fazer o download
      base <- try(fromJSON(paste0("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=100&$filter=(Indicador%20eq%20'INPC'%20or%20Indicador%20eq%20'IPCA')%20and%20Data%20eq%20'",data_focus,"'%20and%20baseCalculo%20eq%200&$orderby=Data%20desc&$format=json&$select=Indicador,Data,DataReferencia,Mediana")),
                  silent = T)
      i <- i + 1
      # se conseguiu baixar (nao gerou um try-error), para o loop
      if(!is(base, 'try-error')) break
      # mensagem que vai baixar de novo
      message(paste0("A tentativa de donwload nr. ", i, " falhou! Tentando novamente!"))
    }
  }
  # se não foi possivel baixar, apresenta mensagem de erro
  if(is(base, 'try-error')){
    stop("Não foi possível baixar o arquivo do IPCA/INPC do Focus com projecoes anuais!")
  }
  # gerar data.frame
  base <- lapply(base$value, function(x) do.call("cbind", x))
  base <- as.data.frame(do.call("rbind",base))
  # converter os valores das projecoes para campo numerico e dividir por 100
  base$Mediana <- as.numeric(base$Mediana) / 100
  # funcao para filtrar a base pela maior data para cada indicador
  extr_data <-  function(x){
    x <- x[x$Data==max(x$Data),]
    return(x)
  }
  # filtrar os indicadores por data
  base_2 <- lapply(split(base, f= base$Indicador), extr_data)
  # unir a lista em um data.frame
  base_2 <- as.data.frame(do.call("rbind", base_2))
  # transformar para o formato "wide" (colocar o nome das variaveis nas colunas)
  base_2 <- reshape2::dcast(base_2, DataReferencia ~ Indicador, value.var = "Mediana", fun.aggregate = sum)
  # retorna
  return(base_2)
}
#_______________________________________________________________________________________________________________________
# funcao para extrair as projecoes mensais do focus para o ipca e inpc ----
#_______________________________________________________________________________________________________________________
inpc_ipca_focus_mes <- function(data_focus = NA)
# INPUT:
# data_focus: data a que se refere a pesquisa do focus, no formato 'AAAA-MM-DD' (colocar as aspas simples ao redor da
#             data). Caso nao seja informada, serao extraidas as informacoes do ultimo focus disponivel - character
# OUTPUT:
# base_2: base contendo as projecoes do inpc e do ipca - data.frame
{
  # extrair as projecoes do Focus
  if(is.na(data_focus)){
    i <- 0
    while(i <= 15){
      # tenta fazer o download
      base <- try(fromJSON("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativaMercadoMensais?$top=100&$filter=(Indicador%20eq%20'IPCA'%20or%20Indicador%20eq%20'INPC')%20and%20baseCalculo%20eq%200&$orderby=Data%20%20desc&$format=json&$select=Indicador,Data,DataReferencia,Mediana"),
                  silent = T)
      i <- i + 1
      # se conseguiu baixar (nao gerou um try-error), para o loop
      if(!is(base, 'try-error')) break
      # mensagem que vai baixar de novo
      message(paste0("A tentativa de donwload nr. ", i, " falhou! Tentando novamente!"))
    }
  } else {
    i <- 0
    while(i <= 15){
      # tenta fazer o download
      base <- try(fromJSON(paste0("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativaMercadoMensais?$top=100&$filter=(Indicador%20eq%20'IPCA'%20or%20Indicador%20eq%20'INPC')%20and%20baseCalculo%20eq%200%20and%20Data%20eq%20'",data_focus,"'&$orderby=Data%20%20desc&$format=json&$select=Indicador,Data,DataReferencia,Mediana")),
                  silent = T)
      i <- i + 1
      # se conseguiu baixar (nao gerou um try-error), para o loop
      if(!is(base, 'try-error')) break
      # mensagem que vai baixar de novo
      message(paste0("A tentativa de donwload nr. ", i, " falhou! Tentando novamente!"))
    }
  }
  # se não foi possivel baixar, apresenta mensagem de erro
  if(is(base, 'try-error')){
    stop("Não foi possível baixar o arquivo do IPCA/INPC do Focus com projecoes mensais!")
  }
  # gerar data.frame
  base <- lapply(base$value, function(x) do.call("cbind", x))
  base <- as.data.frame(do.call("rbind",base))
  # converter os valores das projecoes para campo numerico e dividir por 100
  base$Mediana <- as.numeric(base$Mediana) / 100
  # funcao para filtrar a base pela maior data para cada indicador
  extr_data <-  function(x){
    x <- x[x$Data==max(x$Data),]
    return(x)
  }
  # filtrar os indicadores por data
  base_2 <- lapply(split(base, f= base$Indicador), extr_data)
  # unir a lista em um data.frame
  base_2 <- as.data.frame(do.call("rbind", base_2))
  # transformar para o formato "wide" (colocar o nome das variaveis nas colunas)
  base_2 <- reshape2::dcast(base_2, DataReferencia ~ Indicador, value.var = "Mediana", fun.aggregate = sum)
  # alterar a coluna de data para o formato 'AAAAMM"
  base_2$DataReferencia <- as.numeric(paste0(substr(base_2$DataReferencia, 4, 8), substr(base_2$DataReferencia, 1, 2)))
  # ordenar por data
  base_2 <- base_2[order(base_2$DataReferencia),]
  # retorna
  return(base_2)
}
#_______________________________________________________________________________________________________________________
# funcao para extrair as projecoes do focus para o pib ----
#_______________________________________________________________________________________________________________________
pib_focus <- function(data_focus = NA)
# INPUT:
# data_focus: data a que se refere a pesquisa do focus, no formato 'AAAA-MM-DD' (colocar as aspas simples ao redor da
#             data). Caso nao seja informada, serao extraidas as informacoes do ultimo focus disponivel - character
# OUTPUT:
# base_2: base contendo as projecoes do pib - data.frame
{
  # extrair as projecoes do Focus
  if(is.na(data_focus)){
    i <- 0
    while(i <= 15){
      # tenta fazer o download
      base <- try(fromJSON("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=100&$filter=Indicador%20eq%20'PIB Total'%20and%20baseCalculo%20eq%200&$orderby=Data%20desc&$format=json&$select=Indicador,Data,DataReferencia,Mediana"),
                  silent = T)
      i <- i + 1
      # se conseguiu baixar (nao gerou um try-error), para o loop
      if(!is(base, 'try-error')) break
      # mensagem que vai baixar de novo
      message(paste0("A tentativa de donwload nr. ", i, " falhou! Tentando novamente!"))
    }
  } else {
    i <- 0
    while(i <= 15){
      # tenta fazer o download
      base <- try(fromJSON(paste0("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=100&$filter=Indicador%20eq%20'PIB%20Total'%20and%20Data%20eq%20'",data_focus,"'&$format=json&$select=Indicador,Data,DataReferencia,Mediana")),
                  silent = T)
      i <- i + 1
      # se conseguiu baixar (nao gerou um try-error), para o loop
      if(!is(base, 'try-error')) break
      # mensagem que vai baixar de novo
      message(paste0("A tentativa de donwload nr. ", i, " falhou! Tentando novamente!"))
    }
  }
  # se não foi possivel baixar, apresenta mensagem de erro
  if(is(base, 'try-error')){
    stop("Não foi possível baixar o arquivo do PIB do Focus!")
  }
  # gerar data.frame
  base <- lapply(base$value, function(x) do.call("cbind", x))
  base <- as.data.frame(do.call("rbind",base))
  # converter os valores das projecoes para campo numerico e dividir por 100
  base$Mediana <- as.numeric(base$Mediana) / 100
  # filtrar a data mais recente que foram realizadas as projecoes do Focus
  base <- base[base$Data==max(as.Date(base$Data)),]
  # transformar para o formato "wide" (colocar o nome das variaveis nas colunas)
  base_2 <- reshape2::dcast(base, DataReferencia ~ Indicador, value.var = "Mediana", fun.aggregate = sum)
  # retorna
  return(base_2)
}
#_______________________________________________________________________________________________________________________
# funcao para extrair projecoes do Focus referentes ao PIB, Divida Bruta (DBGG) e Divida Liquida (DLSP) ----
# 
#_______________________________________________________________________________________________________________________
geral_focus <- function(data_focus = NA, pib = 0, divida_bruta = 0, divida_liquida = 0)
# INPUT:
# data_focus: data a que se refere a pesquisa do focus, no formato 'AAAA-MM-DD' (colocar as aspas simples ao redor da
#             data). Caso nao seja informada, serao extraidas as informacoes do ultimo focus disponivel - character
# pib: informe 1 caso deseje extrair a projecao de PIB do Focus - numeric 
# divida_bruta: informe 1 caso deseje extrair a projecao de DBGG do Focus - numeric
# divida_liquida: informe 1 caso deseje extrair a projecao de DLSP do Focus - numeric
# OUTPUT:
# base_2: base contendo as projecoes do pib - data.frame
{
  # identificar a variavel que serah baixa
  if(pib == 1){
    var_baix <- "PIB Total"
    nr_reg <- 5
  } else if(divida_bruta == 1){
    var_baix <- "D%C3%ADvida%20bruta%20do%20governo%20geral"
    nr_reg <- 10
  } else if(divida_liquida == 1){
    var_baix <- "D%C3%ADvida%20l%C3%ADquida%20do%20setor%20p%C3%BAblico"
    nr_reg <- 10
  } else if(sum(pib,divida_bruta,divida_liquida) == 0){
    stop("Escolha um indicador!")
  } else if(sum(pib,divida_bruta,divida_liquida) > 1){
    stop("Escolha apenas um indicador!")
  }
  # extrair as projecoes do Focus
  if(is.na(data_focus)){
    i <- 0
    while(i <= 15){
      # tenta fazer o download
      base <- try(fromJSON(paste0("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=",nr_reg,"&$filter=Indicador%20eq%20'",var_baix,"'%20and%20baseCalculo%20eq%200&$orderby=Data%20desc&$format=json&$select=Indicador,Data,DataReferencia,Mediana")),
                  silent = T)
      i <- i + 1
      # se conseguiu baixar (nao gerou um try-error), para o loop
      if(!is(base, 'try-error')) break
      # mensagem que vai baixar de novo
      message(paste0("A tentativa de donwload nr. ", i, " falhou! Tentando novamente!"))
    }
  } else {
    i <- 0
    while(i <= 15){
      # tenta fazer o download
      base <- try(fromJSON(paste0("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=",nr_reg,"&$filter=Indicador%20eq%20'",var_baix,"'%20and%20baseCalculo%20eq%200%20and%20Data%20eq%20'",data_focus,"'&$orderby=Data%20desc&$format=json&$select=Indicador,Data,DataReferencia,Mediana")),
                  silent = T)
      i <- i + 1
      # se conseguiu baixar (nao gerou um try-error), para o loop
      if(!is(base, 'try-error')) break
      # mensagem que vai baixar de novo
      message(paste0("A tentativa de donwload nr. ", i, " falhou! Tentando novamente!"))
    }
  }
  # se não foi possivel baixar, apresenta mensagem de erro
  if(is(base, 'try-error')){
    stop("Não foi possível baixar o arquivo do PIB do Focus!")
  }
  # gerar data.frame
  base <- lapply(base$value, function(x) do.call("cbind", x))
  base <- as.data.frame(do.call("rbind",base))
  # substituir virgula por ponto
  base$Mediana <- gsub(",",".",base$Mediana)
  # converter em numerico
  base$Mediana <- as.numeric(base$Mediana)
  # converter os valores das projecoes para campo numerico e dividir por 100
  if(pib == 1){
    base$Mediana <- base$Mediana / 100
  }
  # filtrar a data mais recente que foram realizadas as projecoes do Focus
  base <- base[base$Data==max(as.Date(base$Data)),]
  # transformar para o formato "wide" (colocar o nome das variaveis nas colunas)
  base_2 <- reshape2::dcast(base, Data + DataReferencia ~ Indicador, value.var = "Mediana", fun.aggregate = sum)
  # retorna
  return(base_2)
}
#_______________________________________________________________________________________________________________________
# funcao para extrair as tabelas de mortalidade de homens e mulheres do IBGE ----
#_______________________________________________________________________________________________________________________
mort_ibge <- function(ano_mort)
# INPUT:
# ano_mort: ano da tabela de mortalidade que se deseja extrair. Se nao houver o ano informado, serah extraida a mais
#           recente disponivel - numeric
# OUTPUT:
# mort_ambos: tabela contendo a mortalidade de homens e mulheres por idade - data.frame
{
  # criar um nome para o arquivo que serah baixado e salvo no computador
  destino_file <- tempfile(tmpdir = tempdir(check = TRUE))
  # identifica o ano mais recente para o qual ha tabela de mortalidade para ambos os sexos
  i <- 0
  while(!file.exists(paste0(destino_file,".zip"))){
    if(i == 10){
      stop("Erro ao baixar a tabua de mortalidade!!!")
    } else {
      # ano para o qual se deve pesquisar se existe tabela de mortalidade
      ano_pesq <- ano_mort - i
      # definir a url para download
      url1 <- paste0("https://biblioteca.ibge.gov.br/visualizacao/periodicos/3097/tcmb_",ano_mort-i,"_tabelas.zip")
      # tentar baixar o arquivo zip contendo as tabelas de homens e mulheres
      suppressWarnings(tryCatch(download.file(url1, paste0(destino_file,".zip"), method="auto", mode = "wb", quiet = T),
               error=function(e) 1
      ))
      # atualizar o contador
      i <- i + 1
    }
  }
  # extrair o conteudo do zip
  down1 <- unzip(paste0(destino_file,".zip"), exdir = destino_file)
#_______________________________________________________________________________________________________________________
# carregar a tabela contendo a mortalidade dos homens
#_______________________________________________________________________________________________________________________
  # carregar a tabela de mortalidade dos homens
  mort_h <- suppressMessages(as.data.frame(read_excel(paste0(destino_file,"\\homens.xlsx"))))
  # filtrar apenas os anos
  mort_h <- mort_h[mort_h[,1]%in%c(0:120),c(1,2),drop=F]
  # renomear as colunas
  colnames(mort_h) <- c("idade", "tx_mort_h")
#_______________________________________________________________________________________________________________________
# carregar a tabela contendo a mortalidade das mulheres
#_______________________________________________________________________________________________________________________ 
  # mort_m <- suppressMessages(as.data.frame(read_excel(paste0(list.dirs(destino_file)[2],"\\mulheres\\",arq_mulheres))))
  mort_m <- suppressMessages(as.data.frame(read_excel(paste0(destino_file,"\\mulheres.xlsx"))))
  # filtrar apenas os anos
  mort_m <- mort_m[mort_m[,1]%in%c(0:120),c(1,2),drop=F]
  # renomear as colunas
  colnames(mort_m) <- c("idade", "tx_mort_m")
#_______________________________________________________________________________________________________________________
# unir as bases
#_______________________________________________________________________________________________________________________ 
  # unir as bases
  mort_ambos <- merge(mort_h, mort_m, by = "idade", all = T)
  # incluir coluna de ano
  mort_ambos$ano <- ano_pesq
  # converter as colunas para numero
  mort_ambos <- as.data.frame(apply(mort_ambos, 2, function(x) as.numeric(x)))
  # ordenar por idade
  mort_ambos <- mort_ambos[order(mort_ambos$idade),]
  # apagar o arquivo zip baixado
  apaga_arq <- file.remove(paste0(destino_file,".zip"))
  # apagar o diretorio contendo os arquivos que foram extraidos
  apaga_dir <- unlink(destino_file, recursive = T)
  # retorna
  return(mort_ambos)
}
#_______________________________________________________________________________________________________________________
# funcao para extrair as projecoes de resultado primario do focus ----
#_______________________________________________________________________________________________________________________
res_prim <- function(data_focus = NA)
# INPUT:
# data_focus: data a que se refere a pesquisa do focus, no formato 'AAAA-MM-DD' (colocar as aspas simples ao redor da
#             data). Caso nao seja informada, serao extraidas as informacoes do ultimo focus disponivel - character
# OUTPUT:
# base_2: base contendo as projecoes do pib - data.frame
{
  # extrair as projecoes do Focus
  if(is.na(data_focus)){
    i <- 0
    while(i <= 15){
      # tenta fazer o download
      base <- try(fromJSON("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=100&$filter=IndicadorDetalhe%20eq%20'Resultado%20Prim%C3%A1rio'&$orderby=Data%20desc&$format=json&$select=Indicador,IndicadorDetalhe,Data,DataReferencia,Mediana"),
                  silent = T)
      i <- i + 1
      # se conseguiu baixar (nao gerou um try-error), para o loop
      if(!is(base, 'try-error')) break
      # mensagem que vai baixar de novo
      message(paste0("A tentativa de donwload nr. ", i, " falhou! Tentando novamente!"))
    }
  } else {
    i <- 0
    while(i <= 15){
      # carregar as projecoes Focus do resultado primario para a data informada
      base <- try(fromJSON(paste0("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=100&$filter=IndicadorDetalhe%20eq%20'Resultado%20Prim%C3%A1rio'%20and%20Data%20eq%20'",data_focus,"'&$orderby=Data%20desc&$format=json&$select=Indicador,IndicadorDetalhe,Data,DataReferencia,Mediana")),
                  silent = T)
      i <- i + 1
      # se conseguiu baixar (nao gerou um try-error), para o loop
      if(!is(base, 'try-error')) break
      # mensagem que vai baixar de novo
      message(paste0("A tentativa de donwload nr. ", i, " falhou! Tentando novamente!"))
    }
  }
  # se não foi possivel baixar, apresenta mensagem de erro
  if(is(base, 'try-error')){
    stop("Não foi possível baixar o arquivo do resultado primario do Focus!")
  }
  # gerar data.frame
  base <- lapply(base$value, function(x) do.call("cbind", x))
  base <- as.data.frame(do.call("rbind",base))
  # substituir a vírgula por ponto
  base$Mediana <- gsub(",",".",base$Mediana)
  # converter os valores das projecoes para campo numerico e dividir por 100
  base$Mediana <- as.numeric(base$Mediana) / 100
  # filtrar a data mais recente que foram realizadas as projecoes do Focus
  base <- base[base$Data==max(as.Date(base$Data)),]
  # transformar para o formato "wide" (colocar o nome das variaveis nas colunas)
  base_2 <- reshape2::dcast(base, DataReferencia ~ Indicador, value.var = "Mediana", fun.aggregate = sum)
  # retorna
  return(base_2)
}
#_______________________________________________________________________________________________________________________
# funcao para extrair series do SGS ----
#_______________________________________________________________________________________________________________________
extr_sgs <- function(codigo_serie, dataInicial = NULL, dataFinal = NULL)
# INPUT:
# codigo_serie: codigo da serie do SGS - numeric
# dataInicial: data de inicio da serie, no formato DD/MM/AAAA (exemplo: "01/01/2014"). Este campo soh terah efeito 
#              se informada tambem o argumento "dataFinal". Se estiver NULL, serah extraida a serie desde o inicio - 
#              character
# dataFinal: data final da serie, no formato DD/MM/AAAA (exemplo: "01/01/2014"). Este campo soh terah efeito 
#            se informada tambem o argumento "dataInicial" Se NULL, a serie sera extraida ateh a data mais recente 
#            disponivel - character
# OUTPUT:
# base: base contendo a serie do SGS no periodo informado - data.frame
{
  # criar lista vazia para armazenar os valores
  valores <- list()
  # extrair as informacoes do SGS
  if(is.null(dataFinal)|is.null(dataInicial)){
    i <- 0
    for(j in codigo_serie){
      while(i <= 15){
        # tenta fazer o download
        base <- try(fromJSON(paste0("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",j,"/dados?formato=json")),
                    silent = T)
        # armazenar o resultado do download na lista
        valores[[as.character(j)]] <- base
        # atualizar o contador
        i <- i + 1
        # se conseguiu baixar (nao gerou um try-error), para o loop
        if(!is(base, 'try-error')) break
        # mensagem que vai baixar de novo
        message(paste0("A tentativa de donwload nr. ", i, " falhou! Tentando novamente!"))
      }
    }
  } else {
    i <- 0
    for(j in codigo_serie){
      while(i <= 15){
        # tenta fazer o download
        base <- try(fromJSON(paste0("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",j,"/dados?formato=json&dataInicial=",dataInicial,"&dataFinal=",dataFinal)),
                    silent = T)
        # armazenar o resultado do download na lista
        valores[[as.character(j)]] <- base
        # atualizar o contador
        i <- i + 1
        # se a tentativa de download não gerar um 'try-error' (ou seja, conseguiu baixar), entao para o loop
        if(!is(base, 'try-error')) break
        # mensagem que vai tentar baixar de novo
        message(paste0("A tentativa de donwload nr. ", i, " falhou! Tentando novamente!"))
      }
    }
  }
  # se não foi possivel baixar, apresenta mensagem de erro
  if(is(base, 'try-error')){
    stop("Não foi possível baixar a serie desejada!")
  }
  # funcao para formatar os valores dentro da lista principal
  formata_lista <- function(x, valores){
    w <- as.data.frame(do.call("rbind",valores[[x]]))
    w[,"valor"] <- as.numeric(w[,"valor"])
    colnames(w) <- c("data",paste0("serie_",x))
    return(w)
  }
  # aplicar a funcao anterior dentro da lista principal
  auxiliar <- lapply(names(valores), formata_lista, valores = valores)
  # transformar a lista principal em data.frame
  base <- Reduce(function(x, y) merge(x, y, by = "data", all = T), auxiliar)
  # transformar o formato da coluna para data
  base$data <- as.Date(base$data, "%d/%m/%Y")
  # ordenar por data
  base <- base[order(base$data),]
  # retorna
  return(base)
}
#_______________________________________________________________________________________________________________________
# funcao para carregar os eventos nao recorrentes de receita, despesa e transferencias para projecao da receita ----
# essa funcao agrega os valores por mes/ano e cria colunas separadas para rec., transf. e despesas nao recorrentes
#_______________________________________________________________________________________________________________________
fn_event_nao_recor <- function(tabela)
# INPUT:
# tabela: tabela detalhada contendo os eventos nao recorrentes - data.frame
# OUTPUT:
# event_nao_recor_3: tabela contendo as colunas de data, receitas nao recorrentes, despesas nao recorrentes e 
#                    transferencias nao recorrentes - data.frame
{
  # totalizar por data e tipo
  event_nao_recor_2 <- tabela %>% group_by(data, tipo, classif) %>% 
    summarise(valor = sum(valor), .groups = "keep")
  # criar a tabela apenas com a classificacao 1
  event_class1 <- tabela_classif(event_nao_recor_2, 1, "_nao_recor")
  # verificar se existe a classificacao 2
  if(2%in%unique(event_nao_recor_2$classif)){
    # criar a tabela apenas com a classificacao 2
    event_class2 <- tabela_classif(event_nao_recor_2, 2, "_recor")
    # unir as tabelas
    event_nao_recor_3 <- merge(event_class1, event_class2, by = "data", all = T)
  } else {
    event_nao_recor_3 <- event_class1
  }
  # preencher os NA's com zero
  event_nao_recor_3[is.na(event_nao_recor_3)] <- 0
  # retorna
  return(event_nao_recor_3)
}
#_______________________________________________________________________________________________________________________
# funcao para carregar os eventos nao recorrentes de receita, despesa e transferencias para envio ao DEPEP ----
# essa funcao agrega os valores por mes/ano e cria colunas separadas para rec., transf. e despesas nao recorrentes
#_______________________________________________________________________________________________________________________
fn_event_nao_recor_2 <- function(tabela)
# INPUT:
# tabela: tabela detalhada contendo os eventos nao recorrentes - data.frame
# OUTPUT:
# event_nao_recor_3: tabela contendo as colunas de data, receitas nao recorrentes, despesas nao recorrentes e 
#                    transferencias nao recorrentes - data.frame
{
  # filtrar pelos eventos do Depep
  event_nao_recor <- tabela[tabela$filtro_depep==1,]
  # totalizar por data e tipo
  event_nao_recor_2 <- event_nao_recor %>% group_by(data, tipo) %>% 
    summarise(valor = sum(valor), .groups = "keep")
  # criar as colunas de receita, despesas e transferencias nao recorrentes
  event_nao_recor_3 <- reshape2::dcast(reshape2::melt(as.data.frame(event_nao_recor_2), id.vars = c("data","tipo")), 
                                       data ~ tipo)
  # renomear as colunas
  colnames(event_nao_recor_3)[which(colnames(event_nao_recor_3)%in%c("D","R","T"))] <- 
    paste0(c("desp_nao_recor_depep","rec_nao_recor_depep","transf_nao_recor_depep"))
  # formatar a coluna de data para Date
  event_nao_recor_3$data <- as.Date(as.character(event_nao_recor_3$data), "%Y-%m-%d", time)
  # colocar zero no lugar dos NA's
  event_nao_recor_3[is.na(event_nao_recor_3)] <- 0
  # retorna
  return(event_nao_recor_3)
}
#_______________________________________________________________________________________________________________________
# funcao para carregar as projecoes dos condicionantes da conap ----
#_______________________________________________________________________________________________________________________
fn_cond_conap <- function(plan_var_econ, intervalo, colunas = NULL)
# INPUTS:
# plan_var_econ: nome do diretorio e da planilha contendo as projecoes dos condicionantes da conap - character
# intervalo: intervalo de linhas e colunas que se deseja extrair da planilha de condicionantes da conap, sendo que
#            a primeira coluna deve ser a data e as demais devem ser valores. Alem disso, a primeira linha do intervalo
#            nao deve conter valores, pois o R a utilizarah como nome para as colunas e, assim, estes valores serao 
#            perdidos. Exemplo: C445:CB481" - character
# colunas: nomes das colunas a serem extraidas, sendo que a coluna de "data_base" eh obrigatoria. Se informado NUUL,
#          todas as colunas utilizadas pela Copof serao extraidas. As opcoes sao: "IPCA","IPCA_livres","INPC","ibc_br",
#          "tx_deso","nuci","PIB","cambio_medio","PIB_nominal" - character
# OUTPUT:
# proj: tabela contendo a data os valores observados e projetados dos condicionantes da conap que sao utilizados no
#       fiscal - data.frame
{
  # carregar a tabela de projecoes anuais
  proj <- suppressMessages(as.data.frame(read_excel(plan_var_econ, sheet = "resumo_valor", range = intervalo)))
  # filtrar apenas as colunas de interesse
  proj <- proj[,c(1,2,3,5,11,20,21,22,32,37,38,39,41,78)]
  # atribuir nomes colunas
  colnames(proj) <-  c("data_base","IPCA","IPCA_livres","INPC","ibc_br","tx_deso","nuci","PIB","PIB_aj_sazonal",
                       "cambio_medio","cambio_fim","selic","TJLP","PIB_nominal")
  # transformar as colunas em numero, exceto a coluna de data
  proj[,-1] <- as.data.frame(apply(proj[,-1], 2, as.numeric))
  # dividir os valores por 100
  proj[,c("IPCA","IPCA_livres","INPC","tx_deso","PIB","selic","TJLP")] <-
    proj[,c("IPCA","IPCA_livres","INPC","tx_deso","PIB","selic","TJLP")] / 100
  # converter a coluna de data para o formato "date" se ela estiver no formato POSIXct
  if(class(proj[,1])[1]=="POSIXct"){
    proj[,1] <- as.Date(as.character(proj[,1]), "%Y-%m-%d")
  }
  # filtrar por colunas, se necessario
  if(!is.null(colunas)){
    proj <- proj[,c("data_base",colunas)]
  }
  # ordenar por data
  proj <- proj[order(proj$data_base),]
  # retorna
  return(proj)
}
#_______________________________________________________________________________________________________________________
# funcao para carregar a data de atualizacao dos condicionantes da conap ----
#_______________________________________________________________________________________________________________________
fn_data_atual_cond_conap <- function(plan_var_econ, intervalo = "D2:D2")
# INPUTS:
# plan_var_econ: nome do diretorio e da planilha contendo as projecoes dos condicionantes da conap - character
# intervalo: intervalo de linhas e colunas que se deseja extrair da planilha de condicionantes da conap, sendo que
#            a primeira coluna deve ser a data e as demais devem ser valores. Alem disso, a primeira linha do intervalo
#            nao deve conter valores, pois o R a utilizarah como nome para as colunas e, assim, estes valores serao 
#            perdidos. Exemplo: C445:CB481" - character
# OUTPUT:
# data: data de atualizacao dos condicionantes da Conap - date
{
  # carregar a tabela de projecoes anuais
  data <- suppressMessages(as.data.frame(read_excel(plan_var_econ, sheet = "resumo_valor", range = intervalo, 
                                                    col_names = F)))
  # transformar em data
  data <- as.Date(as.character(data[1,1]), "%Y-%m-%d")
  # retorna
  return(data)
}
#_______________________________________________________________________________________________________________________
# funcao para arredondar um numero sempre para cima ----
#_______________________________________________________________________________________________________________________
fn_arred_cima <- function(x, nr_dig = 1)
# INPUTS:
# x: numero a ser arredondado (nao aceita vetor) - numeric
# nr_dig: numero de digitos a serem arredondados - integer
# OUTPUT:
# d: numero arredondado - numeric  
{
  # funcao para calcular a quantidade de casas decimais
  decimalplaces <- function(x) {
    if (abs(x - round(x)) > .Machine$double.eps^0.5) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
      # nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]])
    } else {
      return(0)
    }
  }
  # alterar as opcoes do R para que ele nao transforme o numero em notacao cientifica
  options(scipen = 999)
  # criar um vetor vazio
  d <- NULL
  for(i in c(1:length(x))){
    # identifica a quantidade de casas decimais
    number_digits <- decimalplaces(x[i])
    # arredonda o numero
    w <- round(x[i], nr_dig)
    # seleciona apenas a parte inteira do numero
    a <- trunc(x[i])
    
    # realiza o arredondamento para cima
    if(w<x[i]&number_digits>0){
      b <- paste0("0.",paste(rep(0,nr_dig-1),collapse = ""), 1)
      d <- c(d, w+as.numeric(b))
    } else {
      d <- c(d, w)
    }
  }
  # retorna o numero
  return(d)
  # retorna a opcao para o R gerar numeros cientificos
  options(scipen = 0)
}
#_______________________________________________________________________________________________________________________
# funcao para arredondar um numero sempre para baixo ----
#_______________________________________________________________________________________________________________________
fn_arred_baixo <- function(x, nr_dig = 1)
# INPUTS:
# x: numero a ser arredondado (nao aceita vetor) - numeric
# nr_dig: numero de digitos a serem arredondados - integer
# OUTPUT:
# d: numero arredondado - numeric  
{
  # funcao para calcular a quantidade de casas decimais
  decimalplaces <- function(x) {
    if (abs(x - round(x)) > .Machine$double.eps^0.5) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
      # nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]])
    } else {
      return(0)
    }
  }
  # alterar as opcoes do R para que ele nao transforme o numero em notacao cientifica
  options(scipen = 999)
  # criar um vetor vazio
  d <- NULL
  for(i in c(1:length(x))){
    # identifica a quantidade de casas decimais
    number_digits <- decimalplaces(x[i])
    # arredonda o numero
    w <- round(x[i], nr_dig)
    # seleciona apenas a parte inteira do numero
    a <- trunc(x[i])
    # realiza o arredondamento para cima
    if(w>x[i]&number_digits>0){
      b <- paste0("0.",paste(rep(0,nr_dig-1),collapse = ""), 1)
      d <- c(d, w-as.numeric(b))
    } else {
      d <- c(d, w)
    }
  }
  # retorna o numero
  return(d)
  # retorna a opcao para o R gerar numeros cientificos
  options(scipen = 0)
}
#_______________________________________________________________________________________________________________________
# funcao para arredondar um numero sempre para baixo ----
#_______________________________________________________________________________________________________________________
# funcao para gerar tabelas separadas para cada classificacao
tabela_classif <- function(tab_eventos, classif, prefixo)
# INPUTS:
# tab_eventos: tabela contendo os eventos recorrentes e nao recorrentes - data.frame
# classif: codigo de classificacao dos eventos para o qual se deseja gerar uma tabela - numeric
# prefixo: prefixo a ser atribuido aos nomes das colunas - character
# OUTPUT:
# tab_eventos_3: tabela contendo apenas os eventos com a classificacao indicada - numeric
{
  # manter apenas os eventos com classificacao igual a 1
  tab_eventos_2 <- tab_eventos[tab_eventos$classif==classif,]
  # excluir a coluna "classif"
  tab_eventos_2$classif <- NULL
  # criar as colunas de receita, despesas e transferencias nao recorrentes
  tab_eventos_3 <- reshape2::dcast(reshape2::melt(as.data.frame(tab_eventos_2), id.vars = c("data","tipo")), 
                                       data ~ tipo)
  # renomear as colunas
  colnames(tab_eventos_3)[which(colnames(tab_eventos_3)%in%c("D","R","T"))] <- 
    paste0(c("ev_desp","ev_rec","ev_transf"),prefixo)
  # formatar a coluna de data para Date
  tab_eventos_3$data <- as.Date(as.character(tab_eventos_3$data), "%Y-%m-%d", time)
  # colocar zero no lugar dos NA's
  tab_eventos_3[is.na(tab_eventos_3)] <- 0
  # retorna
  return(tab_eventos_3)
}
#_______________________________________________________________________________________________________________________
#
#_______________________________________________________________________________________________________________________