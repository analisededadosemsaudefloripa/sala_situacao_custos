options(encoding = "UTF-8")

########################################################################################### 
#pacotes 
###########################################################################################
library(readr)
library(tidyverse)
library(reshape2)


########################################################################################### 
#Banco de dados 
###########################################################################################
dados <- read_csv("bases/apuracao_planilha_base.csv")
dados_cs <- read_csv("bases/dados_cs.csv")


########################################################################################### 
#Retirando caracteres especiais 
###########################################################################################


rm_accent <- function(str,pattern="all") {
  # Rotinas e funções úteis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
  if(!is.character(str))
    str <- as.character(str)

  pattern <- unique(pattern)

  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"

  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )

  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )

  accentTypes <- c("´","`","^","~","¨","ç")

  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))

  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)

  return(str)
}

dados$TIPO <- rm_accent(dados$TIPO) 
dados$UNIDADE <- rm_accent(dados$UNIDADE)



########################################################################################### 
#Fazendo merge para dados dos distritos 
###########################################################################################


dados_cs <- dados_cs[,c(2,3)] %>%unique()


########################################################################################### 
#Ajuste no Banco de dados 
###########################################################################################
#'Os dados deverão ser estruturados hierarquicamente, de forma a permitir o drill down por:
#'Tipo de Unidade
#'Tipo de Custo
#'Tipo de Classificação
#'
#'
#'As unidades foram organizadas da seguinte forma
#'Tipo de Unidade (UNI_0, UNI_1, UNI_2, UNI_3, UNI_4)
#'UNI_0: Florianópolis
#'UNI_1: Atenção em Saúde
#'UNI_2: Atenção Primária
#'UNI_3: Distritos
#'UNI_4: Sede do Distrito
#'UNI_5: Sede dos Distrios Individuais
#'UNI_4: Centros de Saúde
#'UNI_5: Centros de Saúde Individuais
#'UNI_3: NASF
#'UNI_2: Unidades externas
#'UNI_2: Farmácia escola
#'UNI_2: Atenção Especializada
#'UNI_3: UPAs
#'UNI_4: UPAS Individuais
#'UNI_3: Policlínicas
#'UNI_4: Policlínicas Individuais
#'UNI_3: CEOS
#'UNI_4: CEOS Individuais 
#'UNI_3: CAPS
#'UNI_4: CAPS Individuais
#'UNI_3: SAMU
#'UNI_1: Vigilância em Saúde
#'UNI_2: Vigilância Epidemiológica
#'UNI_2: Vigilância Sanitária
#'UNI_2: CCZ
#'UNI_2: LAMUF
#'UNI_1: Nível Central
#'UNI_2: Almoxarifado
#'UNI_2: SMS
#'UNI_1: Conselho Municipal de Saúde
#'UNI_1: DIBEA


#'Ajustando UNI_0

dados$UNI_0 <- "Florianopolis" 
 
#'Ajustando UNI_1
dados$UNI_1 <- NA
for(i in 1:nrow(dados)){
   if((dados$TIPO[i] == "CAPS")   | (dados$TIPO[i] == "CEO")             | (dados$TIPO[i] == "CS")
      | (dados$TIPO[i] == "DS")   | (dados$TIPO[i] == "FARMACIA ESCOLA") | (dados$TIPO[i] == "NASF") 
      | (dados$TIPO[i] == "POLI") | (dados$TIPO[i] == "SAMU")            | (dados$TIPO[i] == "HOSPITAIS") 
      | (dados$TIPO[i] == "UPA")){
      dados$UNI_1[i] <- "Atencao em Saude"
   }else if((dados$TIPO[i] == "CCZ")   | (dados$TIPO[i] == "LAMUF")      | (dados$TIPO[i] == "VIG SANITARIA")
      | (dados$UNIDADE[i] == "VIG EPIDEMIO")){
      dados$UNI_1[i] <- "Vigilancia em Saude"
   }else if((dados$UNIDADE[i] == "SMS")   | (dados$TIPO[i] == "ALMOXARIFADO") | (dados$TIPO[i] == "CONSELHO MUNICIPAL")){
      dados$UNI_1[i] <- "Nivel Central"
   }else if(dados$UNIDADE[i] == "CONSELHO MUNICIPAL"){
      dados$UNI_1[i] <- "Conselho Municipal"
   }else{
      dados$UNI_1[i] <- "DIBEA"
   }
}

#'Ajustando UNI_2
dados$UNI_2 <- NA
for(i in 1:nrow(dados)){
   if((dados$TIPO[i] == "CS")  | (dados$TIPO[i] == "DS")    
   | (dados$TIPO[i] == "NASF")){
      dados$UNI_2[i] <- "Atencao Primaria"
   }else if((dados$TIPO[i] == "CAPS")   | (dados$TIPO[i] == "CEO")              
   | (dados$TIPO[i] == "POLI") | (dados$TIPO[i] == "SAMU")             
   | (dados$TIPO[i] == "UPA")){
      dados$UNI_2[i] <- "Atencao Especializada"
   }else if(dados$TIPO[i] == "FARMACIA ESCOLA"){
      dados$UNI_2[i] <- "Farmacia Escola"
   }else if(dados$UNIDADE[i] == "UNIDADES EXTERNAS"){
      dados$UNI_2[i] <- "Unidades Externas"
   }else if(dados$TIPO[i] == "CCZ"){
      dados$UNI_2[i] <- "CCZ"   
   }else if(dados$TIPO[i] == "LAMUF"){
      dados$UNI_2[i] <- "LAMUF"   
   }else if(dados$TIPO[i] == "VIG SANITARIA"){
      dados$UNI_2[i] <- "Vigilancia Sanitaria"   
   }else if(dados$UNIDADE[i] == "VIG EPIDEMIO"){
      dados$UNI_2[i] <- "Vigilancia Epidemiologica"
   }else if((dados$UNIDADE[i] == "SMS")){
      dados$UNI_2[i] <- "SMS"
   }else if(dados$TIPO[i] == "ALMOXARIFADO"){
      dados$UNI_2[i] <- "Almoxarifado"
#   }else{
#      dados$UNI_2[i] <- "Outros"
   }
}

#'Ajustando UNI_3
#'
for(i in 1:nrow(dados))if(dados$TIPO[i] == "DS"){dados$UNIDADE[i] <- paste0("SEDE ", dados$UNIDADE[i])}
dados$UNI_3 <- NA
for(i in 1:nrow(dados)){
   if((dados$TIPO[i] == "CS")  | (dados$TIPO[i] == "DS")){
      dados$UNI_3[i] <- "Distrito"
   }else if(dados$TIPO[i] == "NASF"){
      dados$UNI_3[i] <- "NASF"   
   }else if(dados$TIPO[i] == "CAPS"){
      dados$UNI_3[i] <- "CAPS"   
   }else if(dados$TIPO[i] == "CEO"){
      dados$UNI_3[i] <- "CEO"   
   }else if(dados$UNIDADE[i] == "POLI"){
      dados$UNI_3[i] <- "POLI"
   }else if((dados$UNIDADE[i] == "SMS")){
      dados$UNI_3[i] <- "SMS"
   }else if(dados$TIPO[i] == "SAMU"){
      dados$UNI_3[i] <- "SAMU"
   }else if(dados$TIPO[i] == "UPA"){
      dados$UNI_3[i] <- "UPA"
   }else if(dados$TIPO[i] == "POLI"){
      dados$UNI_3[i] <- "Policlinica"
#   }else{
#      dados$UNI_3[i] <- "Outros"
   }
}


#'Ajustando UNI_4
#'
dados$UNI_4 <- NA
for(i in 1:nrow(dados)){
   if((dados$TIPO[i] == "CS")){
      dados$UNI_4[i] <- "Centro de Saude"
   }else if(dados$TIPO[i] == "DS"){
      dados$UNI_4[i] <- "Sede do Distrito Sanitario" 
   }else if(dados$TIPO[i] == "UPA"){
      dados$UNI_4[i] <- dados$UNIDADE[i] 
   }else if(dados$TIPO[i] == "CAPS"){
      dados$UNI_4[i] <- dados$UNIDADE[i]
   }else if(dados$TIPO[i] == "CEO"){
      dados$UNI_4[i] <- dados$UNIDADE[i]
   }else if(dados$TIPO[i] == "POLI"){
      dados$UNI_4[i] <- dados$UNIDADE[i] 
#   }else{
#      dados$UNI_4[i] <- "Outros"
   }
}


#'Ajustando UNI_5
#'
dados$UNI_5 <- NA
for(i in 1:nrow(dados)){
   if((dados$TIPO[i] == "CS")){
      dados$UNI_5[i] <- dados$UNIDADE[i]
   }else if(dados$TIPO[i] == "DS"){
      dados$UNI_5[i] <- dados$UNIDADE[i] 
#   }else{
#      dados$UNI_5[i] <- "Outros"
   }
}



#'Criando os totais de custos por Grupo de Custo
#'Foram criados três grupos de custo
#'CAT_1: GASTO_RH
#'REM_ACS_ACE	
#'REM_ADMIN	
#'REM_ENFERMEIRO	
#'REM_MÉDICO	
#'REM_NASF	
#'REM_ODONTOLOGO	
#'REM_TEC_ENF	
#'REM_TEC_ODONTO	
#'REM_OUTROS	
#'REM_ACS_ACE_HE	
#'REM_ADMIN_HE	
#'REM_ENFERMEIRO_HE	
#'REM_MEDICO_HE	
#'REM_NASF_HE	
#'REM_ODONTÓLOGO_HE	
#'REM_TEC_ENF_HE	
#'REM_TEC_ODONTO_HE	
#'REM_OUTROS_HE
#'
#'CAT_2: GASTO_GERAL
#'ALUGUEL	
#'AGUA	
#'LUZ	
#'CENTRAL_TELEFONICA	
#'TELEFONE	
#'INTERNET	
#'SIST_REG_ELETR_SAUDE	
#'IMPRESSAO	
#'LAVANDERIA	
#'LIMPEZA	
#'SEGURANÇA	
#'VEICULOS	
#'MANUT_EQUIP	
#'MANUT_PREDIAL	
#'DIVERSOS
#'
#'
#'CAT_3: GASTO_MATERIAL_MEDICAMENTO
#'OXIGENIO	 
#'ACS 	 
#'ACUNPUNTURA 	 
#'ENFERMAGEM 	 
#'EPI 	 
#'FORMULA 	
#'GENEROS_ALIMENTICIOS	
#'HIGIENE_E_COPARIA	
#'IMPRESSOS	
#'JUDICIAL	
#'MEDICAMENTOS_ESTRATEGICOS	
#'MEDICO_ODONTO	
#'OUTROS_ROUPARIA 	 
#'UNIFORMES 	
#'EXAMES 
#'

#'Criando um identificador único para poder fazer o merge posterior da base
dados$ID <- c(1:nrow(dados))%>%as.character()

#'Separando a base de acordo com as categorias de custo e produçãod
dados_gasto_rh <- dados[,c(18:35,93)]
dados_gasto_gerais <- dados[,c(36:50,93)]
dados_gasto_mm <- dados[,c(68:86,93)]

#'Fazendo melt destas bases
dados_gasto_rh <- melt(dados_gasto_rh)
dados_gasto_rh$TIPO_DE_CUSTO <- "GASTO_RH"
names(dados_gasto_rh) <- c("ID", "VARIAVEL_GASTO","VALOR_GASTO", "TIPO_GASTO")
dados_gasto_gerais <- melt(dados_gasto_gerais)
dados_gasto_gerais$TIPO_DE_CUSTO <- "GASTO_GERAIS"
names(dados_gasto_gerais) <- c("ID", "VARIAVEL_GASTO","VALOR_GASTO", "TIPO_GASTO")
dados_gasto_mm <- melt(dados_gasto_mm)
dados_gasto_mm$TIPO_DE_CUSTO <- "GASTO_MATERIAL_MEDICAMENTO"
names(dados_gasto_mm) <- c("ID", "VARIAVEL_GASTO","VALOR_GASTO", "TIPO_GASTO")
dados_gastos <- rbind(dados_gasto_gerais, dados_gasto_mm, dados_gasto_rh)

#'Substituir NA por 0
dados_gastos[is.na(dados_gastos)] <- 0


#'Agrupando
dados_gastos_agr_variavel <- aggregate(dados_gastos$VALOR_GASTO, by = list(dados_gastos$VARIAVEL_GASTO, dados_gastos$TIPO_GASTO), FUN = sum)
names(dados_gastos_agr_variavel) <- c("VARIAVEL_GASTO","TIPO_GASTO","VALOR_GASTO")
dados_gastos_agr_tipo <- aggregate(dados_gastos$VALOR_GASTO, by = list(dados_gastos$TIPO_GASTO), FUN = sum)
names(dados_gastos_agr_tipo) <- c("TIPO_GASTO","VALOR_GASTO")
#'
#'Fazendo o Merge
banco_custo <- merge(dados_gastos, dados[,c(87:93)], by = "ID", all = T)


#'
#'
#'Criando a parte de análise de custos.
#'A produção será trabalha da seguinte forma
#'CAT 1: PRODUCAO
#'PROD_NASF	
#'PROD_MED_GERAL	
#'PROD_MED_CIRUR	
#'PROD_PEDIATRIA	
#'PROD_MED_ESPEC	
#'PROD_ENF	
#'PROD_ODO	
#'PROD_VACINA	
#'PROD_FARM	
#'PROD_RX	
#'PROD_OUTROS	
#'EXAMES_MED	
#'EXAMES_ENF	
#'EXAMES_ODO	
#'EXAMES_NASF	
#'ENCS_MED	
#'ENCS_ENF	
#'ENCS_ODO	
#'ENCS_NASF


#
