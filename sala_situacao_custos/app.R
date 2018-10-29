options(encoding = "UTF-8")
options(scipen = 999)
########################################################################################### 
#pacotes 
###########################################################################################
library(shiny)
library(shinydashboard)
library(tidyverse)
library(htmltools)
library(stringr)
library(DT)
library(plotly)
library(reshape2)
library(plyr)
library(dplyr)
library(d3Tree)#https://github.com/metrumresearchgroup/d3Tree/blob/master/inst/examples/titanic_shiny/server.R
library(sunburstR)# read in sample visit-sequences.csv data provided in source https://gist.github.com/kerryrodden/7090426#file-visit-sequences-csv




set.seed(1)
########################################################################################### 
#Banco de dados 
###########################################################################################
dados <- read_csv("bases/banco_custo.csv")%>%data.frame%>%mutate(NEWCOL=NA)%>%distinct #https://github.com/metrumresearchgroup/d3Tree/blob/master/inst/examples/titanic_shiny/server.R

 
senhas <- read_csv("bases/senhas.csv")
###########################################################################################
#Login
###########################################################################################
#Baseado em https://stackoverflow.com/questions/43404058/starting-shiny-app-after-password-input-with-shinydashboard
Logged = FALSE
my_username <- senhas$nome
my_password <- senhas$senha


########################################################################################### 
#UI
###########################################################################################
########################################################################################### 
ui <- dashboardPage(skin = "blue",
########################################################################################### 
        dashboardHeader(title = "Sala de Situação de Custos", titleWidth = 550),
        ########################################################################################### 
        dashboardSidebar(
          ########################################################################################### 
          sidebarMenu(
            menuItem("Gastos",tabName = "gasto", icon = icon("dashboard")),
            menuItem("Centros de Custos",tabName = "centro_de_custo", icon = icon("dashboard")),
            menuItem("Custos por Produto",tabName = "custo_por_produto", icon = icon("dashboard"),
                     menuSubItem("Consulta Médica", tabName = "custo_por_produto_consulta_medica"),
                     menuSubItem("Consulta de Enfermagem", tabName = "custo_por_produto_consulta_enfermagem"),
                     menuSubItem("Atend. do Téc. Enfermagem", tabName = "custo_por_produto_tecnico_enfermagem"),
                     menuSubItem("Procedimento Odontológico", tabName = "custo_por_produto_procedimento_odontologico"),
                     menuSubItem("Entrega de Medicação", tabName = "custo_por_produto_entrega_medicacao")),
            menuItem("Custos por Equipe",tabName = "custo_por_equipe", icon = icon("dashboard"),
                     menuSubItem("ESF", tabName = "custo_por_equipe_esf"),
                     menuSubItem("ESB", tabName = "custo_por_equipe_esb")),
            menuItem("Custos Gerados",tabName = "custo_gerado", icon = icon("dashboard"),
                     menuSubItem("Centro Custo Médico", tabName = "custo_gerado_medico"),
                     menuSubItem("Centro Custo Odontológico", tabName = "custo_gerado_dentista")),
            #menuItem("Instruções", icon = icon("question-circle"),
                     #href = "https://github.com/analisededadosemsaudefloripa/sala_situacao_custos"),
            #menuItem("Dados", icon = icon("database"),
                     #href = "http://floripadadosabertos.univille.br/"),
            menuItem("Código-fonte", icon = icon("code"), 
                     href = "https://github.com/analisededadosemsaudefloripa/sala_situacao_custos.git"),
            menuItem("Licença de Uso", icon = icon("cc"), 
                     href = "https://github.com/analisededadosemsaudefloripa/sala_situacao_custos/blob/master/LICENSE")
          )
        ),
        ########################################################################################### 
        dashboardBody(
          tabItems(
            ###########################################################################################
            #Gasto
            ###########################################################################################                         
            tabItem(tabName = "gasto", h2("Análise dos Gastos da Secretaria Municipal de Saúde"),
                    
                    fluidRow(
                             box(title = "Unidade", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("outup_sunburst_unidade_gasto")),
                             box(title = "Tipo de Custo", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("output_sunburst_tipo_custo_gasto"))
                    ),
                    fluidRow(
                             box(title = "Estrutura de Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    selectInput(
                                       inputId="input_analise_gasto",
                                       label="Selecione a análise:",
                                       choices=names(dados),
                                       selected="TIPO"),
                                    selectizeInput(
                                        inputId="hierarquia_gasto",
                                        label="Monte a estrutura de análise:",
                                        choices = names(dados),
                                        multiple = T, 
                                        selected = "",
                                        options = list(plugins = list('drag_drop','remove_button'))),
                                    d3treeOutput(outputId="d3_gasto"))
                             
                    ),
                    fluidRow(
                             box(title = "Acumulado", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotOutput("col_plot_gasto")),
                             box(title = "Distribuição", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotlyOutput("box_plot_gasto"))
                    ),
                    fluidRow(
                             box(title = "Dados dos Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    dataTableOutput('table_gasto'))
                             
                    )
            ),
            ###########################################################################################
            #Centros de Custo
            ###########################################################################################                         
            tabItem(tabName = "centro_de_custo", h2("Análise dos Centros de Custo da Secretaria Municipal de Saúde"),
                    
                    fluidRow(
                             box(title = "Unidade", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("outup_sunburst_unidade_centro_de_custo")),
                             box(title = "Tipo de Custo", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("output_sunburst_tipo_custo_centro_de_custo"))
                    ),
                    fluidRow(
                             box(title = "Estrutura de Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    selectInput(
                                       inputId="input_analise_centro_de_custo",
                                       label="Selecione a análise:",
                                       choices=names(dados),
                                       selected="VARIAVEL"),
                                    selectizeInput(
                                        inputId="hierarquia_centro_de_custo",
                                        label="Monte a estrutura de análise:",
                                        choices = names(dados),
                                        multiple = T, 
                                        selected = "",
                                        options = list(plugins = list('drag_drop','remove_button'))),
                                    d3treeOutput(outputId="d3_centro_de_custo"))
                             
                    ),
                    fluidRow(
                             box(title = "Acumulado", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotOutput("col_plot_centro_de_custo")),
                             box(title = "Distribuição", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotlyOutput("box_plot_centro_de_custo"))
                    ),
                    fluidRow(
                             box(title = "Dados dos Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    dataTableOutput('table_centro_de_custo'))
                             
                    )
            ),
            ###########################################################################################
            #Custos por produto - Consulta Médica
            ###########################################################################################                         
            tabItem(tabName = "custo_por_produto_consulta_medica", h2("Análise de Custos por Produto - Consulta Médica - da Secretaria Municipal de Saúde"),
                    
                    fluidRow(
                             box(title = "Unidade", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("outup_sunburst_unidade_custo_por_produto_consulta_medica")),
                             box(title = "Tipo de Custo", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("output_sunburst_tipo_custo_custo_por_produto_consulta_medica"))
                    ),
                    fluidRow(
                             box(title = "Estrutura de Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    selectInput(
                                       inputId="input_analise_custo_por_produto_consulta_medica",
                                       label="Selecione a análise:",
                                       choices=names(dados),
                                       selected="VARIAVEL"),
                                    selectizeInput(
                                        inputId="hierarquia_custo_por_produto_consulta_medica",
                                        label="Monte a estrutura de análise:",
                                        choices = names(dados),
                                        multiple = T, 
                                        selected = "",
                                        options = list(plugins = list('drag_drop','remove_button'))),
                                    d3treeOutput(outputId="d3_custo_por_produto_consulta_medica"))
                             
                    ),
                    fluidRow(
                             box(title = "Acumulado", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotOutput("col_plot_custo_por_produto_consulta_medica")),
                             box(title = "Distribuição", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotlyOutput("box_plot_custo_por_produto_consulta_medica"))
                    ),
                    fluidRow(
                             box(title = "Dados dos Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    dataTableOutput('table_custo_por_produto_consulta_medica'))
                             
                    )
            ),            
            ###########################################################################################
            #Custos por produto - Consulta de Enfermagem
            ###########################################################################################                         
            tabItem(tabName = "custo_por_produto_consulta_enfermagem", h2("Análise de Custos por Produto - Consulta de Enfermagem - da Secretaria Municipal de Saúde"),
                    
                    fluidRow(
                             box(title = "Unidade", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("outup_sunburst_unidade_custo_por_produto_consulta_enfermagem")),
                             box(title = "Tipo de Custo", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("output_sunburst_tipo_custo_custo_por_produto_consulta_enfermagem"))
                    ),
                    fluidRow(
                             box(title = "Estrutura de Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    selectInput(
                                       inputId="input_analise_custo_por_produto_consulta_enfermagem",
                                       label="Selecione a análise:",
                                       choices=names(dados),
                                       selected="VARIAVEL"),
                                    selectizeInput(
                                        inputId="hierarquia_custo_por_produto_consulta_enfermagem",
                                        label="Monte a estrutura de análise:",
                                        choices = names(dados),
                                        multiple = T, 
                                        selected = "",
                                        options = list(plugins = list('drag_drop','remove_button'))),
                                    d3treeOutput(outputId="d3_custo_por_produto_consulta_enfermagem"))
                             
                    ),
                    fluidRow(
                             box(title = "Acumulado", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotOutput("col_plot_custo_por_produto_consulta_enfermagem")),
                             box(title = "Distribuição", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotlyOutput("box_plot_custo_por_produto_consulta_enfermagem"))
                    ),
                    fluidRow(
                             box(title = "Dados dos Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    dataTableOutput('table_custo_por_produto_consulta_enfermagem'))
                             
                    )
            ),
            ###########################################################################################
            #Custos por produto - Atend. do Téc. de Enfermagem
            ###########################################################################################                         
            tabItem(tabName = "custo_por_produto_tecnico_enfermagem", h2("Análise de Custos por Produto - Atend. do Téc. de Enfermagem - da Secretaria Municipal de Saúde"),
                    
                    fluidRow(
                             box(title = "Unidade", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("outup_sunburst_unidade_custo_por_produto_tecnico_enfermagem")),
                             box(title = "Tipo de Custo", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("output_sunburst_tipo_custo_custo_por_produto_tecnico_enfermagem"))
                    ),
                    fluidRow(
                             box(title = "Estrutura de Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    selectInput(
                                       inputId="input_analise_custo_por_produto_tecnico_enfermagem",
                                       label="Selecione a análise:",
                                       choices=names(dados),
                                       selected="VARIAVEL"),
                                    selectizeInput(
                                        inputId="hierarquia_custo_por_produto_tecnico_enfermagem",
                                        label="Monte a estrutura de análise:",
                                        choices = names(dados),
                                        multiple = T, 
                                        selected = "",
                                        options = list(plugins = list('drag_drop','remove_button'))),
                                    d3treeOutput(outputId="d3_custo_por_produto_tecnico_enfermagem"))
                             
                    ),
                    fluidRow(
                             box(title = "Acumulado", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotOutput("col_plot_custo_por_produto_tecnico_enfermagem")),
                             box(title = "Distribuição", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotlyOutput("box_plot_custo_por_produto_tecnico_enfermagem"))
                    ),
                    fluidRow(
                             box(title = "Dados dos Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    dataTableOutput('table_custo_por_produto_tecnico_enfermagem'))
                             
                    )
            ),
            ###########################################################################################
            #Custos por produto - Procedimento Odontológico
            ###########################################################################################                         
            tabItem(tabName = "custo_por_produto_procedimento_odontologico", h2("Análise de Custos por Produto - Procedimento Odontológico - da Secretaria Municipal de Saúde"),
                    
                    fluidRow(
                             box(title = "Unidade", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("outup_sunburst_unidade_custo_por_produto_procedimento_odontologico")),
                             box(title = "Tipo de Custo", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("output_sunburst_tipo_custo_custo_por_produto_procedimento_odontologico"))
                    ),
                    fluidRow(
                             box(title = "Estrutura de Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    selectInput(
                                       inputId="input_analise_custo_por_produto_procedimento_odontologico",
                                       label="Selecione a análise:",
                                       choices=names(dados),
                                       selected="VARIAVEL"),
                                    selectizeInput(
                                        inputId="hierarquia_custo_por_produto_procedimento_odontologico",
                                        label="Monte a estrutura de análise:",
                                        choices = names(dados),
                                        multiple = T, 
                                        selected = "",
                                        options = list(plugins = list('drag_drop','remove_button'))),
                                    d3treeOutput(outputId="d3_custo_por_produto_procedimento_odontologico"))
                             
                    ),
                    fluidRow(
                             box(title = "Acumulado", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotOutput("col_plot_custo_por_produto_procedimento_odontologico")),
                             box(title = "Distribuição", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotlyOutput("box_plot_custo_por_produto_procedimento_odontologico"))
                    ),
                    fluidRow(
                             box(title = "Dados dos Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    dataTableOutput('table_custo_por_produto_procedimento_odontologico'))
                             
                    )
            ),
            ###########################################################################################
            #Custos por produto - Entréga de Medicação
            ###########################################################################################                         
            tabItem(tabName = "custo_por_produto_entrega_medicacao", h2("Análise de Custos por Produto - Entrega de Medicação - da Secretaria Municipal de Saúde"),
                    
                    fluidRow(
                             box(title = "Unidade", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("outup_sunburst_unidade_custo_por_produto_entrega_mediciacao")),
                             box(title = "Tipo de Custo", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("output_sunburst_tipo_custo_custo_por_produto_entrega_mediciacao"))
                    ),
                    fluidRow(
                             box(title = "Estrutura de Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    selectInput(
                                       inputId="input_analise_custo_por_produto_entrega_mediciacao",
                                       label="Selecione a análise:",
                                       choices=names(dados),
                                       selected="VARIAVEL"),
                                    selectizeInput(
                                        inputId="hierarquia_custo_por_produto_entrega_mediciacao",
                                        label="Monte a estrutura de análise:",
                                        choices = names(dados),
                                        multiple = T, 
                                        selected = "",
                                        options = list(plugins = list('drag_drop','remove_button'))),
                                    d3treeOutput(outputId="d3_custo_por_produto_entrega_mediciacao"))
                             
                    ),
                    fluidRow(
                             box(title = "Acumulado", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotOutput("col_plot_custo_por_produto_entrega_mediciacao")),
                             box(title = "Distribuição", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotlyOutput("box_plot_custo_por_produto_entrega_mediciacao"))
                    ),
                    fluidRow(
                             box(title = "Dados dos Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    dataTableOutput('table_custo_por_produto_entrega_mediciacao'))
                             
                    )
            ),
            ###########################################################################################
            #Custos por equipe - ESF
            ###########################################################################################                         
            tabItem(tabName = "custo_por_equipe_esf", h2("Análise de Custos por ESF da Secretaria Municipal de Saúde"),
                    
                    fluidRow(
                             box(title = "Unidade", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("outup_sunburst_unidade_custo_por_equipe_esf")),
                             box(title = "Tipo de Custo", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("output_sunburst_tipo_custo_custo_por_equipe_esf"))
                    ),
                    fluidRow(
                             box(title = "Estrutura de Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    selectInput(
                                       inputId="input_analise_custo_por_equipe_esf",
                                       label="Selecione a análise:",
                                       choices=names(dados),
                                       selected="VARIAVEL"),
                                    selectizeInput(
                                        inputId="hierarquia_custo_por_equipe_esf",
                                        label="Monte a estrutura de análise:",
                                        choices = names(dados),
                                        multiple = T, 
                                        selected = "",
                                        options = list(plugins = list('drag_drop','remove_button'))),
                                    d3treeOutput(outputId="d3_custo_por_equipe_esf"))
                             
                    ),
                    fluidRow(
                             box(title = "Acumulado", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotOutput("col_plot_custo_por_equipe_esf")),
                             box(title = "Distribuição", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotlyOutput("box_plot_custo_por_equipe_esf"))
                    ),
                    fluidRow(
                             box(title = "Dados dos Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    dataTableOutput('table_custo_por_equipe_esf'))
                             
                    )
            ),
            ###########################################################################################
            #Custos por equipe - ESB
            ###########################################################################################                         
            tabItem(tabName = "custo_por_equipe_esb", h2("Análise de Custos por ESB da Secretaria Municipal de Saúde"),
                    
                    fluidRow(
                             box(title = "Unidade", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("outup_sunburst_unidade_custo_por_equipe_esb")),
                             box(title = "Tipo de Custo", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("output_sunburst_tipo_custo_custo_por_equipe_esb"))
                    ),
                    fluidRow(
                             box(title = "Estrutura de Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    selectInput(
                                       inputId="input_analise_custo_por_equipe_esb",
                                       label="Selecione a análise:",
                                       choices=names(dados),
                                       selected="VARIAVEL"),
                                    selectizeInput(
                                        inputId="hierarquia_custo_por_equipe_esb",
                                        label="Monte a estrutura de análise:",
                                        choices = names(dados),
                                        multiple = T, 
                                        selected = "",
                                        options = list(plugins = list('drag_drop','remove_button'))),
                                    d3treeOutput(outputId="d3_custo_por_equipe_esb"))
                             
                    ),
                    fluidRow(
                             box(title = "Acumulado", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotOutput("col_plot_custo_por_equipe_esb")),
                             box(title = "Distribuição", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotlyOutput("box_plot_custo_por_equipe_esb"))
                    ),
                    fluidRow(
                             box(title = "Dados dos Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    dataTableOutput('table_custo_por_equipe_esb'))
                             
                    )
            ),
            ###########################################################################################
            #Custos Gerado - Médico
            ###########################################################################################                         
            tabItem(tabName = "custo_gerado_medico", h2("Análise de Custos Gerado por Centro de Custo Médicos da Aten. Prim. da Secretaria Municipal de Saúde"),
                    
                    fluidRow(
                             box(title = "Unidade", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("outup_sunburst_unidade_custo_gerado_medico")),
                             box(title = "Tipo de Custo", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("output_sunburst_tipo_custo_custo_gerado_medico"))
                    ),
                    fluidRow(
                             box(title = "Estrutura de Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    selectInput(
                                       inputId="input_analise_custo_gerado_medico",
                                       label="Selecione a análise:",
                                       choices=names(dados),
                                       selected="VARIAVEL"),
                                    selectizeInput(
                                        inputId="hierarquia_custo_gerado_medico",
                                        label="Monte a estrutura de análise:",
                                        choices = names(dados),
                                        multiple = T, 
                                        selected = "",
                                        options = list(plugins = list('drag_drop','remove_button'))),
                                    d3treeOutput(outputId="d3_custo_gerado_medico"))
                             
                    ),
                    fluidRow(
                             box(title = "Acumulado", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotOutput("col_plot_custo_gerado_medico")),
                             box(title = "Distribuição", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotlyOutput("box_plot_custo_gerado_medico"))
                    ),
                    fluidRow(
                             box(title = "Dados dos Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    dataTableOutput('table_custo_gerado_medico'))
                             
                    )
            ),
            ###########################################################################################
            #Custos Gerado - Dentista
            ###########################################################################################                         
            tabItem(tabName = "custo_gerado_dentista", h2("Análise de Custos Gerado por Centro de Custo Odontológico da Aten. Prim. da Secretaria Municipal de Saúde"),
                    
                    fluidRow(
                             box(title = "Unidade", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("outup_sunburst_unidade_custo_gerado_dentista")),
                             box(title = "Tipo de Custo", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("output_sunburst_tipo_custo_custo_gerado_dentista"))
                    ),
                    fluidRow(
                             box(title = "Estrutura de Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    selectInput(
                                       inputId="input_analise_custo_gerado_dentista",
                                       label="Selecione a análise:",
                                       choices=names(dados),
                                       selected="VARIAVEL"),
                                    selectizeInput(
                                        inputId="hierarquia_custo_gerado_dentista",
                                        label="Monte a estrutura de análise:",
                                        choices = names(dados),
                                        multiple = T, 
                                        selected = "",
                                        options = list(plugins = list('drag_drop','remove_button'))),
                                    d3treeOutput(outputId="d3_custo_gerado_dentista"))
                             
                    ),
                    fluidRow(
                             box(title = "Acumulado", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotOutput("col_plot_custo_gerado_dentista")),
                             box(title = "Distribuição", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotlyOutput("box_plot_custo_gerado_dentista"))
                    ),
                    fluidRow(
                             box(title = "Dados dos Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    dataTableOutput('table_custo_gerado_dentista'))
                             
                    )
            )
          ###########################################################################################  

      )
   )
)
########################################################################################### 
server <- function(input, output, session) {
###########################################################################################
###########################################################################################
#Login
###########################################################################################
values <- reactiveValues(authenticated = FALSE)

# Return the UI for a modal dialog with data selection input. If 'failed' 
# is TRUE, then display a message that the previous value was invalid.
dataModal <- function(failed = FALSE) {
  modalDialog(
    textInput("username", "Nome:"),
    passwordInput("password", "Senha:"),
    footer = tagList(
      #modalButton("Cancel"),
      actionButton("entrar", "Entrar")
    )
  )
}

# Show modal when button is clicked.  
# This `observe` is suspended only whith right user credential

obs1 <- observe({
  showModal(dataModal())
})

# When OK button is pressed, attempt to authenticate. If successful,
# remove the modal. 

obs2 <- observe({
  req(input$entrar)
  isolate({
    Username <- input$username
    Password <- input$password
  })
  Id.username <- if(Username %in% my_username){which(my_username == Username)}
  Id.password <- if(Password %in% my_password){which(my_password == Password)}
  if (length(Id.username) > 0 & length(Id.password) > 0) {
    if (Id.username == Id.password) {
      Logged <<- TRUE
        values$authenticated <- TRUE
        obs1$suspend()
        removeModal()

    } else {
      values$authenticated <- FALSE
    }     
  }
})

###########################################################################################
#Gastos 
###########################################################################################
output$outup_sunburst_unidade_gasto <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "GASTO")
   a <- a[,-c(7:9,11)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

output$output_sunburst_tipo_custo_gasto <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "GASTO")
   a <- a[,c(8,9,10)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

  network_gasto <- reactiveValues()
  
  observeEvent(input$d3_gasto_update,{
    network_gasto$nodes <- unlist(input$d3_gasto_update$.nodesData)
    activeNode <- input$d3_gasto_update$.activeNode
    if(!is.null(activeNode)) network_gasto$click <- jsonlite::fromJSON(activeNode)
  })
  
  observeEvent(network_gasto$click,{
    output$clickView<-renderDataTable({
      as.data.frame(network_gasto$click)
    },caption='Last Clicked Node',caption.placement='top')
  })
 
  
  TreeStruct_gasto<-eventReactive(network_gasto$nodes,{
    
    df <- subset(dados, dados$CATEGORIA == "GASTO")
    if(is.null(network_gasto$nodes)){
      df <- subset(dados, dados$CATEGORIA == "GASTO")
    }else{
      
      x.filter <- tree.filter(network_gasto$nodes,subset(dados, dados$CATEGORIA == "GASTO"))
      df <- ddply(x.filter,.(ID),function(a.x){subset(dados, dados$CATEGORIA == "GASTO")%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  observeEvent(input$hierarquia_gasto,{
    output$d3_gasto <- renderD3tree({
      if(is.null(input$hierarquia_gasto)){
        p <- subset(dados, dados$CATEGORIA == "GASTO")
      }else{
        p <- subset(dados, dados$CATEGORIA == "GASTO")%>%select(one_of(c(input$hierarquia_gasto,"NEWCOL")))%>%unique
      }
      
      d3tree(data = list(root = df2tree(struct = p,rootname = 'SMS Florianópolis'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 18)
    })
  })

  output$table_gasto <- renderDataTable(expr = {
    TreeStruct_gasto()%>%select(-NEWCOL)
  },
    escape=F,
    filter = "top",
    rownames= F,
    extensions = list("ColReorder" = NULL,
                      "Buttons" = NULL,
                      "FixedColumns" = list(leftColumns=1),
                      "Scroller" = NULL),
    options = list(
                dom = 'BRrltpi',
                autoWidth=TRUE,
                lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                ColReorder = TRUE,
                buttons =
                  list(
                    'print',
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                    I('colvis')
                  )
              )
  )
  

  
  output$box_plot_gasto <- renderPlotly({
     
      ggplot(TreeStruct_gasto(), aes(TreeStruct_gasto()[,names(TreeStruct_gasto()) == input$input_analise_gasto], VALOR))+  ### Tentando fazer com que o eixo X mostre variáveis exibidas após o click
         geom_boxplot(fill = "blue")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$col_plot_gasto <- renderPlot({
     
     ggplot(TreeStruct_gasto(), aes(TreeStruct_gasto()[,names(TreeStruct_gasto()) == input$input_analise_gasto], VALOR))+
         geom_col(fill = "blue", position = "stack")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })

###########################################################################################
#Centros de Custo
###########################################################################################
output$outup_sunburst_unidade_centro_de_custo <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CENTRO DE CUSTO")
   a <- a[,-c(7:9,11)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

output$output_sunburst_tipo_custo_centro_de_custo <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CENTRO DE CUSTO")
   a <- a[,c(8,9,10)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

  network_centro_de_custo <- reactiveValues()
  
  observeEvent(input$d3_centro_de_custo_update,{
    network_centro_de_custo$nodes <- unlist(input$d3_centro_de_custo_update$.nodesData)
    activeNode <- input$d3_centro_de_custo_update$.activeNode
    if(!is.null(activeNode)) network_centro_de_custo$click <- jsonlite::fromJSON(activeNode)
  })
  
  observeEvent(network_centro_de_custo$click,{
    output$clickView<-renderDataTable({
      as.data.frame(network_centro_de_custo$click)
    },caption='Last Clicked Node',caption.placement='top')
  })
 
  
  TreeStruct_centro_de_custo<-eventReactive(network_centro_de_custo$nodes,{
    
    df <- subset(dados, dados$CATEGORIA == "CENTRO DE CUSTO")
    if(is.null(network_centro_de_custo$nodes)){
      df <- subset(dados, dados$CATEGORIA == "CENTRO DE CUSTO")
    }else{
      
      x.filter <- tree.filter(network_centro_de_custo$nodes,subset(dados, dados$CATEGORIA == "CENTRO DE CUSTO"))
      df <- ddply(x.filter,.(ID),function(a.x){subset(dados, dados$CATEGORIA == "CENTRO DE CUSTO")%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  observeEvent(input$hierarquia_centro_de_custo,{
    output$d3_centro_de_custo <- renderD3tree({
      if(is.null(input$hierarquia_centro_de_custo)){
        p <- subset(dados, dados$CATEGORIA == "CENTRO DE CUSTO")
      }else{
        p <- subset(dados, dados$CATEGORIA == "CENTRO DE CUSTO")%>%select(one_of(c(input$hierarquia_centro_de_custo,"NEWCOL")))%>%unique
      }
      
      d3tree(data = list(root = df2tree(struct = p,rootname = 'SMS Florianópolis'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 18)
    })
  })

  output$table_centro_de_custo <- renderDataTable(expr = {
    TreeStruct_centro_de_custo()%>%select(-NEWCOL)
  },
    escape=F,
    filter = "top",
    rownames= F,
    extensions = list("ColReorder" = NULL,
                      "Buttons" = NULL,
                      "FixedColumns" = list(leftColumns=1),
                      "Scroller" = NULL),
    options = list(
                dom = 'BRrltpi',
                autoWidth=TRUE,
                lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                ColReorder = TRUE,
                buttons =
                  list(
                    'print',
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                    I('colvis')
                  )
              )
  )
  

  
  output$box_plot_centro_de_custo <- renderPlotly({
     
      ggplot(TreeStruct_centro_de_custo(), aes(TreeStruct_centro_de_custo()[,names(TreeStruct_centro_de_custo()) == input$input_analise_centro_de_custo], VALOR))+  ### Tentando fazer com que o eixo X mostre variáveis exibidas após o click
         geom_boxplot(fill = "blue")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$col_plot_centro_de_custo <- renderPlot({
     
     ggplot(TreeStruct_centro_de_custo(), aes(TreeStruct_centro_de_custo()[,names(TreeStruct_centro_de_custo()) == input$input_analise_centro_de_custo], VALOR))+
         geom_col(fill = "blue", position = "stack")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })

###########################################################################################
#Custos por Produto - Consulta Médica
###########################################################################################
output$outup_sunburst_unidade_custo_por_produto_consulta_medica <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO PRODUTO")
   a <- subset(a, a$VARIAVEL == "CUSTO_CONS_MED_DIR")
   a <- a[,-c(7:9,11)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

output$output_sunburst_tipo_custo_custo_por_produto_consulta_medica <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO PRODUTO")
   a <- a[,c(8,9,10)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

  network_custo_por_produto_consulta_medica <- reactiveValues()
  
  observeEvent(input$d3_custo_por_produto_consulta_medica_update,{
    network_custo_por_produto_consulta_medica$nodes <- unlist(input$d3_custo_por_produto_consulta_medica_update$.nodesData)
    activeNode <- input$d3_custo_por_produto_consulta_medica_update$.activeNode
    if(!is.null(activeNode)) network_custo_por_produto_consulta_medica$click <- jsonlite::fromJSON(activeNode)
  })
  
  observeEvent(network_custo_por_produto_consulta_medica$click,{
    output$clickView<-renderDataTable({
      as.data.frame(network_custo_por_produto_consulta_medica$click)
    },caption='Last Clicked Node',caption.placement='top')
  })
 
  
  TreeStruct_custo_por_produto_consulta_medica<-eventReactive(network_custo_por_produto_consulta_medica$nodes,{
    
    df <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
    if(is.null(network_custo_por_produto_consulta_medica$nodes)){
      df <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
    }else{
      
      x.filter <- tree.filter(network_custo_por_produto_consulta_medica$nodes,subset(dados, dados$CATEGORIA == "CUSTO PRODUTO"))
      df <- ddply(x.filter,.(ID),function(a.x){subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  observeEvent(input$hierarquia_custo_por_produto_consulta_medica,{
    output$d3_custo_por_produto_consulta_medica <- renderD3tree({
      if(is.null(input$hierarquia_custo_por_produto_consulta_medica)){
        p <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
      }else{
        p <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")%>%select(one_of(c(input$hierarquia_custo_por_produto_consulta_medica,"NEWCOL")))%>%unique
      }
      
      d3tree(data = list(root = df2tree(struct = p,rootname = 'SMS Florianópolis'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 18)
    })
  })

  output$table_custo_por_produto_consulta_medica <- renderDataTable(expr = {
    TreeStruct_custo_por_produto_consulta_medica()%>%select(-NEWCOL)
  },
    escape=F,
    filter = "top",
    rownames= F,
    extensions = list("ColReorder" = NULL,
                      "Buttons" = NULL,
                      "FixedColumns" = list(leftColumns=1),
                      "Scroller" = NULL),
    options = list(
                dom = 'BRrltpi',
                autoWidth=TRUE,
                lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                ColReorder = TRUE,
                buttons =
                  list(
                    'print',
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                    I('colvis')
                  )
              )
  )
  

  
  output$box_plot_custo_por_produto_consulta_medica <- renderPlotly({
     
      ggplot(TreeStruct_custo_por_produto_consulta_medica(), aes(TreeStruct_custo_por_produto_consulta_medica()[,names(TreeStruct_custo_por_produto_consulta_medica()) == input$input_analise_custo_por_produto_consulta_medica], VALOR))+  ### Tentando fazer com que o eixo X mostre variáveis exibidas após o click
         geom_boxplot(fill = "blue")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$col_plot_custo_por_produto_consulta_medica <- renderPlot({
     
     ggplot(TreeStruct_custo_por_produto_consulta_medica(), aes(TreeStruct_custo_por_produto_consulta_medica()[,names(TreeStruct_custo_por_produto_consulta_medica()) == input$input_analise_custo_por_produto_consulta_medica], VALOR))+
         geom_col(fill = "blue", position = "stack")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })

  
###########################################################################################
#Custos por Produto - Consulta de Enfermagem
###########################################################################################
output$outup_sunburst_unidade_custo_por_produto_consulta_enfermagem <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO PRODUTO")
   a <- subset(a, a$VARIAVEL == "CUSTO_CONS_ENF_DIR")
   a <- a[,-c(7:9,11)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

output$output_sunburst_tipo_custo_custo_por_produto_consulta_enfermagem <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO PRODUTO")
   a <- a[,c(8,9,10)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

  network_custo_por_produto_consulta_enfermagem <- reactiveValues()
  
  observeEvent(input$d3_custo_por_produto_consulta_enfermagem_update,{
    network_custo_por_produto_consulta_enfermagem$nodes <- unlist(input$d3_custo_por_produto_consulta_enfermagem_update$.nodesData)
    activeNode <- input$d3_custo_por_produto_consulta_enfermagem_update$.activeNode
    if(!is.null(activeNode)) network_custo_por_produto_consulta_enfermagem$click <- jsonlite::fromJSON(activeNode)
  })
  
  observeEvent(network_custo_por_produto_consulta_enfermagem$click,{
    output$clickView<-renderDataTable({
      as.data.frame(network_custo_por_produto_consulta_enfermagem$click)
    },caption='Last Clicked Node',caption.placement='top')
  })
 
  
  TreeStruct_custo_por_produto_consulta_enfermagem<-eventReactive(network_custo_por_produto_consulta_enfermagem$nodes,{
    
    df <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
    if(is.null(network_custo_por_produto_consulta_enfermagem$nodes)){
      df <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
    }else{
      
      x.filter <- tree.filter(network_custo_por_produto_consulta_enfermagem$nodes,subset(dados, dados$CATEGORIA == "CUSTO PRODUTO"))
      df <- ddply(x.filter,.(ID),function(a.x){subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  observeEvent(input$hierarquia_custo_por_produto_consulta_enfermagem,{
    output$d3_custo_por_produto_consulta_enfermagem <- renderD3tree({
      if(is.null(input$hierarquia_custo_por_produto_consulta_enfermagem)){
        p <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
      }else{
        p <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")%>%select(one_of(c(input$hierarquia_custo_por_produto_consulta_enfermagem,"NEWCOL")))%>%unique
      }
      
      d3tree(data = list(root = df2tree(struct = p,rootname = 'SMS Florianópolis'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 18)
    })
  })

  output$table_custo_por_produto_consulta_enfermagem <- renderDataTable(expr = {
    TreeStruct_custo_por_produto_consulta_enfermagem()%>%select(-NEWCOL)
  },
    escape=F,
    filter = "top",
    rownames= F,
    extensions = list("ColReorder" = NULL,
                      "Buttons" = NULL,
                      "FixedColumns" = list(leftColumns=1),
                      "Scroller" = NULL),
    options = list(
                dom = 'BRrltpi',
                autoWidth=TRUE,
                lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                ColReorder = TRUE,
                buttons =
                  list(
                    'print',
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                    I('colvis')
                  )
              )
  )
  

  
  output$box_plot_custo_por_produto_consulta_enfermagem <- renderPlotly({
     
      ggplot(TreeStruct_custo_por_produto_consulta_enfermagem(), aes(TreeStruct_custo_por_produto_consulta_enfermagem()[,names(TreeStruct_custo_por_produto_consulta_enfermagem()) == input$input_analise_custo_por_produto_consulta_enfermagem], VALOR))+  ### Tentando fazer com que o eixo X mostre variáveis exibidas após o click
         geom_boxplot(fill = "blue")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$col_plot_custo_por_produto_consulta_enfermagem <- renderPlot({
     
     ggplot(TreeStruct_custo_por_produto_consulta_enfermagem(), aes(TreeStruct_custo_por_produto_consulta_enfermagem()[,names(TreeStruct_custo_por_produto_consulta_enfermagem()) == input$input_analise_custo_por_produto_consulta_enfermagem], VALOR))+
         geom_col(fill = "blue", position = "stack")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
###########################################################################################
#Custos por Produto - Atend. do Téc. de Enfermagem
###########################################################################################
output$outup_sunburst_unidade_custo_por_produto_tecnico_enfermagem <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO PRODUTO")
   a <- subset(a, a$VARIAVEL == "CUSTO_ATD_TEC_DIR")
   a <- a[,-c(7:9,11)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

output$output_sunburst_tipo_custo_custo_por_produto_tecnico_enfermagem <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO PRODUTO")
   a <- a[,c(8,9,10)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

  network_custo_por_produto_tecnico_enfermagem <- reactiveValues()
  
  observeEvent(input$d3_custo_por_produto_tecnico_enfermagem_update,{
    network_custo_por_produto_tecnico_enfermagem$nodes <- unlist(input$d3_custo_por_produto_tecnico_enfermagem_update$.nodesData)
    activeNode <- input$d3_custo_por_produto_tecnico_enfermagem_update$.activeNode
    if(!is.null(activeNode)) network_custo_por_produto_tecnico_enfermagem$click <- jsonlite::fromJSON(activeNode)
  })
  
  observeEvent(network_custo_por_produto_tecnico_enfermagem$click,{
    output$clickView<-renderDataTable({
      as.data.frame(network_custo_por_produto_tecnico_enfermagem$click)
    },caption='Last Clicked Node',caption.placement='top')
  })
 
  
  TreeStruct_custo_por_produto_tecnico_enfermagem<-eventReactive(network_custo_por_produto_tecnico_enfermagem$nodes,{
    
    df <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
    if(is.null(network_custo_por_produto_tecnico_enfermagem$nodes)){
      df <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
    }else{
      
      x.filter <- tree.filter(network_custo_por_produto_tecnico_enfermagem$nodes,subset(dados, dados$CATEGORIA == "CUSTO PRODUTO"))
      df <- ddply(x.filter,.(ID),function(a.x){subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  observeEvent(input$hierarquia_custo_por_produto_tecnico_enfermagem,{
    output$d3_custo_por_produto_tecnico_enfermagem <- renderD3tree({
      if(is.null(input$hierarquia_custo_por_produto_tecnico_enfermagem)){
        p <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
      }else{
        p <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")%>%select(one_of(c(input$hierarquia_custo_por_produto_tecnico_enfermagem,"NEWCOL")))%>%unique
      }
      
      d3tree(data = list(root = df2tree(struct = p,rootname = 'SMS Florianópolis'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 18)
    })
  })

  output$table_custo_por_produto_tecnico_enfermagem <- renderDataTable(expr = {
    TreeStruct_custo_por_produto_tecnico_enfermagem()%>%select(-NEWCOL)
  },
    escape=F,
    filter = "top",
    rownames= F,
    extensions = list("ColReorder" = NULL,
                      "Buttons" = NULL,
                      "FixedColumns" = list(leftColumns=1),
                      "Scroller" = NULL),
    options = list(
                dom = 'BRrltpi',
                autoWidth=TRUE,
                lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                ColReorder = TRUE,
                buttons =
                  list(
                    'print',
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                    I('colvis')
                  )
              )
  )
  

  
  output$box_plot_custo_por_produto_tecnico_enfermagem <- renderPlotly({
     
      ggplot(TreeStruct_custo_por_produto_tecnico_enfermagem(), aes(TreeStruct_custo_por_produto_tecnico_enfermagem()[,names(TreeStruct_custo_por_produto_tecnico_enfermagem()) == input$input_analise_custo_por_produto_tecnico_enfermagem], VALOR))+  ### Tentando fazer com que o eixo X mostre variáveis exibidas após o click
         geom_boxplot(fill = "blue")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$col_plot_custo_por_produto_tecnico_enfermagem <- renderPlot({
     
     ggplot(TreeStruct_custo_por_produto_tecnico_enfermagem(), aes(TreeStruct_custo_por_produto_tecnico_enfermagem()[,names(TreeStruct_custo_por_produto_tecnico_enfermagem()) == input$input_analise_custo_por_produto_tecnico_enfermagem], VALOR))+
         geom_col(fill = "blue", position = "stack")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
###########################################################################################
#Custos por Produto - Procedimentos Odontológicos
###########################################################################################
output$outup_sunburst_unidade_custo_por_produto_procedimento_odontologico <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO PRODUTO")
   a <- subset(a, a$VARIAVEL == "CUSTO_PROC_ODO_DIR")
   a <- a[,-c(7:9,11)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

output$output_sunburst_tipo_custo_custo_por_produto_procedimento_odontologico <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO PRODUTO")
   a <- a[,c(8,9,10)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

  network_custo_por_produto_procedimento_odontologico <- reactiveValues()
  
  observeEvent(input$d3_custo_por_produto_procedimento_odontologico_update,{
    network_custo_por_produto_procedimento_odontologico$nodes <- unlist(input$d3_custo_por_produto_procedimento_odontologico_update$.nodesData)
    activeNode <- input$d3_custo_por_produto_procedimento_odontologico_update$.activeNode
    if(!is.null(activeNode)) network_custo_por_produto_procedimento_odontologico$click <- jsonlite::fromJSON(activeNode)
  })
  
  observeEvent(network_custo_por_produto_procedimento_odontologico$click,{
    output$clickView<-renderDataTable({
      as.data.frame(network_custo_por_produto_procedimento_odontologico$click)
    },caption='Last Clicked Node',caption.placement='top')
  })
 
  
  TreeStruct_custo_por_produto_procedimento_odontologico<-eventReactive(network_custo_por_produto_procedimento_odontologico$nodes,{
    
    df <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
    if(is.null(network_custo_por_produto_procedimento_odontologico$nodes)){
      df <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
    }else{
      
      x.filter <- tree.filter(network_custo_por_produto_procedimento_odontologico$nodes,subset(dados, dados$CATEGORIA == "CUSTO PRODUTO"))
      df <- ddply(x.filter,.(ID),function(a.x){subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  observeEvent(input$hierarquia_custo_por_produto_procedimento_odontologico,{
    output$d3_custo_por_produto_procedimento_odontologico <- renderD3tree({
      if(is.null(input$hierarquia_custo_por_produto_procedimento_odontologico)){
        p <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
      }else{
        p <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")%>%select(one_of(c(input$hierarquia_custo_por_produto_procedimento_odontologico,"NEWCOL")))%>%unique
      }
      
      d3tree(data = list(root = df2tree(struct = p,rootname = 'SMS Florianópolis'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 18)
    })
  })

  output$table_custo_por_produto_procedimento_odontologico <- renderDataTable(expr = {
    TreeStruct_custo_por_produto_procedimento_odontologico()%>%select(-NEWCOL)
  },
    escape=F,
    filter = "top",
    rownames= F,
    extensions = list("ColReorder" = NULL,
                      "Buttons" = NULL,
                      "FixedColumns" = list(leftColumns=1),
                      "Scroller" = NULL),
    options = list(
                dom = 'BRrltpi',
                autoWidth=TRUE,
                lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                ColReorder = TRUE,
                buttons =
                  list(
                    'print',
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                    I('colvis')
                  )
              )
  )
  

  
  output$box_plot_custo_por_produto_procedimento_odontologico <- renderPlotly({
     
      ggplot(TreeStruct_custo_por_produto_procedimento_odontologico(), aes(TreeStruct_custo_por_produto_procedimento_odontologico()[,names(TreeStruct_custo_por_produto_procedimento_odontologico()) == input$input_analise_custo_por_produto_procedimento_odontologico], VALOR))+  ### Tentando fazer com que o eixo X mostre variáveis exibidas após o click
         geom_boxplot(fill = "blue")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$col_plot_custo_por_produto_procedimento_odontologico <- renderPlot({
     
     ggplot(TreeStruct_custo_por_produto_procedimento_odontologico(), aes(TreeStruct_custo_por_produto_procedimento_odontologico()[,names(TreeStruct_custo_por_produto_procedimento_odontologico()) == input$input_analise_custo_por_produto_procedimento_odontologico], VALOR))+
         geom_col(fill = "blue", position = "stack")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
###########################################################################################
#Custos por Produto - Entrega de Medicação
###########################################################################################
output$outup_sunburst_unidade_custo_por_produto_entrega_mediciacao <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO PRODUTO")
   a <- subset(a, a$VARIAVEL == "CUSTO_DISP_FARM_DIR")
   a <- a[,-c(7:9,11)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

output$output_sunburst_tipo_custo_custo_por_produto_entrega_mediciacao <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO PRODUTO")
   a <- a[,c(8,9,10)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

  network_custo_por_produto_entrega_mediciacao <- reactiveValues()
  
  observeEvent(input$d3_custo_por_produto_entrega_mediciacao_update,{
    network_custo_por_produto_entrega_mediciacao$nodes <- unlist(input$d3_custo_por_produto_entrega_mediciacao_update$.nodesData)
    activeNode <- input$d3_custo_por_produto_entrega_mediciacao_update$.activeNode
    if(!is.null(activeNode)) network_custo_por_produto_entrega_mediciacao$click <- jsonlite::fromJSON(activeNode)
  })
  
  observeEvent(network_custo_por_produto_entrega_mediciacao$click,{
    output$clickView<-renderDataTable({
      as.data.frame(network_custo_por_produto_entrega_mediciacao$click)
    },caption='Last Clicked Node',caption.placement='top')
  })
 
  
  TreeStruct_custo_por_produto_entrega_mediciacao<-eventReactive(network_custo_por_produto_entrega_mediciacao$nodes,{
    
    df <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
    if(is.null(network_custo_por_produto_entrega_mediciacao$nodes)){
      df <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
    }else{
      
      x.filter <- tree.filter(network_custo_por_produto_entrega_mediciacao$nodes,subset(dados, dados$CATEGORIA == "CUSTO PRODUTO"))
      df <- ddply(x.filter,.(ID),function(a.x){subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  observeEvent(input$hierarquia_custo_por_produto_entrega_mediciacao,{
    output$d3_custo_por_produto_entrega_mediciacao <- renderD3tree({
      if(is.null(input$hierarquia_custo_por_produto_entrega_mediciacao)){
        p <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
      }else{
        p <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")%>%select(one_of(c(input$hierarquia_custo_por_produto_entrega_mediciacao,"NEWCOL")))%>%unique
      }
      
      d3tree(data = list(root = df2tree(struct = p,rootname = 'SMS Florianópolis'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 18)
    })
  })

  output$table_custo_por_produto_entrega_mediciacao <- renderDataTable(expr = {
    TreeStruct_custo_por_produto_entrega_mediciacao()%>%select(-NEWCOL)
  },
    escape=F,
    filter = "top",
    rownames= F,
    extensions = list("ColReorder" = NULL,
                      "Buttons" = NULL,
                      "FixedColumns" = list(leftColumns=1),
                      "Scroller" = NULL),
    options = list(
                dom = 'BRrltpi',
                autoWidth=TRUE,
                lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                ColReorder = TRUE,
                buttons =
                  list(
                    'print',
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                    I('colvis')
                  )
              )
  )
  

  
  output$box_plot_custo_por_produto_entrega_mediciacao <- renderPlotly({
     
      ggplot(TreeStruct_custo_por_produto_entrega_mediciacao(), aes(TreeStruct_custo_por_produto_entrega_mediciacao()[,names(TreeStruct_custo_por_produto_entrega_mediciacao()) == input$input_analise_custo_por_produto_entrega_mediciacao], VALOR))+  ### Tentando fazer com que o eixo X mostre variáveis exibidas após o click
         geom_boxplot(fill = "blue")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$col_plot_custo_por_produto_entrega_mediciacao <- renderPlot({
     
     ggplot(TreeStruct_custo_por_produto_entrega_mediciacao(), aes(TreeStruct_custo_por_produto_entrega_mediciacao()[,names(TreeStruct_custo_por_produto_entrega_mediciacao()) == input$input_analise_custo_por_produto_entrega_mediciacao], VALOR))+
         geom_col(fill = "blue", position = "stack")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
      
###########################################################################################
#Custos por Equipe - ESF
###########################################################################################
output$outup_sunburst_unidade_custo_por_equipe_esf <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO EQUIPE")
   a <- subset(a, a$VARIAVEL == "CUSTO_DIR_ESF")
   a <- a[,-c(7:9,11)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

output$output_sunburst_tipo_custo_custo_por_equipe_esf <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO EQUIPE")
   a <- a[,c(8,9,10)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

  network_custo_por_equipe_esf <- reactiveValues()
  
  observeEvent(input$d3_custo_por_equipe_esf_update,{
    network_custo_por_equipe_esf$nodes <- unlist(input$d3_custo_por_equipe_esf_update$.nodesData)
    activeNode <- input$d3_custo_por_equipe_esf_update$.activeNode
    if(!is.null(activeNode)) network_custo_por_equipe_esf$click <- jsonlite::fromJSON(activeNode)
  })
  
  observeEvent(network_custo_por_equipe_esf$click,{
    output$clickView<-renderDataTable({
      as.data.frame(network_custo_por_equipe_esf$click)
    },caption='Last Clicked Node',caption.placement='top')
  })
 
  
  TreeStruct_custo_por_equipe_esf<-eventReactive(network_custo_por_equipe_esf$nodes,{
    
    df <- subset(dados, dados$CATEGORIA == "CUSTO EQUIPE")
    if(is.null(network_custo_por_equipe_esf$nodes)){
      df <- subset(dados, dados$CATEGORIA == "CUSTO EQUIPE")
    }else{
      
      x.filter <- tree.filter(network_custo_por_equipe_esf$nodes,subset(dados, dados$CATEGORIA == "CUSTO EQUIPE"))
      df <- ddply(x.filter,.(ID),function(a.x){subset(dados, dados$CATEGORIA == "CUSTO EQUIPE")%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  observeEvent(input$hierarquia_custo_por_equipe_esf,{
    output$d3_custo_por_equipe_esf <- renderD3tree({
      if(is.null(input$hierarquia_custo_por_equipe_esf)){
        p <- subset(dados, dados$CATEGORIA == "CUSTO EQUIPE")
      }else{
        p <- subset(dados, dados$CATEGORIA == "CUSTO EQUIPE")%>%select(one_of(c(input$hierarquia_custo_por_equipe_esf,"NEWCOL")))%>%unique
      }
      
      d3tree(data = list(root = df2tree(struct = p,rootname = 'SMS Florianópolis'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 18)
    })
  })

  output$table_custo_por_equipe_esf <- renderDataTable(expr = {
    TreeStruct_custo_por_equipe_esf()%>%select(-NEWCOL)
  },
    escape=F,
    filter = "top",
    rownames= F,
    extensions = list("ColReorder" = NULL,
                      "Buttons" = NULL,
                      "FixedColumns" = list(leftColumns=1),
                      "Scroller" = NULL),
    options = list(
                dom = 'BRrltpi',
                autoWidth=TRUE,
                lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                ColReorder = TRUE,
                buttons =
                  list(
                    'print',
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                    I('colvis')
                  )
              )
  )
  

  
  output$box_plot_custo_por_equipe_esf <- renderPlotly({
     
      ggplot(TreeStruct_custo_por_equipe_esf(), aes(TreeStruct_custo_por_equipe_esf()[,names(TreeStruct_custo_por_equipe_esf()) == input$input_analise_custo_por_equipe_esf], VALOR))+  ### Tentando fazer com que o eixo X mostre variáveis exibidas após o click
         geom_boxplot(fill = "blue")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$col_plot_custo_por_equipe_esf <- renderPlot({
     
     ggplot(TreeStruct_custo_por_equipe_esf(), aes(TreeStruct_custo_por_equipe_esf()[,names(TreeStruct_custo_por_equipe_esf()) == input$input_analise_custo_por_equipe_esf], VALOR))+
         geom_col(fill = "blue", position = "stack")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })

###########################################################################################
#Custos por Equipe - ESB
###########################################################################################
output$outup_sunburst_unidade_custo_por_equipe_esb <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO EQUIPE")
   a <- subset(a, a$VARIAVEL == "CUSTO_DIR_ESB")
   a <- a[,-c(7:9,11)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

output$output_sunburst_tipo_custo_custo_por_equipe_esb <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO EQUIPE")
   a <- a[,c(8,9,10)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

  network_custo_por_equipe_esb <- reactiveValues()
  
  observeEvent(input$d3_custo_por_equipe_esb_update,{
    network_custo_por_equipe_esb$nodes <- unlist(input$d3_custo_por_equipe_esb_update$.nodesData)
    activeNode <- input$d3_custo_por_equipe_esb_update$.activeNode
    if(!is.null(activeNode)) network_custo_por_equipe_esb$click <- jsonlite::fromJSON(activeNode)
  })
  
  observeEvent(network_custo_por_equipe_esb$click,{
    output$clickView<-renderDataTable({
      as.data.frame(network_custo_por_equipe_esb$click)
    },caption='Last Clicked Node',caption.placement='top')
  })
 
  
  TreeStruct_custo_por_equipe_esb<-eventReactive(network_custo_por_equipe_esb$nodes,{
    
    df <- subset(dados, dados$CATEGORIA == "CUSTO EQUIPE")
    if(is.null(network_custo_por_equipe_esb$nodes)){
      df <- subset(dados, dados$CATEGORIA == "CUSTO EQUIPE")
    }else{
      
      x.filter <- tree.filter(network_custo_por_equipe_esb$nodes,subset(dados, dados$CATEGORIA == "CUSTO EQUIPE"))
      df <- ddply(x.filter,.(ID),function(a.x){subset(dados, dados$CATEGORIA == "CUSTO EQUIPE")%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  observeEvent(input$hierarquia_custo_por_equipe_esb,{
    output$d3_custo_por_equipe_esb <- renderD3tree({
      if(is.null(input$hierarquia_custo_por_equipe_esb)){
        p <- subset(dados, dados$CATEGORIA == "CUSTO EQUIPE")
      }else{
        p <- subset(dados, dados$CATEGORIA == "CUSTO EQUIPE")%>%select(one_of(c(input$hierarquia_custo_por_equipe_esb,"NEWCOL")))%>%unique
      }
      
      d3tree(data = list(root = df2tree(struct = p,rootname = 'SMS Florianópolis'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 18)
    })
  })

  output$table_custo_por_equipe_esb <- renderDataTable(expr = {
    TreeStruct_custo_por_equipe_esb()%>%select(-NEWCOL)
  },
    escape=F,
    filter = "top",
    rownames= F,
    extensions = list("ColReorder" = NULL,
                      "Buttons" = NULL,
                      "FixedColumns" = list(leftColumns=1),
                      "Scroller" = NULL),
    options = list(
                dom = 'BRrltpi',
                autoWidth=TRUE,
                lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                ColReorder = TRUE,
                buttons =
                  list(
                    'print',
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                    I('colvis')
                  )
              )
  )
  

  
  output$box_plot_custo_por_equipe_esb <- renderPlotly({
     
      ggplot(TreeStruct_custo_por_equipe_esb(), aes(TreeStruct_custo_por_equipe_esb()[,names(TreeStruct_custo_por_equipe_esb()) == input$input_analise_custo_por_equipe_esb], VALOR))+  ### Tentando fazer com que o eixo X mostre variáveis exibidas após o click
         geom_boxplot(fill = "blue")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$col_plot_custo_por_equipe_esb <- renderPlot({
     
     ggplot(TreeStruct_custo_por_equipe_esb(), aes(TreeStruct_custo_por_equipe_esb()[,names(TreeStruct_custo_por_equipe_esb()) == input$input_analise_custo_por_equipe_esb], VALOR))+
         geom_col(fill = "blue", position = "stack")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })  
  
    
###########################################################################################
#Custos Gerado - Médico
###########################################################################################
output$outup_sunburst_unidade_custo_gerado_medico <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO GERADO")
   a <- subset(a, a$VARIAVEL == "CUSTO_MED_GERADO")
   a <- a[,-c(7:9,11)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

output$output_sunburst_tipo_custo_custo_gerado_medico <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO GERADO")
   a <- a[,c(8,9,10)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

  network_custo_gerado_medico <- reactiveValues()
  
  observeEvent(input$d3_custo_gerado_medico_update,{
    network_custo_gerado_medico$nodes <- unlist(input$d3_custo_gerado_medico_update$.nodesData)
    activeNode <- input$d3_custo_gerado_medico_update$.activeNode
    if(!is.null(activeNode)) network_custo_gerado_medico$click <- jsonlite::fromJSON(activeNode)
  })
  
  observeEvent(network_custo_gerado_medico$click,{
    output$clickView<-renderDataTable({
      as.data.frame(network_custo_gerado_medico$click)
    },caption='Last Clicked Node',caption.placement='top')
  })
 
  
  TreeStruct_custo_gerado_medico<-eventReactive(network_custo_gerado_medico$nodes,{
    
    df <- subset(dados, dados$CATEGORIA == "CUSTO GERADO")
    if(is.null(network_custo_gerado_medico$nodes)){
      df <- subset(dados, dados$CATEGORIA == "CUSTO GERADO")
    }else{
      
      x.filter <- tree.filter(network_custo_gerado_medico$nodes,subset(dados, dados$CATEGORIA == "CUSTO GERADO"))
      df <- ddply(x.filter,.(ID),function(a.x){subset(dados, dados$CATEGORIA == "CUSTO GERADO")%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  observeEvent(input$hierarquia_custo_gerado_medico,{
    output$d3_custo_gerado_medico <- renderD3tree({
      if(is.null(input$hierarquia_custo_gerado_medico)){
        p <- subset(dados, dados$CATEGORIA == "CUSTO GERADO")
      }else{
        p <- subset(dados, dados$CATEGORIA == "CUSTO GERADO")%>%select(one_of(c(input$hierarquia_custo_gerado_medico,"NEWCOL")))%>%unique
      }
      
      d3tree(data = list(root = df2tree(struct = p,rootname = 'SMS Florianópolis'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 18)
    })
  })

  output$table_custo_gerado_medico <- renderDataTable(expr = {
    TreeStruct_custo_gerado_medico()%>%select(-NEWCOL)
  },
    escape=F,
    filter = "top",
    rownames= F,
    extensions = list("ColReorder" = NULL,
                      "Buttons" = NULL,
                      "FixedColumns" = list(leftColumns=1),
                      "Scroller" = NULL),
    options = list(
                dom = 'BRrltpi',
                autoWidth=TRUE,
                lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                ColReorder = TRUE,
                buttons =
                  list(
                    'print',
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                    I('colvis')
                  )
              )
  )
  

  
  output$box_plot_custo_gerado_medico <- renderPlotly({
     
      ggplot(TreeStruct_custo_gerado_medico(), aes(TreeStruct_custo_gerado_medico()[,names(TreeStruct_custo_gerado_medico()) == input$input_analise_custo_gerado_medico], VALOR))+  ### Tentando fazer com que o eixo X mostre variáveis exibidas após o click
         geom_boxplot(fill = "blue")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$col_plot_custo_gerado_medico <- renderPlot({
     
     ggplot(TreeStruct_custo_gerado_medico(), aes(TreeStruct_custo_gerado_medico()[,names(TreeStruct_custo_gerado_medico()) == input$input_analise_custo_gerado_medico], VALOR))+
         geom_col(fill = "blue", position = "stack")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })

###########################################################################################
#Custos Gerado - Dentista
###########################################################################################
output$outup_sunburst_unidade_custo_gerado_dentista <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO GERADO")
   a <- subset(a, a$VARIAVEL == "CUSTO_ODO_GERADO")
   a <- a[,-c(7:9,11)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

output$output_sunburst_tipo_custo_custo_gerado_dentista <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO GERADO")
   a <- a[,c(8,9,10)]

   while (ncol(a)>2){
      for(i in 1:nrow(a)){
         ifelse(!is.na(a[,2][i]), a[,1][i] <- paste0(a[,1][i], "-", a[,2][i]), a[,1][i] <- a[,1][i])
      }
      a <- a[,-2]
   }
   
   names(a) <- c("V1", "V2")
   a <- aggregate(a$V2, by = list(a$V1), FUN = sum)
   
   sunburst(a, legend = F, percent = T, count = T)
})

  network_custo_gerado_dentista <- reactiveValues()
  
  observeEvent(input$d3_custo_gerado_dentista_update,{
    network_custo_gerado_dentista$nodes <- unlist(input$d3_custo_gerado_dentista_update$.nodesData)
    activeNode <- input$d3_custo_gerado_dentista_update$.activeNode
    if(!is.null(activeNode)) network_custo_gerado_dentista$click <- jsonlite::fromJSON(activeNode)
  })
  
  observeEvent(network_custo_gerado_dentista$click,{
    output$clickView<-renderDataTable({
      as.data.frame(network_custo_gerado_dentista$click)
    },caption='Last Clicked Node',caption.placement='top')
  })
 
  
  TreeStruct_custo_gerado_dentista<-eventReactive(network_custo_gerado_dentista$nodes,{
    
    df <- subset(dados, dados$CATEGORIA == "CUSTO GERADO")
    if(is.null(network_custo_gerado_dentista$nodes)){
      df <- subset(dados, dados$CATEGORIA == "CUSTO GERADO")
    }else{
      
      x.filter <- tree.filter(network_custo_gerado_dentista$nodes,subset(dados, dados$CATEGORIA == "CUSTO GERADO"))
      df <- ddply(x.filter,.(ID),function(a.x){subset(dados, dados$CATEGORIA == "CUSTO GERADO")%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  observeEvent(input$hierarquia_custo_gerado_dentista,{
    output$d3_custo_gerado_dentista <- renderD3tree({
      if(is.null(input$hierarquia_custo_gerado_dentista)){
        p <- subset(dados, dados$CATEGORIA == "CUSTO GERADO")
      }else{
        p <- subset(dados, dados$CATEGORIA == "CUSTO GERADO")%>%select(one_of(c(input$hierarquia_custo_gerado_dentista,"NEWCOL")))%>%unique
      }
      
      d3tree(data = list(root = df2tree(struct = p,rootname = 'SMS Florianópolis'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 18)
    })
  })

  output$table_custo_gerado_dentista <- renderDataTable(expr = {
    TreeStruct_custo_gerado_dentista()%>%select(-NEWCOL)
  },
    escape=F,
    filter = "top",
    rownames= F,
    extensions = list("ColReorder" = NULL,
                      "Buttons" = NULL,
                      "FixedColumns" = list(leftColumns=1),
                      "Scroller" = NULL),
    options = list(
                dom = 'BRrltpi',
                autoWidth=TRUE,
                lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                ColReorder = TRUE,
                buttons =
                  list(
                    'print',
                    list(
                      extend = 'collection',
                      buttons = c('csv', 'excel'),
                      text = 'Download'
                    ),
                    I('colvis')
                  )
              )
  )
  

  
  output$box_plot_custo_gerado_dentista <- renderPlotly({
     
      ggplot(TreeStruct_custo_gerado_dentista(), aes(TreeStruct_custo_gerado_dentista()[,names(TreeStruct_custo_gerado_dentista()) == input$input_analise_custo_gerado_dentista], VALOR))+  ### Tentando fazer com que o eixo X mostre variáveis exibidas após o click
         geom_boxplot(fill = "blue")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$col_plot_custo_gerado_dentista <- renderPlot({
     
     ggplot(TreeStruct_custo_gerado_dentista(), aes(TreeStruct_custo_gerado_dentista()[,names(TreeStruct_custo_gerado_dentista()) == input$input_analise_custo_gerado_dentista], VALOR))+
         geom_col(fill = "blue", position = "stack")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })



###########################################################################################
}    
###########################################################################################
shinyApp(ui, server)


