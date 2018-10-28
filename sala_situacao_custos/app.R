options(encoding = "UTF-8")

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
            menuItem("Custos por Produto",tabName = "custo_por_produto", icon = icon("dashboard")),
            menuItem("Custos por Equipe",tabName = "custo_por_equipe", icon = icon("dashboard")),
            menuItem("Custos Gerados",tabName = "custo_gerado", icon = icon("dashboard")),
            #menuItem("Instruções", icon = icon("question-circle"),
                     #href = "https://github.com/analisededadosemsaudefloripa/sala_situacao_custos"),
            #menuItem("Dados", icon = icon("database"),
                     #href = "http://floripadadosabertos.univille.br/"),
            menuItem("Código-fonte", icon = icon("code"), 
                     href = "https://github.com/lpgarcia18/lista_de_pacientes_ap"),
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
            #Custos por produto
            ###########################################################################################                         
            tabItem(tabName = "custo_por_produto", h2("Análise de Custos por Produto da Secretaria Municipal de Saúde"),
                    
                    fluidRow(
                             box(title = "Unidade", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("outup_sunburst_unidade_custo_por_produto")),
                             box(title = "Tipo de Custo", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("output_sunburst_tipo_custo_custo_por_produto"))
                    ),
                    fluidRow(
                             box(title = "Estrutura de Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    selectInput(
                                       inputId="input_analise_custo_por_produto",
                                       label="Selecione a análise:",
                                       choices=names(dados),
                                       selected="VARIAVEL"),
                                    selectizeInput(
                                        inputId="hierarquia_custo_por_produto",
                                        label="Monte a estrutura de análise:",
                                        choices = names(dados),
                                        multiple = T, 
                                        selected = "",
                                        options = list(plugins = list('drag_drop','remove_button'))),
                                    d3treeOutput(outputId="d3_custo_por_produto"))
                             
                    ),
                    fluidRow(
                             box(title = "Acumulado", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotOutput("col_plot_custo_por_produto")),
                             box(title = "Distribuição", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotlyOutput("box_plot_custo_por_produto"))
                    ),
                    fluidRow(
                             box(title = "Dados dos Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    dataTableOutput('table_custo_por_produto'))
                             
                    )
            ),
            ###########################################################################################
            #Custos por equipe
            ###########################################################################################                         
            tabItem(tabName = "custo_por_equipe", h2("Análise de Custos por ESF e ESB da Secretaria Municipal de Saúde"),
                    
                    fluidRow(
                             box(title = "Unidade", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("outup_sunburst_unidade_custo_por_equipe")),
                             box(title = "Tipo de Custo", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("output_sunburst_tipo_custo_custo_por_equipe"))
                    ),
                    fluidRow(
                             box(title = "Estrutura de Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    selectInput(
                                       inputId="input_analise_custo_por_equipe",
                                       label="Selecione a análise:",
                                       choices=names(dados),
                                       selected="VARIAVEL"),
                                    selectizeInput(
                                        inputId="hierarquia_custo_por_equipe",
                                        label="Monte a estrutura de análise:",
                                        choices = names(dados),
                                        multiple = T, 
                                        selected = "",
                                        options = list(plugins = list('drag_drop','remove_button'))),
                                    d3treeOutput(outputId="d3_custo_por_equipe"))
                             
                    ),
                    fluidRow(
                             box(title = "Acumulado", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotOutput("col_plot_custo_por_equipe")),
                             box(title = "Distribuição", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotlyOutput("box_plot_custo_por_equipe"))
                    ),
                    fluidRow(
                             box(title = "Dados dos Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    dataTableOutput('table_custo_por_equipe'))
                             
                    )
            ),
            ###########################################################################################
            #Custos Gerado
            ###########################################################################################                         
            tabItem(tabName = "custo_gerado", h2("Análise de Custos Gerado por Médicos e Dentistas da Aten. Prim. da Secretaria Municipal de Saúde"),
                    
                    fluidRow(
                             box(title = "Unidade", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("outup_sunburst_unidade_custo_gerado")),
                             box(title = "Tipo de Custo", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 sunburstOutput("output_sunburst_tipo_custo_custo_gerado"))
                    ),
                    fluidRow(
                             box(title = "Estrutura de Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    selectInput(
                                       inputId="input_analise_custo_gerado",
                                       label="Selecione a análise:",
                                       choices=names(dados),
                                       selected="VARIAVEL"),
                                    selectizeInput(
                                        inputId="hierarquia_custo_gerado",
                                        label="Monte a estrutura de análise:",
                                        choices = names(dados),
                                        multiple = T, 
                                        selected = "",
                                        options = list(plugins = list('drag_drop','remove_button'))),
                                    d3treeOutput(outputId="d3_custo_gerado"))
                             
                    ),
                    fluidRow(
                             box(title = "Acumulado", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotOutput("col_plot_custo_gerado")),
                             box(title = "Distribuição", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE,
                                 plotlyOutput("box_plot_custo_gerado"))
                    ),
                    fluidRow(
                             box(title = "Dados dos Gastos", status = "primary", solidHeader = TRUE,
                                 collapsible = TRUE, width = 12,
                                    dataTableOutput('table_custo_gerado'))
                             
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
#Custos por Produto
###########################################################################################
output$outup_sunburst_unidade_custo_por_produto <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO PRODUTO")
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

output$output_sunburst_tipo_custo_custo_por_produto <- renderSunburst({
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

  network_custo_por_produto <- reactiveValues()
  
  observeEvent(input$d3_custo_por_produto_update,{
    network_custo_por_produto$nodes <- unlist(input$d3_custo_por_produto_update$.nodesData)
    activeNode <- input$d3_custo_por_produto_update$.activeNode
    if(!is.null(activeNode)) network_custo_por_produto$click <- jsonlite::fromJSON(activeNode)
  })
  
  observeEvent(network_custo_por_produto$click,{
    output$clickView<-renderDataTable({
      as.data.frame(network_custo_por_produto$click)
    },caption='Last Clicked Node',caption.placement='top')
  })
 
  
  TreeStruct_custo_por_produto<-eventReactive(network_custo_por_produto$nodes,{
    
    df <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
    if(is.null(network_custo_por_produto$nodes)){
      df <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
    }else{
      
      x.filter <- tree.filter(network_custo_por_produto$nodes,subset(dados, dados$CATEGORIA == "CUSTO PRODUTO"))
      df <- ddply(x.filter,.(ID),function(a.x){subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  observeEvent(input$hierarquia_custo_por_produto,{
    output$d3_custo_por_produto <- renderD3tree({
      if(is.null(input$hierarquia_custo_por_produto)){
        p <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")
      }else{
        p <- subset(dados, dados$CATEGORIA == "CUSTO PRODUTO")%>%select(one_of(c(input$hierarquia_custo_por_produto,"NEWCOL")))%>%unique
      }
      
      d3tree(data = list(root = df2tree(struct = p,rootname = 'SMS Florianópolis'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 18)
    })
  })

  output$table_custo_por_produto <- renderDataTable(expr = {
    TreeStruct_custo_por_produto()%>%select(-NEWCOL)
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
  

  
  output$box_plot_custo_por_produto <- renderPlotly({
     
      ggplot(TreeStruct_custo_por_produto(), aes(TreeStruct_custo_por_produto()[,names(TreeStruct_custo_por_produto()) == input$input_analise_custo_por_produto], VALOR))+  ### Tentando fazer com que o eixo X mostre variáveis exibidas após o click
         geom_boxplot(fill = "blue")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$col_plot_custo_por_produto <- renderPlot({
     
     ggplot(TreeStruct_custo_por_produto(), aes(TreeStruct_custo_por_produto()[,names(TreeStruct_custo_por_produto()) == input$input_analise_custo_por_produto], VALOR))+
         geom_col(fill = "blue", position = "stack")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
###########################################################################################
#Custos por Equipe
###########################################################################################
output$outup_sunburst_unidade_custo_por_equipe <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO EQUIPE")
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

output$output_sunburst_tipo_custo_custo_por_equipe <- renderSunburst({
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

  network_custo_por_equipe <- reactiveValues()
  
  observeEvent(input$d3_custo_por_equipe_update,{
    network_custo_por_equipe$nodes <- unlist(input$d3_custo_por_equipe_update$.nodesData)
    activeNode <- input$d3_custo_por_equipe_update$.activeNode
    if(!is.null(activeNode)) network_custo_por_equipe$click <- jsonlite::fromJSON(activeNode)
  })
  
  observeEvent(network_custo_por_equipe$click,{
    output$clickView<-renderDataTable({
      as.data.frame(network_custo_por_equipe$click)
    },caption='Last Clicked Node',caption.placement='top')
  })
 
  
  TreeStruct_custo_por_equipe<-eventReactive(network_custo_por_equipe$nodes,{
    
    df <- subset(dados, dados$CATEGORIA == "CUSTO EQUIPE")
    if(is.null(network_custo_por_equipe$nodes)){
      df <- subset(dados, dados$CATEGORIA == "CUSTO EQUIPE")
    }else{
      
      x.filter <- tree.filter(network_custo_por_equipe$nodes,subset(dados, dados$CATEGORIA == "CUSTO EQUIPE"))
      df <- ddply(x.filter,.(ID),function(a.x){subset(dados, dados$CATEGORIA == "CUSTO EQUIPE")%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  observeEvent(input$hierarquia_custo_por_equipe,{
    output$d3_custo_por_equipe <- renderD3tree({
      if(is.null(input$hierarquia_custo_por_equipe)){
        p <- subset(dados, dados$CATEGORIA == "CUSTO EQUIPE")
      }else{
        p <- subset(dados, dados$CATEGORIA == "CUSTO EQUIPE")%>%select(one_of(c(input$hierarquia_custo_por_equipe,"NEWCOL")))%>%unique
      }
      
      d3tree(data = list(root = df2tree(struct = p,rootname = 'SMS Florianópolis'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 18)
    })
  })

  output$table_custo_por_equipe <- renderDataTable(expr = {
    TreeStruct_custo_por_equipe()%>%select(-NEWCOL)
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
  

  
  output$box_plot_custo_por_equipe <- renderPlotly({
     
      ggplot(TreeStruct_custo_por_equipe(), aes(TreeStruct_custo_por_equipe()[,names(TreeStruct_custo_por_equipe()) == input$input_analise_custo_por_equipe], VALOR))+  ### Tentando fazer com que o eixo X mostre variáveis exibidas após o click
         geom_boxplot(fill = "blue")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$col_plot_custo_por_equipe <- renderPlot({
     
     ggplot(TreeStruct_custo_por_equipe(), aes(TreeStruct_custo_por_equipe()[,names(TreeStruct_custo_por_equipe()) == input$input_analise_custo_por_equipe], VALOR))+
         geom_col(fill = "blue", position = "stack")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
###########################################################################################
#Custos Gerado
###########################################################################################
output$outup_sunburst_unidade_custo_gerado <- renderSunburst({
   a <- dados
   a <- subset(a, a$CATEGORIA == "CUSTO GERADO")
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

output$output_sunburst_tipo_custo_custo_gerado <- renderSunburst({
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

  network_custo_gerado <- reactiveValues()
  
  observeEvent(input$d3_custo_gerado_update,{
    network_custo_gerado$nodes <- unlist(input$d3_custo_gerado_update$.nodesData)
    activeNode <- input$d3_custo_gerado_update$.activeNode
    if(!is.null(activeNode)) network_custo_gerado$click <- jsonlite::fromJSON(activeNode)
  })
  
  observeEvent(network_custo_gerado$click,{
    output$clickView<-renderDataTable({
      as.data.frame(network_custo_gerado$click)
    },caption='Last Clicked Node',caption.placement='top')
  })
 
  
  TreeStruct_custo_gerado<-eventReactive(network_custo_gerado$nodes,{
    
    df <- subset(dados, dados$CATEGORIA == "CUSTO GERADO")
    if(is.null(network_custo_gerado$nodes)){
      df <- subset(dados, dados$CATEGORIA == "CUSTO GERADO")
    }else{
      
      x.filter <- tree.filter(network_custo_gerado$nodes,subset(dados, dados$CATEGORIA == "CUSTO GERADO"))
      df <- ddply(x.filter,.(ID),function(a.x){subset(dados, dados$CATEGORIA == "CUSTO GERADO")%>%filter_(.dots = list(a.x$FILTER))%>%distinct})
    }
    df
  })
  
  observeEvent(input$hierarquia_custo_gerado,{
    output$d3_custo_gerado <- renderD3tree({
      if(is.null(input$hierarquia_custo_gerado)){
        p <- subset(dados, dados$CATEGORIA == "CUSTO GERADO")
      }else{
        p <- subset(dados, dados$CATEGORIA == "CUSTO GERADO")%>%select(one_of(c(input$hierarquia_custo_gerado,"NEWCOL")))%>%unique
      }
      
      d3tree(data = list(root = df2tree(struct = p,rootname = 'SMS Florianópolis'), layout = 'collapse'),activeReturn = c('name','value','depth','id'),height = 18)
    })
  })

  output$table_custo_gerado <- renderDataTable(expr = {
    TreeStruct_custo_gerado()%>%select(-NEWCOL)
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
  

  
  output$box_plot_custo_gerado <- renderPlotly({
     
      ggplot(TreeStruct_custo_gerado(), aes(TreeStruct_custo_gerado()[,names(TreeStruct_custo_gerado()) == input$input_analise_custo_gerado], VALOR))+  ### Tentando fazer com que o eixo X mostre variáveis exibidas após o click
         geom_boxplot(fill = "blue")+
         theme_classic()+
         ylab(" ")+
         xlab(" ")+
         theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$col_plot_custo_gerado <- renderPlot({
     
     ggplot(TreeStruct_custo_gerado(), aes(TreeStruct_custo_gerado()[,names(TreeStruct_custo_gerado()) == input$input_analise_custo_gerado], VALOR))+
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


