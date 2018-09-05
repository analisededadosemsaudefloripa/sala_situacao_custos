options(encoding = "UTF-8")

########################################################################################### 
#pacotes 
###########################################################################################
library(readr)
library(shiny)
library(ggplot2)
library(shinydashboard)
library(tidyverse)
library(htmltools)
library(readr)
library(stringr)
library(DT)
library(plotly)
library(reshape2)
library(treemap)
library(d3treeR)

set.seed(1)
########################################################################################### 
#Banco de dados 
###########################################################################################
dados <- read_csv("bases/banco_custo.csv")
senhas <- read_csv("bases/senhas.csv")
###########################################################################################
#Login
###########################################################################################
#Baseado em https://stackoverflow.com/questions/43404058/starting-shiny-app-after-password-input-with-shinydashboard
Logged = FALSE
my_username <- senhas$nome
my_password <- senhas$senha
###########################################################################################
#Formação das treemap
###########################################################################################
#Modelo de treemap interativo com click
#https://github.com/d3treeR/d3treeR/issues/9

tm_custo_tipo_unidade <- treemap(banco_custo,
                                index=c("UNI_0","UNI_1","UNI_2","UNI_3","UNI_4","UNI_5", "VARIAVEL_GASTO"),
                                vSize="VALOR_GASTO",
                                vColor = "TIPO_GASTO",
                                type="categorical",
                                fontsize.labels=12,
                                lowerbound.cex.labels=1,
                                draw = F,
                                palette = "Set1")

tm_unidade_tipo_custo <- treemap(banco_custo,
                                index=c("TIPO_GASTO","VARIAVEL_GASTO","UNI_5"),
                                vSize="VALOR_GASTO",
                                vColor = "VARIAVEL_GASTO",
                                type="categorical",
                                fontsize.labels=12,
                                lowerbound.cex.labels=1,
                                draw = F,
                                palette = "Set1")

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
            menuItem("Custo por tipo de unidade",tabName = "custo_tipo_unidade", icon = icon("dashboard"),
                     menuSubItem("Condensado", tabName = "condensado_custo_tipo_unidade"),
                     menuSubItem("Expandido", tabName = "expandido_custo_tipo_unidade")),
            menuItem("Unidade por tipo de custo",tabName = "unidade_tipo_custo", icon = icon("dashboard"),
                     menuSubItem("Condensado", tabName = "condensado_unidade_tipo_custo"),
                     menuSubItem("Expandido", tabName = "expandido_unidade_tipo_custo")),
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
            #Custos por Tipo de Unidade - Condensado
            ###########################################################################################                         
            #para modelos condensados d3tree2, para expandidos d3tree
            tabItem(tabName = "condensado_custo_tipo_unidade", h2("Custos por Tipo de Unidade"),
                    
                    fluidRow(
                      tabBox(title = "Custos", width=12,
                             tabPanel("Gráfico", d3tree2Output("tree_condensado_custo_tipo_unidade")),
                             tabPanel("Dados", textOutput( "clickedinfo_condensado_custo_tipo_unidade")),
                             tabPanel("Sobre os Custos", htmlOutput("info_condensado_custo_tipo_unidade"))
                      )
                    )
            ), 
            ###########################################################################################
            #Custos por Tipo de Unidade - Expandido
            ###########################################################################################                         
            #para modelos condensados d3tree2, para expandidos d3tree
            tabItem(tabName = "expandido_custo_tipo_unidade", h2("Custos por Tipo de Unidade"),
                    
                    fluidRow(
                      tabBox(title = "Custos", width=12,
                             tabPanel("Gráfico", d3treeOutput("tree_expandido_custo_tipo_unidade")),
                             tabPanel("Dados", textOutput( "clickedinfo_expandido_custo_tipo_unidade")),
                             tabPanel("Sobre os Custos", htmlOutput("info_expandido_custo_tipo_unidade"))
                      )
                    )
            ), 
            ###########################################################################################
            #Unidade por Tipo de Custo - Condensado
            ###########################################################################################                         
            #para modelos condensados d3tree2, para expandidos d3tree
            tabItem(tabName = "condensado_unidade_tipo_custo", h2("Unidade por Tipo de Custo"),
                    
                    fluidRow(
                      tabBox(title = "Custos", width=12,
                             tabPanel("Gráfico", d3tree2Output("tree_condensado_unidade_tipo_custo")),
                             tabPanel("Dados", textOutput( "clickedinfo_condensado_unidade_tipo_custo")),
                             tabPanel("Sobre os Custos", htmlOutput("info_condensado_unidade_tipo_custo"))
                      )
                    )
            ), 
            ###########################################################################################
            #Unidade por Tipo de Custo - Expandido
            ###########################################################################################                         
            #para modelos condensados d3tree2, para expandidos d3tree
            tabItem(tabName = "expandido_unidade_tipo_custo", h2("Unidade por Tipo de Custo"),
                    
                    fluidRow(
                      tabBox(title = "Custos", width=12,
                             tabPanel("Gráfico", d3treeOutput("tree_expandido_unidade_tipo_custo")),
                             tabPanel("Dados", textOutput( "clickedinfo_expandido_unidade_tipo_custo", container = p )),
                             tabPanel("Sobre os Custos", htmlOutput("info_expandido_unidade_tipo_custo"))
                      )
                    )
            )
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
#Custos por Tipo de Unidade - Condensado
###########################################################################################
#para modelos condensados d3tree2, para expandidos d3tree
output$tree_condensado_custo_tipo_unidade <- renderD3tree2({
   d3tree2(tm_custo_tipo_unidade, rootname = "Secretaria Municipal de Saúde de Florianópolis") 
})

v_condensado_custo_tipo_unidade <- reactiveValues(msg = "")

observeEvent(input$tree_condensado_custo_tipo_unidade_click, {
   v_condensado_custo_tipo_unidade$msg <- paste("Clicked ", input$tree_condensado_custo_tipo_unidade_click$name)
})

output$clickedinfo_condensado_custo_tipo_unidade <- renderText(v_condensado_custo_tipo_unidade$msg)


###########################################################################################
#Custos por Tipo de Unidade - Expandido
###########################################################################################
#para modelos condensados d3tree2, para expandidos d3tree
output$tree_expandido_custo_tipo_unidade <- renderD3tree({
   d3tree(tm_custo_tipo_unidade, rootname = "Secretaria Municipal de Saúde de Florianópolis") 
})

v_expandido_custo_tipo_unidade <- reactiveValues(msg = "")

observeEvent(input$tree_expandido_custo_tipo_unidade_click, {
   v_expandido_custo_tipo_unidade$msg <- paste("Clicked ", input$tree_expandido_custo_tipo_unidade_click$name)
})

output$clickedinfo_expandido_custo_tipo_unidade <- renderText(v_expandido_custo_tipo_unidade$msg)


###########################################################################################
#Unidade por Tipo de Custo - Condensado
###########################################################################################
#para modelos condensados d3tree2, para expandidos d3tree
output$tree_condensado_unidade_tipo_custo <- renderD3tree2({
   d3tree2(tm_unidade_tipo_custo, rootname = "Secretaria Municipal de Saúde de Florianópolis") 
})

v_condensado_unidade_tipo_custo <- reactiveValues(msg = "")

observeEvent(input$tree_condensado_unidade_tipo_custo_click, {
   v_condensado_unidade_tipo_custo$msg <- paste("Clicked ", input$tree_condensado_unidade_tipo_custo_click$name)
})

output$clickedinfo_condensado_unidade_tipo_custo <- renderText(v_condensado_unidade_tipo_custo$msg)

###########################################################################################
#Unidade por Tipo de Custo - Expandido
###########################################################################################
#para modelos condensados d3tree2, para expandidos d3tree
output$tree_expandido_unidade_tipo_custo <- renderD3tree({
   d3tree(tm_unidade_tipo_custo, rootname = "Secretaria Municipal de Saúde de Florianópolis") 
})

# from https://github.com/rstudio/leaflet/blob/master/inst/examples/shiny.R
#v <- reactiveValues(msg = "")

#observeEvent(input$tree_expandido_click_unidade_tipo_custo, {
#   v$msg <- paste("Clicado em ", input$tree_expandido_click_unidade_tipo_custo$name)
#})

#output$clickedinfo_expandido_unidade_tipo_custo <- renderText(v$msg)




###########################################################################################
}    
###########################################################################################
shinyApp(ui, server)


