# Sala de Situação de Custos da Secretaria Municipal de Saúde de Florianópolis

A Sala de Situação de Custos da Secretaria Municipal de Saúde de Florianópolis foi construída a partir de dados de 2017.

Nela, os dados estão organizados para exibição de informações com relação:

* ao tipo de gasto

* ao custo por centros de custo

* ao custo por produtos
  * consultas médicas  
  * consultas de enfermagem  
  * atendimentos de técnicos de enfermagem  
  * procedimentos odontológicos  
  * dispensação de medicamentos

* ao custo por equipes
  * equipe de saúde da família
  * equipe de saúde bucal
  
* ao custo gerado por:
  * centros de custo médicos
  * centros de custos odontológicos
  
Como os dados estão estruturados hierarquicamente, de forma a permitir o drill down, de informações, é importante que se conheça a hieraquia para se realizar uma correta intepretação ou busca das informações. A estrutura está organizada por:

* Tipo de Unidade (UNI_0, UNI_1, UNI_2, UNI_3, UNI_4, UNI_5)
 * UNI_0: Florianópolis
  * UNI_1: Atenção em Saúde
    * UNI_2: Atenção Primária
      * UNI_3: Distritos
        * UNI_4: Sede do Distrito
          * UNI_5: Sede dos Distrios Individuais
        * UNI_4: Centros de Saúde
          * UNI_5: Centros de Saúde Individuais
      * UNI_3: NASF
    * UNI_2: Unidades externas
    * UNI_2: Farmácia escola
    * UNI_2: Atenção Especializada
      * UNI_3: UPAs
        * UNI_4: UPAS Individuais
      * UNI_3: Policlínicas
        * UNI_4: Policlínicas Individuais
      * UNI_3: CEOS
        * UNI_4: CEOS Individuais 
      * UNI_3: CAPS
        * UNI_4: CAPS Individuais
      * UNI_3: SAMU
  * UNI_1: Vigilância em Saúde
    * UNI_2: Vigilância Epidemiológica
    * UNI_2: Vigilância Sanitária
    * UNI_2: Vigilância Ambiental
    * UNI_2: LAMUF
  * UNI_1: Nível Central
    * UNI_2: Almoxarifado
    * UNI_2: SEDE SMS
  * UNI_1: Conselho Municipal de Saúde
  * UNI_1: DIBEA

* Tipo de Custo (Os custos estão agrupados em 5 categorias, que agrupam Variáveis da seguinte forma)
 * CAT_1: Gastos com RH 
    * Remuneração de ACSs e ACEs
    * Remuneração de Técnicos Administrativos
    * Remuneração de Enfermeiros
    * Remuneração de Médicos
    * Remuneração de Profissionais do NASF
    * Remuneração de Odontólogos
    * Remuneração de Técnicos de Enfermagem
    * Remuneração de Técnicos de Odontologia
    * Remuneração de Outros Profissionais
    * Hora-extra de ACSs e ACEs 
    * Hora-extra de Técnicos Administrativos
    * Hora-extra de Enfermeiros
    * Hora-extra de Médicos
    * Hora-extra de Profissionais do NASF
    * Hora-extra dee Odontólogos
    * Hora-extra de Técnicos de Enfermagem
    * Hora-extra de Técnicos de Odontologia
    * Hora-extra de Outros Profissionais
  * CAT 2: Gasto Geral  
    * Aluguel	
    * Água	
    * Luz	
    * Central Telefônica	
    * Telefone	
    * Internet	
    * Sistema de Registro Eletrônico em Saude	
    * Impressão	
    * Lavanderia	
    * Limpeza	
    * Segurança	
    * Veículos	
    * Manutenção de Equipamentos	
    * Manutenção Predial	
    * Construção	
    * Diversos
  * CAT 3: Gasto Material  
    * Oxigênio	
    * Acs	
    * Acupuntura	
    * Enfermagem	
    * EPI	
    * Formula	
    * Gêneros Alimentícios	
     * Higiene E Coparia	
    * Impressos	
    * Judicial	
    * Médico	
    * Odontológico	
    * Outros	
    * Rouparia	
    * Uniformes
  * CAT 4: Gasto Medicamentos  
    * Medicamentos Básicos
    * Medicamentos Controlados
    * Medicamentos Estratégicos	
  * CAT 5: Exames  
    * Contratação de Exames e Consultas	

 * Tipo de produtos
    * Consultas médicas
    * Consultas de enfermagem
    * Atendimentos de técnicos de enfermagem
    * Procedimentos odontológicos
    * Dispensação de medicamentos
    
 * Tipo de equipes
    * Equipe de Saúde da Família
    * Equipe de Saúde Bucal
    
* Tipo de gastos gerados
    * Por centro de custo médico
    * Por centro de custo odontológico


 * Tipo de produtos
    * Consultas médicas
    * Consultas de enfermagem
    * Atendimentos de técnicos de enfermagem
    * Procedimentos odontológicos
    * Dispensação de medicamentos
    
 * Tipo de equipes
    * Equipe de Saúde da Família
    * Equipe de Saúde Bucal
    
* Tipo de gastos gerados
    * Por centro de custo médico
    * Por centro de custo odontológico


