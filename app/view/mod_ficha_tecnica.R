box::use(
  bs4Dash[...],
  dplyr[
    arrange,
    collect,
    summarise,
    mutate,
    filter,
    pull,
    glimpse
  ],
  echarts4r[...],
  magrittr[`%>%`],
  stats[rnorm],
  shiny[...],
  utils[as.roman]
)
#' ficha_tecnica UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
ui <- function(id){
  ns <- NS(id)

  nome.cargo <- function(nome, cargo) {
    tagList(
      h5(class = "mb-0", nome),
      p(cargo)
    )
  }

  titulo.integrantes <- function(titulo, ...) {
    tagList(
      h5(class = "mb-0", titulo),
      lapply(list(...), function(x) { p(class = "mb-0", x) }),
      p(class = "mb-2", "")
    )
  }
  
  tagList(
    column(
      width = 12,
      bs4Dash::tabBox(
        title = 'Ficha Técnica',
        side = 'right',
        id = 'ficha-tecnica',
        type = 'tabs',
        elevation = 2,
        width = 12,
        maximizable = TRUE,
        selected = 'equipe-tecnica',
        tabPanel(
          title = "Equipe Técnica",
          value = "equipe-tecnica",
          class = "p-2",
          fluidRow(
            class = "align-center justify-content-center text-center mb-3",
            column(
              width = 2,
              titulo.integrantes(
                "Coordenado por",
                "Elisângela Nogueira Teixeira",
                "Victor Pereira Do Nascimento Santos"
              ),
              titulo.integrantes(
                "Edição",
                "Elisângela Nogueira Teixeira"
              )
            ),
            column(
              width = 2,
              titulo.integrantes(
                "Autores principais",
                "Elisângela Nogueira Teixeira",
                "José Élcio Batista",
                "Victor Pereira Do Nascimento Santos"
              )
            ),
            column(
              width = 2,
              titulo.integrantes(
                "Equipe de pesquisa",
                "Anderson Passos Bezerra",
                "Ellen Garcia da Silveira",
                "Maria Gabrielle Sousa de Santana",
                "Rômulo Andrade da Silva"
              ),
              titulo.integrantes(
                "Coordenação de dados",
                "Victor Pereira Do Nascimento Santos"
              )
            ),
            column(
              width = 2,
              titulo.integrantes(
                "Análise Estatística",
                "Anderson Passos Bezerra",
                "Rômulo Andrade da Silva",
                "Victor Pereira Do Nascimento Santos"
              ),
              titulo.integrantes(
                "Diagramação",
                "Elisângela Nogueira Teixeira",
                "Maria Evilene Avelino da Silva"
              )
            ),
            column(
              width = 2,              
              titulo.integrantes(
                "Revisão",
                "Elisângela Nogueira Teixeira"
              ),
              titulo.integrantes(
                "Imprensa",
                "Rayana Vasconcelos da Costa",
                "Rebecca Fontes Martins Leitão"
              )
            ),
            column(
              width = 2,
              titulo.integrantes(
                "Suporte técnico",
                "Ana Cláudia de Vasconcelos Teixeira",
                "Augusto César de Sousa Feitosa",
                "Jessé do Nascimento Pereira"
              ),             
              titulo.integrantes(
                "Design",
                "Miligrama (Edição)",
                "Elisângela Nogueira Teixeira"
              )
            )
          ),
          fluidRow(
            class = 'align-center justify-content-center text-center mt-2',
            column(
              width = 12,
              h4("Realização"),
              hr(class = "divider mt-1 mb-1")
            )
          ),
          fluidRow(
            class = 'align-center justify-content-center text-center mt-2',
            column(
              width = 12,
              p("Diretoria do Observatório de Governança Municipal"),
              p("Instituto de Planejamento de Fortaleza"),
              p("Prefeitura Municipal de Fortaleza")
            )
          ),
          fluidRow(
            class = "align-center justify-content-center text-center mt-2",
            column(
              width = 12,
              h4("Colaboração"),
              hr(class = "divider mt-1 mb-1")
            )
          ),
          fluidRow(
            class = "align-center justify-content-center text-center mt-2",
            column(
              width = 4,
              p("Centro Cultural Bom Jardim"),
              p("Escola de Gastronomia Social Ivens Dias Branco"),
              p("Federação das Indústrias do Estado do Ceará"),
              p("Gabinete do Vice-Prefeito de Fortaleza")
            ),
            column(
              width = 4,
              p("Instituto Dragão do Mar"),
              p("Instituto Municipal de Desenvolvimento de Recursos Humanos"),
              p("íris - Laboratório de Inovação e Dados do Estado do Ceará"),
              p("Citinova - Fundação de Ciência, Tecnologia e Inovação de Fortaleza")
            ),
            column(
              width = 4,
              p("Secretaria Especial de Políticas Públicas de Juventude"),
              p("Secretaria Estadual de Educação"),
              p("Secretaria Municipal da Educação"),
              p("Secretaria Municipal do Desenvolvimento Econômico")
            )
          )
        ),
        tabPanel(
          title = "Secretariado",
          value = "secretariado",
          fluidRow(
            column(
              width = 12,
              class = "mt-2 text-center",
              h4("José Sarto Nogueira Moreira"),
              p("Prefeito de Fortaleza"),
              br(),
              h4("José Élcio Batista"),
              p("Vice-Prefeito de Fortaleza"),
              hr(class = "divider")
            )
          ),
          fluidRow(
            class = "text-center",
            column(
              width = 4,
              nome.cargo("Renato Carvalho Borges", "Secretário Chefe do Gabinete do Prefeito"),
              nome.cargo("Renato César Pereira Lima", "Secretário Municipal de Governo"),
              nome.cargo("Fernando Antônio Costa De Oliveira", "Procurador Geral do Município"),
              nome.cargo("Maria Christina Machado Publio", "Secretária Chefe da Controladoria e Ouvidoria Geral do Município"),
              nome.cargo("Luis Eduardo Soares de Holanda", "Secretário Municipal da Segurança Cidadã"),
              nome.cargo("Flávia Roberta Bruno Teixeira", "Secretária Municipal das Finanças"),
              nome.cargo("Marcelo Jorge Borges Pinheiro", "Secretário Municipal do Planejamento, Orçamento e Gestão")
            ),
            column(
              width = 4,
              nome.cargo("Antonia Dalila Saldanha de Freitas", "Secretária Municipal da Educação"),
              nome.cargo("Ana Estela Fernandes Leite", "Secretária Municipal da Saúde"),
              nome.cargo("Samuel Antonio Silva Dias", "Secretário Municipal da Infraestrutura"),                            
              nome.cargo("Ferruccio Petri Feitosa", "Secretário Municipal da Conservação e Serviços Públicos"),
              nome.cargo("Ozires Andrade Pontes", "Secretário Municipal do Esporte e Lazer"),                           
              nome.cargo("Rodrigo Nogueira Diogo de Siqueira", "Secretário Municipal do Desenvolvimento Econômico")
            ),
            column(
              width = 4,
              nome.cargo("Luciana Mendes Lobo", "Secretária Municipal do Urbanismo e Meio Ambiente"),              
              nome.cargo("Alexandre Pereira Lima", "Secretário Municipal do Turismo de Fortaleza"),
              nome.cargo("José Ilário Gonçalves Marques", "Secretário Municipal dos Direitos Humanos e Desenvolvimento Social"),
              nome.cargo("Francisco Adail de Carvalho Fontenele", "Secretário Municipal de Desenvolvimento Habitacional"),              
              nome.cargo("Elpídio Nogueira Moreira", "Secretário Municipal da Cultura"),              
              nome.cargo("Davi Gomes Barroso", "Secretário Municipal da Juventude"),
              nome.cargo("João De Aguiar Pupo", "Secretário Municipal da Gestão Regional")
            )
          )
        )        
      )
    )
  )
}
    
#' ficha_tecnica Server Functions
#'
#' @noRd 
server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ficha_tecnica_ui("ficha_tecnica_1")
    
## To be copied in the server
# mod_ficha_tecnica_server("ficha_tecnica_1")
