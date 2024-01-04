box::use(
  dplyr[group_by],
  echarts4r[
    e_charts,
    e_title,
    e_line,
    e_bar,
    e_toolbox_feature,
    e_x_axis,
    e_axis_labels,
    e_y_axis,
    echarts4rOutput,
    e_tooltip,
    renderEcharts4r],
  shiny[...],
 readxl[read_xlsx],lubridate,nycflights13,dplyr[mutate,arrange],readr[parse_number,locale]
)

#' @export
ui <- function(id) {  
  ns <- NS(id)


  jsCode <- "const customHref = function(link){
        const links = document.getElementsByTagName('a');
        Object.entries(links).forEach( (elem, i) => {
                if(elem[1].getAttribute('data-value') === link){
                        elem[1].click()
                }
        });
}"
  
  tagList(
    fluidPage(
      # tags$h1("Panorama da Gestão de Resíduos sólidos de Fortaleza"),
      # tags$p(" "),
      # tags$p("A Prefeitura de Fortaleza tem intensificado esforços para aprimorar a gestão de resíduos sólidos na cidade, abordando desafios persistentes com soluções inovadoras 
      #      e sustentáveis. Dentre as iniciativas, destaca-se a expansão da coleta seletiva e programas de reciclagem, fundamentais para o manejo consciente dos resíduos gerados 
      #      pela população. Educação ambiental também é uma pauta prioritária, visando a sensibilização dos cidadãos quanto à separação adequada de resíduos e redução do uso de 
      #      materiais descartáveis. Esforços contínuos são realizados para a erradicação de lixões a céu aberto, substituindo-os por aterros sanitários adequados. Visitem nossa 
      #      página para visualizar dashboards atualizados e compreender melhor as estratégias e avanços da nossa cidade na gestão de resíduos sólidos. Juntos, construiremos uma 
      #      Fortaleza mais limpa e sustentável." ),
      # 
      
      
      
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = jsCode, functions = c()),
      fluidRow(
        class = "image-container d-flex justify-content-center align-items-center", # Adicionada a classe image-container e outras classes de utilidade do Bootstrap
        div(
          class = "text-center mb-5 mt-3", # Classe text-center para centralizar a imagem e mb-3 para dar uma margem abaixo da imagem
          img(class = "img-fluid logo_maisfort", src="obsr_maisfort.png")
        ),
        div(
          class = "col-lg-12 d-flex flex-wrap justify-content-around mt-5", # Mudado para col-lg-12 e adicionado d-flex e justify-content-around
          a(
            class = "imglink mr-1",
            href = "#residometro",
            onclick = "customHref('residometro')",
            img(class = "img-fluid rounded mb-2",
                src="residuometro.png")
          ),
          a(
            class = "imglink mr-1",
            href = "#reciclometro",
            onclick = "customHref('reciclometro')",
            img(class = "img-fluid rounded mb-2",
                src="reciclometro.png")
          ),
          a(
            class = "imglink mr-1",
            href = "#coleta",
            onclick = "customHref('pontos_coleta')",
            img(class = "img-fluid rounded mb-2",
                src="coleta.png")
          ),
          a(
            class = "imglink",
            href = "#entrega",
            onclick = "customHref('pontos_entrega')",
            img(class = "img-fluid rounded mb-2",
                src="entrega.png")
          )                
        ),

      )
    )    
  )
  
}







#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    })    
}


