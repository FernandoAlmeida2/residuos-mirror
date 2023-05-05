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

box::use(
  ../mod/database[obsr],
  ../mod/utils[...]
)

# Internal auxiliar functions
## to_ton <- function(x) { x/1000 }
## br_format <- scales::label_comma(accuracy = 1, big.mark = ".", decimal.mark = ",")

update_data <- function(obsr, year) {
  today <- Sys.Date()
  curr_day  <- lubridate::mday(today)
  tblname <- sprintf("br_acfor_relatorio_pesagem_%04d", year)

  dplyr::tbl(obsr, tblname) %>%
    collect %>%
    mutate(
      dia = as.integer(substring(data_saida, 1, 2)),
      mes = as.integer(substring(data_saida, 4, 5)),
      ano = as.integer(substring(data_saida, 7, 10)),
      peso_liquido = as.numeric(peso_liquido)
    ) %>%
    filter(dia == curr_day)
}

filtra_regional <- function (x) {
  x %>%
    filter(stringr::str_detect(regional, "^SER")) %>%
    mutate(
      regional = sprintf("SR%02d",  as.integer(as.roman(stringr::str_trim(gsub("SER", "", regional)))))
    ) %>%
    mutate(
      regional = factor(regional, levels = )
    )
}

format_bar_plot <- function(x) {
  x %>%
    e_tooltip(
      formatter = htmlwidgets::JS("
            function(params) {
              return '<span>'
              + params.value[0] + '<br/>'
              + parseFloat(params.value[1]).toLocaleString('pt-BR')
              + ' t'
              + '</span>';
            }"
            )
    ) %>%
    e_y_axis(formatter = e_axis_formatter(locale = "pt-BR")) %>%
    e_legend(show = FALSE) %>%
    e_toolbox_feature(feature = c("dataView", "dataZoom", "saveAsImage"))
}

aba_residometro <- function(ns, x) {
  tagList(
    fluidRow(
      valueBox(
        value = textOutput(ns(sprintf("residometro_%s_total", x))),
        subtitle = "Total",
        color = "primary",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = textOutput(ns(sprintf("residometro_%s_total_coleta_domiciliar", x))),
        subtitle = "Coleta Domiciliar",
        color = "success",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = textOutput(ns(sprintf("residometro_%s_total_especial_urbana", x))),
        subtitle = "Coleta Especial Urbana",
        color = "success",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = textOutput(ns(sprintf("residometro_%s_total_podacao", x))),
        subtitle = "Poda",
        color = "success",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = textOutput(ns(sprintf("residometro_%s_total_entulho", x))),
        subtitle = "Entulho",
        color = "success",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = textOutput(ns(sprintf("residometro_%s_total_coleta_seletiva", x))),
        subtitle = "Coleta Seletiva",
        color = "success",
        icon = icon("trash"),
        width = 2
      )
    ),
    fluidRow(
      box(
        title = "Peso líquido por regional (t)", 
        elevation = 4,
        closable = FALSE, 
        width = 6,
        solidHeader = TRUE, 
        status = "primary",
        collapsible = FALSE,
        shinycssloaders::withSpinner(
          type = 8,
          color = "#0e2e45",
          echarts4rOutput(ns(sprintf("residometro_%s_regional", x)))
        )
      ),
      box(
        title = "Peso líquido por território (t)", 
        elevation = 4,
        closable = FALSE, 
        width = 6,
        solidHeader = TRUE, 
        status = "primary",
        collapsible = FALSE,
        shinycssloaders::withSpinner(
          type = 8,
          color = "#0e2e45",
          echarts4rOutput(ns(sprintf("residometro_%s_territorio", x)))
        )
      )
    )
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    tabBox(
      title = p(
        class="text-uppercase font-weight-bold",
        sprintf("Residômetro (Ano Base: %04d)", lubridate::year(Sys.Date()))
      ),
      elevation = 2,
      id = "residometro",
      width = 12,
      collapsible = FALSE,
      closable = FALSE,
      type = "tabs",
      status = "primary",
      solidHeader = TRUE,
      selected = "Diário",
      tabPanel(
        "Diário",
        aba_residometro(ns, "diario")
      ),
      tabPanel(
        "Mensal",
        aba_residometro(ns, "mensal")
      ),
      tabPanel(
        "Anual",
        aba_residometro(ns, "anual")
      )
    )
  )  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    today <- Sys.Date()
    curr_year <- lubridate::year(today)
    curr_month <- lubridate::month(today) - 2
    curr_day  <- lubridate::mday(today) - 2
    timeout_ms <- 5000 # Timeout for refresh daily data
    curr_year_tblname <- sprintf("br_acfor_relatorio_pesagem_%04d", curr_year)

    pesagem_anual <- dplyr::tbl(obsr, curr_year_tblname) %>%
      collect %>%
      mutate(
        dia = as.integer(substring(data_saida, 1, 2)),
        mes = as.integer(substring(data_saida, 4, 5)),
        ano = as.integer(substring(data_saida, 7, 10)),
        peso_liquido = as.numeric(peso_liquido)
      )

    pesagem_mensal <- pesagem_anual %>% filter(mes == curr_month)

    dados <- shiny::reactiveValues(
      pesagem_diaria = update_data(obsr, curr_year)
    )

    observe({
      now <- lubridate::dmy_hms(format(Sys.time(), "%d/%m/%Y %H:%M"))
      
      dados$pesagem_diaria <- update_data(obsr, curr_year)
      
      shiny::invalidateLater(timeout_ms, session)
    })

    e_common(theme = "roma")

    ############
    ## Diário ##
    ############
    output$residometro_diario_total <- renderText({

      total <- dados$pesagem_diaria %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      sprintf("%s t", br_format(total))
    })

    output$residometro_diario_total_coleta_domiciliar <- renderText({

      total <- dados$pesagem_diaria %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      total_domiciliar <- dados$pesagem_diaria %>%
        filter(material == "DOMICILIAR") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
        sprintf("%s t (%s%%)", br_format(total_domiciliar),
                br_format(100*total_domiciliar/total))
    })

    output$residometro_diario_total_especial_urbana <- renderText({

      total <- dados$pesagem_diaria %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
        
      total_ceu <- dados$pesagem_diaria %>%
        filter(material %in% c("ESPECIAL URBANA", "LIXO ESPECIAL URBANA", "LIXO ESPECIAL URBANA - MECANIZADA")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
          
      sprintf("%s t (%s%%)", br_format(total_ceu), br_format(100*total_ceu/total))
    })

    
    output$residometro_diario_total_podacao <- renderText({

      total <- dados$pesagem_diaria %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      total_podacao <- dados$pesagem_diaria %>%
        filter(material %in% c("CAPINA", "PODAÇÃO")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_podacao), br_format(100*total_podacao/total))
    })

    output$residometro_diario_total_entulho <- renderText({

      total <- dados$pesagem_diaria %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      total_entulho <- dados$pesagem_diaria %>%
        filter(material == "ENTULHO") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_entulho), br_format(100*total_entulho/total))
    })

    output$residometro_diario_total_coleta_seletiva <- renderText({

      total <- dados$pesagem_diaria %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      total_seletiva <- dados$pesagem_diaria %>%
        filter(material == "COLETA SELETIVA") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_seletiva/total
      
      sprintf("%s t (%s%%)", br_format(total_seletiva), if (perc < 1) "< 1" else br_format(perc))
    })

    output$residometro_diario_regional <- renderEcharts4r({
      dados$pesagem_diaria %>%
        filtra_regional %>%
        group_by(regional) %>% 
        summarise(peso_liquido_total = to_ton(sum(peso_liquido))) %>% 
        arrange(regional) %>%
        e_chart(regional) %>% 
        e_bar(peso_liquido_total) %>%
        format_bar_plot
    })

    output$residometro_diario_territorio <- renderEcharts4r({
      dados$pesagem_diaria %>%
        group_by(zgl) %>% 
        summarise(peso_liquido_total = to_ton(sum(peso_liquido))) %>% 
        arrange(zgl) %>%
        e_chart(zgl) %>% 
        e_bar(peso_liquido_total) %>%
        format_bar_plot
    })

    ############
    ## Mensal ##
    ############
    output$residometro_mensal_total = renderText({

      tot <- pesagem_mensal %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      sprintf("%s t", br_format(tot))
      
    })
    
    output$residometro_mensal_total_coleta_domiciliar <- renderText({

      total <- pesagem_mensal %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
        
      total_domiciliar <- pesagem_mensal %>%
        filter(material == "DOMICILIAR") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
          
      sprintf("%s t (%s%%)", br_format(total_domiciliar),
              br_format(100*total_domiciliar/total))
    })

    output$residometro_mensal_total_especial_urbana <- renderText({

      total <- pesagem_mensal %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      total_ceu <- pesagem_mensal %>%
        filter(material %in% c("ESPECIAL URBANA", "LIXO ESPECIAL URBANA", "LIXO ESPECIAL URBANA - MECANIZADA")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_ceu), br_format(100*total_ceu/total))
    })

    
    output$residometro_mensal_total_podacao <- renderText({

      total <- pesagem_mensal %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      total_podacao <- pesagem_mensal %>%
        filter(material %in% c("CAPINA", "PODAÇÃO")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_podacao), br_format(100*total_podacao/total))
    })

    output$residometro_mensal_total_entulho <- renderText({

      total <- pesagem_mensal %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      total_entulho <- pesagem_mensal %>%
        filter(material == "ENTULHO") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_entulho), br_format(100*total_entulho/total))
    })

    output$residometro_mensal_total_coleta_seletiva <- renderText({

      total <- pesagem_mensal %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      total_seletiva <- pesagem_mensal %>%
        filter(material == "COLETA SELETIVA") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_seletiva/total
      
      sprintf("%s t (%s%%)", br_format(total_seletiva), if (perc < 1) "< 1" else br_format(perc))
    })


    output$residometro_mensal_regional <- renderEcharts4r({
      pesagem_mensal %>%
        filtra_regional %>%
        group_by(regional) %>% 
        summarise(peso_liquido_total = sum(peso_liquido)) %>% 
        arrange(regional) %>%
        e_chart(regional) %>% 
        e_bar(peso_liquido_total) %>% 
        format_bar_plot
    })

    output$residometro_mensal_territorio <- renderEcharts4r({
      pesagem_mensal %>%
        group_by(zgl) %>% 
        summarise(peso_liquido_total = sum(peso_liquido)) %>% 
        arrange(zgl) %>%
        e_chart(zgl) %>% 
        e_bar(peso_liquido_total) %>%
        format_bar_plot
    })

    ###########
    ## Anual ##
    ###########
    output$residometro_anual_total = renderText({
      tot <- pesagem_anual %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      sprintf("%s t", br_format(tot))
    })

    
    output$residometro_anual_total_coleta_domiciliar <- renderText({

      total <- pesagem_anual %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      total_domiciliar <- pesagem_anual %>%
        filter(material == "DOMICILIAR") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_domiciliar),
              br_format(100*total_domiciliar/total))
    })

    output$residometro_anual_total_especial_urbana <- renderText({

      total <- pesagem_anual %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      total_ceu <- pesagem_anual %>%
        filter(material %in% c("ESPECIAL URBANA", "LIXO ESPECIAL URBANA", "LIXO ESPECIAL URBANA - MECANIZADA")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_ceu), br_format(100*total_ceu/total))
    })

    
    output$residometro_anual_total_podacao <- renderText({

      total <- pesagem_anual %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      total_podacao <- pesagem_anual %>%
        filter(material %in% c("CAPINA", "PODAÇÃO")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_podacao), br_format(100*total_podacao/total))
    })

    output$residometro_anual_total_entulho <- renderText({

      total <- pesagem_anual %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      total_entulho <- pesagem_anual %>%
        filter(material == "ENTULHO") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_entulho), br_format(100*total_entulho/total))
    })

    output$residometro_anual_total_coleta_seletiva <- renderText({

      total <- pesagem_anual %>%
        pull(peso_liquido) %>%
        sum %>% to_ton
      
      total_seletiva <- pesagem_anual %>%
        filter(material == "COLETA SELETIVA") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_seletiva/total
      
      sprintf("%s t (%s%%)", br_format(total_seletiva), if (perc < 1) "< 1" else br_format(perc))
    })

    output$residometro_anual_regional <- renderEcharts4r({
      pesagem_anual %>%
        filtra_regional %>%
        group_by(regional) %>% 
        summarise(peso_liquido_total = sum(peso_liquido)) %>% 
        arrange(regional) %>%
        e_chart(regional) %>% 
        e_bar(peso_liquido_total) %>%
        format_bar_plot
    })

    output$residometro_anual_territorio <- renderEcharts4r({
      pesagem_anual %>%
        group_by(zgl) %>% 
        summarise(peso_liquido_total = sum(peso_liquido)) %>% 
        arrange(zgl) %>%
        e_chart(zgl) %>% 
        e_bar(peso_liquido_total) %>% 
        format_bar_plot
    })




    ## output$residometro_peso_placa <- renderEcharts4r({
    ##   Pesagem_01_01_23 |> 
    ##     dplyr::group_by(PLACA) |> 
    ##     dplyr::summarise(Residômetro=sum(`PESO LIQUIDO`)) |> 
    ##     dplyr::arrange(-Residômetro) |> 
    ##     echarts4r::e_chart(x=PLACA) |> 
    ##     echarts4r::e_bar(Residômetro) |> 
    ##     echarts4r::e_flip_coords() |> 
    ##     echarts4r::e_tooltip()|> 
    ##     echarts4r::e_title('Peso liquido por placa (kg)')

    ## })

    ## output$residometro_peso_ano_heatmap <- renderEcharts4r({
    ##   dplyr::tbl(obsr, "relatorio_pesagem_2022") %>%
    ##     collect %>%
    ##     mutate(
    ##       dia = as.integer(substring(data_saida, 1, 2)),
    ##       mes = as.integer(substring(data_saida, 4, 5)),
    ##       ano = as.integer(substring(data_saida, 7, 10)),
    ##       peso_liquido = as.numeric(peso_liquido),
    ##       data_saida = as.Date(data_saida, "%d/%m/%Y")
    ##     ) %>%
    ##     group_by(data_saida) %>%
    ##     summarise(total = sum(peso_liquido)/100000) %>%
    ##     e_charts(data_saida) %>%
    ##     e_calendar(range = "2022") %>%
    ##     e_heatmap(total, coord_system = "calendar") %>%
    ##     e_visual_map(max = 30)          
    ## })
    
  }) # End Server
}
