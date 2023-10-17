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
    mutate(
      dia = as.integer(substring(data_saida, 1, 2)),
      mes = as.integer(substring(data_saida, 4, 5)),
      ano = as.integer(substring(data_saida, 7, 10)),
      peso_liquido = as.numeric(peso_liquido)
    ) %>%
    filter(dia == curr_day) %>%
    collect
    
}

filtra_regional <- function (x) {
  x %>%
    filter(stringr::str_detect(regional, "^SER")) %>%
    mutate(
      regional = sprintf("SR %02d",  as.integer(as.roman(stringr::str_trim(gsub("SER", "", regional)))))
    )##  %>%
  ## mutate(
  ##   regional = factor(regional, levels = )
  ## )
}

format_bar_plot <- function(x, show_legend = FALSE) {
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
    e_legend(show = show_legend) %>%
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
      column(
        width = 9,  # Ajuste a largura conforme necessário
      box(
        title = "Peso líquido por regional (t)",
        elevation = 4,
        closable = FALSE,
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = FALSE,
        shinycssloaders::withSpinner(
          type = 8,
          color = "#0e2e45",
          echarts4rOutput(ns(sprintf("residometro_%s_regional", x)))
        )
      )
      ),
      # column(
      #   width = 3,  # Ajuste a largura conforme necessário
      #   box(
      #     title = "Caixa de Texto 1",
      #     textOutput(ns("texto1")),  # Ou você pode usar 'verbatimTextOutput' ou 'HTML' conforme necessário
      #     width = NULL
      #   ),
      #   box(
      #     title = "Caixa de Texto 2",
      #     textOutput(ns("texto2")),  # Ou você pode usar 'verbatimTextOutput' ou 'HTML' conforme necessário
      #     width = NULL
      #   )
      # ),


      box(
        title = "Peso líquido por território (t)",
        elevation = 4,
        closable = FALSE,
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = FALSE,
        shinycssloaders::withSpinner(
          type = 8,
          color = "#0e2e45",
          echarts4rOutput(ns(sprintf("residometro_%s_territorio", x)))
        )
      )
    ),
    if (x == "anual") {
      fluidRow(
        box(
          title = "Peso líquido total em toneladas (série histórica)",
          elevation = 4,
          closable = FALSE,
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          collapsible = FALSE,
          shinycssloaders::withSpinner(
            type = 8,
            color = "#0e2e45",
            echarts4rOutput(ns("residometro_anual_heatmap"))
          )          
          
        )        
      )
    }
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  series_historicas <- tagList(
    fluidRow(
      box(
        title = "Peso líquido por regional (t)",
        elevation = 4,
        closable = FALSE,
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = FALSE,
        shinycssloaders::withSpinner(
          type = 8,
          color = "#0e2e45",
          echarts4rOutput(ns("residometro_serie_hist_regional"))
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
          echarts4rOutput(ns("residometro_serie_hist_territorio"))
        )
      )
    )
  )

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
      ),
      tabPanel(
        "Séries Históricas",
        series_historicas
      )
    )
  )  
}

grafico_regional <- function(df) {
  df %>%
    filtra_regional %>%
    group_by(material, regional) %>% 
    summarise(peso_liquido_total = to_ton(sum(peso_liquido))) %>% 
    arrange(-peso_liquido_total) %>%
    e_chart(regional) %>% 
    e_bar(peso_liquido_total, stack="grp") %>%
    e_tooltip(
      formatter = htmlwidgets::JS("
            function(params) {
              return '<span>'
              + params.value[0] + '<br/>'
              + params.seriesName + '<br/>'
              + parseFloat(params.value[1]).toLocaleString('pt-BR')
              + ' t'
              + '</span>';
            }"
            )
    ) %>%
    e_y_axis(formatter = e_axis_formatter(locale = "pt-BR")) %>%
    e_legend(show = TRUE) %>%
    e_toolbox_feature(feature = c("dataView", "dataZoom", "saveAsImage"))
}

grafico_territorio <- function(df) {
  df %>%
    filter(stringr::str_detect(zgl, "^TERRITORIO")) %>%
    group_by(material, zgl) %>% 
    summarise(peso_liquido_total = to_ton(sum(peso_liquido))) %>%
    arrange(-peso_liquido_total) %>%
    e_chart(zgl) %>% 
    e_bar(peso_liquido_total, stack="grp") %>%
    e_tooltip(
      formatter = htmlwidgets::JS("
            function(params) {
              return '<span>'
              + params.value[0] + '<br/>'
              + params.seriesName + '<br/>'
              + parseFloat(params.value[1]).toLocaleString('pt-BR')
              + ' t'
              + '</span>';
            }"
            )
    ) %>%
    e_y_axis(formatter = e_axis_formatter(locale = "pt-BR")) %>%
    e_legend(show = TRUE) %>%
    e_toolbox_feature(feature = c("dataView", "dataZoom", "saveAsImage"))
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    today <- Sys.Date()
    curr_year <- lubridate::year(today)
    curr_month <- lubridate::month(today)-8
    curr_year_tblname <- sprintf("br_acfor_relatorio_pesagem_%04d", curr_year)

    pesagem_anual <- dplyr::tbl(obsr, curr_year_tblname) %>%
      mutate(
        dia = as.integer(substring(data_saida, 1, 2)),
        mes = as.integer(substring(data_saida, 4, 5)),
        ano = as.integer(substring(data_saida, 7, 10)),
        peso_liquido = as.numeric(peso_liquido)
      ) %>%
      collect

    pesagem_mensal <- pesagem_anual %>% filter(mes == curr_month)

    pesagem_diaria <- update_data(obsr, curr_year)

    e_common(theme = "roma")

    ############
    ## Diário ##
    ############
    output$residometro_diario_total <- renderText({

      total <- pesagem_diaria %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      sprintf("%s t", br_format(total))
    })

    output$residometro_diario_total_coleta_domiciliar <- renderText({

      total <- pesagem_diaria %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_domiciliar <- pesagem_diaria %>%
        filter(material == "DOMICILIAR") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_domiciliar/total
      sprintf("%s t (%s)", br_format(total_domiciliar),
              perc_format(perc))
    })

    output$residometro_diario_total_especial_urbana <- renderText({

      total <- pesagem_diaria %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_ceu <- pesagem_diaria %>%
        filter(material %in% c("ESPECIAL URBANA", "LIXO ESPECIAL URBANA", "LIXO ESPECIAL URBANA - MECANIZADA")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_ceu/total
      sprintf("%s t (%s)", br_format(total_ceu), perc_format(perc))
    })


    output$residometro_diario_total_podacao <- renderText({

      total <- pesagem_diaria %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_podacao <- pesagem_diaria %>%
        filter(material %in% c("CAPINA", "PODAÇÃO")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_podacao/total
      sprintf("%s t (%s)", br_format(total_podacao), perc_format(perc))
    })

    output$residometro_diario_total_entulho <- renderText({

      total <- pesagem_diaria %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_entulho <- pesagem_diaria %>%
        filter(material == "ENTULHO") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_entulho/total
      sprintf("%s t (%s)", br_format(total_entulho), perc_format(perc))
    })

    output$residometro_diario_total_coleta_seletiva <- renderText({

      total <- pesagem_diaria %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_seletiva <- pesagem_diaria %>%
        filter(material == "COLETA SELETIVA") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_seletiva/total
      sprintf("%s t (%s)", br_format(total_seletiva), perc_format(perc))
    })

    output$residometro_diario_regional <- renderEcharts4r({
      pesagem_diaria %>%
        grafico_regional
    }) %>%
      bindCache(nrow(pesagem_diaria))

    output$residometro_diario_territorio <- renderEcharts4r({
      pesagem_diaria %>%
        grafico_territorio
    }) %>%
      bindCache(nrow(pesagem_diaria))

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

      perc <- 100*total_domiciliar/total
      sprintf("%s t (%s%%)", br_format(total_domiciliar),
              perc_format(perc))
    })

    output$residometro_mensal_total_especial_urbana <- renderText({

      total <- pesagem_mensal %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_ceu <- pesagem_mensal %>%
        filter(material %in% c("ESPECIAL URBANA", "LIXO ESPECIAL URBANA", "LIXO ESPECIAL URBANA - MECANIZADA")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_ceu/total
      sprintf("%s t (%s)", br_format(total_ceu), perc_format(perc))
    })


    output$residometro_mensal_total_podacao <- renderText({

      total <- pesagem_mensal %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_podacao <- pesagem_mensal %>%
        filter(material %in% c("CAPINA", "PODAÇÃO")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_podacao/total
      sprintf("%s t (%s)", br_format(total_podacao), perc_format(perc))
    })

    output$residometro_mensal_total_entulho <- renderText({

      total <- pesagem_mensal %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_entulho <- pesagem_mensal %>%
        filter(material == "ENTULHO") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_entulho/total
      sprintf("%s t (%s)", br_format(total_entulho), perc_format(perc))
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

      sprintf("%s t (%s)", br_format(total_seletiva), perc_format(perc))
    })


    output$residometro_mensal_regional <- renderEcharts4r({
      pesagem_mensal %>%
        grafico_regional
    }) %>%
      bindCache(nrow(pesagem_mensal))

    output$residometro_mensal_territorio <- renderEcharts4r({
      pesagem_mensal %>%
        grafico_territorio
    }) %>%
      bindCache(nrow(pesagem_mensal))

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

      perc <- 100*total_ceu/total
      sprintf("%s t (%s)", br_format(total_ceu), perc_format(perc))
    })


    output$residometro_anual_total_podacao <- renderText({

      total <- pesagem_anual %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_podacao <- pesagem_anual %>%
        filter(material %in% c("CAPINA", "PODAÇÃO")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_podacao/total
      sprintf("%s t (%s)", br_format(total_podacao), perc_format(perc))
    })

    output$residometro_anual_total_entulho <- renderText({

      total <- pesagem_anual %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_entulho <- pesagem_anual %>%
        filter(material == "ENTULHO") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_entulho/total
      sprintf("%s t (%s)", br_format(total_entulho), perc_format(perc))
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
      sprintf("%s t (%s)", br_format(total_seletiva), perc_format(perc))
    })

    output$residometro_anual_regional <- renderEcharts4r({
      pesagem_anual %>%
        grafico_regional
    }) %>%
      bindCache(nrow(pesagem_anual))

    output$residometro_anual_territorio <- renderEcharts4r({
      pesagem_anual %>%
        grafico_territorio
    }) %>%
      bindCache(nrow(pesagem_anual))


    output$residometro_anual_heatmap <- renderEcharts4r({
      res <- pesagem_anual %>%
        mutate(
          data_saida = as.Date(data_saida, "%d/%m/%Y")
        ) %>%
        group_by(data_saida) %>%
        summarise(total = to_ton(sum(peso_liquido)))


      min_val <- res %>%
        pull(total) %>%
        min

      max_val <- res %>%
        pull(total) %>%
        max

      res %>%
        e_charts(data_saida) %>%
        e_calendar(range = curr_year) %>%
        e_heatmap(total, coord_system = "calendar") %>%
        e_visual_map(min = min_val, max = max_val)
    }) %>%
      bindCache(nrow(pesagem_anual))


    output$residometro_serie_hist_regional <- renderEcharts4r({
      
      dplyr::bind_rows(lapply(2016:2023, function(ano){
        dplyr::tbl(obsr, sprintf("br_acfor_relatorio_pesagem_%04d", ano)) %>%
          collect %>%
          mutate(
            dia = as.integer(substring(data_saida, 1, 2)),
            mes = as.integer(substring(data_saida, 4, 5)),
            ano = as.integer(substring(data_saida, 7, 10)),
            peso_liquido = as.numeric(peso_liquido)
          )
      })) %>%
        filter(ano >= 2016 & ano <= 2023) %>%
        mutate(ano = as.factor(ano)) %>%
        filtra_regional %>%
        group_by(ano, regional) %>% 
        summarise(peso_liquido_total = to_ton(sum(peso_liquido))) %>%
        group_by(regional) %>%
        e_chart(ano) %>% 
        e_line(peso_liquido_total) %>%
        format_bar_plot(show_legend = TRUE)
    })

    
   
    
  }) # End Server
}

