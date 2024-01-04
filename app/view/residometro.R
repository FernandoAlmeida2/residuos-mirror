box::use(
  bs4Dash[...],
  dplyr[
    arrange,
    collect,
    summarise,
    mutate,
    filter,
    pull,
    glimpse,
    ungroup
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
        value = h5(textOutput(ns(sprintf("residometro_%s_total", x))), style="color: #255B54; font-weight: 700;"),
        subtitle = p("Total", style="color: #255B54;"),
        color = "success",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = h5(textOutput(ns(sprintf("residometro_%s_total_coleta_domiciliar", x))), style="font-weight: 700;"),
        subtitle = p("Coleta Domiciliar"),
        color = "primary",
        icon = icon("trash"),
        width = 3
      ),
      valueBox(
        value = h5(textOutput(ns(sprintf("residometro_%s_total_especial_urbana", x))), style="font-weight: 700;"),
        subtitle = p("Coleta Especial Urbana"),
        color = "primary",
        icon = icon("trash"),
        width = 3
      ),
      valueBox(
        value = h5(textOutput(ns(sprintf("residometro_%s_total_podacao", x))), style="font-weight: 700;"),
        subtitle = p("Poda"),
        color = "primary",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = h5(textOutput(ns(sprintf("residometro_%s_total_entulho", x))), style="font-weight: 700;"),
        subtitle = p("Entulho"),
        color = "primary",
        icon = icon("trash"),
        width = 2
      )
      # valueBox(
      #   value = h5(textOutput(ns(sprintf("residometro_%s_total_coleta_seletiva", x))), style="font-weight: 700;"),
      #   subtitle = p("Coleta Seletiva"),
      #   color = "primary",
      #   icon = icon("trash"),
      #   width = 2
      # )
    ),
    div(
      class = "filter-box w-50",
      uiOutput(ns(paste0("input_residometro_", x)))
    ),
    fluidRow(
      box(
        title =  div(
          "Peso líquido por regional (t) - ",
          span(textOutput(ns(paste0("title_regional_", x)), inline = TRUE), class = "font-italic small")
        ),
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
      ),
      box(
        title = div(
          "Peso líquido por território (t) - ",
          span(textOutput(ns(paste0("title_territorio_", x)), inline = TRUE), class = "font-italic small")
        ),
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
        title = div(
          "Peso líquido por regional (t) - ",
          span("de 2016 até 2020", class = "font-italic small")
        ),
        elevation = 4,
        closable = FALSE,
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = FALSE,
        shinycssloaders::withSpinner(
          type = 8,
          color = "#0e2e45",
          echarts4rOutput(ns("residometro_serie_hist_regional_antes"))
        )
      ),
      box(
        title = div(
          "Peso líquido por regional (t) - ",
          span("a partir de 2021", class = "font-italic small")
      ),
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
      )
      # box(
      #   title = "Peso líquido por território (t)",
      #   elevation = 4,
      #   closable = FALSE,
      #   width = 12,
      #   solidHeader = TRUE,
      #   status = "primary",
      #   collapsible = FALSE,
      #   shinycssloaders::withSpinner(
      #     type = 8,
      #     color = "#0e2e45",
      #     echarts4rOutput(ns("residometro_serie_hist_territorio"))
      #   )
      # )
    )
  )

  tagList(
    tabBox(
      title = p(
        class="text-uppercase font-weight-bold",
        "Residuômetro ",
        span(sprintf("(Ano Base: %04d)", lubridate::year(Sys.Date())), class = "font-italic small")
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
  
  df_filtered <- df %>% filtra_regional
  
  regional_order <- df_filtered %>%
    group_by(regional) %>%
    summarise(peso_liquido_total = sum(peso_liquido)) %>%
    arrange(-peso_liquido_total)
  
  df_filtered %>%
    group_by(material, regional) %>% 
    summarise(peso_liquido_total = to_ton(sum(peso_liquido))) %>%
    arrange(factor(regional, levels = regional_order$regional)) %>%
    e_chart(regional, reorder = FALSE) %>%
    e_bar(peso_liquido_total, stack="grp") %>%
    e_tooltip(
      formatter = htmlwidgets::JS("
        function(params) {
          return '<span>'
          + params.value[0] + '<br/>'
          + params.seriesName + '<br/>'
          + parseFloat(params.value[1])
              .toLocaleString('pt-BR', {style: 'decimal',
                                          maximumFractionDigits: params.value[1] < 1000 ? 2 : 0})
          + ' t'
          + '</span>';
        }")
    ) %>%
    e_y_axis(formatter = e_axis_formatter(locale = "pt-BR")) %>%
    e_legend(show = TRUE) %>%
    e_toolbox_feature(feature = c("dataView", "dataZoom", "saveAsImage"))
}

grafico_territorio <- function(df) {
  
  df_filtered <- df %>%
    filter(stringr::str_detect(zgl, "^TERRITORIO") | stringr::str_detect(zgl, "^ZGL"))
  
  zgl_order <- df_filtered %>%
    group_by(zgl) %>%
    summarise(peso_liquido_total = sum(peso_liquido)) %>%
    arrange(-peso_liquido_total)
  
  df_filtered %>%
    group_by(material, zgl) %>% 
    summarise(peso_liquido_total = to_ton(sum(peso_liquido))) %>%
    arrange(factor(zgl, levels = zgl_order$zgl)) %>%
    e_chart(zgl, reorder = FALSE) %>% 
    e_bar(peso_liquido_total, stack="grp") %>%
    e_tooltip(
      formatter = htmlwidgets::JS("
            function(params) {
              return '<span>'
              + params.value[0] + '<br/>'
              + params.seriesName + '<br/>'
              + parseFloat(params.value[1])
                .toLocaleString('pt-BR',
                   {style: 'decimal', maximumFractionDigits: params.value[1] < 1000 ? 2 : 0})
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
    
    ns <- session$ns
    
    today <- Sys.Date()
    curr_year <- lubridate::year(today)
    curr_month <- lubridate::month(today)
    curr_year_tblname <- sprintf("br_acfor_relatorio_pesagem_%04d", curr_year)
    
    last_data <- reactive ({
      if(!is.null(input$option_date_yearly)) {
        if(input$option_date_yearly != curr_year) {
          selected_year_tblname <- sprintf("br_acfor_relatorio_pesagem_%04d", as.numeric(input$option_date_yearly))
          query_data <- sprintf('SELECT data_entrada FROM %s ORDER BY ".db_pkid" DESC LIMIT 1', selected_year_tblname)
        } else {
            query_data <- sprintf('SELECT data_entrada FROM %s ORDER BY ".db_pkid" DESC LIMIT 1', curr_year_tblname)
        }
      } else {
        query_data <- sprintf('SELECT data_entrada FROM %s ORDER BY ".db_pkid" DESC LIMIT 1', curr_year_tblname)
        #query_data <- sprintf('SELECT data_entrada FROM %s ORDER BY "data_entrada" DESC LIMIT 1', curr_year_tblname)
      }
      dplyr::collect(dplyr::tbl(obsr, dplyr::sql(query_data)))$data_entrada
    })
    
    selected_day <- reactive ({
      if(!is.null(input$option_date_daily)) {
        as.numeric(input$option_date_daily)
      } else {
        as.numeric(substring(last_data(), 1, 2))
      }
    })
    
    selected_month <- reactive ({
      if(!is.null(input$option_date_monthly)) {
        month_br_to_number(input$option_date_monthly)
      } else {
        as.numeric(substring(last_data(), 4, 5))
      }
    })
    
    selected_year <- reactive ({
      if(!is.null(input$option_date_yearly)) {
        as.numeric(input$option_date_yearly)
      } else {
        as.numeric(substring(last_data(), 7, 10))
      }
    })
    
    selected_date <- reactive({
        paste0(if(selected_day() < 10) "0", selected_day(), "/",
               if(selected_month() < 10) "0", selected_month(), "/", selected_year())
    })

    pesagem_anual <- reactive({
      dplyr::tbl(obsr, sprintf("br_acfor_relatorio_pesagem_%s", substring(selected_date(), 7, 10))) %>%
        mutate(
          dia = as.integer(substring(data_entrada, 1, 2)),
          mes = as.integer(substring(data_entrada, 4, 5)),
          ano = as.integer(substring(data_entrada, 7, 10)),
          peso_liquido = as.numeric(peso_liquido)
        ) %>%
        collect
    })

    pesagem_mensal <- reactive({
      pesagem_anual() %>% filter(mes == as.integer(substring(selected_date(), 4, 5)))
    })
      
    pesagem_diaria <- reactive({
      pesagem_anual() %>% filter(data_entrada == selected_date())
    })

    e_common(theme = "roma")
    
    ### renderizando títulos
    output$title_regional_diario <- renderText({
      paste("Última atualização:", last_data())
    })
    
    output$title_regional_mensal <- renderText({
      paste0("Mês de referência: ", month_br_text(selected_date()), "/", selected_year())
    })
    
    output$title_regional_anual <- renderText({
      paste("Ano de referência:", selected_year())
    })
    
    output$title_territorio_diario <- renderText({
      paste("Última atualização:", last_data())
    })
    
    output$title_territorio_mensal <- renderText({
      paste0("Mês de referência: ", month_br_text(selected_date()), "/", selected_year())
    })
    
    output$title_territorio_anual <- renderText({
      paste("Ano de referência:", selected_year())
    })
    
    ### renderizando opções de pesquisa
    output$input_residometro_diario <- renderUI({
      last_year <- as.integer(substring(last_data(), 7, 10))
      last_month <- substring(last_data(), 4, 5)
      
      choices_day <- ifelse(last_month != curr_month & last_year != curr_year,
                            lubridate::days_in_month(selected_date())[[1]],
                            as.numeric(substring(last_data(), 1, 2)))
      selectInput(
        inputId = ns("option_date_daily"),
        label = h5(class="subtitle-option", "Dias anteriores"),
        choices = c(1:choices_day),
        selected = selected_day(),
        width = "100%"
      )
    })
    
    output$input_residometro_mensal <- renderUI({
      choices_month <- ifelse(as.integer(substring(last_data(), 7, 10)) != curr_year, 12,
                              as.integer(substring(last_data(), 4, 5)))
      selectInput(
        inputId = ns("option_date_monthly"),
        label = h5(class="subtitle-option", "Meses anteriores"),
        choices = months_array_slice(choices_month),
        selected = number_to_month_br(selected_month()),
        width = "100%"
      )
    })
    
    output$input_residometro_anual <- renderUI({
      selectInput(
        inputId = ns("option_date_yearly"),
        label = h5(class="subtitle-option", "Anos anteriores"),
        choices = c(2016:curr_year),
        selected = selected_year(),
        width = "100%"
      )
    })

    ############
    ## Diário ##
    ############
    
    output$residometro_diario_total <- renderText({

      total <- pesagem_diaria() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      sprintf("%s t", br_format(total))
    })

    output$residometro_diario_total_coleta_domiciliar <- renderText({

      total <- pesagem_diaria() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_domiciliar <- pesagem_diaria() %>%
        filter(material == "DOMICILIAR") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_domiciliar/total
      sprintf("%s t (%s)", br_format(total_domiciliar),
              perc_format(perc))
    })

    output$residometro_diario_total_especial_urbana <- renderText({

      total <- pesagem_diaria() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_ceu <- pesagem_diaria() %>%
        filter(material %in% c("ESPECIAL URBANA", "LIXO ESPECIAL URBANA", "LIXO ESPECIAL URBANA - MECANIZADA")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_ceu/total
      sprintf("%s t (%s)", br_format(total_ceu), perc_format(perc))
    })


    output$residometro_diario_total_podacao <- renderText({

      total <- pesagem_diaria() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_podacao <- pesagem_diaria() %>%
        filter(material %in% c("CAPINA", "PODAÇÃO")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_podacao/total
      sprintf("%s t (%s)", br_format(total_podacao), perc_format(perc))
    })

    output$residometro_diario_total_entulho <- renderText({

      total <- pesagem_diaria() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_entulho <- pesagem_diaria() %>%
        filter(material == "ENTULHO") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_entulho/total
      sprintf("%s t (%s)", br_format(total_entulho), perc_format(perc))
    })

    output$residometro_diario_total_coleta_seletiva <- renderText({

      total <- pesagem_diaria() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_seletiva <- pesagem_diaria() %>%
        filter(material == "COLETA SELETIVA") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_seletiva/total
      sprintf("%s t (%s)", br_format(total_seletiva), perc_format(perc))
    })

    output$residometro_diario_regional <- renderEcharts4r({
      #print(nrow(pesagem_diaria))
      
      pesagem_diaria() %>%
        grafico_regional
    }) #%>%
      #bindCache(nrow(pesagem_diaria))
      #bindCache(as.integer(substring(selected_date, 1, 2)))

    output$residometro_diario_territorio <- renderEcharts4r({
      pesagem_diaria() %>%
        grafico_territorio
    })# %>%
      #bindCache(nrow(pesagem_diaria))
      #bindCache(as.integer(substring(selected_date, 1, 2)))

    ############
    ## Mensal ##
    ############
    output$residometro_mensal_total = renderText({

      tot <- pesagem_mensal() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      sprintf("%s t", br_format(tot))

    })

    output$residometro_mensal_total_coleta_domiciliar <- renderText({

      total <- pesagem_mensal() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_domiciliar <- pesagem_mensal() %>%
        filter(material == "DOMICILIAR") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_domiciliar/total
      sprintf("%s t (%s)", br_format(total_domiciliar),
              perc_format(perc))
    })

    output$residometro_mensal_total_especial_urbana <- renderText({

      total <- pesagem_mensal() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_ceu <- pesagem_mensal() %>%
        filter(material %in% c("ESPECIAL URBANA", "LIXO ESPECIAL URBANA", "LIXO ESPECIAL URBANA - MECANIZADA")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_ceu/total
      sprintf("%s t (%s)", br_format(total_ceu), perc_format(perc))
    })


    output$residometro_mensal_total_podacao <- renderText({

      total <- pesagem_mensal() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_podacao <- pesagem_mensal() %>%
        filter(material %in% c("CAPINA", "PODAÇÃO")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_podacao/total
      sprintf("%s t (%s)", br_format(total_podacao), perc_format(perc))
    })

    output$residometro_mensal_total_entulho <- renderText({

      total <- pesagem_mensal() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_entulho <- pesagem_mensal() %>%
        filter(material == "ENTULHO") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_entulho/total
      sprintf("%s t (%s)", br_format(total_entulho), perc_format(perc))
    })

    output$residometro_mensal_total_coleta_seletiva <- renderText({

      total <- pesagem_mensal() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_seletiva <- pesagem_mensal() %>%
        filter(material == "COLETA SELETIVA") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_seletiva/total

      sprintf("%s t (%s)", br_format(total_seletiva), perc_format(perc))
    })


    output$residometro_mensal_regional <- renderEcharts4r({
      pesagem_mensal() %>%
        grafico_regional
    })# %>%
      #bindCache(nrow(pesagem_mensal()))
      #bindCache(as.integer(substring(selected_date, 4, 5)))

    output$residometro_mensal_territorio <- renderEcharts4r({
      pesagem_mensal() %>%
        grafico_territorio
    }) #%>%
      #bindCache(nrow(pesagem_mensal()))
      #bindCache(as.integer(substring(selected_date, 4, 5)))

    ###########
    ## Anual ##
    ###########
    output$residometro_anual_total = renderText({
      tot <- pesagem_anual() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      sprintf("%s t", br_format(tot))
    })


    output$residometro_anual_total_coleta_domiciliar <- renderText({

      total <- pesagem_anual() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_domiciliar <- pesagem_anual() %>%
        filter(material == "DOMICILIAR") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      sprintf("%s t (%s%%)", br_format(total_domiciliar),
              br_format(100*total_domiciliar/total))
    })

    output$residometro_anual_total_especial_urbana <- renderText({

      total <- pesagem_anual() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_ceu <- pesagem_anual() %>%
        filter(material %in% c("ESPECIAL URBANA", "LIXO ESPECIAL URBANA", "LIXO ESPECIAL URBANA - MECANIZADA")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_ceu/total
      sprintf("%s t (%s)", br_format(total_ceu), perc_format(perc))
    })


    output$residometro_anual_total_podacao <- renderText({

      total <- pesagem_anual() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_podacao <- pesagem_anual() %>%
        filter(material %in% c("CAPINA", "PODAÇÃO")) %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_podacao/total
      sprintf("%s t (%s)", br_format(total_podacao), perc_format(perc))
    })

    output$residometro_anual_total_entulho <- renderText({

      total <- pesagem_anual() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_entulho <- pesagem_anual() %>%
        filter(material == "ENTULHO") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_entulho/total
      sprintf("%s t (%s)", br_format(total_entulho), perc_format(perc))
    })

    output$residometro_anual_total_coleta_seletiva <- renderText({

      total <- pesagem_anual() %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      total_seletiva <- pesagem_anual() %>%
        filter(material == "COLETA SELETIVA") %>%
        pull(peso_liquido) %>%
        sum %>% to_ton

      perc <- 100*total_seletiva/total
      sprintf("%s t (%s)", br_format(total_seletiva), perc_format(perc))
    })

    output$residometro_anual_regional <- renderEcharts4r({
      pesagem_anual() %>%
        grafico_regional
    }) #%>%
      #bindCache(nrow(pesagem_anual()))
      #bindCache(as.integer(substring(selected_date, 7, 10)))

    output$residometro_anual_territorio <- renderEcharts4r({
      pesagem_anual() %>%
        grafico_territorio
    }) #%>%
      #bindCache(nrow(pesagem_anual()))
      #bindCache(as.integer(substring(selected_date, 7, 10)))


    output$residometro_anual_heatmap <- renderEcharts4r({
      res <- pesagem_anual() %>%
        mutate(
          data_entrada = as.Date(data_entrada, "%d/%m/%Y")
        ) %>%
        group_by(data_entrada) %>%
        summarise(total = to_ton(sum(peso_liquido)))


      min_val <- res %>%
        pull(total) %>%
        min

      max_val <- res %>%
        pull(total) %>%
        max

      res %>%
        e_charts(data_entrada) %>%
        e_calendar(range = paste0(selected_year())) %>%
        e_heatmap(total, coord_system = "calendar") %>%
        e_visual_map(min = min_val, max = max_val)
    }) #%>%
      #bindCache(nrow(pesagem_anual()))


    output$residometro_serie_hist_regional_antes <- renderEcharts4r({
      
      dplyr::bind_rows(lapply(2016:2020, function(ano){
        curr_year_tblname <- sprintf("br_acfor_relatorio_pesagem_%04d", ano)
        query_data <- sprintf('SELECT data_entrada,peso_liquido,regional FROM %s', curr_year_tblname)
        dplyr::tbl(obsr, dplyr::sql(query_data)) %>%
          collect %>%
          mutate(
            dia = as.integer(substring(data_entrada, 1, 2)),
            mes = as.integer(substring(data_entrada, 4, 5)),
            ano = as.integer(substring(data_entrada, 7, 10)),
            peso_liquido = as.numeric(peso_liquido)
          )
      })) %>%
        mutate(ano = as.factor(ano)) %>%
        filtra_regional %>%
        group_by(ano, regional) %>% 
        summarise(peso_liquido_total = to_ton(sum(peso_liquido))) %>%
        group_by(regional) %>%
        e_chart(ano) %>% 
        e_line(peso_liquido_total) %>%
        format_bar_plot(show_legend = TRUE)
    }) #%>%
      #bindCache(nrow(pesagem_anual()))
    
    
    
    output$residometro_serie_hist_regional <- renderEcharts4r({
      
        dplyr::bind_rows(lapply(2021:curr_year, function(ano){
          curr_year_tblname <- sprintf("br_acfor_relatorio_pesagem_%04d", ano)
          query_data <- sprintf('SELECT data_entrada,peso_liquido,regional FROM %s', curr_year_tblname)
          dplyr::tbl(obsr, dplyr::sql(query_data)) %>%
            collect %>%
            mutate(
              dia = as.integer(substring(data_entrada, 1, 2)),
              mes = as.integer(substring(data_entrada, 4, 5)),
              ano = as.integer(substring(data_entrada, 7, 10)),
              peso_liquido = as.numeric(peso_liquido)
            )
        })) %>%
          mutate(ano = as.factor(ano)) %>%
          filtra_regional %>%
          group_by(ano, regional) %>% 
          summarise(peso_liquido_total = to_ton(sum(peso_liquido))) %>%
          group_by(regional) %>%
          e_chart(ano) %>% 
          e_line(peso_liquido_total) %>%
          format_bar_plot(show_legend = TRUE)
    }) #%>%
      #bindCache(nrow(pesagem_anual))
    
    # output$residometro_serie_hist_territorio <- renderEcharts4r({
    #   dplyr::bind_rows(lapply(2016:2023, function(ano){
    #     dplyr::tbl(obsr, sprintf("br_acfor_relatorio_pesagem_%04d", ano)) %>%
    #       collect %>%
    #       mutate(
    #         dia = as.integer(substring(data_entrada, 1, 2)),
    #         mes = as.integer(substring(data_entrada, 4, 5)),
    #         ano = as.integer(substring(data_entrada, 7, 10)),
    #         peso_liquido = as.numeric(peso_liquido)
    #       )
    #   })) %>%
    #     filter(ano >= 2016 & ano <= 2023) %>%
    #     filter(stringr::str_detect(zgl, "^TERRITORIO")) %>%
    #     mutate(ano = as.factor(ano)) %>%
    #     group_by(ano, zgl) %>%
    #     summarise(peso_liquido_total = to_ton(sum(peso_liquido))) %>%
    #     group_by(zgl) %>%
    #     e_chart(ano) %>%
    #     e_line(peso_liquido_total) %>%
    #     format_bar_plot(show_legend = TRUE)
    # })

    
   
    
  }) # End Server
}

