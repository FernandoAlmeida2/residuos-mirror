box::use(
  shiny[...],
  bs4Dash[...],
  echarts4r[e_theme_register],
  waiter[...],
  fresh[use_theme, use_googlefont]
)

box::use(
  ./mod/theme[
    diobs_theme,
    font1,
    font2,
    font3,
    main_font,
    secondary_font,
    monospace_font,
    primary
  ],
  ./view/inicio,
  ./view/reciclometro,
  ./view/residometro,
  ./view/pontos_coleta,
  ./view/pontos_entrega,
  ./view/mod_metodologia,
  ./view/mod_ficha_tecnica
)

#' @export
ui <- dashboardPage(
  # preloader = list(
  #   html = tagList(
  #     spin_cube_grid(),
  #     br(),
  #     "Carregando ..."
  #   ),
  #   color = primary
  # ),
  shinyjs::useshinyjs(),
  dark = FALSE,
  help = FALSE,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  header = dashboardHeader(
    title = dashboardBrand(
      title = "OBSR",
      color = "primary",
      href = "https://observatoriodefortaleza.fortaleza.ce.gov.br/",
      image = "logo.png",
      opacity = 1.0
    ),
    fixed = FALSE
  ),
  sidebar = dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    status = "primary",
    id = "sidebar",
    collapsed = TRUE,
    sidebarMenu(
      id = "current_tab",
      flat = FALSE,
      compact = FALSE,
      childIndent = TRUE,
      menuItem(
        "Início",
        tabName = "inicio",
        icon = icon("home")
      ),
      menuItem(
        "Residuômetro",
        tabName = "residometro",
        icon = icon("trash-restore")
      ),
      menuItem(
        "Reciclômetro",
        tabName = "reciclometro",
        icon = icon("recycle")
      ),
      menuItem(
        "Pontos de Coleta",
        tabName = "pontos_coleta",
        icon = icon("map-marked")
      ),
      menuItem(
        "Pontos de Entrega",
        tabName = "pontos_entrega",
        icon = icon("map-marked")
      ),
      menuItem(
        "Metodologia",
        tabName = "mod_metodologia",
        icon = icon("book")
      ),
      menuItem(
        "Ficha Técnica",
        tabName = "mod_ficha_tecnica",
        icon = icon("users")
      )
    )
  ),
  body = dashboardBody(
    use_googlefont(font1),
    use_googlefont(font2),
    use_googlefont(font3),
    use_theme(diobs_theme),
    e_theme_register(
      paste(readLines("www/atlas_capital_humano.json"), collapse = ""),
      "diobs"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      # Listen for dark-mode messages
      tags$script("
      $(document).ready(function(){
        $('.brand-image').removeClass('elevation-3 img-circle');
      })

      Shiny.addCustomMessageHandler('dark-mode', function(dark_mode) {
        if (dark_mode) {
          $('#footer-logo').find('img').map(function() { $(this).attr('src', $(this).attr('src').replace('dark', 'light')) });
        } else {
          $('#footer-logo').find('img').map(function() { $(this).attr('src', $(this).attr('src').replace('light', 'dark')) });
        }
      });
    "),
    ),
    tabItems(
      tabItem(
        tabName = "inicio",
        inicio$ui("inicio")
      ),
      tabItem(
        tabName = "residometro",
        residometro$ui("residometro")
      ),
      tabItem(
        tabName = "reciclometro",
        reciclometro$ui("reciclometro")
      ),
      tabItem(
        tabName = "pontos_coleta",
        pontos_coleta$ui("pontos_coleta")
      ),
      tabItem(
        tabName = "pontos_entrega",
        pontos_entrega$ui("pontos_entrega")
      ),
      tabItem(
        tabName = "mod_metodologia",
        mod_metodologia$ui("mod_metodologia")
      ),
      tabItem(
        tabName = "mod_ficha_tecnica",
        mod_ficha_tecnica$ui("mod_ficha_tecnica")
      )
    )
  ),
  controlbar = dashboardControlbar(
    id = "controlbar",
    skin = "light",
    pinned = TRUE,
    overlay = FALSE,
    "Filtros"
  ),
  footer = dashboardFooter(
    fixed = FALSE,
    left = tagList(
      # Versão mobile
      div(
        span(
          format(Sys.Date(), "%Y, "),
          a(
            href = "https://observatoriodefortaleza.fortaleza.ce.gov.br/",
            target = "_blank", "Observatório de Fortaleza"
          )
        )
      )
    ),
    right = tagList(
      tags$ul(
        id = "footer-logo",
        tags$li(
          a(
            href = "https://observatoriodefortaleza.fortaleza.ce.gov.br/",
            target = "_blank",
            alt = "",
            img(
              src = "img/logo-diobs-dark.svg",
              height = "30"
            )
          )
        ),
        tags$li(
          img(
            src = "img/logo-iplanfor-dark.svg",
            height = "25"
          )
        ),
        tags$li(
          a(
            href = "https://www.fortaleza.ce.gov.br/",
            target = "_blank",
            alt = "",
            img(
              src = "img/logo-pmf-dark.svg",
              height = "35"
            )
          )
        )
      )
    )
  ),
  title = "Observatório de Resíduos de Fortaleza"
)
