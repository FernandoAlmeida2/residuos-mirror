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
  ./view/coleta_em_mapas,
  ./view/reciclometro,
  ./view/residometro
)

#' @export
ui <- dashboardPage(
  preloader = list(
    html = tagList(
      spin_cube_grid(),
      br(),
      "Carregando ..."
    ),
    color = primary
  ),
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
        "Reciclômetro",
        tabName = "reciclometro",
        icon = icon("recycle")
      ),
      menuItem(
        "Residômetro",
        tabName = "residometro",
        icon = icon("trash-restore")
      ),
      menuItem(
        "Coleta em Mapas",
        tabName = "coleta_em_mapas",
        icon = icon("map-marked")
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
        tabName = "residometro",
        residometro$ui("residometro")
      ),
      tabItem(
        tabName = "coleta_em_mapas",
        coleta_em_mapas$ui("coleta_em_mapas")
      ),
      tabItem(
        tabName = "reciclometro",
        reciclometro$ui("reciclometro")
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
