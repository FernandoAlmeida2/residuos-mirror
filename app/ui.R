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
  ./view/cartoes_basicos,
  ./view/cartoes_api,
  ./view/cartoes_abas,
  ./view/cartoes_organizaveis,
  ./view/cartoes_descritivos,
  ./view/cartoes_valor,
  ./view/cores,
  ./view/galeria_1,
  ./view/galeria_2
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
      title = "Título",
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
    customArea = fluidRow(
      actionButton(
        inputId = "myAppButton",
        label = NULL,
        icon = icon("users"),
        width = NULL,
        status = "primary",
        style = "margin: auto",
        dashboardBadge(textOutput("btnVal"), color = "danger")
      )
    ),
    sidebarUserPanel(
      image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png",
      name = "Bem Vindo!"
    ),
    sidebarMenu(
      id = "current_tab",
      flat = FALSE,
      compact = FALSE,
      childIndent = TRUE,
      menuItem(
        "Residômetro",
        tabName = "cartoes",
        icon = icon("sliders")
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
        tabName = "cartoes",
        cartoes_basicos$ui("cartoes_basicos")
      ),
      tabItem(
        tabName = "cartoes_api",
        cartoes_api$ui("cartoes_api")
      ),
      tabItem(
        tabName = "cartoes_com_abas",
        cartoes_abas$ui("cartoes_abas")
      ),
      tabItem(
        tabName = "sortablecards",
        cartoes_organizaveis$ui("cartoes_organizaveis")
      ),
      tabItem(
        tabName = "statsboxes",
        cartoes_descritivos$ui("cartoes_descritivos")
      ),
      tabItem(
        tabName = "valueboxes",
        cartoes_valor$ui("cartoes_valor")
      ),
      tabItem(
        tabName = "colors",
        cores$ui("cores")
      ),
      tabItem(
        tabName = "gallery1",
        galeria_1$ui("galeria_1")
      ),
      tabItem(
        tabName = "gallery2",
        galeria_2$ui("galeria_2")
      )
    )
  ),
  controlbar = dashboardControlbar(
    id = "controlbar",
    skin = "light",
    pinned = TRUE,
    overlay = FALSE,
    cartoes_basicos$ui("teste")
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
  title = "Modelo de Dashboard da DIOBS"
)
