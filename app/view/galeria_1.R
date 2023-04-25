box::use(
  shiny[...],
  bs4Dash[...],
  stringi[stri_rand_lipsum]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        title = "Acordeão",
        footer = tagList(
          h4("Acordeão no rodapé!"),
          accordion(
            id = "accordion1",
            accordionItem(
              title = "Item 1", 
              status = "danger",
              p(stri_rand_lipsum(1, start_lipsum = TRUE))
            ),
            accordionItem(
              title = "Item 2", 
              status = "warning",
              p(stri_rand_lipsum(1, start_lipsum = TRUE))
            )
          )
        )
      ),
      box(
        title = "Carrossel",
        carousel(
          id = "mycarousel",
          width = 12,
          carouselItem(
            tags$img(src = "https://via.placeholder.com/500")
          ),
          carouselItem(
            tags$img(src = "https://via.placeholder.com/500")
          ),
          carouselItem(
            tags$img(src = "https://via.placeholder.com/500")
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "bs4Quote",
        fluidRow(
          blockQuote("Blablabla", color = "indigo"),
          blockQuote("Blablabla", color = "danger"),
          blockQuote("Blablabla", color = "teal"),
          blockQuote("Blablabla", color = "orange"),
          blockQuote("Blablabla", color = "warning"),
          blockQuote("Blablabla", color = "fuchsia")
        )
      )
    ),
    fluidRow(
      box(
        title = "Progress bars",
        footer = tagList(
          progressBar(
            value = 5,
            striped = FALSE,
            status = "info"
          ),
          progressBar(
            value = 10,
            striped = TRUE,
            status = "maroon"
          )
        ),
        progressBar(
          value = 80,
          vertical = TRUE,
          status = "success"
        ),
        progressBar(
          value = 100,
          vertical = TRUE,
          striped = TRUE,
          status = "danger"
        )
      ),
      box(
        title = "Alerts",
        elevation = 4,
        actionButton(ns("show_alert"), "Show"),
        actionButton(ns("hide_alert"), "Hide"),
        div(id = ns("alert_anchor"))
      )
    ),
    fluidRow(
      box(
        title = "Callouts",
        callout(
          title = "I am a danger callout!",
          elevation = 4,
          status = "danger",
          width = 12,
          "There is a problem that we need to fix. 
        A wonderful serenity has taken possession of 
        my entire soul, like these sweet mornings of 
        spring which I enjoy with my whole heart."
        )
      )
    ),
    fluidRow(
      box(
        title = "Timeline",
        timelineBlock(
          width = 12,
          reversed = TRUE,
          timelineEnd(color = "danger"),
          timelineLabel("10 Feb. 2014", color = "info"),
          timelineItem(
            title = "Item 1",
            icon = icon("gears"),
            color = "success",
            time = "now",
            footer = "Here is the footer",
            "This is the body"
          ),
          timelineItem(
            title = "Item 2",
            border = FALSE
          ),
          timelineLabel("3 Jan. 2014", color = "primary"),
          timelineItem(
            title = "Item 3",
            icon = icon("paint-brush"),
            color = "warning",
            timelineItemMedia(image = "https://via.placeholder.com/150"),
            timelineItemMedia(image = "https://via.placeholder.com/150")
          ),
          timelineStart(color = "danger")
        )
      ),
      timelineBlock(
        width = 6,
        timelineEnd(color = "danger"),
        timelineLabel("10 Feb. 2014", color = "info"),
        timelineItem(
          title = "Item 1",
          icon = icon("gears"),
          color = "success",
          time = "now",
          footer = "Here is the footer",
          "This is the body"
        ),
        timelineItem(
          title = "Item 2",
          border = FALSE
        ),
        timelineLabel("3 Jan. 2014", color = "primary"),
        timelineItem(
          title = "Item 3",
          icon = icon("paint-brush"),
          color = "warning",
          timelineItemMedia(image = "https://via.placeholder.com/150"),
          timelineItemMedia(image = "https://via.placeholder.com/150")
        ),
        timelineStart(color = "danger")
      )
    ),
    br(),
    fluidRow(
      box(
        title = "Stars",
        starBlock(5),
        starBlock(5, color = "success"),
        starBlock(1, color = "danger"),
        starBlock(3, color = "info")
      ),
      box(
        title = "Attachment example",
        attachmentBlock(
          image = "https://adminlte.io/themes/v3/dist/img/user1-128x128.jpg",
          title = "Test",
          href = "https://google.com",
          "This is the content"
        )
      )
    ),
    h4("bs4Table"),
    fluidRow(
      bs4Table(
        cardWrap = TRUE,
        bordered = TRUE,
        striped = TRUE,
        list(
          list(
            income = "$2,500 USD", 
            status = dashboardBadge(
              "Pending",
              position = "right",
              color = "danger",
              rounded = TRUE
            ), 
            progress = progressBar(value = 50, status = "pink", size = "xxs"), 
            text = "test", 
            confirm = actionButton(
              "go",
              "Go"
            )
          ),
          list("$2,500 USD", "NA", "NA", "test", "NA")
        )
      )
    ) 
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    

    # alerts ------------------------------------------------------------------

    observeEvent(input$show_alert, {
      createAlert(
        id = "alert_anchor",
        options = list(
          title = "Be Careful!",
          status = "danger",
          closable = TRUE,
          width = 12,
          content = "Danger alert preview. This alert is dismissable. 
          A wonderful serenity has taken possession of my entire soul, 
          like these sweet mornings of spring which 
          I enjoy with my whole heart."
        )
      )
    })

    observeEvent(input$hide_alert, {
      closeAlert(id = "alert_anchor")
    })

    # alert callback event
    observeEvent(input$alert_anchor, {
      alertStatus <- if (input$alert_anchor) "opened" else "closed"
      toastColor <- if (input$alert_anchor) "bg-lime" else "bg-fuchsia"
      toast(
        title = sprintf("Alert succesfully %s!", alertStatus),
        options = list(
          class = toastColor,
          autohide = TRUE,
          position = "bottomRight"
        )
      )
    })

  })
}
