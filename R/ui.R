accordion <- function(...) {
  div(class = "accordion container", ...)
}

accordion_item <- function(header, body, show = FALSE, id = header, direction = c("column", "row")) {
  id <- gsub("\\s+|\\(|\\)", "-", id)
  btn <- tags$button(
    class = "accordion-button",
    class = if (!show) "collapsed",
    type = "button",
    "data-bs-toggle" = "collapse",
    "data-bs-target" = paste0("#", id),
    header
  )

  div(
    class = "accordion-item",
    h2(
      class = "accordion-header", btn
    ),
    div(
      id = id,
      class = "accordion-collapse collapse",
      class = if (show) "show",
      div(
        class = "accordion-body",
        style = paste0("display:flex;flex-direction:", match.arg(direction)),
        body
      )
    )
  )
}
