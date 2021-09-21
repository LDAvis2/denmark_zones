modal <- function(id, title, body, buttons = list()) {
  div(
    class = "modal fade",
    id = id,
    tabindex = "-1",
    role = "dialog",
    "aria-hidden" = "true",
    div(
      class = "modal-dialog modal-dialog-centered modal-lg",
      div(
        class="modal-content",
        div(
          class="row justify-content-center",
          style="padding: 70px; text-align: center;",
          h1(title),
          p(body),
          div(
            Map(
              function(label, file) {
                a(
                  class="btn btn-secondary btn-md text-uppercase",
                  href = file,
                  download = basename(file),
                  label
                )
              }, buttons, names(buttons)
            )
          )
        )
      )
    )
  )
}
