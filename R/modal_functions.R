
#### Error modal
show_error_modal <- function(message) {
  shiny::showModal(
    shiny::modalDialog(
      title = shiny::tagList("Error"),
      message,
      easyClose = TRUE,
      footer = shiny::modalButton("Close")#,class = "custom-modal modal-error"
    )|> shiny::tagAppendAttributes(class = "custom-modal modal-error")
  )
}

### Warning modal
show_warning_modal <- function(message) {
  shiny::showModal(
    shiny::modalDialog(
      title = shiny::tagList("Warning"),
      message,
      easyClose = TRUE,
      footer = shiny::modalButton("Close")
      # class = "custom-modal modal-warning"
    ) |> shiny::tagAppendAttributes(class = "custom-modal modal-warning")

  )
}

#### Success modal
show_success_modal <- function(message) {
  shiny::showModal(
    shiny::modalDialog(
      title = shiny::tagList("Success"),
      message,
      easyClose = TRUE,
      footer = shiny::modalButton("Close")#,class = "custom-modal modal-success"
    ) |> shiny::tagAppendAttributes(class = "custom-modal modal-success")
  )
}

#### Informational modal
show_info_modal <- function(message) {
  shiny::showModal(
    shiny::modalDialog(
      title = shiny::tagList("Information"),
      message,
      easyClose = TRUE,
      footer = shiny::modalButton("Close")#,class = "custom-modal modal-info"
    ) |> shiny::tagAppendAttributes(class = "custom-modal modal-info")
  )
}


### Backup to use in case things fail again
# show_warning_modal <- function(message) {
#
#   shiny::showModal(
#     shiny::modalDialog(
#       title = shiny::div(
#         style = "color:#CC5500; font-weight:bold;",
#         "ERROR!!!"
#       ),
#
#       shiny::div(message),
#
#       easyClose = TRUE,
#
#       footer = shiny::modalButton("OK"),
#
#       # Apply style to entire modal content
#       size = "m",
#
#       # Add custom CSS class via tagList
#       shiny::tagList(
#         shiny::tags$head(
#           shiny::tags$style(shiny::HTML("
#             .modal-content {
#               background-color: #FFD580 !important;
#               color: #CC5500 !important;
#             }
#             .modal-header {
#               border-bottom: 1px solid #CC5500;
#             }
#             .modal-footer {
#               border-top: 1px solid #CC5500;
#             }
#           "))
#         )
#       )
#     )
#   )
# }
