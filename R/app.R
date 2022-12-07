library(shiny)

monthApp <- function(...) {
  months <- c(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  )


source('ui.R', local = TRUE)
source('server.R', local = TRUE)

# Run the application
shinyApp(ui = ui, server = server)

}



