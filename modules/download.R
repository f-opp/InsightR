#Download Module
downloadObjUI <- function(id) {
  ns <- NS(id)
  
  downloadButton(ns("data_download"), label = "Download Data", class = "btn-primary")
}

downloadObj <- function(input, output, session, data, extension) {
  
  output$data_download <- downloadHandler(
    filename = function() {
      paste("02-step-", Sys.Date(), extension, sep="")
    },
    content = function(file) {
      write.csv(data(), file) # add parentheses to data arg if reactive
    }
  )
}