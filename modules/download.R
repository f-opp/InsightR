#Download Module
downloadObjUI <- function(id) {
  ns <- NS(id)
  
  downloadButton(ns("data_download"), label = "Download Data", class = "btn-primary")
}

downloadObj <- function(input, output, session, name , data, extension) {
  output$data_download <- downloadHandler(

    filename = function() {
      paste("insightR-", name(), "-", Sys.Date(), extension(), sep="")
    },
    content = function(file) {
      if (class(data())=="data.frame"){
        write.csv(data(), file) # add parentheses to data arg if reactive
      }
      if (class(data())!="data.frame"){
        saveRDS(data(), file) # add parentheses to data arg if reactive
      }
      
    }
  )
}