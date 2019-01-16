library(shiny)
library(shinyURL)

shinyServer(function(input, output, session) {
  a<- getURL(session, inputId = ".shinyURL")
  saveRDS(object = a,file = "omar.rds")
  shinyURL.server(session)
  
})
