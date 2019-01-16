#' @details The \code{getURL} method contains server logic for encoding
#'   and restoring the widgets' values. It is called from inside the app's 
#'   server script, and can take the \code{session} objects as argument.
#' @section ShinyURL options:  
#' \describe{
#'  \item{\code{debug = TRUE}}{Print debug messages to the console}
#' }
#' @param session Typically the same as the optional parameter passed into the 
#'   Shiny server function as an argument; if missing defaults to 
#'   \code{getDefaultReactiveDomain()}
#' @param inputId Named list of options
#' @return \code{getURL} returns a reactive expression evaluating to 
#'   the app's URL.
#' @rdname shinyURL
#' @export
getURL = function(session, inputId) {
  clientData = isolate(reactiveValuesToList(session$clientData))
  
  ## base URL which is not supposed to change
  baseURL = paste0(clientData$url_protocol, "//",
                   clientData$url_hostname,
                   ## add port number if present
                   if( (port=clientData$url_port)!="" ) paste0(":", port),
                   clientData$url_pathname)
  
  queryString = reactive({
    ## all.names = FALSE excludes objects with a leading dot, in particular the
    ## ".url" field to avoid self-dependency
    inputValues = reactiveValuesToList(session$input, all.names=FALSE)
    
    ## quit if there is there are no inputs to encode
    if (length(inputValues)==0) return()
    
    ## remove actionButtons
    isActionButton = unlist(lapply(inputValues, inherits, "shinyActionButtonValue"), use.names=FALSE)
    inputValues = inputValues[!isActionButton]
    
    ## remove ggvis specific inputs
    idx = grep("_mouse_(over|out)$", names(inputValues))
    if ( length(idx) > 0 ) inputValues = inputValues[-idx]
    
    inputValues = mapply(function(name, value) {
      ## this is important to be able to have all checkboxes unchecked
      if (is.null(value))
        ""
      else {
        if (length(value) == 1L) {
          ## encode TRUE/FALSE as T/F
          if (is.logical(value)) {
            if (isTRUE(value)) "T" else "F"
          }
          else value
        }
        else {
          cl = class(value)
          ## expand checkbox group and multiple select vectors
          if (cl=="character") {
            setNames(as.list(value), sprintf("%s[%s]", name, seq_along(value)))
          }
          ## encode range vectors as comma separated string
          else {
            if (cl=="Date") value = as.integer(value)
            paste(value, collapse=",")
          } 
        }
      }
    }, names(inputValues), inputValues, SIMPLIFY=FALSE)
    
    ## remove names of sublists before flattening
    names(inputValues)[sapply(inputValues, is.list)] = ""
    inputValues = unlist(inputValues)
    ##HACK: remove hot table app -----------
    #inputValues$hot_btable_fbapp_sbase <- NULL
    ##--------------------------------------
    #TODO inputValues remove hot_btable_fbapp_sbase id
    #URLencode(paste(names(inputValues), inputValues, sep = "=", collapse = "&"))
    res<- inputValues
  })
  
  observe({
    debugMsg(".updateURL")
    updateTextInput(session, inputId, value = url())
    updateTextInput(session, ".shinyURL.queryString", value = queryString())
  }, priority = -999)
  
  url = reactive({
    print(baseURL)
    #print("reactive")
    #print( paste(c(baseURL, queryString()), collapse = "?"))
    paste(c(baseURL, queryString()), collapse = "?")
    
  })
  
  a<- queryString()
  a
}

