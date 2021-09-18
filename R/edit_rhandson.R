library(shiny)
library(rhandsontable)

# these functions come from rstudio/addinexamples
stableColumnLayout <- function(...) {
  dots <- list(...)
  n <- length(dots)
  width <- 12 / n
  class <- sprintf("col-xs-%s col-md-%s", width, width)
  shiny::fluidRow(lapply(dots, function(el) {
    shiny::div(class = class, el)
  }))
}

isErrorMessage <- function(object) {
  inherits(object, "error_message")
}

errorMessage <- function(type, message) {
  structure(list(type = type, message = message),
            class = "error_message")
}

edit_rhadson <- function (externalData)
{
  context <- rstudioapi::getActiveDocumentContext()
  text <- context$selection[[1]]$text
  defaultData <- text
  fname = gsub("\\\\", "/", tempfile())
  ui <-
    miniUI::miniPage(
      miniUI::gadgetTitleBar("Edit a data.frame"),
      miniUI::miniContentPanel(
        stableColumnLayout(
          shiny::textInput("data",
                           "Data", value = defaultData),
          shiny::radioButtons(
            "outType",
            "Output type",
            choices = c(
              `Update original data` = "update",
              `Print updates to script (no update)` = "print"
            )
          )
        ),
        shiny::uiOutput("pending"),
        rHandsontableOutput("hot")
      )
    )
  server <- function(input, output, session) {
    values = shiny::reactiveValues()
    setHot = function(x)
      values[["hot"]] = x
    reactiveData <- shiny::reactive({
      dataString <- input$data
      if (!nzchar(dataString))
      {
        if(!is.null(externalData)){

          golem::print_dev("External data done")
        data <- as.data.frame(externalData)
        } else {
          if (!exists(dataString, envir = .GlobalEnv))
            return(errorMessage(
              "data",
              paste("No dataset named '",
                    dataString, "' available.")
            )) else {
              data <- get(dataString, envir = .GlobalEnv)
            }
        }
      } else {
        return(errorMessage("data", "No dataset available."))
      }

      data
    })
    output$pending <- shiny::renderUI({
      data <- reactiveData()
      if (isErrorMessage(data))
        htmltools::h4(style = "color: #AA7732;", data$message)
    })
    output$hot <- renderRHandsontable({
      data <- reactiveData()
      if (isErrorMessage(data))
        return(NULL)
      # browser()
      if (is.null(input$hot))
        DF = data
      else
        DF = hot_to_r(input$hot)
      setHot(DF)
      rhandsontable(DF, width = 1800, height = 2000) %>% hot_table(highlightCol = TRUE,
                                                                   highlightRow = TRUE)
    })
    shiny::observeEvent(input$done, {
      if (input$outType == "print") {
        rslt <- capture.output(dput(values[["hot"]]))
        rstudioapi::insertText(Inf, paste0(input$data,
                                           " = ", paste(rslt, collapse = "\n")))
      }
      else {
        if (nzchar(input$data) && !is.null(values[["hot"]])) {
          saveRDS(values[["hot"]], fname)
          code <- paste(input$data, " = readRDS('", fname,
                        "')", sep = "")
          rstudioapi::sendToConsole(code)
        }
      }
      invisible(shiny::stopApp())
    })
  }
  viewer <- shiny::dialogViewer("Edit", width = 1000, height = 800)
  shiny::runGadget(ui, server, viewer = browserViewer(browser = getOption("browser")))
}
