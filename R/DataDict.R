#' DataDictionary
#' @description
#' The class representing a canonical data dictionary
#' @import tibble
#' @import dplyr
#' @importFrom janitor clean_names
#'



DataDictionary <- R6::R6Class(
  classname = "DataDictionary",
  public = list (
    definition = (
      tibble::tibble(
        "idx",
        "dataset_code",
        "original_variable_code",
        "original_variable_name",
        "original_variable_description",
        "original_category",
        "variable_name",
        "variable_code",
        "variable_type",
        "variable_description",
        "suggested_column_name_short",
        "suggested_column_name_long",
        "suggested_variable_categories"
      ) %>% janitor::clean_names() %>% filter(FALSE)
    ),
    dataset_code = NULL,
    datadict_u = NULL,


    #' @description
    #' Initialize the DataDictionary with the data it documents and at least a mock data dictionary
    #' @param dataset_code uniquely identifies a dataset
    #' @param datadict an existing datadict to be validated. It is expected the user does
    #' the initial work of at least a basic data dictionary.
    #'
    #' @return
    #' @export
    #'
    #' @examples
    initialize = function(dataset_code, datadict) {
      self$dataset_code <- dataset_code
      if(is.null(datadict)){
        datadict <- self$definition
      }
      dd <-
        datadict %>% mutate(across(where(is.character), as.character))
      if (self$validate_datadict(dd)) {
        self$datadict_u = dd

      } else {
        missing_cols <- setdiff(names(self$definition), names(dd))
        extra_cols <- setdiff(names(dd), names(self$definition))
        print(list("Missing cols" = missing_cols))
        print(list("Extra cols" = extra_cols))
        ddd <- dd
        purrr::walk(missing_cols, function(x) {
          ddd <<- ddd %>%
            mutate({
              {
                x
              }
            } := "")
        })
        print("Missing cols were added to datadict_u")
        # edit_rhadson(ddd)
        self$datadict_u <- ddd
      }
    },
    validate_datadict = function(datadict) {
      n <- names(datadict)
      if (all(n %in% names(self$definition)) &&
          length(n) == length(names(self$definition))) {
        return(TRUE)
      }
      return(FALSE)

    },
    document_data = function(data, only_new = TRUE) {
      # Check to see which variables are missing and
      # set a datadict with that extra data to be edited
      n <- names(data)
      if (nrow(self$datadict_u) == 0) {
        d <- names(data)
        i <- 1
      } else {
        s <- self$datadict_u$original_variable_name
        d <- setdiff(n, s)
        i <- max(self$datadict_u$idx) + 1
      }
      edit <- map_dfr(d, function(x) {
        t <- tibble_row(
          idx = i,
          dataset_code = self$dataset_code,
          original_variable_code = "",
          original_variable_name = x,
          original_variable_description = "",
          original_category = "",
          variable_name = x,
          variable_code = x,
          variable_type = class(x),
          variable_description = x,
          suggested_column_name_short = glue::glue("{self$dataset_code}.{stringr::str_to_lower(x)}") %>% as.character(.),
          suggested_column_name_long = glue::glue("{self$dataset_code}.{stringr::str_to_lower(x)}") %>% as.character(.),
          suggested_variable_categories = "",
          is_new = TRUE
        )
        i <<- i + 1
        t
      })
      if (only_new != TRUE) {
        edit <- bind_rows(self$datadict_u %>% mutate(is_new = FALSE),
                          edit)
      }
      new_ds <- edit_rhadson(edit)

      if (only_new) {
        self$datadict_u <- bind_rows(self$datadict_u,
                                     new_ds)
      } else {
        self$datadict_u <- new_ds
      }
      return(self$datadict_u)
    }

  )
)
#active = list (
#              ),
#private = list (
#              ),)
