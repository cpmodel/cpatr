library(RestRserve)
library(dplyr)
library(tidyr)
library(lubridate)
library(jsonlite)

options(scipen = 999)
options(digits = 4)

yaml = T

# Source scripts
# source(file.path('modules','mitigation','mitigation_function.R'))
# D <- readRDS(file.path('modules','mitigation','data_input.RDS'))

source(file.path('AF_APIFunction.R'))
D <- readRDS(file.path('data_input.RDS'))

app = Application$new(middleware = list(CORSMiddleware$new()))


# Calculate growth ----

growth_fun <- function(value, factor){
  v <- value * factor
  return(v)
}

app$add_post(
  path = "/growth",
  FUN = function(.req, .res) {

    postd = .req$body %>% rawToChar() %>% jsonlite::fromJSON()
    value = postd$value
    if(class(value) == 'list'){
      value <- as.data.frame(value)
    }

    res <- try(growth_fun(value, postd$factor), silent = T)

    if(class(res) == 'try-error'){
      result <- list(status = FALSE,
                     msg = 'Invalid Inputs',
                     details = res %>% as.character() %>% paste(collapse = ';')) %>%
        RestRserve::to_json()

      .res$status_code = 401L
      .res$set_content_type("application/json")
      .res$set_body(result)
    }else{
        result = RestRserve::to_json(res)
        .res$set_content_type("application/json")
        .res$set_body(result)
        .res$encode = identity
    }

  })

app$add_route("/growth", method = "OPTIONS", FUN = function(.req, .res) {
  .res$set_header("Allow", "POST, OPTIONS")
})

# app$add_post(
#   path = "/mitigation_old",
#   FUN = function(.req, .res) {
#
#     postd = .req$body %>% rawToChar() %>% jsonlite::fromJSON()
#
#     params = postd$params
#     values_input = postd$init_data
#     if(is.null(values_input)){values_input = NA}
#
#     res <- try(mitigation_function(
#       params$start_year,
#       params$end_year,
#       params$sectors,
#       params$fuel_types,
#       params$countries,
#       values_input, D = D), silent = T)
#
#     if(class(res) == 'try-error'){
#       result <- list(status = FALSE,
#                      msg = 'Invalid Inputs',
#                      details = res %>% as.character() %>% paste(collapse = ';')) %>%
#         RestRserve::to_json()
#
#       .res$status_code = 401L
#       .res$set_content_type("application/json")
#       .res$set_body(result)
#     }else{
#       #result = RestRserve::to_json(res)
#       result = jsonlite::toJSON(res, auto_unbox = T)
#       .res$set_content_type("application/json")
#       .res$set_body(result)
#       .res$encode = identity
#     }
#
#   })
#
# app$add_route("/mitigation_old", method = "OPTIONS", FUN = function(.req, .res) {
#   .res$set_header("Allow", "POST, OPTIONS")
# })

app$add_post(
  path = "/mitigation",
  FUN = function(.req, .res) {

    postd = .req$body %>% rawToChar() %>% jsonlite::fromJSON()

    if(is.null(postd$values_input)){
      values_input = NA
    }else{
      values_input =postd$values_input
    }

    res <- try(mitigation_function(
                postd$start_year,
                postd$end_year,
                postd$params_input,
                values_input,
                D_matrix = D), silent = T)

    if(class(res) == 'try-error'){
      result <- list(status = FALSE,
                     msg = 'Invalid Inputs',
                     details = res %>% as.character() %>% paste(collapse = ';')) %>%
        RestRserve::to_json()

      .res$status_code = 401L
      .res$set_content_type("application/json")
      .res$set_body(result)
    }else{
      #result = RestRserve::to_json(res)
      result = jsonlite::toJSON(res, auto_unbox = T)
      .res$set_content_type("application/json")
      .res$set_body(result)
      .res$encode = identity
    }

  })

app$add_route("/mitigation", method = "OPTIONS", FUN = function(.req, .res) {
  .res$set_header("Allow", "POST, OPTIONS")
})

# Yaml File for Swagger ----
if(yaml){
  app$add_openapi(path = "/openapi.yaml", file_path = 'openapi.yaml')
  app$add_swagger_ui(path = "/doc", path_openapi = "/openapi.yaml", use_cdn = TRUE)
}


backend <- BackendRserve$new()
backend$start(app, http_port = 8080)





