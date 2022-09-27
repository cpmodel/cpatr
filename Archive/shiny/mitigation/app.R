
library(shiny)
# library(shinythemes)
library(shinyFeedback)
library(shinyWidgets)
library(httr)
library(dplyr)
library(tidyr)
library(purrr)
library(reactable)

countries <- read.csv('countries.csv')
sectors <- read.csv('sectors.csv')
ftypes <- read.csv('ftypes.csv')

run_request_wrapped <- function(url, path, data){
  post_d = RestRserve::to_json(data)
  res = httr::POST(
    paste0(url, path),
    body = post_d,
    encode = "json"
  )
  return(res)
}



ui <-  navbarPage(    #theme = shinytheme('lumen'),
                      title = 'Logo placeholder',
                      #title = 'Placeholder for Logo',
                      id = 'navbar',
                      collapsible  = TRUE,
                      windowTitle = 'CPAT Tool',

                  tabPanel(span('CPAT Tool', style = 'font-size: 24px'),

                           # tags$head(includeCSS("styles.css")),
                           useShinyFeedback(),

                           column(1,),

                           column(2, id = 'colz1',

                                  fluidRow(
                                    multiInput(
                                      inputId = "countries",
                                      label = "Select Country :",
                                      #choices = countries$countrycode,
                                      choiceNames = countries$countryname,
                                      choiceValues = countries$countrycode,
                                      width = '100%',
                                    ),

                                    multiInput(
                                      inputId = "sectors",
                                      label = "Select Sector :",
                                      choiceNames = sectors$Sector,
                                      choiceValues = sectors$Code,
                                      width = '100%',
                                    ),

                                    multiInput(
                                      inputId = "fuel_types",
                                      label = "Select Fuel Type :",
                                      choiceNames = ftypes$Type,
                                      choiceValues = ftypes$Val,
                                      width = '100%',
                                    )
                                  )


                                  ),

                           column(9,
                                  sliderInput('years', label = 'Select Years',
                                              value = c(2019, 2035),sep = "",
                                              min = 2019, max = 2035, step = 1),

                                  actionButton('reset', 'Reset inputs'),
                                  #rHandsontableOutput('rht'),
                                  reactableOutput('rht'))



                           ),
                  tabPanel(span('About', style = 'font-size: 24px'))

)

server <- function(session, input, output) {

  output$rht <- renderReactable({

    req(input$sectors)
    req(input$fuel_types)
    req(input$countries)



    params_input = apply(expand.grid(tolower(input$countries),
                      'mit.ener',
                      input$fuel_types,
                      input$sectors,
                      '1'), 1, paste, collapse=".")

    st_y <- input$years[1]


    if(st_y == 2019){st_y = 2020}
    data = list(
      start_year = st_y,
      end_year = input$years[2],
      params_input = params_input,
      values_input = NULL
    )

    res <- run_request_wrapped(
      url = 'http://cpat-r.easyclimatedata.com:8080/',
      path = 'mitigation',
      data = data
    )

    res2 <- res$content %>% rawToChar() %>% jsonlite::fromJSON() %>% as.data.frame()
    colnames(res2) = gsub('X','', colnames(res2))

    res2 %>%
      distinct() %>%
      drop_na() %>%
      pivot_wider(names_from = name, values_from = value) %>%
      reactable()
    # res2 %>% rhandsontable::rhandsontable(rowHeaders = NULL)
  })

  observeEvent(input$reset,{
    updateMultiInput(session, 'countries', selected  = character(0))
    updateMultiInput(session, 'sectors', selected  = character(0))
    updateMultiInput(session, 'fuel_types', selected  = character(0))
    updateRadioButtons(session, 'years', selected = c(2019, 2035))
  })

}


shinyApp(ui = ui, server = server)
