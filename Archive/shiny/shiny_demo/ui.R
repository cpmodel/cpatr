shinyUI(
  navbarPage(
    title = 'My project',
    windowTitle = 'CPAT Shiny Demo',

    tabPanel("Main",

             column(3,
                    h3('Selection pane'),
                    selectInput(inputId  = 'country2',
                                label = 'Select Country',
                                choices = c('USA', 'Canada')
                    ),

                    selectInput(inputId = 'policy2',
                                label = 'Select policy',
                                choices = c('Carbon Tax', 'Coal Excise')),

                    sliderInput('year_policy2',
                                label =  HTML("Year to introduce new policy<br/>
                                              Year to reach target level"),
                                sep = "",
                                value = c(2022,2025),
                                min = 2022, max = 2035),

                    sliderInput('tg_carbon_price2',
                                label =  HTML("Starting carbon price (real USD per ton CO2)<br/>
                                              Target level of carbon price"),
                                sep = "",
                                value = c(0,50),
                                min = 0, max = 100),

             ),

             column(9,
                    h3('Content pane'),
                      fluidRow(
                        reactable(head(mtcars), fullWidth = F)
                      ),
                    br(),
                     fluidRow(
                        column(5,
                             plotOutput('plot1',  width = "100%")
                              ),
                        column(5,
                             plotOutput('plot2',  width = "100%")
                       )
                     )
             )

             ),

    tabPanel(title = "Main Two",

             column(3,
                    h3('Selection pane'),
                    selectInput(inputId  = 'country',
                                label = 'Select Country',
                                choices = c('USA', 'Canada')
                    ),

                    selectInput(inputId = 'policy',
                                label = 'Select policy',
                                choices = c('Carbon Tax', 'Coal Excise')),

                    numericInput(inputId = 'year_policy',
                                 label = 'Year to introduce new policy',
                                 value = 2022, step = 1, min = 2022, max = 2030),

                    numericInput(inputId = 'st_carbon_price',
                                 label = 'Starting carbon price (real USD per ton CO2)',
                                 value = 0),

                    numericInput(inputId = 'tg_carbon_price',
                                 label = 'Target level of carbon price',
                                 value = 50),

                    numericInput(inputId = 'target_year',
                                 label = 'Year to reach target level',
                                 value = 2035, step = 1, min = 2025, max = 2035)
             ),

             column(9,
                    h3('Content pane')
             )

    ),

    tabPanel("About",

             'some content 3'

             )
  )
)
