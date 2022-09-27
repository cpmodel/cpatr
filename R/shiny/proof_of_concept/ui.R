shinyUI(
  navbarPage(
    title = 'Logo placeholder',
    windowTitle = 'Shiny Proof of Concept',

    tabPanel(
      #"Main",
             title = div(span("Main", style = 'color:#0C6554; font-size:16pt')),
             tags$head( includeCSS("styles.css") ),

             fluidPage(

               fluidRow(column(1,),
                column(10,id = 'pane1',
                       div(span('Input Pane',
                            style = 'margin-left:18px;color:#0C6554; font-weight:bold; font-size:16pt')),
                       column(2,selectInput('sectors', 'Select Sector', choices = c('Energy'))),
                       column(2,selectInput('country', 'Select Country', choices = c('US', 'UK'))),
                       column(2,'Selectors placeholder')

                       ),
                column(1)),

               br(),

               fluidRow(column(4,),
                        column(4,align = "center",
                               actionButton('calc', 'Calculate', width = '70%',
                                            style="color: #fff; background-color: #5C8AA3;
                                            border-color: #5C8AA3")),
                        column(4)),

               br(),

               fluidRow(column(1,),
                        column(10, id = 'pane3',
                               div(span('Results Pane',
                                        style = 'margin-left:18px;color:#0C6554; font-weight:bold; font-size:16pt')),
                               ),
                        column(1)),

               br(),

               fluidRow(column(3,),
                        column(3,align = "center",
                               actionButton('save', 'Save to Database', width = '70%',
                                            style="color: #fff; background-color: #5C8AA3;
                                            border-color: #5C8AA3")),
                        column(3,align = "center",
                               actionButton('export', 'Export as Excel', width = '70%',
                                            style="color: #fff; background-color: #5C8AA3;
                                            border-color: #5C8AA3")),
                        column(3)),

             ),


             ),


    tabPanel(title = div(span("About", style = 'color:#0C6554; font-size:16pt')),
             column(1),
             column(10, includeMarkdown('Description.Rmd')))
  )
)
