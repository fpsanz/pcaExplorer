tabPanel("Home",
         column(
             width = 2,
             box(
                 width = 12,
                 height = "900px",
                 title = "",
                 status = "info",
                 column(width = 11, offset = 1,
                        br(),
                        fileInput(
                            "matriz",
                            "Select matrix",
                            accept = c(".xlsx", ".tsv", ".csv"),
                            buttonLabel = "File",
                            width = "90%"
                        ),
                        p("Are samples in matrix by rows or columns?"),
                        switchInput("samplesrows", value=TRUE, onLabel = "rows", 
                                    offLabel = "cols", size = "normal"),
                        br(),
                        fileInput(
                            "sampledata",
                            "Select Sample data file",
                            accept = c(".xlsx", ".tsv", ".csv"),
                            buttonLabel = "File",
                            width = "90%"
                        )
                 ),# fin column box
                 tags$div(class = "bottomdiv",
                          column(width = 9, offset = 1,
                                 tags$hr(),
                                 fluidRow(
                                     actionButton("aboutButton", "About"),
                                     actionButton(inputId = "resetbutton", "Reset", size = "md", color = "danger"))
                          ),
                          style="position: absolute; bottom: 10px; width:100%;")
             )
         ),
         column(10,
                fluidRow( box( width = 12, status = "info",
                               title = "PCA explorer App"
                               # h3("Ivan Ferreiro"),
                               # p("Desde aquí, desde mi casa"),
                               # p("Veo la playa vacía"),
                               # p("Ya lo estaba, hace unos días"),
                               # p("Ahora, está llena de lluvia")
                ) ),
                br(),
                fluidRow(
                    column(2, 
                           uiOutput("kmo") ),
                    column(width = 8, 
                           box( width = 12, status = "info",
                                bsAlert("datosmessage"),
                                DT::dataTableOutput("matrix")
                           )
                    )),
                br(),
                fluidRow(
                    column(width = 6, offset = 3, 
                           box( width = 12 ,status = "info",
                                bsAlert("samplesmessage"),
                                DT::dataTableOutput("sampleData")
                           ) 
                    )
                )
         )
)