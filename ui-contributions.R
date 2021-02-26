tabPanel("EigenPlot",
         tabsetPanel(
             tabPanel("Variables",
                      fluidRow(
                          column(5,
                                 h3("Contributions of variables to Dim-1"),
                                 bsAlert("vardim1"),
                                 plotOutput("vard1")
                                 ),
                          column(5, offset = 2,
                                 h3("Contributions of variables to Dim-2"),
                                 bsAlert("vardim2"),
                                 plotOutput("vard2")
                                 )
                          ),
                      fluidRow(
                          column(5,
                                 h3("Contributions of variables to Dim-3"),
                                 bsAlert("vardim3"),
                                 plotOutput("vard3")
                                 ),
                          column(5, offset = 2,
                                 h3("Contributions of variables to Dim-4"),
                                 bsAlert("vardim4"),
                                 plotOutput("vard4")
                                 )
                          )
                      ), #fin tab variables
             tabPanel("Individuals",
                      fluidRow(
                          column(5,
                                h3("Contributions of individuals to Dim-1"),
                                 bsAlert("inddim1"),
                                plotOutput("ind1")
                                ),
                          column(5, offset = 2,
                                 h3("Contributions of individuals to Dim-2"),
                                 bsAlert("inddim2"),
                                 plotOutput("ind2")
                                 )
                      ),
                      fluidRow(
                          column(5,
                                h3("Contributions of individuals to Dim-3"),
                                 bsAlert("inddim3"),
                                plotOutput("ind3")
                                ),
                          column(5, offset = 2,
                                 h3("Contributions of individuals to Dim-4"),
                                 bsAlert("inddim4"),
                                 plotOutput("ind4")
                                 )
                      ))
         ) # tabpanel conrib_plot
)