tabPanel("HCPC",
         column(3,
                tags$h4(""),
                sliderInput(inputId = "sliderhcpc",label = "Select number of components", min=2, max=10, step = 1, value = 2)
         ),
         column(8,
                tags$h3("Interactive hierarchical cluster on PCA"),
                bsAlert("hcpcmessage"),
                fluidRow(
                    column(width = 6,
                           plotOutput("hcpc")
                ),
                    column(width=6,
                           plotOutput("clusterhcpc"))
                ),
                br(),
                fluidRow(
                    plotOutput("dendrohcpc")
                )
         )
) #tabpanel hierachicalplot