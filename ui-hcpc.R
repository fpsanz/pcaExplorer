tabPanel("HCPC",
         column(3,
                tags$h4(""),
                sliderInput(inputId = "sliderhcpc",label = "Select number of components", min=2, max=10, step = 1, value = 2)
         ),
         column(8,
                tags$h3("Interactive hierarchical cluster on PCA"),
                bsAlert("hcpcmessage"),
                plotOutput("hcpc"),
                plotOutput("clusterhcpc"),
                plotOutput("dendrohcpc")
         )
) #tabpanel hierachicalplot