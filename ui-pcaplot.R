tabPanel("PCAplot",
             column(3,
                    uiOutput("varsel"),
                    uiOutput("dimensions"),
                    selectInput("tipopca", "Select plot",
                                choices =list(Individuals="ind",Variables="vars",Biplot="biplot"),
                                selected = "ind")
                    ),
             column(8, 
                    bsAlert("pcamessage"),
                    plotOutput("pcaplot", width = "100%", height = "600px") 
                    )
    )# tabpanel pcaplot
