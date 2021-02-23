tabPanel("PCAplot",
         column(3,
                uiOutput("varsel"),
                selectInput("tipopca", "Select plot",
                            choices =list(Individuals="ind",Variables="vars",Biplot="biplot"),
                            selected = "ind")
                ),
         column(9, 
                bsAlert("pcamessage"),
                plotOutput("pcaplot") 
                )
)# tabpanel pcaplot