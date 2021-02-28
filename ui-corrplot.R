tabPanel("Correlations",
         column(5,
            h3("Scree plot"),
            bsAlert("screemessage"),
            plotOutput("screeplot")),
         column(5, offset = 2,
            h3("Correlation plot"),
            bsAlert("corrmessage"),
            plotOutput("corrplot")
                )
         ) #tabpanel Corrplot