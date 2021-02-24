tabPanel("CorrPlot",
         column(3),
         column(8,
            bsAlert("corrmessage"),
            h3("Scree plot"),
            renderPlot("screeplot"),
            h3("Correlation plot"),
            renderPlot("corrplot")
                )
         ) #tabpanel Corrplot