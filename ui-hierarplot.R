tabPanel("HierachicalPlot",
         column(3,
                tags$h4("Select data for hierarchical cluster"),
                switchInput("dendroswitch", value = TRUE, onLabel = "raw", offLabel = "pca",
                            onStatus = "primary", offStatus = "danger")
                ),
         column(8,
                bsAlert("dendromessage"),
                plotOutput("dendroplot")
                )
         ) #tabpanel hierachicalplot