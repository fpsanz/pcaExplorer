library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(psych)
source("aux.R")
#

ui <- navbarPage(id ="navpanel",
        title ="HolaCaracola",
        theme = shinytheme("superhero"),
        collapsible = TRUE,
        source(file = "ui-home.R", local = TRUE, encoding = "UTF-8")$value,
        source(file = "ui-pcaplot.R", local = TRUE, encoding = "UTF-8")$value,
        source(file = "ui-corrplot.R", local = TRUE, encoding = "UTF-8")$value,
        source(file = "ui-eigenplot.R", local = TRUE, encoding = "UTF-8")$value,
        source(file = "ui-hierarplot.R", local = TRUE, encoding = "UTF-8")$value,
        tags$script(HTML("var header = $('.navbar > .container-fluid > .navbar-collapse');
                       header.append('<div style=\"float:right\"><ul style=\"list-style-type: none;margin:0;padding:0;overflow:hidden;\"><li style=\"float:left;margin-right: 10px;margin-top: 12px;height: 30px;\"><a href=\"https://jacob.cea.fr/drf/ifrancoisjacob/Pages/Departements/MIRCen/themes/astrocytes-reactifs-biomarqueurs-imagerie-cibles-therapeutiques.aspx\"><img src=\"mircen.png\" alt=\"imib\", style=\"width:90px;\"></a></li><li style=\"float:left;\"><a href=\"http://www.imib.es/web/personal.jsf?id=7961\"><img src=\"imib.png\" alt=\"imib\", style=\"width:60px;\"></a></li></div>');
                       console.log(header)")),
        includeCSS("./www/mystyle.css"),
        setShadow( class = "box")
    )
#
server <- function(input, output, session){
    
    tr <-reactiveValues(count=0)
    matrizDatos <- reactiveValues()
    
## tab Home #########################################
    observeEvent(input$matriz,{ # cargar mtriz datos
        matrizDatos$datos <- read.table(input$matriz$datapath, header = T, sep = ";", row.names = 1)
        
    }) 
    
    # trasponer matriz al pulsar switch
    observeEvent(input$samplesrows,{
        validate(need(input$matriz,""))
        tr$count=tr$count+1
        if(tr$count>=1){
            matrizDatos$datos <- t(matrizDatos$datos)
            }
        })
    # cargar coldata
    observeEvent(input$sampledata,{ # cargar coldata
        matrizDatos$samples <- read.table(input$sampledata$datapath, header = T, sep = ";")
    }) 
    
    output$kmo <- renderUI({
        validate(need(matrizDatos$datos,""))
        kmo <- round( KMO(matrizDatos$datos)$MSA, 3)
        tagList(htmltools::tags$h3(paste0("MSA: ",kmo)),
        br(),
        htmltools::tags$h4("MSA intervals:"),
        htmltools::tags$p("Values >0.9: marvelous"),
        htmltools::tags$p("Values 0.8-0.9: meritorious"),
        htmltools::tags$p("Values 0.7-0.8: middling "),
        htmltools::tags$p("Values 0.6-0.7: mediocre"),
        htmltools::tags$p("Values 0.5-0.6: miserable"),
        htmltools::tags$p("Values < 0.5: unacceptable"),
        
        )
    })
    
    # mostrar matriz datos
    output$matrix <- DT::renderDataTable({
        if(is.null(matrizDatos$datos) ){
            createAlert(session, "datosmessage", alertId ="messagedatos", 
                        title = "Missing data", content = "Please upload matrix to preview data", 
                        append = FALSE, style = "danger")
            return(NULL)
        }else{
            closeAlert(session, "messagedatos")
            DT::datatable(matrizDatos$datos, 
                          style = "bootstrap4", options = list(scrollX=TRUE))
        }            
        })
    # mostrar coldata
    output$sampleData <- DT::renderDataTable({
        if(is.null(matrizDatos$datos) ){
            createAlert(session, "samplesmessage", alertId ="messagesamples", 
                        title = "Missing samples info",
                        content = "Please upload sample datatable to preview data about samples", 
                        append = FALSE, style = "danger")
            return(NULL)
        }else{
            closeAlert(session, "messagesamples")
            DT::datatable(matrizDatos$samples,
                          style = "bootstrap4", options = list(scrollX=TRUE))
        }
        })
    
## tab del PCA plot ###################################
    output$varsel <- renderUI({
        selectizeInput("varselected", label="Select vars (max. 3)",
                       choices = names(matrizDatos$samples), multiple=T,
                    selected=NULL, options = list(maxItems=3))
    })
    # seleccionar qué componentes plotear
    ndmax <- reactive({matrizDatos$datos})
    output$dimensions <- renderUI({
        validate(need(ndmax(),""))
        if( length(names(matrizDatos$datos))<5 ){
            ndmax=length(names(matrizDatos$datos))
        }else{
            ndmax <- 5
        }
        selectizeInput("ndmax", "Select dimensions to plot", choices = seq_len(ndmax),
                    multiple = TRUE, selected = c(1,2), options = list(maxItems=2))
    })
    # pca plot ################
   output$pcaplot <- renderPlot({
        if(is.null(input$varselected) | length(input$ndmax)!=2 ){
            createAlert(session, "pcamessage", alertId ="messagepca",
                        title = "Missing variable info",
                        content = "Please select var(s) to colour and 2 dimensions",
                        append = FALSE, style = "danger")
            shiny:::reactiveStop()
        }else{
            closeAlert(session, "messagepca")
            variables <- unite( matrizDatos$samples, col = "variable", input$varselected, sep="_" )$variable
            PCAdf <- PCA(matrizDatos$datos, graph = F)
            if(input$tipopca=="ind"){
                fviz_pca_ind(PCAdf, col.ind = variables, pointsize=3,
                             labelsize=5, axes = as.numeric(input$ndmax))+
                    theme(text = element_text(size=18))
            }else if(input$tipopca=="biplot"){
                fviz_pca_biplot(PCAdf, col.ind = variables, pointsize=3,
                                labelsize=5, axes = as.numeric(input$ndmax))+
                    theme(text = element_text(size=18))
            }else{
                fviz_pca_var(PCAdf, pointsize=3, labelsize=5, axes = as.numeric(input$ndmax))+
                    theme(text = element_text(size=18))
            }
        }
   })
}
#
shinyApp(ui = ui, server = server)