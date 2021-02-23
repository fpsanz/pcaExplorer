library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)
library(tidyverse)
library(FactoMineR)
library(factoextra)
#

ui <- navbarPage(
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
    observeEvent(input$samplesrows,{
        validate(need(input$matriz,""))
        tr$count=tr$count+1
        if(tr$count>=1){
            matrizDatos$datos <- t(matrizDatos$datos)
            }
        })
    observeEvent(input$sampledata,{ # cargar coldata
        matrizDatos$samples <- read.table(input$sampledata$datapath, header = T, sep = ";")
    }) 
    
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
        selectInput("varselected", label="Select vars (max. 3)",
                       choices = names(matrizDatos$samples), multiple=3, selected=NULL)
    })
    
   output$pcaplot <- renderPlot({
       if(is.null(input$varselected) ){
           createAlert(session, "pcamessage", alertId ="messagepca", 
                       title = "Missing variable info",
                       content = "Please select var(s) to colour", 
                       append = FALSE, style = "danger")
           return(NULL)
       }else{
            closeAlert(session, "messagepca")
            variables <- unite( matrizDatos$samples, col = "variable", input$varselected, sep="_" )$variable
            PCAdf <- PCA(matrizDatos$datos, graph = F)
            if(input$tipopca=="ind"){
                fviz_pca_ind(PCAdf, col.ind = variables)
           }else if(input$tipopca=="biplot"){
               fviz_pca_biplot(PCAdf, col.ind = variables)
           }else{
               fviz_pca_var(PCAdf)
           }
        }
   }) 
}
#
shinyApp(ui = ui, server = server)