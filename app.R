library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)
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
    
    matrizDatos <- reactiveValues()
    
    observeEvent(input$matriz,{ # cargar mtriz datos
        matrizDatos$datos <- read.table(input$matriz$datapath, header = T, sep = ";", row.names = 1)
    }) 
    
    observeEvent(input$sampledata,{ # cargar coldata
        matrizDatos$samples <- read.table(input$sampledata$datapath, header = T, sep = ";")
    }) 
    
    output$varsel <- renderUI({
        selectInput("varselected", label="Select vars (max. 3)",
                       choices = names(matrizDatos$samples), multiple=3, selected=NULL)
    })
    
    
    output$matrix <- DT::renderDataTable({
        if(is.null(matrizDatos$datos) ){
            createAlert(session, "datosmessage", alertId ="messagedatos", 
                        title = "Missing data", content = "Please upload matrix to preview data", 
                        append = FALSE, style = "danger")
            return(NULL)
        }else{
            closeAlert(session, "messagedatos")
            DT::datatable(matrizDatos$datos)
        }            
        })
        
    output$sampleData <- DT::renderDataTable({
        if(is.null(matrizDatos$datos) ){
            createAlert(session, "samplesmessage", alertId ="messagesamples", 
                        title = "Missing samples info", content = "Please upload sample datatable to preview data about samples", 
                        append = FALSE, style = "danger")
            return(NULL)
        }else{
            closeAlert(session, "messagesamples")
            DT::datatable(matrizDatos$samples)
        }
        })
}
#
shinyApp(ui = ui, server = server)