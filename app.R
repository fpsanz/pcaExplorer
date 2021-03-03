library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)
library(shinyBS)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(psych)
library(corrplot)
source("aux.R")

#
ui <- fluidPage(
    fluidRow(class = "headerLogos",
        column(width = 3,
               HTML('<a href="https://jacob.cea.fr/drf/ifrancoisjacob/Pages/Departements/MIRCen/themes/astrocytes-reactifs-biomarqueurs-imagerie-cibles-therapeutiques.aspx" target="_blank"><img src="mircenBlancoNombre.png" alt="micen", style="height:40px; padding-top:5px"></a>')), 
        column(width = 3, offset = 6,
               HTML('<a href="http://www.imib.es/web/personal.jsf?id=7961" target="_blank"><img src="imibNombre.png" alt="imib", style="height:40px; padding-top:3px; float:right"></a>') )
    ),
    navbarPage(id ="navpanel",
        title ="HolaCaracola",
        theme = shinytheme("superhero"),
        collapsible = TRUE,
        fluid = TRUE,
        source(file = "ui-home.R", local = TRUE, encoding = "UTF-8")$value,
        source(file = "ui-pcaplot.R", local = TRUE, encoding = "UTF-8")$value,
        source(file = "ui-corrplot.R", local = TRUE, encoding = "UTF-8")$value,
        source(file = "ui-contributions.R", local = TRUE, encoding = "UTF-8")$value,
        source(file = "ui-hierarplot.R", local = TRUE, encoding = "UTF-8")$value,
        source(file = "ui-help.R", local = TRUE, encoding = "UTF-8")$value,
        includeCSS("./www/mystyle.css"),
        setShadow(class = "box"),
        useShinyalert()
    )
    )
#
server <- function(input, output, session){
    
## tab Home #########################################
    
    tr <-reactiveValues(count=0) #contador de trasposicion de matriz
    matrizDatos <- reactiveValues() #contenedor de datos
    PCA <- reactiveValues(df=NULL)
    
    # about button #################
    observeEvent(input$aboutButton, {
        shinyalert("PCA explorator", HTML("Authors:<br>
    Miriam Riquelme Pérez 
    <a href='https://www.linkedin.com/in/miriam-riquelme-perez/' target='_blank'> 
    <img src='linkedin_little.svg'> </a> <a href='mailto:miriam.riquelmep@gmail.com'>
    <img src='email.svg'></a><br>
    Fernando Pérez Sanz 
    <a href='https://www.linkedin.com/in/fernandoperez72/' target='_blank'> 
    <img src='linkedin_little.svg'> 
    </a> <a href='mailto:fernando.perez@ffis.es'> <img src='email.svg'></a><br>
    For any suggestion or bug, please contact us"),
    imageUrl = "dna-svg-small-13.gif", 
    imageWidth = 200, imageHeight = 100, html=TRUE)})
    
    ## reset button ############
    observeEvent(input$resetbutton,{
        session$reload()
    })
    
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
        htmltools::tags$h4(class="mytxtcolor", "MSA intervals:"),
        htmltools::tags$p(class="mytxtcolor", "Values >0.9: marvelous"),
        htmltools::tags$p(class="mytxtcolor", "Values 0.8-0.9: meritorious"),
        htmltools::tags$p(class="mytxtcolor", "Values 0.7-0.8: middling "),
        htmltools::tags$p(class="mytxtcolor", "Values 0.6-0.7: mediocre"),
        htmltools::tags$p(class="mytxtcolor", "Values 0.5-0.6: miserable"),
        htmltools::tags$p(class="mytxtcolor", "Values < 0.5: unacceptable"),
        
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
## .............................. ###############
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
    output$elipses <- renderUI({
        if(input$tipopca == "ind"){
            switchInput("elipses", "Elipses", onLabel = "Yes", offLabel = "No", value = FALSE)
        }
    })
    ### pca plot ################
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
            PCA$df <- PCA(matrizDatos$datos, graph = F)
            if(input$tipopca=="ind"){
                fviz_pca_ind(PCA$df, col.ind = variables, pointsize=3,
                             labelsize=5, axes = as.numeric(input$ndmax), addEllipses = input$elipses)+
                    theme(text = element_text(size=18))
            }else if(input$tipopca=="biplot"){
                fviz_pca_biplot(PCA$df, col.ind = variables, pointsize=3,
                                labelsize=5, axes = as.numeric(input$ndmax))+
                    theme(text = element_text(size=18))
            }else{
                fviz_pca_var(PCA$df, pointsize=3, labelsize=5, axes = as.numeric(input$ndmax))+
                    theme(text = element_text(size=18))
            }
        }
   })
# .............................. ###############
# tab corrplot etc.. ######################
    ### screeplot ###############
    output$screeplot <- renderPlot({
        if(is.null(PCA$df)){
            createAlert(session, "screemessage", alertId ="messagescree",
                        title = "Missing PCA object",
                        content = "Please first visit PCAplot tab",
                        append = FALSE, style = "danger")
            shiny:::reactiveStop()
        }else{
            closeAlert(session, "messagescree")
            fviz_eig(PCA$df, addlabels = TRUE, bar_width=0.5)
        }
    })
    
    ### corrplot ########################
    output$corrplot <- renderPlot({
        if(is.null(PCA$df)){
            createAlert(session, "corrmessage", alertId ="messagecorr",
                        title = "Missing PCA object",
                        content = "Please first visit PCAplot tab",
                        append = FALSE, style = "danger")
            shiny:::reactiveStop()
        }else{
            closeAlert(session, "messagecorr")
            var <- get_pca_var(PCA$df)
            corrplot(var$cos2, is.corr = FALSE)
        }
    })
    # .............................. ###############
# tab contributions ##################################
    ### subtab variables #################
    output$vard1 <- renderPlot({
        if(is.null(PCA$df)){
            createAlert(session, "vardim1", alertId ="dim1var",
                        title = "Missing PCA object",
                        content = "Please first visit PCAplot tab",
                        append = FALSE, style = "danger")
            shiny:::reactiveStop()
        }else{
            closeAlert(session, "dim1var")
            fviz_contrib(PCA$df, choice = "var", axes = 1, width=0.5) +
                theme(text = element_text(size=20), axis.text.x = element_text(angle=70, hjust=1)) 
        }
    })
    output$vard2 <- renderPlot({
        if(is.null(PCA$df)){
            createAlert(session, "vardim2", alertId ="dim2var",
                        title = "Missing PCA object",
                        content = "Please first visit PCAplot tab",
                        append = FALSE, style = "danger")
            shiny:::reactiveStop()
        }else{
            closeAlert(session, "dim2var")
            fviz_contrib(PCA$df, choice = "var", axes = 2, width=0.5) +
                theme(text = element_text(size=20), axis.text.x = element_text(angle=70, hjust=1)) 
        }
    })
    output$vard3 <- renderPlot({
        if(is.null(PCA$df)){
            createAlert(session, "vardim3", alertId ="dim3var",
                        title = "Missing PCA object",
                        content = "Please first visit PCAplot tab",
                        append = FALSE, style = "danger")
            shiny:::reactiveStop()
        }else{
            closeAlert(session, "dim3var")
            fviz_contrib(PCA$df, choice = "var", axes = 3, width=0.5) +
                theme(text = element_text(size=20), axis.text.x = element_text(angle=70, hjust=1)) 
        }
    })
    output$vard4 <- renderPlot({
        if(is.null(PCA$df)){
            createAlert(session, "vardim4", alertId ="dim4var",
                        title = "Missing PCA object",
                        content = "Please first visit PCAplot tab",
                        append = FALSE, style = "danger")
            shiny:::reactiveStop()
        }else{
            closeAlert(session, "dim4var")
            fviz_contrib(PCA$df, choice = "var", axes = 4, width=0.5) +
                theme(text = element_text(size=20), axis.text.x = element_text(angle=70, hjust=1)) 
        }
    })
    ### subtab individuos #################
    output$ind1 <- renderPlot({
        if(is.null(PCA$df)){
            createAlert(session, "inddim1", alertId ="dim1ind",
                        title = "Missing PCA object",
                        content = "Please first visit PCAplot tab",
                        append = FALSE, style = "danger")
            shiny:::reactiveStop()
        }else{
            closeAlert(session, "dim1ind")
            fviz_contrib(PCA$df, choice = "ind", axes = 1, width=0.5) +
                theme(text = element_text(size=20), axis.text.x = element_text(angle=70, hjust=1)) 
        }
    })
    output$ind2 <- renderPlot({
        if(is.null(PCA$df)){
            createAlert(session, "inddim2", alertId ="dim2ind",
                        title = "Missing PCA object",
                        content = "Please first visit PCAplot tab",
                        append = FALSE, style = "danger")
            shiny:::reactiveStop()
        }else{
            closeAlert(session, "dim2ind")
            fviz_contrib(PCA$df, choice = "ind", axes = 2, width=0.5) +
                theme(text = element_text(size=20), axis.text.x = element_text(angle=70, hjust=1)) 
        }
    })
    output$ind3 <- renderPlot({
        if(is.null(PCA$df)){
            createAlert(session, "inddim3", alertId ="dim3ind",
                        title = "Missing PCA object",
                        content = "Please first visit PCAplot tab",
                        append = FALSE, style = "danger")
            shiny:::reactiveStop()
        }else{
            closeAlert(session, "dim3ind")
            fviz_contrib(PCA$df, choice = "ind", axes = 3, width=0.5) +
                theme(text = element_text(size=20), axis.text.x = element_text(angle=70, hjust=1)) 
        }
    })
    output$ind4 <- renderPlot({
        if(is.null(PCA$df)){
            createAlert(session, "inddim4", alertId ="dim4ind",
                        title = "Missing PCA object",
                        content = "Please first visit PCAplot tab",
                        append = FALSE, style = "danger")
            shiny:::reactiveStop()
        }else{
            closeAlert(session, "dim4ind")
            fviz_contrib(PCA$df, choice = "ind", axes = 4, width=0.5) +
                theme(text = element_text(size=20), axis.text.x = element_text(angle=70, hjust=1)) 
        }
    })
## tab dendrograma ########################
    output$dendroplot <- renderPlot({
        if(is.null(matrizDatos$datos) | is.null(PCA$df)){
            createAlert(session, "dendromessage", alertId ="messagedendro",
                        title = "Missing PCA object or data table",
                        content = "Please first upload your data and visit PCAplot tab",
                        append = FALSE, style = "danger")
            shiny:::reactiveStop()
        }else{
            closeAlert(session, "messagedendro")
            ColorGroup <- unite( matrizDatos$samples, col = "variable", input$varselected, sep="_" )
            if(isTRUE(input$dendroswitch)){
                distSamples <- dist(matrizDatos$datos)
                hc <- hclust(distSamples, method = "ward.D2")
                dend <- as.dendrogram(hc)
                vectColors <- as.numeric(as.factor(ColorGroup$variable))[order.dendrogram(dend)]
                coloresDendro <- gg_color_hue(vectColors)
                labels_colors(dend) <- coloresDendro
                plot(dend, horiz = TRUE, cex.lab = 2)
            }else{
                distSamples <- dist(PCA$df$ind$cos2)
                hc <- hclust(distSamples, method = "ward.D2")
                dend <- as.dendrogram(hc)
                vectColors <- as.numeric(as.factor(ColorGroup$variable))[order.dendrogram(dend)]
                coloresDendro <- gg_color_hue(vectColors)
                labels_colors(dend) <- coloresDendro
                plot(dend, horiz = TRUE, cex = 4)
            }
        }
    })
    
    
} #fin server


#
shinyApp(ui = ui, server = server)