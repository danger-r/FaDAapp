## UI ####
heatmapPcaUI <- function(){
  tagList(
    tags$h3("Heatmap:", style = "color: steelblue;"),
    plotlyOutput(outputId ="Heatmap", width = "90%") %>% withSpinner(color="#0dc5c1")
)}

PCA_UI <- function(){
      tagList(
    tags$h3("Principal Component Analysis:", style = "color: steelblue;"),
    plotlyOutput(outputId ="ACP", width = "80%") %>% withSpinner(color="#0dc5c1")
  )
}

FixedPCA_UI <- function(){
  tagList(
    tags$h3("Principal Component Analysis:", style = "color: steelblue;"),
    plotOutput(outputId ="FixedPCA", width = "80%") %>% withSpinner(color="#0dc5c1"),
    downloadButton('dFixedPCATiff', label="Download as .Tiff"),
    downloadButton('dFixedPCASvg', label="Download as .SVG"),
    downloadButton('dFixedPCAPdf', label="Download as .pdf")
  )
}

FixedHeatmap_UI <- function(){
      tagList(
        tags$h3("Heatmap:",style = "color: steelblue;"),
        plotOutput(outputId ="FixedHeatmap", width = "80%") %>% withSpinner(color = "#0dc5c1"),
        tags$br(),
        #sliderInput(inputId = "Heatmap_width", label = "Change width (%)", min = 0, max = 100, value = 60),  #not used, was to midified image size
        #sliderInput(inputId = "Heatmap_height", label = "Change height (px)", min = 0, max = 800, value = 400), #not used, was to midified image size

        downloadButton('dFixedHeatmapTiff', label="Download as .Tiff"),
        downloadButton('dFixedHeatmapSvg', label="Download as .SVG"),
        downloadButton('dFixedHeatmapPdf', label="Download as .pdf")
      )}

## Server Functions ####
CalcACPonly <- function(input, calc_table){         #calculate knn ACP data.frame
  ACP_table <- reactive({
    return(funCalcACPonly(calc_table() ))
  })
  return(ACP_table)
}

ACP <- function(input,used_groups,calc_table,colorFunction, CalcACPonly){              #interactive PCA
  ACP <- function(){
     return(funACP(used_groups(), calc_table(), colorFunction(), input$file1$name, CalcACPonly()))
 }
  return(ACP)
}

FixedPCA <- function(input,used_groups,calc_table,colorFunction, CalcACPonly){        #static PCA
  FixedPCA <- function(){
    validate( need( !is.null(calc_table()), " " ) )
    #Remplacement des NA par KNN, nécessite une transposition
    return(funFixedPCA(used_groups(),calc_table(),colorFunction(),input$file1$name, CalcACPonly()))
  }
  return(FixedPCA)
}

heatMap <- function(input,used_groups,calc_table,colorFunction){              #interactive heatmap
Heatmap <- function(){
    validate( need( !is.null(calc_table()), " " ) )

    heat <- funHeatmap(used_groups(),calc_table(),colorFunction(),input$color1,input$Middlecolor,input$color2,input$Clustering,input$file1$name)
    return(heat)
  }
  return(Heatmap)
}

FixedHeatmap <- function(input, used_groups, calc_table, colorFunction) {        #static heatmap
  FixedHeatmap <- function(){
    validate( need( !is.null(calc_table()), " " ) )

    return(funFixedHeatmap(used_groups(),calc_table(),colorFunction(),input$color1,input$Middlecolor,input$color2,input$Clustering,input$file1$name))  }
  return(FixedHeatmap)
}

## Independant Functions ###
funCalcACPonly <- function(calc_Table){                        #calculate knn ACP data.frame
  req( !is.null(calc_Table), library(future), cancelOutput = TRUE)                        #
  req( !is.null(calc_Table), library(promises), cancelOutput = TRUE)                       #
  plan(multisession)                                                                      #    
  
  if ( length( which(is.na(calc_Table) == TRUE)) >= 1 ) {
#future        future_promise(seed=TRUE, {  
    import::from(impute, impute.knn)
    ACPcalcTable <- t(calc_Table)
    dfkNN <- impute.knn(calc_Table, k = 10, rowmax = 0.5, colmax =0.8, rng.seed=362436069)
    dfkNN <- t(dfkNN$data)
    ACP_table <-as.matrix(dfkNN)
#end future              })
  }
  else{ACPcalcTable <- as.matrix(calc_Table)}
  
  ACP_table <- prcomp(ACPcalcTable, scale = TRUE)  
  return(ACP_table)
  
}

funHeatmap <- function(used_Groups,calc_Table,colors,infoColor1,infoMiddleColor,infoColor2,infoClustering,infoFilename){
  Groups = as.factor(used_Groups)
  labCol <- as.matrix(cbind(Groups))
  #breaks for the core of the distribution
  breaks=seq(-1.5, 1.5, by=0.1)
  breaks=append(breaks, 10)
  breaks=append(breaks, -10, 0)
  
  req( !is.null(calc_Table), suppressPackageStartupMessages(library(gplots)))     #For colorpanels in the heatmap

  mycol <- colorpanel(n=length(breaks)-1,low= infoColor1 , mid=infoMiddleColor,high=infoColor2)

  heatmapPlot <- heatmaply(t(scale(calc_Table)), Colv= infoClustering, Rowv = TRUE,
                           col_side_colors=used_Groups,
                           scale = "none",
                           colors=mycol,
                           breaks=breaks
  )  ### %>%  partial_bundle() #partial_bundle to reduce file size and inrease speed (but decrease quality)

  heatmapPlot <- heatmapPlot %>% config(plot_ly(), toImageButtonOptions= list(filename = paste(infoFilename,"_heatmap",sep = ""),
                                                                             format = "png", scale = 2))
  return(heatmapPlot)
}

funFixedHeatmap <- function(used_Groups,calc_Table, colors, infoColor1,infoMiddleColor,infoColor2, infoClustering, infoFilename){
  
  #load library:
  req( !is.null(calc_Table), library(ComplexHeatmap), cancelOutput = TRUE)
  req( !is.null(calc_Table), import::from(circlize, colorRamp2), cancelOutput = TRUE)

  req( !is.null(calc_Table), library(future), cancelOutput = TRUE)                        #
  req( !is.null(calc_Table), library(promises), cancelOutput = TRUE)                       #
  plan(multisession)                                                                      #    

    future_promise(seed=TRUE, {                                                   #executed asynchronously
  #Set heatmap colors
  col_fun = colorRamp2(c(-2, 0, 2), c(infoColor1, infoMiddleColor, infoColor2) )

  #Set annotations
  Parameter = colors[as.factor(used_Groups)]
  names(Parameter) <- used_Groups
  ha_annot <- HeatmapAnnotation(Group= used_Groups, col= list(Group= Parameter), which='col')

  HeatmapPlot <- Heatmap( t(scale(calc_Table)),
           column_title = infoFilename,
           col= col_fun, name = " ",
           cluster_columns = infoClustering,
           top_annotation = ha_annot,
           heatmap_legend_param= list(
             title = "scale", at = c(-2, 0, 2),
             labels = c("low", " ", "high")) )

  FixedHeatmapPlot <- draw(HeatmapPlot, heatmap_legend_side = "left", annotation_legend_side = "left")

      return(FixedHeatmapPlot) 
    })                                                           #end future_promise
  }


funFixedPCA <- function(used_Groups,calcTable,colors,infoFilename, ACP_table){
  req( !is.null(calc_Table), library(future), cancelOutput = TRUE)                        #
  req( !is.null(calc_Table), library(promises), cancelOutput = TRUE)                       #
  plan(multisession)                                                                      #    
  future_promise(seed=TRUE, { 
  
  
  dataforACP <- ACP_table
  ACPdf <- data.frame(dataforACP$x, "group" = used_Groups)
  eig <- data.frame(Components= c(paste("Comp.", rep(1:5))),
                    Values=c(100 * dataforACP$sdev[1:5]^2 / sum(dataforACP$sdev[1:5]^2)))

  Group = as.factor(used_Groups)

  graph_acp <- ggplot(ACPdf, aes(x= PC1, y= PC2, color = factor(Group))) +
    theme_bw() +
    geom_hline(yintercept = 0, color = "gray70") +
    geom_vline(xintercept = 0, color = "gray70") +
    scale_color_manual(values = colors) +
    geom_point(aes(color = factor(used_Groups)), alpha = 0.55, size = 3) +
    xlab( paste("PC1 (",format(eig[1,2], digits = 3), "% of explained variance)") ) +
    ylab( paste("PC2 (",format(eig[2,2], digits = 3), "% of explained variance)") ) +
    stat_ellipse(type = "t", level = 0.95, geom = "polygon", size= 1.5,
                 alpha = 0.2, fill="lightgrey", show.legend=FALSE )

  graph_component <- ggplot(eig, aes(Components, Values)) +
    geom_bar(stat = "identity", fill="steelblue")+
    theme_bw() +
    xlab( "Components" ) +
    ylab( "% of explained variance")+
    ggtitle("% of explained variance")

  graph_2acp <- grid.arrange(graph_acp, graph_component, nrow = 1, widths = c(2,1))

  return(graph_2acp)
      })
}

funACP <- function(used_Groups,calcTable,colors,infoFilename, ACP_table){
  dataforACP <- ACP_table
  ACPdf <- data.frame(dataforACP$x, "group" = used_Groups)
  eig <- data.frame(Components= c(paste("Comp.", rep(1:5))),
                    Values=c(100 * dataforACP$sdev[1:5]^2 / sum(dataforACP$sdev[1:5]^2)))

  Sample= rownames(ACPdf)
  Group = as.factor(used_Groups)

  graph_acp <- ggplot(ACPdf, aes(x= PC1, y= PC2, color = Group)) +
    theme_classic() +
    geom_hline(yintercept = 0, color = "gray70") +
    geom_vline(xintercept = 0, color = "gray70") +
    scale_color_manual(values = colors) +
    geom_point(aes(color = factor(group), Sample=Sample), alpha = 0.55, size = 3) +
    xlab( paste("PC1 (",format(eig[1,2], digits = 3), "% of explained variance)") ) +
    ylab( paste("PC2 (",format(eig[2,2], digits = 3), "% of explained variance)") ) +
    stat_ellipse(type = "t", level = 0.95)


  id =rownames(calcTable)
  graphacp <- ggplotly(graph_acp, tooltip="id")  %>% toWebGL() %>% config(plot_ly(),
                                                                        toImageButtonOptions= list(filename = paste(infoFilename,"_PCA",sep = ""),
                                                                                                   format = "png", scale = 2),
                                                displaylogo = FALSE,
                                                modeBarButtonsToRemove = c('lasso2d','sendDataToCloud','zoom2d',
                                                                           'resetScale2d','hoverClosestCartesian','hoverCompareCartesian'))
  return(graph_acp)
}



## Output to UI ####

heatmapOutput <- function(output,reacHeatmap, reacACP, reacFixedHeatmap, reacFixedPCA){
  output$Heatmap <- renderPlotly({   reacHeatmap()     })
  output$ACP <- renderPlotly({    reacACP()      })
  output$FixedPCA <- renderPlot({    reacFixedPCA()      })
  output$FixedHeatmap <- renderPlot({    reacFixedHeatmap() }, height = "auto")
}



## Download ####
FixedheatmapDownload <- function(input,output,reacFixedHeatmap){
  output$dFixedHeatmapTiff = downloadHandler(filename =
                                              reactive(paste(input$file1$name,"_heatmap.tiff",sep = "")),
                                            content = function(file, compression = "lzw", res = 600) {
                                              tiff(file)
                                              print( reacFixedHeatmap() )
                                              dev.off()                      })
  output$dFixedHeatmapSvg = downloadHandler(filename =
                                             reactive(paste(input$file1$name,"_heatmap.svg",sep = "")),
                                              content = function(file, compression = "lzw", res = 600) {
                                             svg(file)
                                             print( reacFixedHeatmap() )
                                             dev.off()                      })
  output$dFixedHeatmapPdf = downloadHandler(filename =
                                              reactive(paste(input$file1$name,"_heatmap.pdf",sep = "")),
                                            content = function(file, compression = "lzw", res = 600) {
                                              pdf(file)
                                              print( reacFixedHeatmap() )
                                              dev.off()                      })

}

FixedPCADownload <- function(input,output,reacFixedPCA){
  output$dFixedPCATiff = downloadHandler(filename =
                                              reactive(paste(input$file1$name,"_PCA.tiff",sep = "")),
                                              content = function(file) {
                                               tiff(file, units="in",width= 10, height= 5, compression = "lzw",  res = 600)
                                               print( reacFixedPCA() )
                                               dev.off()                      })


  output$dFixedPCASvg = downloadHandler(filename =
                                              reactive(paste(input$file1$name,"_PCA.svg",sep = "")),
                                            content = function(file, compression = "lzw", res = 600) {
                                              svg(file,  width= 10, height= 5)
                                              print( reacFixedPCA() )
                                              dev.off()                      })

  output$dFixedPCAPdf = downloadHandler(filename =
                                          reactive(paste(input$file1$name,"_PCA.pdf",sep = "")),
                                        content = function(file, compression = "lzw", res = 600) {
                                          pdf(file,  width= 10, height= 5)
                                          print( reacFixedPCA() )
                                          dev.off()                      })

}
##End
