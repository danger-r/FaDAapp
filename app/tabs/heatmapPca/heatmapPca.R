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
    downloadButton('dFixedPCASvg', label="Download as .SVG")
  )
}
FixedHeatmap_UI <- function(){
      tagList(
        tags$h3("Heatmap:",style = "color: steelblue;"),
        plotOutput(outputId ="FixedHeatmap",width = "80%") %>% withSpinner(color = "#0dc5c1"),
        tags$br(),
        downloadButton('dFixedHeatmapTiff', label="Download as .Tiff"),
        downloadButton('dFixedHeatmapSvg', label="Download as .SVG")
      )}

## Server Functions ####
ACP <- function(input,used_groups,calc_table,colorFunction){
  ACP <- function(){
    validate( need( !is.null(calc_table()), "  " ) )
    #Col_group = c( "darkgreen", "blue", "lightgrey", "cyan2", "orange", "darkred", "red", "blueviolet")

    #Remplacement des NA par KNN, nécessite une transposition
    return(funACP(used_groups(),calc_table(),colorFunction(),input$file1$name))
  }
  return(ACP)
}
FixedPCA <- function(input,used_groups,calc_table,colorFunction){
  FixedPCA <- function(){
    validate( need( !is.null(calc_table()), " " ) )
    #Remplacement des NA par KNN, nécessite une transposition
    return(funFixedPCA(used_groups(),calc_table(),colorFunction(),input$file1$name))
  }
  return(FixedPCA)
}

heatMap <- function(input,used_groups,calc_table,colorFunction){
Heatmap <- function(){
    validate( need( !is.null(calc_table()), " " ) )

    heat <- funHeatmap(used_groups(),calc_table(),colorFunction(),input$color1,input$Middlecolor,input$color2,input$Clustering,input$file1$name)
    return(heat)
  }
  return(Heatmap)
}

FixedHeatmap <- function(input,used_groups,calc_table,colorFunction) {
  FixedHeatmap <- function(){
    validate( need( !is.null(calc_table()), " " ) )

    return(funFixedHeatmap(used_groups(),calc_table(),colorFunction(),input$color1,input$Middlecolor,input$color2,input$Clustering,input$file1$name))  }
  return(FixedHeatmap)
}

## Independant Functions ####

funHeatmap <- function(used_Groups,calc_Table,colors,infoColor1,infoMiddleColor,infoColor2,infoClustering,infoFilename){
  Groups = as.factor(used_Groups)
  labCol <- as.matrix(cbind(Groups))
  #breaks for the core of the distribution
  breaks=seq(-1.5, 1.5, by=0.1)
  breaks=append(breaks, 10)
  breaks=append(breaks, -10, 0)
  #if(length(calc_Table) < 200) {rowFontSize = 1.5 - 0.5*(length(calc_Table)/100)} else {rowFontSize = 0.5}
  #if(length(calc_Table[,1]) < 200) {colFontSize = 1.3 - 0.4*(length(calc_Table[,1])/100)} else {colFontSize = 0.5}
  mycol <- colorpanel(n=length(breaks)-1,low= infoColor1 , mid=infoMiddleColor,high=infoColor2)

  if (infoClustering == "supervised") {dendro = FALSE} else {dendro = TRUE}


  heatmapPlot <- heatmaply(t(scale(calc_Table)), Colv= dendro, Rowv = TRUE,
                           col_side_colors=used_Groups,
                           scale = "none",
                           colors=mycol,
                           breaks=breaks

  )


  heatmapPlot <- heatmapPlot %>% config(plot_ly(), toImageButtonOptions= list(filename = paste(infoFilename,"_heatmap",sep = "")))
  return(heatmapPlot)
}

funFixedHeatmap <- function(used_Groups,calc_Table,gcol,infoColor1,infoMiddleColor,infoColor2,infoClustering,infoFilename){

  if (infoClustering == "supervised") {dendro = FALSE} else {dendro = TRUE}
  #Set heatmap colors
  col_fun = colorRamp2(c(-2, 0, 2), c(infoColor1, infoMiddleColor, infoColor2) )


    #Set annotation
  col_ann <- gcol[1:length(levels(as.factor(used_Groups)))]
  names(col_ann) <- levels(as.factor(used_Groups))
  ha_annot <- HeatmapAnnotation(df = data.frame(Groups= used_Groups),
                                col = list(Groups = col_ann))

  heat <- Heatmap( t(scale(calc_Table)),
                               column_title = infoFilename,
                               col= col_fun, name = " ",
                               top_annotation = ha_annot,
                               cluster_columns = dendro,
                   heatmap_legend_param = list(direction = "horizontal")
                               )

  FixedHeatmapPlot <- draw(heat, merge_legend = TRUE, heatmap_legend_side = "bottom",
                    annotation_legend_side = "bottom")

    return(FixedHeatmapPlot)                          }



funACP <- function(used_Groups,calcTable,colors,infoFilename){
  if ( length( which(is.na(calcTable) == TRUE)) >= 1 ) {
    calcTable <- t(calcTable)
    dfkNN <- impute.knn(calcTable, k = 10, rowmax = 0.5, colmax =0.8, rng.seed=362436069)
    dfkNN <- t(dfkNN$data)
    calcTable <-as.matrix(dfkNN)
  }

  ACP <- prcomp(calcTable, scale = TRUE)
  ACPdf <- data.frame(ACP$x, "group" = used_Groups)
  id =rownames(calcTable)
  eig <- data.frame(Components= c(paste("Comp.", rep(1:5))),
                    Values=c(100 * ACP$sdev[1:5]^2 / sum(ACP$sdev[1:5]^2)))

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

  graphacp <- ggplotly(graph_acp, tooltip="id")  %>% config(toImageButtonOptions= list(filename = paste(infoFilename,"_PCA",sep = "")),
                                                displaylogo = FALSE,
                                                modeBarButtonsToRemove = c('lasso2d','sendDataToCloud','zoom2d',
                                                                           'resetScale2d','hoverClosestCartesian','hoverCompareCartesian'))
  return(graph_acp)
}

funFixedPCA <- function(used_Groups,calcTable,colors,infoFilename){
  if ( length( which(is.na(calcTable) == TRUE)) >= 1 ) {
    calcTable <- t(calcTable)
    dfkNN <- impute.knn(calcTable, k = 10, rowmax = 0.5, colmax =0.8, rng.seed=362436069)
    dfkNN <- t(dfkNN$data)
    calcTable <-as.matrix(dfkNN)
  }

  ACP <- prcomp(calcTable, center=TRUE, scale = TRUE)
  ACPdf <- data.frame(ACP$x, "group" = used_Groups)
  eig <- data.frame(Components= c(paste("Comp.", rep(1:5))),
                    Values=c(100 * ACP$sdev[1:5]^2 / sum(ACP$sdev[1:5]^2)))

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
    ggtitle("% of explained variance for each component of the PCA")


  graph_2acp <- grid.arrange(graph_acp, graph_component, nrow = 1, widths = c(2,1))

    return(graph_2acp)
}



## Output to UI ####

heatmapOutput <- function(output,reacHeatmap, reacACP, reacFixedHeatmap, reacFixedPCA){
  output$Heatmap <- renderPlotly({   reacHeatmap()     })
  output$ACP <- renderPlotly({    reacACP()      })
  output$FixedPCA <- renderPlot({    reacFixedPCA()      })
  output$FixedHeatmap <- renderPlot({    reacFixedHeatmap()  })

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
                                             svglite(file)
                                             print( reacFixedHeatmap() )
                                             dev.off()                      })
}

FixedPCADownload <- function(input,output,reacFixedPCA){
  output$dFixedPCATiff = downloadHandler(filename =
                                              reactive(paste(input$file1$name,"_PCA.tiff",sep = "")),
                                              content = function(file, compression = "lzw", res = 600) {
                                               tiff(file)
                                               print( reacFixedPCA() )
                                               dev.off()                      })

  output$dFixedPCASvg = downloadHandler(filename =
                                              reactive(paste(input$file1$name,"_PCA.svg",sep = "")),
                                            content = function(file, compression = "lzw", res = 600) {
                                              svglite(file)
                                              print( reacFixedPCA() )
                                              dev.off()                      })
}

##End
