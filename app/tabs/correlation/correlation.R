## UI ####
correlogramUI <- function(){
  tagList(
    tags$h3("Correlogram:",style = "color: steelblue;"),
    plotOutput(outputId ="Correlogram"),
    downloadButton('dCorrelogramTiff', label="Download as .Tiff"),
    downloadButton('dCorrelogramSvg', label="Download as .SVG"),
    tags$hr(), # Horizontal line --
    tags$h3("Correlation (r) table:",style = "color: steelblue;"),
    DTOutput(outputId ="Corr_table") %>% withSpinner(color="#0dc5c1"),
    tags$br(),
    tags$h3("p-values table:",style = "color: steelblue;"),
    DTOutput(outputId ="pvalCorr_table") %>% withSpinner(color="#0dc5c1"),
    plotlyOutput(outputId ="Scater_Plot_Cor")
  )
}

corrGraphUI <- function(){ #The new UI for the correlogram graph
  tagList(
    tags$h3("Correlation graphs:",style = "color: steelblue;"),
    DTOutput("Corr_table2") %>% withSpinner(color="#0dc5c1"),
    tags$h3("Select a cell in the above table to display the associated correlation graph",style = "color: #b73338;"),
    plotlyOutput(outputId ="corrGraph", width = "70%") %>% withSpinner(color="#0dc5c1")
  )
}

corrSidebarUI <- function(){
  tagList(
    sliderInput("corrSelect","Highlight in bold in the table correlations above the selected threshold: ",0,1,0.75),

  )
}


## Server Functions ####
correlogram <- function(input,reacCalcTable){
  Correlogram <- function(){
    validate( need( !is.null(reacCalcTable()), "Please, upload a properly formatted dataset or use the example." ) )

    #Remplacement des NA par KNN, nécessite une transposition
    funCorrelogram(reacCalcTable())

  }
  return(Correlogram)
}

corrTable <- function(input,reacCalcTable){
  Correlationtable <- reactive({
    validate( need( !is.null(reacCalcTable()), "  " ) )

    funCorrTable(reacCalcTable(),input$Test, input$corrSelect)

  })
  return(Correlationtable)
}

pvalCorrTable <- function(input,reacCalcTable){
  pval_Correlation_table <- reactive({
    validate( need( !is.null(reacCalcTable()), " " ) )
    funPvalCorrTable(reacCalcTable(),input$Test)

  })
  return(pval_Correlation_table)
}

corrGraph <- function(input,reacCalcTable){
  return(reactive({
    validate( need( !is.null(reacCalcTable()), "Please, upload a properly formatted dataset or use the example." ) )
    validate( need(!is.null(input$Corr_table2_cells_selected),"Select a cell on the table above to display a graph"))
    funCorrGraph(reacCalcTable(),input$Corr_table2_cells_selected)
  })
  )
}

## Independant Functions ####

funCorrelogram <- function(calcTable){

  if ( length( which(is.na(calcTable) == TRUE)) >= 1 )
  { calcTable <- t(calcTable)
  dfkNN <- impute.knn(calcTable, k = 10, rowmax = 0.5, colmax =0.8, rng.seed=362436069)
  dfkNN <- t(dfkNN$data)
  calcTable <-as.matrix(dfkNN)
  }

  mcor <-cor(calcTable)
  col <- colorRampPalette(c("blue", "white", "red"))(20)
  corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45, diag=F, col=col)
  return(mcor)
}

funCorrTable <- function(calcTable,infoTest,infoCorrSelect){
  if (infoTest == "parametric") {
    mcor <-cor(calcTable, method = "pearson", use = "complete.obs")   }
  else { #unparametric
    mcor <-cor(calcTable, method = "spearman", use = "complete.obs")  }
  mcor <- format(mcor, digits = 3)
  #print(mcor, bordered = TRUE)
  return(list("table"=mcor,"corrSelect"=infoCorrSelect))
}

funPvalCorrTable <- function(calcTable,infoTest){
  if (infoTest == "parametric") {
    mcor_test <- cor.mtest(calcTable, method = "pearson", na.action = "na.exclude" )$p   }
  else { #unparametric
    mcor_test <-cor.mtest(calcTable, method = "spearman", na.action = "na.exclude")$p  }

  colnames(mcor_test)<-colnames(calcTable)
  rownames(mcor_test)<-colnames(calcTable)
  mcor_test <- format(mcor_test, digits = 3)
  #print(mcor_test, bordered = TRUE)
  return(mcor_test)

  #pAdj <- p.adjust(mcor_test, method = "BH" )
  #resAdj <- matrix(pAdj, ncol = dim(mcor_test)[1] )

}

funCorrGraph <- function(calcTable,infoCellSelected){
  indx <- infoCellSelected[2]
  indy <- infoCellSelected[1]
  y <- calcTable[,indy]
  x <- calcTable[,indx]
  id <- rownames(calcTable)
  plot <- ggplot(calcTable,aes(x,y)) +
    theme_classic() +
    geom_point(aes(id = id)) +
    geom_smooth(data = calcTable,method='lm') +
    xlab(colnames(calcTable)[indx]) +
    ylab(colnames(calcTable)[indy])

  #print(plot)

  gplot <- ggplotly(plot) %>% layout( dragmode = "select")
  return(gplot)
}

## Output to UI ####
correlogramOutput <- function(output,reacCorrTable,reacPvalCorrTable,reacCorrelogram,reacNameTable){

  output$Correlogram <- renderPlot({    reacCorrelogram()      })

  output$Corr_table <- renderDT({     datatable(reacCorrTable()$table,
                                                extensions="Buttons",
                                                options = list(pageLength = 20, searching = FALSE, dom = 'Bt',
                                                               buttons =  list( 'copy',
                                                                                list(extend='csv',
                                                                                     filename = paste(reacNameTable(),"_CorrTable",sep="")),
                                                                                list(extend='excel',
                                                                                     filename = paste(reacNameTable(),"_CorrTable",sep="")),
                                                                                list(extend='pdf',
                                                                                     filename= paste(reacNameTable(),"_CorrTable",sep="")) )
                                                ) )  %>%  formatStyle(colnames(reacCorrTable()$table), fontWeight = styleInterval(  c(-reacCorrTable()$corrSelect,reacCorrTable()$corrSelect)-1e-6,c('bold','normal','bold')))
  })



  output$pvalCorr_table <- renderDT({     datatable(reacPvalCorrTable(),
                                                    extensions="Buttons",
                                                    options = list(pageLength = 20, searching = FALSE, dom = 'Bt',
                                                                   buttons =  list( 'copy',
                                                                                    list(extend='csv',
                                                                                         filename = paste(reacNameTable(),"_PvalCorrTable",sep="")),
                                                                                    list(extend='excel',
                                                                                         filename = paste(reacNameTable(),"_PvalCorrTable",sep="")),
                                                                                    list(extend='pdf',
                                                                                         filename= paste(reacNameTable(),"_PvalCorrTable",sep="")) )
                                                    ) )
  })

}

corrGraphOutput <- function(output,reacCalcTable,reacCorrGraph,reacNameTable,reacCorrTable){
  output$Corr_table2 <- renderDT({     datatable(reacCorrTable()$table,
                                                 selection= list(mode='single',selected = matrix(c(1,1), ncol = 2),target='cell'),
                                                 extensions="Buttons",
                                                 options = list(pageLength = 20, searching = FALSE, dom = 'Bt',
                                                                buttons =  list( 'copy',
                                                                                 list(extend='csv',
                                                                                      filename = paste(reacNameTable(),"_CorrTable",sep="")),
                                                                                 list(extend='excel',
                                                                                      filename = paste(reacNameTable(),"_CorrTable",sep="")),
                                                                                 list(extend='pdf',
                                                                                      filename= paste(reacNameTable(),"_CorrTable",sep="")) )
                                                 ) )  %>%  formatStyle(colnames(reacCorrTable()$table), fontWeight = styleInterval( c(-reacCorrTable()$corrSelect , reacCorrTable()$corrSelect)-1e-6,c('bold','normal','bold')))
  })

  output$corrGraph <- renderPlotly({  reacCorrGraph()  })
}

## Download ####

corrDownload <- function(input,output,reacCorrelogram){
  output$dCorrelogramTiff = downloadHandler(filename =
                                              reactive(paste(input$file1$name,"_Correlogram.tiff",sep = "")),
                                            content = function(file, compression = "lzw", res = 600) {
                                              tiff(file)
                                              print( reacCorrelogram() )
                                              dev.off()                      })
  output$dCorrelogramSvg = downloadHandler(filename =
                                             reactive(paste(input$file1$name,"_Correlogram.svg",sep = "")),
                                           content = function(file, compression = "lzw", res = 600) {
                                             svglite(file)
                                             print( reacCorrelogram() )
                                             dev.off()                      })
}
