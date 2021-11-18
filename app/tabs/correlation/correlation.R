## UI ####
#correlogram page:
correlogramUI <- function(){
  tagList(
    tags$h3("Correlogram:",style = "color: steelblue;"),
    plotOutput(outputId ="Correlogram"),   # or plotlyOutput for interactive
    radioButtons(inputId = "CorrFormat", "Change correlogram shape:",                         # to choose circle or square in correlogram
                 choices = c(circle = "circle", ellipse = "ellipse", number = "number",
                             classic = "color"),
                 selected = "circle", inline = TRUE),
    radioButtons(inputId = "CorrLayout", "Change correlogram layout:",                          # to choose correlogram orientation
                 choices = c(lower = "lower", upper = "upper", full = "full"),
                 selected = "lower", inline = TRUE),
 tags$hr(), # Horizontal line --
    tags$h3("Correlation (r) table:",style = "color: steelblue;"),
    DTOutput(outputId ="Corr_table") %>% withSpinner(color="#0dc5c1"),                    #table of correlation values ("Corr_table")
    tags$br(),
    tags$h3("p-values table:",style = "color: steelblue;"),
    DTOutput(outputId ="pvalCorr_table") %>% withSpinner(color="#0dc5c1"),                 #p-value table ("pvalCorr_table")
    plotlyOutput(outputId ="Scater_Plot_Cor")                                              #interactive plot of individual correlation
  )
}

corrGraphUI <- function(){ #The  UI for the correlogram graph
  tagList(
    tags$h3("Correlation graphs:",style = "color: steelblue;"),
    DTOutput("Corr_table2") %>% withSpinner(color="#0dc5c1"),
    tags$h3("Select a cell in the above table to display the associated correlation graph",style = "color: #b73338;"),
    plotlyOutput(outputId ="corrGraph", width = "70%") %>% withSpinner(color="#0dc5c1")
  )
}

#to higlight correlation in the crooelation table :
corrSidebarUI <- function(){
  tagList(
    sliderInput("corrSelect","Highlight correlations above the selected threshold: ",0,1,0.75)
    )}

#to higlight p-values in the p-value table:
pValCorrSidebarUI <- function(){
  tagList(
    sliderInput("pValCorrSelect","Highlight in the table p-values above the selected threshold: ",0.0001, 1.000, 0.05)
)}


## Server Functions ####
  # correlogram plot:
correlogram <- function(input,reacCalcTable){
  Correlogram <- function(){
    validate( need( !is.null(reacCalcTable()), "Please, upload a properly formatted dataset or use the example." ) )
    funCorrelogram(reacCalcTable(),input$Test,input$Corrcolor1,input$CorrMiddlecolor,input$Corrcolor2,
                   input$CorrFormat, input$CorrLayout)
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

### p-value Correlation table:
pvalCorrTable <- function(input,reacCalcTable){
  pval_Correlation_table <- reactive({
    validate( need( !is.null(reacCalcTable()), " " ) )
    funPvalCorrTable(reacCalcTable(),input$Test, input$pValCorrSelect)

  })
  return(pval_Correlation_table)
}

  # graph of indiviual pairs:
corrGraph <- function(input,reacCalcTable){
  return(reactive({
    validate( need( !is.null(reacCalcTable()), "Please, upload a properly formatted dataset or use the example." ) )
    validate( need(!is.null(input$Corr_table2_cells_selected),"Select a cell on the table above to display a graph"))
    funCorrGraph(reacCalcTable(),input$Corr_table2_cells_selected)
  })
  )
}


## Independant Functions ####

funCorrelogram <- function(calcTable,infoTest, infoCorrColor1,infoCorrMiddleColor,infoCorrColor2,
                           infoCorrFormat, infoCorrLayout){

#If NA, Replace NA by KNN, need  transposition
  if ( length( which(is.na(calcTable) == TRUE)) >= 1 )
  { calcTable <- t(calcTable)
     import::from(impute, impute.knn)   #knn function
  dfkNN <- impute.knn(calcTable, k = 10, rowmax = 0.5, colmax =0.8, rng.seed=362436069)
  dfkNN <- t(dfkNN$data)
  calcTable <-as.matrix(dfkNN)
  }

  if (infoTest == "parametric") {
    mcor <- cor(calcTable, method = "pearson", use = "complete.obs")   }
  else {                                                                   #unparametric
    mcor <- cor(calcTable, method = "spearman", use = "complete.obs")  }

  col <- colorRampPalette(c(infoCorrColor1, infoCorrMiddleColor, infoCorrColor2))(20)

   # change format of correlogramn (input is infoCorrFormat for shape; infoCorrLayout for correlogram orientation):
  CorrMethod <- switch(infoCorrFormat,
                  circle = "circle", ellipse = "ellipse", number = "number",
                  color = "color")
  CorrLayout <- switch(infoCorrLayout,
                       lower = "lower", upper = "upper", full = "full")

  corrplot(mcor, type= CorrLayout, order="hclust", tl.col="black", method= CorrMethod,        #corrplot function
           tl.srt=45, diag=F, col = col)
  return(mcor)

  #interactive corplot:
  #gcorrelogram <- ggcorrplot(mcor, hc.order = TRUE, type = "lower",
  #           colors = c(infoCorrColor1, infoCorrMiddleColor, infoCorrColor2))
  #correlogram_ly <- ggplotly(gcorrelogram) %>% layout( dragmode = "select") #%>%  partial_bundle() #%>% toWebGL()
  #return(correlogram_ly)
}

funCorrTable <- function(calcTable,infoTest,infoCorrSelect){
                                        #If NA, Replace NA by KNN, need  transposition
  if ( length( which(is.na(calcTable) == TRUE)) >= 1 )
  { calcTable <- t(calcTable)
  dfkNN <- impute.knn(calcTable, k = 10, rowmax = 0.5, colmax =0.8, rng.seed=362436069)
  dfkNN <- t(dfkNN$data)
  calcTable <-as.matrix(dfkNN)
  }

  if (infoTest == "parametric") {                                           #parametric-> pearson
    mcor <-cor(calcTable, method = "pearson", use = "complete.obs")   }
  else {                                                                    #unparametric-> spearman
    mcor <-cor(calcTable, method = "spearman", use = "complete.obs")  }
  mcor <- format(mcor, digits = 3)
  return(list("table"=mcor, "corrSelect"=infoCorrSelect))
}

funPvalCorrTable <- function(calcTable,infoTest, infopValCorrSelect){
  if (infoTest == "parametric") {                                           #parametric-> pearson
    mcor_test <- cor.mtest(calcTable, method = "pearson", na.action = "na.exclude" )$p   }
  else {                                                                    #unparametric-> spearman
    mcor_test <-cor.mtest(calcTable, method = "spearman", na.action = "na.exclude")$p  }

  colnames(mcor_test)<-colnames(calcTable)
  rownames(mcor_test)<-colnames(calcTable)
  mcor_test <- format(mcor_test, digits = 3)
  return(list("table"=mcor_test, "pValCorrSelect"=infopValCorrSelect))
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

  gplot <- ggplotly(plot) %>% layout( dragmode = "select") %>%  partial_bundle()
  return(gplot)
}

## Output to UI ####
correlogramOutput <- function(output,reacCorrTable,reacPvalCorrTable,reacCorrelogram,reacNameTable){

  output$Correlogram <- renderPlot({    reacCorrelogram()      })  #or renderPlotly for interactive version

  output$Corr_table <- renderDT({     datatable(reacCorrTable()$table,
                                                extensions="Buttons",
                                                options = list(pageLength = 20, searching = FALSE, dom = 'Bt',
                                                               buttons =  list( 'copy',
                                                                                list(title = paste(reacNameTable(),"_CorrTable",sep=""), extend='csv',
                                                                                     filename = paste(reacNameTable(),"_CorrTable",sep="")),
                                                                                list(title = paste(reacNameTable(),"_CorrTable",sep=""), extend='excel',
                                                                                     filename = paste(reacNameTable(),"_CorrTable",sep="")),
                                                                                list(title = paste(reacNameTable(),"_CorrTable",sep=""), extend='pdf',
                                                                                     filename= paste(reacNameTable(),"_CorrTable",sep="")) )
  ## formatStyle to higlight correlation according threshold (corrSelect):                                                           
                                                ) )  %>%  formatStyle(colnames(reacCorrTable()$table),
                                                                      fontWeight = styleInterval(  c(-reacCorrTable()$corrSelect, reacCorrTable()$corrSelect), c('bold','normal','bold')),
                                                                      backgroundColor = styleInterval(  c(-reacCorrTable()$corrSelect, reacCorrTable()$corrSelect), c('lightyellow', 'white','lightyellow'))
                                                                      )
  })

  output$pvalCorr_table <- renderDT({     datatable(reacPvalCorrTable()$table,
                                                    extensions="Buttons",
                                                    options = list(pageLength = 20, searching = FALSE, dom = 'Bt',
                                                                   buttons =  list( 'copy',
                                                                                    list(title = paste(reacNameTable(),"_PvalCorrTable",sep=""), extend='csv',
                                                                                         filename = paste(reacNameTable(),"_PvalCorrTable",sep="")),
                                                                                    list(title = paste(reacNameTable(),"_PvalCorrTable",sep=""), extend='excel',
                                                                                         filename = paste(reacNameTable(),"_PvalCorrTable",sep="")),
                                                                                    list(title = paste(reacNameTable(),"_PvalCorrTable",sep=""), extend='pdf',
                                                                                         filename= paste(reacNameTable(),"_PvalCorrTable",sep=""))))) %>%
                                     
                  ## formatStyle to higlight p-values  according threshold (pValCorrSelect):                                                           
                                                                formatStyle(
                                                                  colnames(reacPvalCorrTable()$table),
                                                                  fontWeight = styleInterval( c(reacPvalCorrTable()$pValCorrSelect), c('bold','normal')),
                                                                  backgroundColor = styleInterval( c(reacPvalCorrTable()$pValCorrSelect), c('lightyellow', 'white')) )

  })

}

corrGraphOutput <- function(output,reacCalcTable,reacCorrGraph,reacNameTable,reacCorrTable){
  output$Corr_table2 <- renderDT({     datatable(reacCorrTable()$table,
                                                 selection= list(mode='single', selected = matrix(c(1,1), ncol = 2), target = 'cell'),
                                                 extensions="Buttons",
                                                 options = list(pageLength = 20, searching = FALSE, dom = 'Bt',
                                                                buttons =  list( 'copy',
                                                                                 list(title = paste(reacNameTable(),"_CorrTable",sep=""), extend='csv',
                                                                                      filename = paste(reacNameTable(),"_CorrTable",sep="")),
                                                                                 list(title = paste(reacNameTable(),"_CorrTable",sep=""), extend='excel',
                                                                                      filename = paste(reacNameTable(),"_CorrTable",sep="")),
                                                                                 list(title = paste(reacNameTable(),"_CorrTable",sep=""), extend='pdf',
                                                                                      filename= paste(reacNameTable(),"_CorrTable",sep="")) )
                                                 ) )  %>%  formatStyle(colnames(reacCorrTable()$table), fontWeight = styleInterval( c(-reacCorrTable()$corrSelect , reacCorrTable()$corrSelect),c('bold','normal','bold')),
                                                                       backgroundColor = styleInterval(  c(-reacCorrTable()$corrSelect, reacCorrTable()$corrSelect), c('lightyellow', 'white','lightyellow')))
  })

  output$corrGraph <- renderPlotly({  reacCorrGraph()  })
}

##End
