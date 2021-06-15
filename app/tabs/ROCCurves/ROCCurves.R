## UI ####
ROCCurvesUI <- function(){
  tagList(
    tags$h3("ROC curves:",style = "color: steelblue;"),
    uiOutput(outputId = "parameter1"),                                #to choose paramters to analyse
    uiOutput(outputId = "group1"),                                    #to choose first group
    uiOutput(outputId = "group2"),                                    #to choose second group
    radioButtons(inputId = "LineSize", "Change size of curves",       #to choose ROC curve line size
                 choices = c(small = "small", medium = "medium", large = "large"),
                 selected = "medium", inline = TRUE),
    plotOutput(outputId = "ROCPlot", width = "50%")  %>% withSpinner(color = "#0dc5c1"),
    downloadButton('dROCPlotTiff', label="Download as .Tiff"),
    downloadButton('dROCPlotSvg', label="Download as .SVG"),
    tags$br(),
    tags$h3("ROC analysis:",style = "color: steelblue;"),
    DTOutput(outputId ="AUC_table")  %>% withSpinner(color="#0dc5c1"),      #table of ROC values
    tags$br()
  )
}

## Server Functions ####
 #Function for plotting the ROC curve:
ROCPlot <- function(input, reacUsedTable, reacColorFunction, reacCalcTable){
  ROCPlot <- function(){
    validate(
      need( !is.null(reacUsedTable()), " " ),
      need( input$group1 != input$group2, "You must select differents groups" )
    )
    funROCPlot(reacUsedTable(),input$SelectColor,
               input$group1, input$group2, input$parameter1, reacCalcTable(),
               input$LineSize)
  }
  return(ROCPlot)
}

#Function for the AUc table:
AUCTable <- function(input, reacUsedTable, reacCalcTable){
  AUC_table <- reactive({
    validate(
      need( !is.null(reacUsedTable()), "  " ),
      need( input$group1 != input$group2, " " )
    )
    funAUCTable(reacUsedTable(),input$group1,input$group2,reacCalcTable())

  })
  return(AUC_table)
}

## Independant Functions ####
  # ROC curve:
funROCPlot <- function(used_Table, infoSelectColor,
                       infoGroup1, infoGroup2, infoParameter1, calcTable,
                       infoLineSize) {
      # prepare a table with selected groups:
  df3 <- used_Table[c(which(used_Table[,1] == infoGroup1), which(used_Table[,1] == infoGroup2)),]
  group_df3 <- df3[,1]
  df4 <- df3[,-1]
      # change size of lines:
  LSize <- switch(infoLineSize,
                 small = 2, medium = 4, large = 6)
  colors <- brewer.pal(n = length(levels(as.factor(colnames(used_Table[,-1])))),
                            name = infoSelectColor)
  
      # apply function to add several ROC curves (input is 'parameter1'):
  p <- lapply(infoParameter1, function(parameter){
    gROC <- roc( as.numeric(as.factor(group_df3)), as.numeric( df4[, which(colnames(df4) == parameter)] ))
    my_roc <- plot.roc(gROC)
    pos <- 0.4 - 0.05*(which(infoParameter1 == parameter)-1) #Calculate the position of the AUC print in the graph
    gcol <- colors[which(colnames(used_Table[,-1]) == parameter)] #Associate the color of the AUC print with the corresponding courb
    plot(my_roc, xlim=c(1,0), print.auc=TRUE, print.auc.pattern="AUC : %.3f", print.auc.col=gcol,
         col=gcol, print.auc.y=pos, print.auc.x=0.5, lwd=LSize, lty=1, add = TRUE)
    par(new=TRUE) #Put multiple graph in single plot
  })
  return(p)
}

funAUCTable <- function(used_Table,infoGroup1,infoGroup2, calcTable){       #prepare a table with selected groups (by default,
                                                                            #first group (input is infoGroup1) and second group (input is infoGroup2) are selected
    
  df3 <- used_Table[c(which(used_Table[,1] == infoGroup1), which(used_Table[,1] == infoGroup2)),]
  group_df3 <- df3[,1]
  df4 <- df3[,-1]

  Valroc <- matrix(nrow=6, ncol= length(colnames(df4) ), dimnames = list(list("AUC","Threshold","Specificity","Sensitivity","NPV","PPV"), colnames(df4) ) )
  Valroc[1,] <- apply(df4, 2, function(x) #Get the AUC
    roc( as.numeric(as.factor(group_df3)), as.numeric(x))$auc )
  Valroc[c(2:6),] <- apply(df4, 2, function(x){ #Get the others values
    rocc <- roc( as.numeric(as.factor(group_df3)), as.numeric(x))
    val <- coords(rocc,"best",ret = c("threshold","specificity","sensitivity","npv","ppv"),
                  transpose = TRUE)
    if(class(val) == "matrix"){
      return(val[,1])
    }else{
      return(val)
    }
  })

  Valroc <- format(Valroc, digits = 3)
  return(Valroc)
}

## Output to UI ####
ROCCurvesOutput <- function(output, reacCalcTable, reacROCPlot,
                            reacAUCTable, reacUsedGroups,
                            reacNameTable, reacPlotHeight){

  output$AUC_table <- renderDT({   datatable(reacAUCTable(),               #ROC values table
                                               extensions="Buttons",
                                               options = list(pageLength = 20, searching = FALSE, dom = 'tB',
                                                              buttons = list( 'copy',
                                                                              list(title = paste(reacNameTable(),"_AUCTable", sep=""), extend='csv',
                                                                                   filename = paste(reacNameTable(),"_AUCTable",sep="")),
                                                                              list(title = paste(reacNameTable(),"_AUCTable", sep=""), extend='excel',
                                                                                   filename = paste(reacNameTable(),"_AUCTable", sep="")),
                                                                              list(title = paste(reacNameTable(),"_AUCTable", sep=""), extend='pdf',
                                                                                   filename= paste(reacNameTable(),"_AUCTable", sep="")) ))
  )
  })

  output$ROCPlot <- renderPlot(height = "auto", width = "auto",
                               {  reacROCPlot()} )                    #ROC plot

  output$parameter1 <- renderUI({  df <- reacCalcTable()
  selectInput("parameter1","Parameter(s) to test:", colnames(df), selected = colnames(df)[1], multiple = TRUE)  })

  output$group1 <- renderUI({
    group <- reacUsedGroups()
    selectInput("group1","First group:", levels(as.factor(group)),
                selected = levels(as.factor(group))[1] )  })

  output$group2 <- renderUI({
    group <- reacUsedGroups()
    selectInput("group2","Second group:", levels(as.factor(group)),
                selected = levels(as.factor(group))[2] )  })

  output$group2 <- renderUI({
    group <- reacUsedGroups()
    selectInput("group2","Second group:", levels(as.factor(group)),
                selected = levels(as.factor(group))[2] )  })

 }

                      
## Download function: ####
ROCDownload <- function(input,output,reacROCPlot){
  output$dROCPlotTiff = downloadHandler(filename =
                                          reactive(paste(input$file1$name,"_ROCCurve.tiff",sep = "")),
                                        content = function(file, compression = "lzw", res = 600) {
                                          tiff(file)
                                          print( reacROCPlot() )
                                          dev.off()                      })
  output$dROCPlotSvg = downloadHandler(filename =
                                         reactive(paste(input$file1$name,"_ROCCurve.svg",sep = "")),
                                       content = function(file, compression = "lzw", res = 600) {
                                         svglite(file)
                                         print( reacROCPlot() )
                                         dev.off()                      })
}
## End
