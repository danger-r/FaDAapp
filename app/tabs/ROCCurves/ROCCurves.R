## UI ####
ROCCurvesUI <- function(){
  tagList(
    tags$h3("ROC curves:",style = "color: steelblue;"),
    uiOutput(outputId ="parameter1"),
    uiOutput(outputId ="group1"),
    uiOutput(outputId ="group2"),
    plotOutput(outputId ="ROCPlot", width = "50%")  %>% withSpinner(color="#0dc5c1"),
    downloadButton(outputId ='dROCPlot', label="Download"),
    tags$br(),
    tags$h3("AUC table:",style = "color: steelblue;"),
    DTOutput(outputId ="AUC_table")  %>% withSpinner(color="#0dc5c1"),
    tags$br()
  )
}

## Server Functions ####
ROCPlot <- function(input,reacUsedTable,reacColorFunction){
  ROCPlot <- function(){
    validate(
     need( !is.null(reacUsedTable()), "Please select a properly formatted data set" ),
     need( input$group1 != input$group2, "You must select differents groups" )
    )
    funROCPlot(reacUsedTable(),input$SelectColor,input$group1,input$group2,input$parameter1)
  }
  return(ROCPlot)
}

AUCTable <- function(input,reacUsedTable){
  AUC_table <- reactive({
    validate(
      need( !is.null(reacUsedTable()), "Please select a properly formatted data set" ),
      need( input$group1 != input$group2, "You must select differents groups" )
    )

    funAUCTable(reacUsedTable(),input$group1,input$group2)

  })
  return(AUC_table)
}
## Independant Functions ####
funROCPlot <- function(used_Table,infoSelectColor,infoGroup1,infoGroup2,infoParameter1){
  df3 <- used_Table[c(which(used_Table[,1] == infoGroup1), which(used_Table[,1] == infoGroup2)),]

  group_df3 <- df3[,1]
  df4 <- df3[,-1]

  colors <- brewer.pal(n = length(levels(as.factor(colnames(used_Table[,-1])))), name = infoSelectColor)

  lapply(infoParameter1,function(parameter){
    gROC <- roc( as.numeric(as.factor(group_df3)), as.numeric( df4[, which(colnames(df4) == parameter)] ))
    my_roc <- plot.roc(gROC)
    pos <- 0.4 - 0.05*(which(infoParameter1 == parameter)-1) #Calculate the position of the AUC print in the graph
    gcol <- colors[which(colnames(used_Table[,-1]) == parameter)] #Associate the color of the AUC print with the corresponding courb
    plot(my_roc, xlim=c(1,0), print.auc=TRUE, print.auc.pattern="AUC : %.3f", print.auc.col=gcol,
         percent=F, col=gcol, print.auc.y=pos, print.auc.x=0.5, lwd=4, lty=1,add = TRUE)
    par(new=TRUE) #Put multiple graph in single plot
  })

  # return(gROC)
}

funAUCTable <- function(used_Table,infoGroup1,infoGroup2){
  df3 <- used_Table[c(which(used_Table[,1] == infoGroup1), which(used_Table[,1] == infoGroup2)),]
  group_df3 <- df3[,1]
  df4 <- df3[,-1]
  Valroc <- matrix(nrow=6, ncol= length(colnames(df4) ), dimnames = list(list("AUC","Threshold","Specificity","Sensitivity","NPV","PPV"), colnames(df4) ) )
  Valroc[1,] <- apply(df4, 2, function(x) #Get the AUC
    roc( as.numeric(as.factor(group_df3)), as.numeric(x))$auc )
  Valroc[c(2:6),] <- apply(df4, 2, function(x){ #Get the others values
     rocc <- roc( as.numeric(as.factor(group_df3)), as.numeric(x))
     val <- coords(rocc,"best",ret = c("threshold","specificity","sensitivity","npv","ppv"))
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
ROCCurvesOutput <- function(output,reacCalcTable,reacROCPlot,reacAUCTable,reacUsedGroups,reacNameTable){
  output$AUC_table <- renderDT({     datatable(reacAUCTable(),
                                               extensions="Buttons",
                                               options = list(pageLength = 20, searching = FALSE, dom = 'Bt',
                                                              buttons = list( 'copy',
                                                                                 list(extend='csv',
                                                                                      filename = paste(reacNameTable(),"_AUCTable",sep="")),
                                                                                 list(extend='excel',
                                                                                      filename = paste(reacNameTable(),"_AUCTable",sep="")),
                                                                                 list(extend='pdf',
                                                                                      filename= paste(reacNameTable(),"_AUCTable",sep="")) ))
                                                              )
  })

  output$ROCPlot <- renderPlot({    reacROCPlot()    })

  output$parameter1 <- renderUI({  df <- reacCalcTable()
    selectInput("parameter1","Parameter to test:", colnames(df), selected = colnames(df)[1], multiple = TRUE)  })

  output$group1 <- renderUI({

                               group <- reacUsedGroups()
                               selectInput("group1","First group to test:", levels(as.factor(group)),
                                            selected = levels(as.factor(group))[1] )  })

  output$group2 <- renderUI({
                               group <- reacUsedGroups()
                               selectInput("group2","Second group to test:", levels(as.factor(group)),
                                            selected = levels(as.factor(group))[2] )  })

}

## Download ####
ROCDownload <- function(input,output,reacROCPlot){
  output$dROCPlot = downloadHandler(filename =
                                      reactive(paste(input$file1$name,"_ROCCurves.tiff",sep = "")), #set the filename of the downloaded file
                                    content = function(file, compression = "lzw", res = 600) {
                                      tiff(file)
                                      print( reacROCPlot() )
                                      dev.off()                      }, contentType = "image/png")
}
