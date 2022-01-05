## UI ####

tableDescrMainUI <- function(){
  tagList(
    tags$h3("Descriptive table:",style = "color: steelblue;"),
    DTOutput(outputId = "Input_tableDescr")  %>% withSpinner(color="#0dc5c1"),    #descrtiptive table
    tags$br(),
    tags$h3("Comparison table:",style = "color: steelblue;"),
    DTOutput(outputId ="Input_tableStat")  %>% withSpinner(color="#0dc5c1"),      #statistical table
    br(),
    downloadButton('dtableStat_Pdf', label="Download tables as a pdf file"),

    tags$h3("Select column(s) in the comparison table to display graph",style = "color: #b73338;"),
    plotlyOutput(outputId ="PlotDescr")  %>% withSpinner(color="#0dc5c1"),        # individual & interactive plot
    textInput("PlotTitle","Enter a title"),                                       # possibility to add a title
    textInput("PlotYAxis","Enter a Y axis label")                                 # possibility to add Y axis name
  )
} #Display on the main panel

#Highlight p-values:
pValSidebarUI <- function(){
  tagList( sliderInput("pValSelect", "Highlight in the table p-values above the selected threshold: ",0.0001, 1.000, 0.05),
)}

# select n digits to display:
SelDigitsSidebarUI <- function(){
  tagList(sliderInput("seldigits","Adjust number of digits in the table: ",0, 8, 3)
  )}


## Server Functions ####
  # Descriptive table  (median,mean,sd,...):
MytableDescr <- function(input,used_groups,calc_table){
  tableDescr <- reactive({
    validate( need( !is.null(calc_table()),
                    "Please, upload a properly formatted dataset or use the example." ) )
    return(funMytableDescr(used_groups(),calc_table(),input$Test))
  })
  return(tableDescr)
}

  # Table of stats:
MytableStat <- function(input,used_groups,calc_table){
  MytableStat <- reactive({
    validate( need( !is.null(calc_table()), " " ) )
    return(funMytableStat(used_groups(),calc_table(),input$Design,
                          input$Test,input$correction, input$Equalvariance,
                          input$pValSelect, input$seldigits))

  })
  return(MytableStat)
}

  # Plot the data:
plotDescr <- function(input,used_groups,calc_table,colorFunction,mytableStat){
  plotDescr <- reactive({
    df2 <- calc_table()
    validate( need( !is.null(df2), "" ) )
    selected <- input$Input_tableStat_columns_selected
    validate( need( !is.null(selected), "" ) )
    dim(selected) <- length(selected)
    data <- apply(selected,1,function(x){     #create the dataset to iterate with in the funPlotDescr function

      return(list("value"=df2[,x],"gene"=colnames(df2)[x],"pvalue"=mytableStat()$table[2,x]))
    })

    return(funPlotDescr(used_groups(),df2,colorFunction(),data,input$Graph,
                        input$file1$name,input$PlotTitle,input$PlotYAxis, input$Setylim))
  })
  return(plotDescr)
}


## Independant Functions ####

 #descriptive table:
funMytableDescr <- function(used_Groups,calc_Table,infoTest){

  req( !is.null(calc_Table), library(DT), cancelOutput = TRUE)  #For datatables functions

  ngroup <- length(levels(as.factor(used_Groups)))

  if (infoTest == "parametric") {                                                        #parametric -> mean, SD
    ValDesc <- matrix(nrow= ngroup*2+1, ncol= length(colnames(calc_Table) ),
                      dimnames = list(c(paste("mean-", levels(as.factor(used_Groups))),
                                        paste("sd-", levels(as.factor(used_Groups))), "Normality p-value") ,
                                      colnames(calc_Table) ) )

    for (i in 1: ngroup ) {
      subdata <- calc_Table[which(used_Groups == levels(as.factor(used_Groups))[i]),]
      ValDesc[i,] <- colMeans( subdata, na.rm=TRUE)
      ValDesc[i+ngroup,] <- apply( subdata, 2, sd, na.rm=TRUE)
     if (nrow(calc_Table) >=5000) { calc_Tableshapi <- calc_Table[1:5000,]
          ValDesc[ngroup*2+1,] <- apply( calc_Tableshapi, 2, function (x) shapiro.test(x)$p )} else {
        ValDesc[ngroup*2+1,] <- apply( calc_Table[], 2, function (x) shapiro.test(x)$p )}
    }
  }

  else {                                                                       # unparametric -> median, IQR
    ValDesc <- matrix(nrow= ngroup*2, ncol= length(colnames(calc_Table) ),
                      dimnames = list(c(paste("median-", levels(as.factor(used_Groups))), paste("IQR-", levels(as.factor(used_Groups)))) ,
                                      colnames(calc_Table) ) )

    for (i in 1: ngroup ) {
      subdata <- calc_Table[which(used_Groups == levels(as.factor(used_Groups))[i]),]
      ValDesc[i,] <- apply( subdata, 2, median, na.rm=TRUE)
      ValDesc[i+ngroup,] <- apply( subdata, 2, IQR, na.rm=TRUE)  }
  }


  ValDesc2 <- format(ValDesc, digits = 4)
  #print(ValDesc2, bordered = TRUE )
  return(ValDesc2)
}

 #stat table:
funMytableStat <- function(used_Groups,calc_Table,infoDesign,
                           infoTest, infoCorrection,infoEqualvariance, infopValSelect, infoseldigits){

  req( !is.null(calc_Table), library(DT), cancelOutput = TRUE)  #For datatables functions
  req( !is.null(calc_Table), library(heatmaply), cancelOutput = TRUE)  #needed for table config option
  
  Val <- matrix(nrow= 1, ncol= length(colnames(calc_Table) ), dimnames = list(list("p.value"), colnames(calc_Table) ) )      ## create a matrix according n of paramters in the 'calc_Table'

###for 2 groups:
  if (length(levels(as.factor(used_Groups))) == 2) {
  #paired/unpaired:
    if (infoDesign == "TRUE") {P = TRUE} else {P = FALSE}
    if (infoEqualvariance == "TRUE") {VarEq = TRUE} else {VarEq = FALSE}

    #parametric -> t.test
    if (infoTest == "parametric") {
      Val[1,] <- apply( calc_Table, 2, function (x)
        t.test( x ~ used_Groups, data = calc_Table, paired = P, var.equal = VarEq, na.omit = FALSE)$p.value )
    }
    else{                     # nonparametric -> wilcox/MannWhitney test
      Val[1,] <- apply( calc_Table, 2, function (x)
        wilcox.test( x ~ used_Groups, data = calc_Table, paired = P)$p.value ) }

    p.adj <- apply( Val, 1, function (x)
      p.adjust( x, method = infoCorrection, n = length(Val) ) )
    colnames(p.adj) = paste("p.adjusted ",infoCorrection,sep = "")

    Val2 <- format(rbind(Val, t(p.adj)), digits = infoseldigits, scientific =FALSE)
  return(list("table"=Val2, "pValSelect"=infopValSelect))
  }

###for more than 2 groups:
  else {
    if (infoTest == "parametric") {  #parametric
      if (infoDesign == "paired") {  #paired
        Val[1,] <-  apply( calc_Table, 2,function(x)
          unlist(summary(aov(x ~ factor(used_Groups) + Error( factor(rownames(calc_Table))), data = calc_Table))[1])[9] )
        ValTukey <- matrix(nrow= 1, ncol= length(colnames(calc_Table)),
                           dimnames = list("pairwise comparisons", colnames(calc_Table) ) )
        #      ValTukey <- apply( df2, 2,function(y, f)  TukeyHSD( aov( y ~ f) )$f[,4], f = group )
      }
      else {                  #unpaired -> Anova

        Val[1,] <-  apply( calc_Table, 2, function(y)  anova(lm( y ~ as.factor(used_Groups)))$"Pr(>F)"[1] )
        temp <- TukeyHSD( aov( calc_Table[,1] ~ factor(used_Groups)) )$`factor(used_Groups)`
        ValTukey <- matrix(nrow= length(rownames(temp)), ncol= length(colnames(calc_Table)),
                           dimnames = list( rownames(temp), colnames(calc_Table) ) )
        ValTukey <- apply( calc_Table, 2,function(y)  TukeyHSD( aov( y ~ as.factor(used_Groups) ) )$`as.factor(used_Groups)`[,4])
      }
    }
    else {              #unparametric -> Kruskal Wallis
      if (infoDesign == "paired") {  #paired
        validate( "Sorry, Friedman test is not implemented yet" )
      }
      else {  #unpaired
        import::from(FSA, dunnTest) # For dunnTest from FSA package

        Val[1,] <-  apply( calc_Table, 2, function(y)  kruskal.test(y ~ as.factor(used_Groups))$p.value )
        temp <-  dunnTest(calc_Table[,1] ~ as.factor(used_Groups), method="bh")$res[1]

        ValTukey <- matrix(nrow= length(levels(temp$Comparison)), ncol= length(colnames(calc_Table)),
                           dimnames = list( levels(temp$Comparison), colnames(calc_Table)))
        ValTukey <- apply( calc_Table, 2, function(y)  dunnTest(y ~ as.factor(used_Groups), method="bh" )$res$P.adj )
        rownames(ValTukey) <- temp[,1]
        #Dt= dunnTest(df2[,1] ~ as.factor(group), method="bh")$res$P.adj
        #ValTukey <- apply( df2, 2, function(y)  kruskalmc(y ~ as.factor(group), p.adj="holm" ) )
      }
    }

    p.adj <- apply( Val, 1, function (x)                            #multiple testing correction (according 'infoCorrection' selected by user)
      p.adjust( x, method = infoCorrection, n = length(Val) ) )
    colnames(p.adj) = paste("p.adjusted ",infoCorrection,sep = "")

    Val2 <- format(rbind(Val, t(p.adj), ValTukey), digits = infoseldigits)              #format matrix Val in Val2 with no scientific numbers
  }

  Val2 <- format(Val2, digits = infoseldigits, scientific =FALSE)
  #print(Val2, bordered = TRUE )
  return(list("table"=Val2, "pValSelect"=infopValSelect))
}




funPlotDescr <- function(used_Groups, calc_Table, gcol, data,
                         infoGraph,infoFilename, infoPlotTitle="", infoPlotYAxis="", infoSetylim){   #interactive plots of selected parameter
  dim(data) <- length(data)
  group <- as.factor(used_Groups)
  rown <- rownames(calc_Table)

  if (infoSetylim == "TRUE") {ylim0 = 0} else {ylim0 = NA}

  req( !is.null(calc_Table), library(ggplot2), cancelOutput = TRUE)  #Plot graphs
  req( !is.null(calc_Table), library(future), cancelOutput = TRUE)
  req( !is.null(calc_Table), library(promises), cancelOutput = TRUE)
  plan(multisession)
  
  #future_promise:
    future_promise(seed=TRUE, {
     
  plotList <- lapply(data,function(elem){
    datatoto <- data.frame(Value = elem$value, group = group, id = rown )
    ec <- max(elem$value)-min(elem$value)


    if (infoGraph == "whiskers") {                          #whiskers (according infoGraph input from radioButton)
      plot <-
        ggplot(data = datatoto, aes(x = group, y =  Value, fill = group, id = id) ) +
        geom_boxplot(col="black", outlier.shape = NA) + theme_classic() +
        theme(plot.title = element_text(hjust = 0.5, color="darkred", size=14, face="bold.italic") )+
        scale_fill_manual(values = gcol) +
        geom_point()+
        ylim(ylim0, max(datatoto$Value))+      ###start y axis to 0
        theme(legend.position="none") +
        labs(title=infoPlotTitle,
             y = infoPlotYAxis, x = "Groups") +
        annotate("text",x = levels(group)[length(levels(group))], y = max(elem$value)+0.1*ec, label = elem$gene, size = 4) #Display name on the graph
        #annotate("text",x = levels(group)[length(levels(group))], y = max(elem$value)-ec/20+0.1*ec, label = format(elem$pvalue, digits = 4), size = 4) #Display P-value on the graph

    }
    else{ if (infoGraph == "point") {                                         #point (according infoGraph input from radioButton)
      plot <-
        ggplot(data = datatoto, aes(x = group, y = Value, colour = group, id = id) ) +
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5, color="darkred", size=14, face="bold.italic") )+
        geom_point(position=position_jitterdodge(dodge.width=0), size = 2) +
        ylim(ylim0, max(datatoto$Value))+      ###start y axis to 0
        scale_color_manual(values = gcol) +
        theme(legend.position="none") +
        labs(title=infoPlotTitle,
             y = infoPlotYAxis, x = "Groups") +
        annotate("text",x = levels(group)[length(levels(group))], y = max(elem$value)+0.1*ec, label = elem$gene, size = 4)
      #+ #Display P-value on the graph        annotate("text",x = levels(group)[length(levels(group))], y = max(elem$value)-ec/20+0.1*ec, label = elem$pvalue, size = 4)

    }
      else{ if (infoGraph == "violin") {                                        #violin plot (according infoGraph input from radioButton)
        plot <-
          ggplot(data = datatoto, aes(x = group, y = Value, fill = group) ) +
          geom_violin(mapping = NULL, data = NULL, stat = "ydensity", position = "dodge", draw_quantiles = NULL, trim = TRUE,
                      scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)+
          geom_point(position=position_jitterdodge(dodge.width=0), size = 2, aes(x = group, fill = group, id = id)) +
          theme_classic() +
          ylim(ylim0, max(datatoto$Value))+      ###start y axis to 0
          theme(plot.title = element_text(hjust = 0.5, color="darkred", size=14, face="bold.italic") )+
          scale_fill_manual(values = gcol) +
          theme(legend.position="none") +
          labs(title=infoPlotTitle,
               y = infoPlotYAxis, x = "Groups") +
          annotate("text",x = levels(group)[length(levels(group))], y = max(elem$value)+0.1*ec, label = elem$gene, size = 4)
        #+ #Display P-value on the graph          annotate("text",x = levels(group)[length(levels(group))], y = max(elem$value)-ec/20+0.1*ec, label = elem$pvalue, size = 4)
      }

        else{ if (infoGraph == "grouped_bar") {                                        #grouped bar  plot (according infoGraph input from radioButton)
          plot <-
            ggplot(data = datatoto, aes(x = group, y = Value, fill = group) ) +
            geom_bar(stat = "identity", width = 0.5) +
            geom_point(position=position_jitterdodge(dodge.width=0), size = 2, aes(x = group, fill = group, id = id)) +
            theme_classic() +
            ylim(ylim0, max(datatoto$Value))+      ###start y axis to 0
            theme(plot.title = element_text(hjust = 0.5, color="darkred", size=14, face="bold.italic") )+
            scale_fill_manual(values = gcol) +
            theme(legend.position="none") +
            labs(title=infoPlotTitle,
                 y = infoPlotYAxis, x = "Groups") +
            annotate("text",x = levels(group)[length(levels(group))], y = max(elem$value)+0.1*ec, label = elem$gene, size = 4)
         }


        else{                                        #bar plot (according infoGraph input from radioButton)
          datatoto$id <- factor(datatoto$id, levels = rown)
          plot <-
            ggplot(data = datatoto, aes(x = id, y = Value, fill = group) ) +
            geom_bar(stat = "identity", width = 0.5) +
            theme_classic() +
            theme(plot.title = element_text(hjust = 0.5, color="darkred", size=14, face="bold.italic"),
                  axis.text.x=element_text(angle=90))+
            scale_fill_manual(values = gcol) +
            ylim(ylim0, max(datatoto$Value))+      ###start y axis to 0
            geom_point(position=position_jitterdodge(dodge.width=0.7))+
            theme(legend.position="none") +
            labs(title=infoPlotTitle,
                 y = infoPlotYAxis, x = "Samples") +
            annotate("text",x = levels(datatoto$id )[length(levels(datatoto$id ))-1], y = 1.2*max(elem$value), label = elem$gene, size = 4)+
            facet_grid(.~group, scales="free", space="free_x") ### to highlight the groups in the bar plot
        }
      }
    }
    }


    gplot <-  config(layout( ggplotly(plot), dragmode = "select"),
                     toImageButtonOptions= list(filename = paste(infoFilename, paste(infoGraph,"_Plot",sep = ""),sep="_"),
                                                format = "png", scale = 2),
                     displaylogo = FALSE,
                     modeBarButtonsToRemove = c('lasso2d','sendDataToCloud','zoom2d',
                                                'resetScale2d','hoverClosestCartesian','hoverCompareCartesian'
                     ))
    return(gplot)
  })

  if(length(plotList) == 1){
    res <- plotList[[1]]
  }else{
    res <- subplot(as.vector(plotList),titleY = TRUE, titleX = TRUE)
  }
  return(res)
    
  })## end futue promise
}


## Output to UI ####
tableDescrOutput <- function(output,reacMytableStat,reacMytableDescr,reacPlotDescr,reacNameTable){


  output$Input_tableDescr <- renderDT({     datatable(reacMytableDescr(),
                                                      extensions="Buttons",
                                                      options = list(pageLength = 20, searching = FALSE, dom = 'Bt',
                                                                     buttons =  list( 'copy',                                                                                                                                                                            list(title = paste(reacNameTable(),"_DescrTable",sep=""), extend='csv',
                                                                                           filename = paste(reacNameTable(),"_DescrTable",sep="")),
                                                                                      list(title = paste(reacNameTable(),"_DescrTable",sep=""), extend='excel',
                                                                                           filename = paste(reacNameTable(),"_DescrTable",sep="") ) ))
  )

  })

  output$PlotDescr <- renderPlotly({ reacPlotDescr()     })

  output$Input_tableStat <- renderDT({
                                        datatable(reacMytableStat()$table,
                                    selection = list(mode = 'multiple', selected = "none", #1
                                                     target = 'column', pageLength = 3),
                                    extensions="Buttons",
                                    options = list(pageLength = 20, searching = FALSE, dom = 'Bt',
                                              buttons = list( 'copy',
                                                        list(title = paste(reacNameTable(),"_StatTable",sep=""),
                                                        extend='csv',
                                                        filename = paste(reacNameTable(),"_StatTable",sep="")),
                                                        list(title = paste(reacNameTable(),"_StatTable",sep=""),
                                                        extend='excel',
                                                        filename = paste(reacNameTable(),"_StatTable",sep=""))
                                                      )  )) %>%

  ###To highlight significantivce values according selecte thresold ('pValSelect')
    formatStyle(
      colnames(reacMytableStat()$table),
      fontWeight = styleInterval( c(reacMytableStat()$pValSelect), c('bold','normal')),
      backgroundColor = styleInterval( c(reacMytableStat()$pValSelect), c('lightyellow', 'white')) )

  })

}




tableStatDownload <- function(input, output,reacMytableDescr, reacNameTable, reacMytableStat){
   output$dtableStat_Pdf = downloadHandler(
                                  filename =
                                         reactive(paste(reacNameTable(),"_StatTable.pdf",sep = "")),

                                  content = function(file) {
                                    maxcol = 5
                                    df <- rbind(reacMytableDescr(), reacMytableStat()$table)
                                    npages = ceiling(ncol(df)/maxcol)

                              pdf(file, height=11, width=8.5, pagecentre=FALSE,
                                  title="FaDA table", paper ="a4")  #"a4r" for landscape

                                    idx = seq(1,maxcol)
                                    tt <- ttheme_default(core=list(fg_params=list(hjust=1, x=0.95)),
                                                         colhead=list(fg_params=list(col="darkblue")) )
                                    grid.table(df[,idx], theme=tt)
                                    for ( i in 2:npages) {
                                      grid.newpage()
                                      if(i*maxcol <= ncol(df)){
                                        idx = seq(1+((i-1)*maxcol),i*maxcol)
                                      } else {
                                        idx = seq(1+((i-1)*maxcol), ncol(df))
                                      }
                                      grid.table(df[,idx], theme=tt) }
                                    dev.off()
                                  } )}



######## End #rm(list=ls())
