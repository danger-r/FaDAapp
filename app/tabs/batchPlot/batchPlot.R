## UI ####
batchPlotUI <- function(){
  tagList(
    plotOutput(outputId ="Plot1") %>% withSpinner(color="#0dc5c1"), downloadButton('dPlot1', label="Download")
  )
}

## Server Functions ####
batchPlot <- function(input,used_groups,calc_table,colorFunction){
  Plot1 <- function(){
    validate( need( !is.null(calc_table()), "Please select a properly formatted data set" ) )
    df <- calc_table()
    graphList <- funBatchPlot(used_groups(),df,t(df),colorFunction(),input$Graph)
    grid.arrange(grobs = graphList, ncol= 4) #Arrange the graphs output
  }
  return(Plot1)
}

MoreBatchPlot <- function(input,used_groups,calc_table,colorFunction){
  Plot2 <- function(){
    validate( need( !is.null(calc_table()), "Please select a properly formatted data set" ) )
    df <- calc_table()
    graphList <- funMoreBatchPlot(used_groups(),df,t(df),colorFunction(),input$Graph)
    #grid.arrange(grobs = graphList, ncol= 2) #Arrange the graphs output
    marrangeGrob(graphList, nrow=2, ncol=2, vp=viewport(width=0.9, height=0.9))
    #marrangeGrob(grobs = graphList, nrow=2,ncol=2, layout, vp=viewport(width=0.9, height=0.9))
    #g <- arrangeGrob(grobs = graphList)#, nrow=2,ncol=2, layout, vp=viewport(width=0.9, height=0.9))
  }
  return(Plot2)
}

## Independant Functions ####
funBatchPlot <- function(used_Groups,calc_Table,tCalc_Table,colors,infoGraph){
  if ( length(levels(as.factor(used_Groups))) * length(colnames(calc_Table)) > 25 ) {
    print("Too many data")
    stop("Too many data to plot on the screen, please download the file instead")
  }

  rown <- rownames(calc_Table)
  #print(rown)

    graphList <- mclapply(1:nrow(tCalc_Table),function(i){


    gene= rownames(tCalc_Table)[i]
    datatoto <- data.frame(Expression = tCalc_Table[i,], group = as.factor(used_Groups), id = rown)
    if (infoGraph == "whiskers") {
      plot <- ggplot(data = datatoto, aes(x = group, y = Expression, fill = group)) +
        geom_boxplot(col="black", outlier.colour = NA) + theme_classic() +
        theme(plot.title = element_text(hjust = 1, color="darkred", size=10, face="bold.italic"),
              axis.text.x=element_text(angle= 45, hjust = 1))+
        scale_fill_manual(values = colors) +
        geom_point(position=position_jitterdodge(dodge.width=0))+
        theme(legend.position="none") +
        labs(title= gene, x= "", y="")
    }
    else{ if (infoGraph == "point") {
      plot <- ggplot(data = datatoto, aes(x = group, y = Expression, color = group)) + theme_classic() +
        theme(plot.title = element_text(hjust = 1, color="darkred", size=10, face="bold.italic"),
              axis.text.x=element_text(angle= 45, hjust = 1) )+
        scale_color_manual(values = colors) +
        geom_point(position=position_jitterdodge(dodge.width=0))+
        theme(legend.position="none") +
        labs(title= gene, x= "", y="")
    }

      else{ if (infoGraph == "violin") {
        plot <- ggplot(data = datatoto, aes(x = group, y = Expression, fill = group) ) +
          geom_violin(mapping = NULL, data = NULL, stat = "ydensity", position = "dodge", scale = "area") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5, color="darkred", face="bold.italic") )+
          geom_point(position=position_jitterdodge(dodge.width=0) ) +
          scale_fill_manual(values = colors) +
          theme(legend.position="none") +
          labs(title= gene , x= "", y="")
      }

        else{
          print(datatoto$id)
          datatoto$id <- factor(datatoto$id, levels = rown)
          plot <- ggplot(data = datatoto, aes(x = id, y = Expression, fill = group)) +
            geom_bar(stat = "identity") + theme_classic() +
            theme(plot.title = element_text(hjust = 1, color="darkred", size=10, face="bold.italic"),
                  axis.text.x=element_text(angle= 45, hjust = 1))+
            scale_fill_manual(values = colors) +
            geom_point(position=position_jitterdodge(dodge.width=0))+
            theme(legend.position="none") +
            labs(title= gene, x= "", y="")
        }
      }}
    return(plot)
  })
  return(graphList)
}
funMoreBatchPlot <- function(used_Groups, calc_Table,tCalc_Table,colors,infoGraph){
  rown <- rownames(calc_Table)

    graphList <- mclapply(1:nrow(tCalc_Table),function(i){
    gene= rownames(tCalc_Table)[i]
    datatoto <- data.frame(Expression = tCalc_Table[i,], group = as.factor(used_Groups), id = rown)
    if (infoGraph == "whiskers") {
      plot <- ggplot(data = datatoto, aes(x = group, y = Expression, fill = group)) +
        geom_boxplot(col="black", outlier.colour = NA) + theme_classic() +
        theme(plot.title = element_text(hjust = 1, color="darkred", size=10,
                                        face="bold.italic"),
              axis.text.x=element_text(angle= 45, hjust = 1))+
        scale_fill_manual(values = colors) +
        geom_point(position=position_jitterdodge(dodge.width=0))+
        theme(legend.position="none") +
        labs(title= gene, x= "", y="")
    }
    else{ if (infoGraph == "point") {
      plot <- ggplot(data = datatoto, aes(x = group, y = Expression, color = group)) + theme_classic() +
        theme(plot.title = element_text(hjust = 1, color="darkred", size=10,
                                        face="bold.italic"),
              axis.text.x=element_text(angle= 45, hjust = 1) )+
        scale_color_manual(values = colors) +
        geom_point(position=position_jitterdodge(dodge.width=0))+
        theme(legend.position="none") +
        labs(title= gene, x= "", y="")
    }

      else{ if (infoGraph == "violin") {
        plot <- ggplot(data = datatoto, aes(x = group, y = Expression, fill = group) ) +
          geom_violin(mapping = NULL, data = NULL, stat = "ydensity", position = "dodge", scale = "area") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 1, color="darkred", size=10, face="bold.italic") )+
          geom_point(position=position_jitterdodge(dodge.width=0) ) +
          scale_fill_manual(values = colors) +
          theme(legend.position="none") +
          labs(title= gene , x= "", y="")
      }

        else{
          print(datatoto$id)
          datatoto$id <- factor(datatoto$id, levels = rown)
          plot <- ggplot(data = datatoto, aes(x = id, y = Expression, fill = group)) +
            geom_bar(stat = "identity") + theme_classic() +
            theme(plot.title = element_text(hjust = 1, color="darkred", size=10,
                                            face="bold.italic"),
                  axis.text.x=element_text(angle= 45, hjust = 1))+
            scale_fill_manual(values = colors) +
            geom_point(position=position_jitterdodge(dodge.width=0))+
            theme(legend.position="none") +
            labs(title= gene, x= "", y="")
        }
      }}
    return(plot)
  })
  return(graphList)
}


## Output to UI ####
batchPlotOutput <- function(output,reacBatchPlot){
  output$Plot1 <- renderPlot({    reacBatchPlot()    })
}
MoreBatchPlotOutput <- function(output,reacMoreBatchPlot){
  output$Plot2 <- renderPlot({    reacMoreBatchPlot()    })
}

## Download ####
batchPlotDownload <- function(input,output, reacUsedTable, reacBatchPlot,reacMoreBatchPlot){
  output$dPlot1 =   downloadHandler(filename =reactive(paste(input$file1$name,"_Graphs.pdf",sep = "")),
                                    content = function(file, compression = "lzw", res = 600) {
                                                             df <- reacUsedTable()
                                                             group <- df[,1]
                                                             df2 <- df[,-1]

                                    if ( length(levels(as.factor(group))) * length(colnames(df2))  < 25 ) {
                                                               pdf(file)
                                                               print( reacBatchPlot() )
                                                               dev.off()      }

                                    else {
                                      ggsave(file, reacMoreBatchPlot() )
                                      #dev.off()
                                      }
  } )}
