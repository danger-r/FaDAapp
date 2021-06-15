## Sources ####
source("./app/general/inputFunctions.R")

## UI ####
readTableUI <- function(){
  tagList(
 conditionalPanel(condition="input.tabs != 'Tutorial'",
    br("To use our example dataset, click the box:"),
    switchInput(inputId = "TestTable",
                value = FALSE, onStatus = "success"),

    conditionalPanel(condition = "!input.TestTable",
      tagList(
        tags$p(tags$strong("Upload your file:")),
        tags$div(
          fileInput(inputId = "file1",label = NULL, multiple = FALSE,
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                    ),
          style="display: inline-block;vertical-align:top;width:50%;"
        ),
        tags$div(
          actionButton(inputId = "sw_html",
                       label = NULL, icon = icon("question"),
                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
          style="display:inline-block;vertical-align:top;"
        ),
        tags$div(
          downloadButton(outputId = 'Extable', label=NULL,
                         style="color: #fff; background-color: #33b7b2; border-color: #2e6da4;"),
          style="display:inline-block;vertical-align:top;"
        ),
        bsTooltip("sw_html","How to prepare your file",placement = "bottom", trigger = "hover",
                  options = NULL), #bsTooltip display a text when the cursor trigger the button (trigger option) The target is the sw_html
        bsTooltip("Extable","Download our example file",placement = "bottom", trigger = "hover",
                  options = NULL)
      )
     )
    )
  )
}

generalDataOptionUI <- function(){
  tagList(
    radioButtons(inputId = "Transform",
                 "Transform data?",
                 choices = c(Log2 = "Log2", none = "none"), selected = "none", inline = TRUE),
    bsTooltip("Transform","Log2 will set all the negative value to 0",placement = "bottom", trigger = "hover",
              options = NULL),
    selectInput(inputId = "SelectColor",
                "Change group colors:", c(rownames(brewer.pal.info[which(brewer.pal.info$category == "qual"),]) ), selected = "Set1" )
  )
}

generalParametricOptionUI <- function(){
  tagList(
    radioButtons(inputId = "Test",
                 "Test:",
                 choices = c(parametric = "parametric", nonparametric = "nonparametric"), selected = "parametric", inline = TRUE)
  )
}

generalTableHead <- function(){
  tagList(
    tags$div(
      h3(textOutput("filename")),
      style="display: inline-block;vertical-align:top;"
    ),
    tags$div(
      h3(" "),
      style="display: inline-block;vertical-align:top;"
    ),
    tags$div(
      actionButton(inputId = "sw_head",
                   label = NULL, icon = icon("question"),
                   style="color: black; background-color: white;"),
      style="display:inline-block;vertical-align:bottom;"
    ),
    bsTooltip("sw_head","Show head of the table"),
    tags$br(),tags$br(),
    bsModal("modal",textOutput("filename2"),trigger = "sw_head", size = 'large',
              DTOutput("head")

    )
  )

}

TutorialOptionUI <- function(){
  br("If you like FaDA, please cite:", tags$b("reference coming soon !", style = "color: black;"))
}

## Server ####
observeGeneral <- function(input,session,reacUsedTable){
  observeEvent(input$sw_html,{
      sendSweetAlert( session = session, title = NULL,
        text = tags$span(
          tags$h3("Prepare your file:",style = "color: steelblue;"),
          tags$b("First column or first row must be your sample identifiers with no replicates.",
                 tags$br(), "Second column or row must be your group identifiers named \"group\".",
                 tags$br(), "Numeric or integer values start at the third column or row.",
                 tags$br(), tags$b("Table must be a tab delimited text file (*.txt) with", style = "color: darkred;"),
                 span("point(\'. \') or comma(\', \') as decimal separator", style = "color:steelblue"),
                 tags$br(),      "and",      "enjoy !", icon("thumbs-up"),
                 tags$br(),tags$br(),tags$br(),
                 tags$img(src = "tableexample.png", width = "450px", height = "350px"),
                 "or",
                 tags$br(),
                 tags$img(src = "transposetableexample.png", width = "450px", height = "211px")
                )
              ),html = TRUE,type = 'info')
      })

  observeEvent(input$Transform,{ ## It's
    if(input$Transform == "Log2"){
      df <- reacUsedTable()
      for(el in df[2:length(df)]){
        if(length(el[el<=0])>0){
          sendSweetAlert(session,title = NULL,
                         text = tags$h3("Warning : Negatives values detected, theses values will be set NA",style = "color: red;")
          )
        }
      }
    }
  })
}


usedTable <- function(input, session){
  used_table <- reactive({
      return(funUsedTable(session,input$file1,input$TestTable))
    })
  return(used_table)
}

usedGroups <- function(input,used_table){
  used_groups <- reactive({
    funUsedGroups(used_table())
  })
  return(used_groups)
}

calcTable <- function(input,used_table){
  calc_table  <- reactive({
    funCalcTable(used_table(),input$Transform)
  })
  return(calc_table)
}

colorFunction <- function(input,used_table){
  colorFunction <- reactive({
    #rownames(brewer.pal.info)
    gcol <- funColorFunction(used_table(),input$SelectColor)
  })
  return(colorFunction)
}

nameTable <- function(input){

  NameTable <- reactive({

    if(input$TestTable){
      return("ExampleTable")
    }else {
      validate( need( !is.null(input$file1), "Please select a properly formatted data set" ) )
      return(input$file1$name)
    }

  })
  return(NameTable)
}

## Independant Functions ####

errorOutputAlert <- function(errorValue,session){
    sendSweetAlert(session = session, title = NULL,
                  text = tags$span(
                    switch(errorValue,
                           tags$h3("Error : Samples must have a unique id",style = "color: darkred;"),
                           tags$h3("Error : Table not correctly formatted",style = "color: darkred;"),
                           tags$h3("Error : Parameter values must be numeric or integer",style = "color: darkred;")
                    )
                  ),html = TRUE, type = 'error')
}

funUsedTable <- function(session,infoFile,infoTestTable = FALSE,shinyDependant = TRUE){
  #Now we take the file input into the validingInput function (to be renamed?) into the inputFunction.R file.
  if (infoTestTable == TRUE) {
    errorOutput <- validingInput("TestTable.txt")

  }
  else{
    if (is.null(infoFile)) {return(NULL)}
    else {
      errorOutput <- validingInput(infoFile$datapath)
    }
  }
  if(errorOutput$error != 0 & shinyDependant){ #The shinyDepedentOption is used to make the function indepedent to shiny (errorOutputAlert need the session value provided by shiny)
    errorOutputAlert(errorOutput$error,session)
  }
  return(errorOutput$dataframe)
}

funUsedGroups <- function(usedTable){
  return(usedTable[,1])
}

funCalcTable <- function(usedTable,infoTransform){
    validate( need( !is.null(usedTable), "Please select a properly formatted data set" ) )
    calcTable <- usedTable[,-1]
    if (infoTransform == "Log2")
      {
      calcTable[calcTable <= 0] <- NA
      calcTable <- apply(calcTable, 2, function(x) log2(x) )
    }
    return(as.data.frame(calcTable))
}

funColorFunction <- function(usedTable,infoSelectColor){
  validate( need( !is.null(usedTable), " " ) )
  group <- usedTable[,1]
  group <- as.factor(group)
  gcol <- brewer.pal(n = length(levels(group)), name = infoSelectColor)
  return(gcol)
}

## Output to UI ####

generalOutput <- function(input,output,reacUsedTable,reacNameTable){
  output$filename <- renderText({reacNameTable()})
  output$filename2 <- renderText({reacNameTable()})

  output$head <- renderDT(head(reacUsedTable()[,c(1:7)]))

}


## Download ####
extableDownload <- function(output,session){
  output$Extable <- downloadHandler(filename = "ExampleTable.txt",
                                    content = function(file) {
                                      testTable <- read.table("TestTable.txt", header = TRUE, row.names = 1, sep = "\t", as.is = TRUE)
                                      write.table(testTable, file, dec = ".", row.names = TRUE,
                                                  col.names = TRUE, sep = "\t")                    })
}
