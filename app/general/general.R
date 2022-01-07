## Sources ####
source("./app/general/inputFunctions.R")

## general UI ####
readTableUI <- function(){
  tagList(
 conditionalPanel(condition="input.tabs != 'Tutorial'",           #conditional panel: if the tab Tutorial is selected
    br("To use our example dataset, click the box:"),
    switchInput(inputId = "TestTable",                            #switch button to use the example dataset (the 'TestTable.txt' file)
                value = FALSE, onStatus = "success"),

    conditionalPanel(condition = "!input.TestTable",              #conditional panel: if TestTable (example) is not selected, display the upload bar 
      tagList(
        tags$p(tags$strong("Upload your file:")),
        tags$div(
          fileInput(inputId = "file1",label = NULL, multiple = FALSE,
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                    ),
          style="display:inline-block;vertical-align:top;width:50%;"
        ),
        tags$div(
          actionButton(inputId = "sw_html",                                       #action button to display help to how prepare the data table 
                       label = NULL, icon = icon("question"),
                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
          style="display:inline-block;vertical-align:top;"
        ),
        tags$div(
          downloadButton(outputId = 'Extable', label=NULL,                        #button to downlad the example table (TestTable.txt)
                         style="color: #fff; background-color: #33b7b2; border-color: #2e6da4;"),
          style="display:inline-block;vertical-align:top;"
        ),
        bsTooltip("sw_html","How to prepare your file",placement = "bottom", trigger = "hover",
                  options = NULL), #bsTooltip display a text when the cursor trigger the button (trigger option) The target is the sw_html
        bsTooltip("Extable","Download our example file",placement = "bottom", trigger = "hover",
                  options = NULL) #bsTooltip display a text when the cursor trigger the button (trigger option) The target is the Extable
      )
     )
    )
  )
}

              ## general options in the sidebar:
generalDataOptionUI <- function(){
  tagList(
# options to log transform data:
        radioButtons(inputId = "Transform", "Transform data?",
                 choices = c(Log2 = "Log2", Log10 = "Log10", None = "none"),
                 selected = "none", inline = TRUE),
        bsPopover("Transform",
                title= "Logarithmic transformations", "Log2 & Log10 will set all the negative values to 0",
                  placement = "bottom", trigger = "hover", options = NULL),  #bsTooltip for log transform

# options to choose parametric or nonparametric tests/descriptive table:
        radioButtons(inputId = "Test", "Test:",
                     choices = c(Parametric = "parametric", Nonparametric = "nonparametric"),
                     selected = "parametric",
                     inline = TRUE),
        bsPopover("Test", title= "Shapiro-Wilk normality test", "If p-value is <0.05, data are not normally distributed, and, with less than 30 samples, the use of non-parametric tests is recommended.",
                  placement = "bottom", trigger = "hover", options = NULL),

# options to choose colors:
    selectInput(inputId = "SelectColor",
                "Change group colors:", c(rownames(brewer.pal.info[which(brewer.pal.info$category == "qual"),]) ), selected = "Set1" )
  )
}

        ## options for statistics:
TestParametricOptionUI <- function(){
  tagList(
    
# make choice of multiple tests corrections:
      radioButtons("correction", "Multiple tests correction:",
                   choices = c(BH = "BH",
                               Bonferroni = "bonferroni",
                               None = "none"), selected = "BH"),
# possible choices: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")
# help:
        bsPopover("correction",
                title= "Multiple tests correction", "In case of multiple statistical tests, correction is recommanded in order to avoid false positives. Bonferroni and Benjamini-Hochberg procedures are proposed",
                placement = "bottom", trigger = "hover", options = NULL),

# make choice of paired/unpaired design:
   radioButtons(inputId = "Design","Design:",
                   choices = c(Paired = "paired", Unpaired = "unpaired"), selected = "unpaired", inline = TRUE),

# make choice of equal/unequal variance (Welch t.test):
  radioButtons(inputId = "Equalvariance","Equal variance:",
             choices = c(Yes = "TRUE", No = "FALSE"), selected = "TRUE", inline = TRUE),
# help:
bsPopover("Equalvariance",
          title= "Variance in Student's t-test",
          "Student's t-test assumes that both populations have the same variance. In case variance are inequal, Welch's t-test is used.",
          placement = "right", trigger = "hover", options = NULL),

# make choice of plot type:
      radioButtons(inputId = "Graph",
                   "Graph options:",
                   choices = c(Whisker = "whiskers", Points = "point", Violins = "violin",
                               "Individual bars" = "bar", "Grouped bars" = "grouped_bar"), selected = "whiskers"),


# make choice to set ylim to 0:
strong("Set Y axis origin to O:"),
materialSwitch(inputId = "Setylim", status = "info")

)  }

generalTableHead <- function(){
  
  library(DT)
  
  
  tagList(
    tags$div(
      h3(textOutput("filename")),                                    #indicate the name of the uploaded table
      style="display: inline-block;vertical-align:top;"
    ),
    tags$div(
      h3(" "),
      style="display: inline-block;vertical-align:top;"
    ),
    tags$div(
      actionButton(inputId = "sw_head",                         #action button to display header of the uploaded table
                   label = NULL, icon = icon("question"),
                   style="color: black; background-color: white;"),
      style="display:inline-block;vertical-align:bottom;"
    ),
    bsTooltip("sw_head","Display the beginning of the table"),
    tags$br(),tags$br(),
    bsModal("modal",textOutput("filename2"),trigger = "sw_head", size = 'large',
            dataTableOutput("head")

    )
  )

}

        ## optionsTutorial UI:
TutorialOptionUI <- function(){
  text = tags$span(
  ##citing FaDA:
  br("If you like FaDA and use it, please consider citing the related article:",
       tags$br(),
  a("FaDA: A web application for regular laboratory data analyses", href = "https://pubmed.ncbi.nlm.nih.gov/34928943/", target="_blank", style = "color: steelblue;"),
  a("Danger et al, PLoS One. 2021 Dec 20;16(12):e0261083", href= "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0261083", target="_blank", style = "color: grey;")),
  
    tags$br(),
  tags$b("FaDA",style = "color: #337ab7;"), "application code is available through Github:", a("https://github.com/danger-r/FaDAapp", href="https://github.com/danger-r/FaDAapp", target="_blank", style = "color: steelblue;"),
  
  br( a(icon("mail-bulk"), href="mailto:fada.contact@univ-nantes.fr"),
      "If you have any question, you can send an e-mail to",
  br(a("fada.contact", href="mailto:fada.contact@univ-nantes.fr",  style = "color: steelblue;")) )
  )
}

        ## options for correlogramm (colors):
corrPlotOptionUI <- function(){
  tagList(
    tags$h3("correlogram option:"),
    selectInput(inputId ="Corrcolor1", "Color for low correlation:", c("blue","yellow","green","red","grey","violet")),
    selectInput(inputId ="CorrMiddlecolor", "Middle level color:",   c("white","grey","black")),
    selectInput(inputId ="Corrcolor2", "Color for high correlation:",   c("red","yellow","blue","green","grey","violet")),
  )
}

        ## options for heatmap (colors + clustering):
HeatmapOptionUI <- function(){
  tagList(
    tags$h3("Heatmap option:"),
    selectInput(inputId ="color1", "Color for low levels:", c("blue","yellow","green","red","grey","violet")),
    selectInput(inputId ="Middlecolor", "Middle level color:",   c("black","grey","white")),
    selectInput(inputId ="color2", "Color for high levels:",   c("red","yellow","blue","green","grey","violet")),

# make choice for unsupervised clustering:
  strong("Unsupervised clustering:"),
    materialSwitch(inputId = "Clustering", status = "info")
  )
}



## Server part ####

observeGeneral <- function(input,session,reacUsedTable){
  
          ## display instructions to prepare table:
  observeEvent(input$sw_html,{
      sendSweetAlert( session = session, title = NULL,
        text = tags$span(
          tags$h3("Prepare your file:", style = "color: steelblue;"),
          tags$br("First column or first row must be your sample identifiers with", tags$b("no replicates.", style = "color: darkred;")),
          tags$br("Second column or row must be your group identifiers named",
                 tags$b(" \"Group\". ",style = "color: darkred;")),
          tags$br("Numeric or integer values start at the third column or row."),

          tags$br("Table must be a tab delimited text or comma-separated values file",
                 tags$b( "(*.txt or *.csv)" , style = "color: darkred;"),
                 tags$b("with point(\'. \') or comma(\', \') as decimal separator"),
                 tags$br("and"), tags$b("enjoy !"), icon("thumbs-up")),
                 tags$br(),tags$br(),tags$br(),
                 tags$img(src = "tableexample.png", width = "450px", height = "350px"),
                 "or",
                 tags$br(),
                 tags$img(src = "transposetableexample.png", width = "450px", height = "211px")
                ),
              html = TRUE,type = 'info')
      })

            ## display an alert if negative values for log transformations -> values set to NA:
  observeEvent(input$Transform,{
    if(input$Transform == "Log2"){
      df <- reacUsedTable()
      for(el in df[2:length(df)]){
        if(length(el[el<=0])>0){
          sendSweetAlert(session,title = NULL,
                         text = tags$h3("Warning : Negative values detected, theses values will be set NA",
                                        style = "color: red;")
          )
        }
      }
    }
    if(input$Transform == "Log10"){
      df <- reacUsedTable()
      for(el in df[2:length(df)]){
        if(length(el[el<=0])>0){
          sendSweetAlert(session,title = NULL,
                         text = tags$h3("Warning : Negative values detected, theses values will be set NA",
                                        style = "color: red;")
          )
        }
      }
    }


  })
}


          ## function for reactive table:
usedTable <- function(input, session){
  used_table <- reactive({
      return(funUsedTable(session,input$file1,input$TestTable))
    })
  return(used_table)
}

          ## function for reactive groups:
usedGroups <- function(input,used_table){
  used_groups <- reactive({
    funUsedGroups(used_table())
  })
  return(used_groups)
}

          ## function for reactive transformed table:
calcTable <- function(input,used_table){
  calc_table  <- reactive({
    funCalcTable(used_table(),input$Transform)
  })
  return(calc_table)
}

          ## function for colors input:
colorFunction <- function(input,used_table){
  colorFunction <- reactive({
    gcol <- funColorFunction(used_table(),input$SelectColor)
  })
  return(colorFunction)
}

nameTable <- function(input){

  NameTable <- reactive({

    if(input$TestTable){
      return("ExampleTable")
    }else {
      validate( need( !is.null(input$file1), "  " ) )
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
                                        #take the file input into the validingInput function into the inputFunction.R file.
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

          ## log transformations:
funCalcTable <- function(usedTable,infoTransform){
    validate( need( !is.null(usedTable), "Please, upload a properly formatted dataset or use the example." ) ) ####Please, upload a properly formatted dataset or use the example.#
    calcTable <- usedTable[,-1]
    if (infoTransform == "Log2"){
      calcTable[calcTable <= 0] <- NA
      calcTable <- apply(calcTable, 2, function(x) log2(x) )}
    if (infoTransform == "Log10"){
      calcTable[calcTable <= 0] <- NA
      calcTable <- apply(calcTable, 2, function(x) log10(x) )
    }
    return(as.data.frame(calcTable))
}

      ## function to adjust n of colors to n of groups:
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
  output$Extable <- downloadHandler(filename = "TestTable.txt",
                                    content = function(file) {
                                      file.copy("TestTable.txt", file)
                                    })
}
                         
################## end
