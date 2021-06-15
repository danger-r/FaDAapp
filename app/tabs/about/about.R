## UI ####
aboutUI <- function(){
  tagList(
    htmlOutput("text1")
  )
}
## Serveur Functions ####

about <- function(){
  Text1 <- function(){
    text = tags$span(
      tags$h3("To perform a rapid analysis of your beloved data !", style = "color: blue;"),
      "The Shiny web application", tags$b("FaDA",style = "color: blue;"), "has been designed to to accelerate common lab data analyses, allowing a first glance at data, highlighting significative differences, significant associations/correlations or identifiying potential outliers. Different type of data can be used such as gene expression or cytometry measures.",
      tags$br(),
      tags$br(),
      tags$b("Various analyses and visulaisation can be performed:"),
      tags$br(),
      tags$li("Higlight significative differences: the descriptive table displays descriptive statistics and the comparison table displays  statistical analyses with unadjuste p-values and adjusted p-values. Results depends of choosen parameters; for more details, see tutorial page."),
      tags$li("plot of each parameter, using whisker, dot, violin or bar plot"),
      tags$li("Heatmap with supervised or unsupervised samples clustering"),
      tags$li("Principal Component Analysis to highlight variability in your dataset"),
      tags$li("Correlation of parameters"),
      tags$li("ROC curves"),

      tags$h3("From the" , a("CRTI", href="http://crti.univ-nantes.fr/", style = "color: steelblue;"), " Research Center of Nantes !", style = "color: blue;"),
      tags$br("This work was conducted in the CRTI in Nantes (France), in the context of the ", tags$b("Fondation Centaure", style = "color: steelblue;")," (RTRS), which supports a French transplantation research network, the ", tags$b("DHU Oncogreffe", style = "color: steelblue;")," and the ", tags$b("LabEX IGO", style = "color: steelblue;"), " supported by the National Research Agency via the investment of the future program ANR-11-LABX-0016-01 and the LABEX TRANSPLANTEX (ANR-11-LABX-0070_TRANSPLANTEX). This research was also supported by ", tags$b("INSERM", style = "color: steelblue;"), " and ", tags$b("IHU-CESTI", style = "color: steelblue;"), " institutes receiving financial support from the French Government managed by the National Research Agency (Investment into the Future Program ANR-10-IBHU-005), Nantes Metropole, and the Pays de la Loire Region."),
      tags$h3("People that participated to this project:", style = "color: blue;"),

      tags$li( a("Richard Danger", href="mailto:richard.danger@univ-nantes.fr",  style = "color: steelblue;"),
           "(PhD), research associate in the CRTI of Nantes (France), supported by the a ", tags$b("Marie Sklodowska-Curie fellowship", style = "color: steelblue;"), "from the European Commission's H2020 framework."),
      tags$li("Yodit Feseha"),
      tags$li("Quentin Moiteaux"),
      tags$li("Gérard Ramstein"),
      tags$li("Sophie Brouard"),
      tags$br(),
      tags$br("We are most grateful to the ", tags$b("GenoBiRD Core Facility", style = "color: steelblue;")," for its technical support."),
      tags$br(),

      tags$h3("Please cite:", style = "color: blue;"), tags$b("reference coming soon !", style = "color: black;"),

      tags$br(),
      img(src= "logo-pf-bird.png", width =100),
      img(src= "logo.png", width = 200), #, class = "pull-right"),
      img(src= "INSERM-logo.png", width = 200),
      img(src= "Logo-IGO.png", width = 200),
      img(src= "logo-EU.png", width = 200),
      img(src= "logo-mca.png", width = 100),
      img(src= "Univ.png", width = 100),

      html = TRUE)
  }
  return(Text1)
}

observeUploadFileToChangeTabs <- function(input,session){
  observeEvent(c(input$file1,input$TestTable),{
    if(input$tabs == "about"){
      if(!is.null(input$file1) | input$TestTable == TRUE)
      updateTabsetPanel(session, "tabs", selected = "tableDescr")
    }
  })
}

## Output to UI ####

aboutOutput <- function(output,Text1){
  output$text1 <- renderUI({  Text1() })
}
