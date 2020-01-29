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
      tags$h3("Fast data visualization and analysis of biological data", style = "color: #337ab7;"),
      tags$b("FaDA",style = "color: #337ab7;"), "is a user-friendly data visualization and analysis tool developed in R programming with RShiny web application developer. FaDA provides users of non-computational background to analyze their data for statistical analysis and quick visualization tools. This application is useful for common laboratory data outputs, including quantitative PCR, Flow Cytometry or ELISA.",
      tags$br(),

      tags$br(),
      tags$h4("FaDA Visualization &  Analysis Tools", style="color: #33b7b2;"),
      tags$li("Descriptive Statistics- A Summary Statistical Analysis of Data Input", style="list-style-type: square;"),
      tags$li("Batch Plots- Data plot of parameters using Whisker, Dot, Violin or Bar Type of graphs", style="list-style-type: square;"),
      tags$li("Heatmap & PCA Plots- Supervised or Unsupervised Sample Clustering with interactive plot options", style="list-style-type: square;"),
      tags$li("Data Correlation- Evidencing Parameter Associations", style="list-style-type: square;"),
      tags$li("ROC curves- Characterizing Discriminative Characteristics", style="list-style-type: square;"),


      tags$br(),
      a(icon("mail-bulk"), href="mailto:fada.contact@univ-nantes.fr"),
            "If you have any question, you can send an e-mail to",
      a("fada.contact", href="mailto:fada.contact@univ-nantes.fr",  style = "color: steelblue;"),
      tags$br(),

      tags$br(),
      tags$h4("Acknowledgment", style= "color: #33b7b2"),
      tags$h5("Project Contribution -Yodit Feseha, Quentin Moiteaux, Gerard Ramstein, Sophie Brouard & Richard Danger"),
      a("Richard Danger", style = "color: steelblue;"), "(PhD), research associate in the CRTI of Nantes (France), was supported by the", tags$b("Marie Sklodowska-Curie Fellowship", style = "color: steelblue;"), "from the European Commission's H2020 framework under the Grant Agreement No. 706296.",
      a("Yodit Feseha", style = "color: steelblue;"), "was supported by a Marie Sklodowska-Curie fellowship from the European Commission's H2020 research and innovation programme under the Innovative Training Network (ITN) programme Grant Agreement No. 721532.",
      tags$br(),
      "This work was conducted in the CRTI in Nantes (France), in the context of the", tags$b("Foundation Centaure", style = "color: steelblue;")," (RTRS), which supports a French Transplantation Research Network, the ", tags$b("DHU Oncogreffe", style = "color: steelblue;")," and the", tags$b("LabEX IGO", style = "color: steelblue;"), "supported by the National Research Agency via the investment of the future program ANR-11-LABX-0016-01 and the LABEX TRANSPLANTEX (ANR-11-LABX-0070_TRANSPLANTEX). This research was also supported by", tags$b("INSERM", style = "color: steelblue;"), "and", tags$b("IHU-CESTI", style = "color: steelblue;"), "institutes receiving financial support from the French Government managed by the National Research Agency (Investment into the Future Program ANR-10-IBHU-005), Nantes Metropole, and the Pays de la Loire Region.",
      tags$br(),
      tags$br(),
      "Technical Support and application hosting:", a("GenoBird Core Facility", href="https://pf-bird.univ-nantes.fr/", style = "color: steelblue;"),
      tags$br(),


      tags$br(),
      tags$h4("Citation", style = "color: #33b7b2;"),
      "Programming this application cost tears and sweat. If you like FaDA and use it, please consider citing the app:",  tags$b("Reference coming soon !", style = "color: black;"),
      tags$br(),
      tags$b("FaDA",style = "color: #337ab7;"), "application code is available through Github:",  tags$b("Link coming soon !", style = "color: black;"),
      tags$br(),


      tags$br(),
      tags$h4("Terms of Use", style = "color: #33b7b2;"),
      "This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License.",
      tags$br(),
      "This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE",
      tags$br(),
      "See the GNU General Public License for more details at :", a("GNU General Public License", href="https://www.gnu.org/licenses/", style = "color: steelblue;"),

      tags$br(),
      "Uploaded data and analysis results will not be saved on our servers. However, the application is provided", '" as is "', "without warranty of any kind, and thus, you must refrain fom uploading any confidential data. You may instead download the code and run the app locally on your private computer and network. We are not responsible for the confidentiality, availability, security, loss, misuse or misappropriation of any data you submit to this application.", style = "color: black;",

      tags$br(),
      tags$br(),
      img(src= "logo-pf-bird.png", width =100, style="margin:50px 0px"),
      img(src= "logo.png", width = 150), style="margin:50px 10px", #, class = "pull-right"),
      img(src= "INSERM-logo.png", width = 150, style="margin:50px 10px"),
      img(src= "Logo-IGO.png", width = 150, style="margin:50px 10px"),
      img(src= "logo-EU.png", width = 100, style="margin:50px 10px"),
      img(src= "logo-mca.png", width = 100, style="margin:50px 10px"),
      img(src= "Univ.png", width = 150, style="margin:50px 10px"),
      hr(),
      tags$h4("Copyright 2020 Center of Research in Transplantation and Immunology
Version 2020.01.21", align="center"),


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
