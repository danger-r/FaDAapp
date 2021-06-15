## UI ####
introductionUI <- function(){
  tagList(
    htmlOutput("Textintro")
  )
}
## Serveur Functions ####

introduction <- function(){
  Textintro <- function(){
    text = tags$span(
      tags$br(),
      tags$br(),
      tags$h3("FaDA Data Analysis Tools", style =  "color: #337ab7;"),
      tags$br(),
    tags$h4("Prepare and Upload Your Dataset", style="color: #33b7b2;"),
    "To allow a flexible use of the application with a minimum preparation time, samples identification can be in columns or rows. FaDA input requires:",
        tags$li("First column or row of sample identifiers", style="list-style-type: square;"),
        tags$li("All identifiers should have unique names", style="list-style-type: square;"),
        tags$li("Second row or column must be labeled as ", a("Group"), style="list-style-type: square;"),
        tags$li("Points or comma could be used as decimal separators", style="list-style-type: square;"),
        tags$li("Table of data must be a text format: tabular-delimited '.txt' or '.csv' file", style="list-style-type: square;"),
    tags$br(),
    "FaDA provides an example of a dataset that can be used to explore the application.",

    tags$br(), "data table in columns:",
     img(src= "tableexample.png", width =400, style="margin:20px 300px"),
    tags$br(),
    "or in rows:",
    img(src= "transposetableexample.png", width =500, style="margin:20px 300px"),
    tags$br(),
    tags$br(),

    tags$h4("Statistical Summary", style="color: #33b7b2;"),
    "FaDA provides comprehensive statistical analysis options on the sidebar, with an option to transform data to log2 or log10 values. Statistical analysis output can be downloaded in CSV or Excel files.",


   tags$h4("Selecting suitable Statistical Analysis Method", style="color: #33b7b2;"),
   "The p-value of the Shapiro-Wilk normality test indicates whether the distribution of the dataset differs from Gaussian distribution guiding the users toward parametric or nonparametric test.",
    tags$br(),
    tags$li( tags$u("Parametric statistical analysis:"), "descriptive table provides the mean and standard deviation of the uploaded data. Group comparisons are performed using parametric t.test or ANOVA test with Tukey's 'Honest Significant Difference' method for multiple groups comparisons.", style="list-style-type: square;"),
  tags$li(tags$u("Non-parametric Analysis:"),"this setting is set to provide median and interquartile range (IQR) per groups with group comparisons performed using Mann-Whitney or the Kruskal Wallis test with Dunn's test for multiple comparisons.", style="list-style-type: square;"),
    tags$li(tags$u("Multiple Test Correction:"),"p-values corrections are performed using Bonferroni or Benjamin & Hochberg (BH) methods to correct for positive error rates due to multiple testing.", style="list-style-type: square;"),
    tags$br(),

    tags$h4("Data Visualization Options", style="color: #33b7b2;"),
    "In the sidebar, several graphical option can be selected, including Box-and-whiskers, points, bar and violin plots.",
    "Batch Graph Plots: Data can also be visualized and downloaded in a batch of all parameters. Graphs in selected types",
    tags$br(),
    tags$br(),
  img(src= "charts.png", width =1000, style="margin:20px 10px"),
   tags$br(),
   tags$br(),


    tags$h4("Heatmaps & PCA Plots", style="color: #33b7b2;"),
    "Both fixed and interactive Heatmap and PCA options are available. Interactive plots allow identifying potential outliers by hovering on the graph to identify the sample labels. Color Schemes of figures are available in sidebar (groups colors selection is not available yet for interactive heatmap).",
  img(src= "heatmapPCA.png", width =800, style="margin:10px 50px"),
  tags$br(),


   tags$h4("Correlation Plots", style="color: #33b7b2;"),
    "Correlation analysis of parameters can be assessed with parametric Pearson correlation or non-parametric  Spearman correlation. Individual correlation graphs display a scatter plot between two selected parameters. Tables of correlation (r) values and statistical significance test are available for users to view and download.",
  "The sliding bar can be used to highlight correlations above the selected threshold in the table.",
  img(src="corr.png", width =1000, style="margin:10px 0px"),
  tags$br(),


  tags$h4("ROC Curves", style="color: #33b7b2;"),
  "Receiver Operating Characteristics: True/False positive rates of selected parameters can be viewed in on ROC curve table.",

  img(src="roc.png",width =1000, style="margin:10px 0px"),
  tags$br(),



  hr(),
  tags$h4("Terms of Use", style = "color: #33b7b2;"),
  "This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License.",
  tags$br(),
  "This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE",
  tags$br(),
  "See the GNU General Public License for more details at :", a("GNU General Public License", href="https://www.gnu.org/licenses/", style = "color: steelblue;"),

  tags$br(),
  "Uploaded data and analysis results will not be saved on our servers. However, the application is provided", '" as is "', "without warranty of any kind, and thus, you must refrain fom uploading any confidential data. You may instead download the code and run the app locally on your private computer and network. We are not responsible for the confidentiality, availability, security, loss, misuse or misappropriation of any data you submit to this application.", style = "color: black;",
  tags$h4("Copyright 2020 Center of Research in Transplantation and Immunology
Version 2020.02.03", align="center"),





      html = TRUE)
  }
  return(Textintro)
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

introductionOutput <- function(output,Textintro){
  output$Textintro <- renderUI({  Textintro() })
}

