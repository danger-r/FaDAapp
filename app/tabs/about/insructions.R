## UI ####
introductionUI <- function(){
  tagList(
    htmlOutput("Textintro")   ##Display text
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
     a("Various statistical options are supported with FaDA,
        however, conclusions should be drawn only with adequate methods and recommendations from a statistician should be seek for, if needed."),

      tags$br(),
      tags$h4("Prepare and Upload Your Dataset", style="color: #33b7b2;"),
    "To allow a flexible use of the application with a minimum preparation time, samples identification can be in columns or rows. FaDA input requires:",
        tags$li("First column or row corresponds to sample identifiers", style="list-style-type: square;"),
        tags$li("Sample identifiers should have unique names", style="list-style-type: square;"),
        tags$li("Second row or column must be labeled ", tags$b(a("Group")), style="list-style-type: square;"),
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
    "FaDA provides comprehensive statistical analysis options on the sidebar, and an option to transform data to log2 or log10 values. Statistical analysis output can be copied, downloaded in CSV, Excel or pdf files.",


   tags$h4("Selecting suitable Statistical Analysis Method", style="color: #33b7b2;"),
   "Parametric tests are commonly used but require a normal sampling distribution. If the sample size is large enough, approximately normally distributed samples can be analysed using parametric tests (central limit theorem). Nonparametric tests do not make assumption about the underlying distribution of variables. Such tests should be used with small sample size, non-normal distributed data, ordinal data and ranked data. They are thus more robust but also frequently less powerful.",
   tags$br(),
   "The p-value of the Shapiro-Wilk normality test indicates whether the distribution of the dataset differs from a Gaussian distribution guiding the users toward parametric or nonparametric test.",
   tags$br(),
   tags$br(),
    tags$li( tags$u("Parametric statistical analysis:"), "descriptive table provides the mean and standard deviation of the uploaded data. Group comparisons are performed using parametric t.test or ANOVA test with Tukey's 'Honest Significant Difference' method for multiple groups comparisons. The Welch's t.test is proposed in case of unequal variance.", style="list-style-type: square;"),
  tags$li(tags$u("Non-parametric analysis:"),"using this setting provides median and interquartile range (IQR) per groups with group comparisons performed using Mann-Whitney or the Kruskal Wallis test with Dunn's test for multiple comparisons.", style="list-style-type: square;"),
    tags$li(tags$u("Multiple Testing Correction:"),"p-values can be adjusted to account for multiple comparisons in order to avoid false positives. FaDA allows to perform corrections using the Bonferroni method that controls the family-wise error rate or the Benjamin & Hochberg (BH) method that corrects the the false discovery rate (FDR), which is the proportion of false discoveries among the rejected hypotheses.
While the Bonferroni method is useful with a fairly small number of comparisons, it may be too conservative and may lead to a high rate of false negatives. Both methods assume that the individual tests are independents (not correlated).", style="list-style-type: square;"),
    tags$br(),

    tags$h4("Data Visualization Options", style="color: #33b7b2;"),
    "In the sidebar, several graphical options can be selected, including plots using box-and-whiskers, points, bars and violins.",
    "Batch Graph Plots: Data can also be visualized and downloaded in a batch for all parameters.",
    tags$br(),
    tags$br(),
  img(src= "charts.png", width =1000, style="margin:20px 10px"),
   tags$br(),
   tags$br(),


    tags$h4("Heatmaps & PCA Plots", style="color: #33b7b2;"),
    "Both fixed and interactive Heatmap and PCA options are available. Interactive plots allow identifying potential outliers by hovering on the graph to identify sample labels. Color Schemes of figures are available in sidebar (groups color selection is not available for the interactive heatmap, yet).",
  img(src= "heatmapPCA.png", width =800, style="margin:10px 50px"),
  tags$br(),


   tags$h4("Correlation Plots", style="color: #33b7b2;"),
    "Correlation analysis of parameters can be assessed with parametric Pearson correlation or non-parametric Spearman correlation. Individual correlation graphs display a scatter plot between two selected parameters. Tables of correlation (r) values and statistical significance test are available for users to view and download.",
  "The sliding bar can be used to highlight correlations above the selected threshold in the table.",
  img(src="corr.png", width =1000, style="margin:10px 0px"),
  tags$br(),


  tags$h4("ROC Curves", style="color: #33b7b2;"),
  "Receiver Operating Characteristics: True/False positive rates of selected parameters can be viewed on the ROC curve table.",

  img(src="roc.png",width =1000, style="margin:10px 0px"),
  tags$br(),
  hr(),

  tags$h4("Terms of Use", style = "color: #33b7b2;"),
  "This program is a free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License.",
  tags$br(),
  "This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.",
  tags$br(),
  "See the GNU General Public License for more details at :", a("GNU General Public License", href="https://www.gnu.org/licenses/", style = "color: steelblue;"),

  tags$br(),
  "Uploaded data and analysis results will not be saved on our servers. However, the application is provided", '" as is "', "without warranty of any kind, and thus, you must refrain from uploading any confidential data. You may instead download the code and run the app locally on your private computer and network. We are not responsible for the confidentiality, availability, security, loss, misuse or misappropriation of any data you submit to this application.", style = "color: black;",

  tags$br(),
  hr(),
      
#version      
  tags$h4("Copyright 2022, Center of Research in Transplantation and Immunology, Version 2022.01.04", align="center"),

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

####END####
