# ====== import R libraries: ####
library(shiny)            #For shiny
library(shinythemes)      #For graphics of shiny interface
library(shinycssloaders)  #For the spinner of load during the computing of the functions
library(shinyBS)          #For tooltips, popovers and alerts
library(shinyWidgets)     #For some shiny functions
library(ggplot2)          #Plot graphs
library(plotly)           #Plot interactives graphs
library(gridExtra)        #Grid display
library(grid)             #Grid display
library(gplots)           #For colorpanels in the heatmap
library(ComplexHeatmap)   #For  heatmaps
library(circlize)         #For  heatmaps
library(heatmaply)        #For interactive heatmaps
library(corrplot)         #For correlogram
#library(ggcorrplot)       #For interactive correlogram
library(data.table)       #For the import table (fread)
library(DT)               #For datatabkes functions
if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
  if(!require("impute"))
    BiocManager::install("impute")
}
library(impute)           #For knn on the heatmap when there is NA values
library(FSA)              #For some tests (DunnTest)
library(RColorBrewer)     #For the color palette
library(pROC)             #For ROC Curves
if (!require("devtools"))
  install.packages("devtools")
if (!require("svglite"))
  devtools::install_github("r-lib/svglite")
library(svglite)          #For svg images


# ====== files Sources (which are in corresponding tab) ####
source("./app/general/general.R")
source("./app/tabs/about/about.R")
source("./app/tabs/about/insructions.R")
source("./app/tabs/tableDescr/tableDescr.R")
source("./app/tabs/batchPlot/batchPlot.R")
source("./app/tabs/heatmapPca/heatmapPca.R")
source("./app/tabs/correlation/correlation.R")
source("./app/tabs/ROCCurves/ROCCurves.R")


# === Define UI for application ####
ui <- fluidPage(theme = shinytheme("united"),

# titlePanel ####
                titlePanel(" ",windowTitle = "FaDA"),
                tags$head(
                  tags$style(
                    HTML("@import url('//fonts.googleapis.com/css?family=Righteous|');"),
                    HTML(".shiny-output-error-validation {
                                        color: red;
                          }")
                  )
                ),

                headerPanel(                              ### add of FaDA and CRTI logos 
                  div(img(src= "IconOct19.png", class = "pull-left"),       
                      div(img(src= "logo.png", class = "pull-right")) )   ),

                
# sidebarPanel (sidebar with conditional panels, to hide unecessary options, depending of the selected tabset): ####
                sidebarPanel( width = 4,# Input: Select a file
                              conditionalPanel(
                                condition="input.TestTable | input.tabs != 'about' & input.tabs != 'Tutorial'",
                                               generalTableHead() ),
                                                readTableUI(), #Defined in the general.R
                              conditionalPanel(condition = "input.tabs != 'about' & input.tabs != 'Tutorial'  ",
                                          generalDataOptionUI() ),
                              conditionalPanel(condition = "input.tabs == 'Tutorial'",
                                               TutorialOptionUI() ),
                              conditionalPanel(condition = "input.tabs == 'tableDescr' ",
                                               TestParametricOptionUI() ),
                              conditionalPanel(condition = "input.tabs == 'tableDescr' ",
                                               pValSidebarUI(), SelDigitsSidebarUI()), #Defined in the tableDescr.R
                              conditionalPanel(condition = "input.tabs == 'correlation'",
                                               corrPlotOptionUI(), corrSidebarUI(),  #to select threshold for highlighting correlation values
                                          pValCorrSidebarUI()), #to select threshold for highlighting pvalues
                              conditionalPanel(condition = "input.tabs == 'Heatmap_PCA' ",
                                               HeatmapOptionUI() ),
                              conditionalPanel(condition = "input.tabs == 'rocCurves' ")

                              ),

# mainPanel ####
                mainPanel(
                  tabsetPanel(type = "pills", id = 'tabs',
                              
   ### The following functions are defined in the corresponding R file:
                        #About###
                                tabPanel("About", icon = icon("eye"), value = 'about',
                                                  #tags$style(".well {background-color:red;}"),
                                                  aboutUI()#Defined in the about.R
                                               ),
                        #Tutorial###
                              tabPanel("Tutorial",icon = icon("chart-line"), value = 'Tutorial',
                                       introductionUI()),

                        #Table 2 panels###
                             tabPanel("Data Analysis", icon = icon("table"), value = 'tableDescr',
                                      tags$br(),
                                       tabsetPanel(type = "pills", id = 'Table2panels',
                                                   tabPanel("Analysis", value = "Analysis",
                                                            tableDescrMainUI()), #Defined in the tableDescr.R
                                                   tabPanel("Grouped plots", icon = icon("signal"),
                                                            value = "batchPlot",
                                                            batchPlotUI() ) #Defined in the tableDescr.R
                                       )  ),
                        #Heatmap & PCA###
                             tabPanel("Heatmap & PCA", icon = icon("signal"), value = "Heatmap_PCA",
                                      tags$br(),
                              tabsetPanel(type = "pills", id = 'Heatmap_PCA', selected = "FixedHeatmap",
                                          tabPanel(title= "Heatmap & PCA",  value = "FixedHeatmap",
                                                   FixedHeatmap_UI(), tags$br(), FixedPCA_UI() ), #Defined in the heatmapPca.R
                                          tabPanel(title= "Interactive visualisations",  value = "iHeatmap",                                                                    heatmapPcaUI(),tags$br(),PCA_UI()
                                                   ) #Defined in the heatmapPca.R
                                      )),
                          #Correlation###
                                      tabPanel("Correlation", icon = icon("chart-line"), value = 'correlation',
                                               tags$br(),
                                        tabsetPanel(type = "pills",id = 'corrTabs',
                                                  tabPanel("Correlogram", value = "correlation_correlogram",
                                                              correlogramUI()), #Defined in the correlation.R
                                                  tabPanel("Correlation Graphs", value = "correlation_corrGraph",
                                                                    corrGraphUI())
                                                          )
                                               ),

                            #ROC###
                                      tabPanel("ROC curves", icon = icon("chart-line"), value = 'rocCurves',
                                                  ROCCurvesUI() #Defined in the ROCCurves.R
                                               )  )
                          )
                )



######
# =======Define server
server <- function(input, output, session) {

# ======Call Server Functions (reactive functions) ####
# General:
  reacUsedTable <- usedTable(input,session)               #reacUsedTable is the input table (formated) uploaded by the user
  reacUsedGroups <- usedGroups(input,reacUsedTable)       #reacUsedGroups is the group name vector taken from usedTable
  reacCalcTable <- calcTable(input,reacUsedTable)         # reacusedTable recalculated in log if log option is selected, without the group column
  reacColorFunction <- colorFunction(input,reacUsedTable) #selected colors
  reacNameTable <- nameTable(input)                       #parameter names from uploaded table
  observeGeneral(input,session,reacUsedTable)

# About:
  Text1 <- about()
  observeUploadFileToChangeTabs(input,session)
  Textintro <- introduction()

# Description Table : summary and p-values (Tab 1)
  reacMytableDescr <- MytableDescr(input,reacUsedGroups,reacCalcTable)      #descriptive table (using groups (reacUsedGroups) and calculated Table (reacCalcTable) as input)
  reacMytableStat <- MytableStat(input,reacUsedGroups,reacCalcTable)        #statistic table  (using groups (reacUsedGroups) and calculated Table (reacCalcTable) as input)
  reacPlotDescr <- plotDescr(input,reacUsedGroups,reacCalcTable,reacColorFunction,reacMytableStat)       #individual plot of selected parameters

# Batch Plots (Tab 2):
  reacBatchPlot <- batchPlot(input,reacUsedGroups,reacCalcTable,reacColorFunction)          #plot of all parameters
  reacMoreBatchPlot <- MoreBatchPlot(input,reacUsedGroups,reacCalcTable,reacColorFunction)  #plot of all parameters, to download if too many parameters

# Heatmap and PCA (Tab 3):
  reacFixedHeatmap <- FixedHeatmap(input,reacUsedGroups,reacCalcTable,reacColorFunction)    #static heatmap (using groups (reacUsedGroups), calculated Table (reacCalcTable), colors (reacColorFunction) as input)
  reacHeatmap <- heatMap(input,reacUsedGroups,reacCalcTable,reacColorFunction)              #interactive heatmap 
  reacACP <- ACP(input,reacUsedGroups,reacCalcTable,reacColorFunction)                      #interactive PCA
  reacFixedPCA <- FixedPCA(input,reacUsedGroups,reacCalcTable,reacColorFunction)            #static PCA

# Correlation Table (Tab 4):
  reacCorrelogram <- correlogram(input,reacCalcTable)                                       #correlogram
  reacCorrTable <- corrTable(input,reacCalcTable)                                           #correlation table
  reacPvalCorrTable <- pvalCorrTable(input,reacCalcTable)                                   #p-values of correlation
  reacCorrGraph <- corrGraph(input,reacCalcTable)                                           #individual correlation graph

# ROC Curve (Tab 5):
  reacROCPlot <- ROCPlot(input,reacUsedTable,reacColorFunction, reacCalcTable)              #ROC curve
  reacAUCTable <- AUCTable(input,reacUsedTable, reacCalcTable)                              #table of ROC values (AUCs, Youden index, NPV, PPV...)

# ====== Output ####
# General:
  generalOutput(input,output,reacUsedTable,reacNameTable)

# About:
  aboutOutput(output,Text1)
  introductionOutput(output, Textintro)

# Page 1 (summary and p-values):
  tableDescrOutput(output,reacMytableStat,reacMytableDescr,reacPlotDescr,reacNameTable)    #Description table + stat table + individual plots
  batchPlotOutput(output,reacBatchPlot)                                                    #"batch" plots with all parameters in a single sub-tabset

# Page 3 (Heatmap and PCA):
  heatmapOutput(output,reacHeatmap,reacACP,reacFixedHeatmap,reacFixedPCA)

# Page 4 (Correlation):
  correlogramOutput(output,reacCorrTable,reacPvalCorrTable,reacCorrelogram,reacNameTable)   #main correlation page with tables and correlogram
  corrGraphOutput(output,reacCalcTable,reacCorrGraph,reacNameTable,reacCorrTable)           #sub-tabset with individual pairs of variables

# Page 5 (ROC Curve):
  ROCCurvesOutput(output,reacCalcTable,reacROCPlot,reacAUCTable,reacUsedGroups,
                  reacNameTable)

# =========== Download Graphs ####
  extableDownload(output,session)
  FixedheatmapDownload(input,output,reacFixedHeatmap)
  FixedPCADownload(input,output,reacFixedPCA)
  batchPlotDownload(input,output, reacUsedTable, reacBatchPlot, reacMoreBatchPlot)
  ROCDownload(input,output,reacROCPlot)
  }

###
# Run the application
shinyApp(ui = ui, server = server)
###End
