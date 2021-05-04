# FaDA Shiny application - FaDA: A Shiny web application to accelerate common lab data analyses

FaDA application provides a free and intuitive interface allowing biologists without bioinformatic skills to easily and quickly perform common lab data analyses.
The application is freely accessible at https://shiny-bird.univ-nantes.fr/app/Fada


##  FaDA’s Development Documentation
The sources are in the folder: inst/fada_app/app.

#### UI Structure:
FaDA transforms the data furnished by the user into different statistical analyses.
The web UI of FaDA is into two major parts :
·	Sidebar panel
·	Main panel
The main panel is subdivided into 6 tabs:
o	About
o	Description Table
o	Batch Plots
o	Correlations
o	Heatmap and PCA
o	ROC Curves
	Each tab is associated with a folder containing the corresponding R code.
The sidebar panel is always visible and the content of the sidebar panel depends on the main panel view.

#### Code Structure
The app.R is the main R code file, all the other R file are called here.
There is two main functions defined: ui and server
- The ui (handle user input, server output and the display of the ui) and the server (process the ui input to calculate output) communicate via keywords associated to each input and output function declared in the ui function.
The ui part of shiny is divided inside the app :
·	app.R : It contains the main ui function that call all the other ui functions and set up the structure of the ui (sidebar panel/main panel)
·	app/general/general.R : It’s the general sidebar ui panel which is general for all the tabs
·	app/tabs/*.R : Each tabs have at least one ui function

- The Server Function :
The server part of Shiny is also divided inside the app, like the division of the ui part:
General Structure of the Source Code :
A tab source code is only a group of functions called in app.R :
	UI : functions that handle the ui part of the tab (input and output)
	Server Functions : functions that handle the input from ui and process the output within a reactive value. These functions return a reactive value output.
	Independent Functions : each function of server functions is associated to a independent to shiny function. These functions calculate the output of the server with the input.
	Output to UI : functions that handle the output to the UI
	Download : functions that handle the download part

app.R :
 
UI fonctions from tabs source file are called here (sidebar and mainpanel).
Some are displayed with a condition (conditionalPanel)

 
Server functions are called here (not independents functions). There isn’t any order priority, it connect the servers functions in tabs R file with the shiny app.
Fast explanation of some reac functions :
	- reacUsedTable is the input table (formated) provided by the user.
	- reacUsedGroups is the group name vector taken from usedTable
	- reacCalcTable is the usedTable recalculated in log or not (if log option is selected) without the group column
	- observeGeneral and observeUpload is observer that add an alert action in shiny if certain condition are completed (no return value)
	- about return the all the content in about tab
	- reacMytableDescr return a fast stats analysis (mean, sd, median, ... )
	- reacMyTableStat return the p-value between groups
	- reacPlotDescr return a plot (whisker, points, bar, ... ) of the tableDescr content of one parameter (gene)
	- reacBatchPlot return all the plotDescr of all parameters
 	- ....
  
## How to modify the app :
Independent function or UI function in a specific tab can be modified in the  corresponding R file (to modify the sidebar, to create a sidebar function and/or to put it into the ui in app.R).
To add a function that needs input, a function as to be created in the corresponding tab, which returned a reactive value. Then, the reactive value has to be called in the app.R ui and in app.R output part (server). 

