# FaDA Shiny application - FaDA: A web application for basic wet-laboratory data analyses

FaDA application provides a free and intuitive interface allowing biologists without bioinformatic skills to easily and quickly perform common lab data analyses.
The application is freely accessible at https://shiny-bird.univ-nantes.fr/app/Fada


##  FaDA’s Development Documentation
Source files are in the folder: inst/fada_app/app

#### UI Structure:
FaDA perform different statistical analyses from the uploaded table.
The web UI of FaDA is composed of two major parts :
·	the sidebar panel
·	the main panel

The main panel is subdivided into 6 tabs:
o	About
o	Description Table
o	Batch Plots
o	Correlations
o	Heatmap and PCA
o	ROC Curves

Each tab is associated with a folder containing the corresponding R code. The sidebar panel is always visible and the content of the sidebar panel depends on the main panel view (conditional tab).

#### General Code Structure
The 'app.R' file is the main R file; all the others R files are called here.
There is two main functions defined: ui and server:
- The ui (handle user input, server output and the display of the ui) and the server (process the ui input to calculate output) communicate via keywords associated to each input and output function declared in the ui function.
The ui part of shiny is divided inside the app:
·	app.R: contains the main ui function that call the other ui functions and set up the structure of the ui (sidebar panel/main panel),
·	app/general/general.R: it contains the general sidebar ui panel which is general for all the tabs,
·	app/tabs/*.R: Each tabs have at least one ui function

- The Server Function:
The server part of Shiny is also divided inside the app, like the division of the ui part.
A tab source code is a group of functions called in app.R:
·	UI: functions that handle the ui part of the tab (input and output)
·	Server functions: functions that handle the input from ui and process the output within a reactive value. These functions return a reactive value output.
·	Independent functions: each server function is associated to an independent to shiny function. These functions calculate the output of the server with the input.
·	Output to UI: functions that handle the output to the UI
·	Download: functions that handle the download part

- 'app.R' file:
UI fonctions from tabs source file are called here (sidebar and mainpanel). Some are displayed with a condition (conditionalPanel)
Server functions are called here (not independents functions). There is no order priority, it connects the servers functions in tabs R file with the shiny app.
Fast explanation of some reactive functions ('reac...') :
	- _reacUsedTable_ is the input table (formated) provided by the user
	- _reacUsedGroups_ is the group name vector taken from usedTable
	- _reacCalcTable_ is the usedTable recalculated in log or not (if log option is selected) without the group column
	- _observeGeneral_ and _observeUpload_ is observer that add an alert action in shiny if certain condition are completed (no return value)
	- _about_ return the all the content in about tab
	- _reacMytableDescr_ return a fast stats analysis (mean, sd, median, ... )
	- _reacMyTableStat_ return the p-value between groups
	- _reacPlotDescr_ return a plot (whisker, points, bar, ... ) of the tableDescr content of one parameter (gene)
	- _reacBatchPlot_ return all the plotDescr of all parameters
	
###### Upload data
A simple demonstration dataset is provided, including virtual data from 2 groups with 5 samples each, to explore the features of the web application. Alongside, user can upload and analyse its own dataset, appropriately formatted. Data are uploaded in a text format (tabular-delimited ‘.txt’ or ‘.csv’ file), with a point or a coma as a decimal separator. To allow for flexible use of the application with minimal preparation time, sample identification is in columns or rows. FaDA input only requires unique names for sample identification, and the second row or column is named “Group” to identify sample group labels.
  
## How to modify the app :
Independent function or UI function in a specific tab can be modified in the corresponding R file (to modify the sidebar, to create a sidebar function and/or to put it into the ui in app.R).
To add a function that needs input, a function has to be created in the corresponding tab, which returned a reactive value. Then, the reactive value has to be called in the app.R ui and in app.R output part (server).
