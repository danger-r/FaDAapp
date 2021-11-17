# FaDA Shiny application - FaDA: A web application for regular laboratory data analyses
<img src="https://github.com/danger-r/FaDAapp/blob/master/www/IconOct19.png" width="100" /> 


The FaDA application provides a free and intuitive interface that allows biologists without bioinformatics skills to easily and quickly perform common laboratory data analyses.
The application is freely accessible at https://shiny-bird.univ-nantes.fr/app/Fada


##  FaDA’s Development Documentation
Source files are in the folder: inst/fada_app/app

#### UI Structure:
FaDA performs different statistical analyses from the uploaded table.
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

Each tab is associated with a folder containing the corresponding R code. The sidebar panel is always visible and its content depends on the main panel view (using conditional tabs).

#### General Code Structure
The 'app.R' file is the main R file; all the others R files are called here.
There is two main functions defined: ui and server:
- the ui part handles user input, server output and ui display, 
- the server part processes the ui input to calculate output, communicates via keywords associated to each input and output functions declared in the ui function.

The ui part of shiny is divided inside the app:
·	app.R: contains the main ui function that calls the other ui functions and set up the structure of the ui (sidebar panel/main panel),
·	app/general/general.R: contains the general sidebar ui panel, common for all the tabs,
·	app/tabs/*.R: each tab have at least one ui function

The server part of Shiny is also divided inside the app, like the division of the ui part.
A tab source code is a group of functions called in app.R:
·	UI: functions that handle the ui part of the tab (input and output).
·	Server functions: functions which handles the input from ui and process the output within a reactive value. These functions return a reactive value output.
·	Independent functions: each server function is associated to an independent shiny function. These functions calculate the output of the server from the input.
·	Output to UI: functions that handle the output to the UI.
·	Download: functions which handles the download part.

- 'app.R' file:
UI fonctions from tab source files are called here (sidebar and main panel). Some are displayed with a condition (conditionalPanel).
Server functions are called here (not independents functions). There is no order priority, it connects the servers functions in tabs R file with the R Shiny app.
Explanations of some reactive functions ('reac...') :
	- _reacUsedTable_ is the input table (formated) provided by the user,
	- _reacUsedGroups_ is the group name vector taken from usedTable,
	- _reacCalcTable_ is the usedTable recalculated in log or not (if log option is selected) without the group column,
	- _observeGeneral_ and _observeUpload_ is observers that add an alert action in Shiny according conditions,
	- _about_ returns the content of the about tab,
	- _reacMytableDescr_ returns descriptive statistics (mean, sd, median, ... according selected options),
	- _reacMyTableStat_ returns p-values between groups (according selected options),
	- _reacPlotDescr_ returns a plot (whisker, points, bar, ... ) of the selected parameter from the statistical table,
	- _reacBatchPlot_ returns the plotDescr of all parameters, in a batch.
	
###### Upload data
The sidebar provides a simple demonstration dataset, including virtual data from 2 groups with 5 samples each, to explore the features of the web application. Alongside, user can upload and analyse its own dataset, appropriately formatted. Data are uploaded in a text format (tabular-delimited ‘.txt’ or ‘.csv’ file), with a point or a coma as a decimal separator, with a file limit size of 5 Mo. While FaDA can process files with thousands of values, analysis of such large datasets should be performed using a local version to improve interactivity thanks to the source code available through GitHub.
To allow for flexible use of the application with minimal preparation time, sample identification is in columns or rows. FaDA input only requires unique names for sample identification, and the second row or column is named “Group” to identify sample group labels.
Users can find the Tutorial page explaining how to prepare dataset. Furthermore, the Tutorial page displays explanations of the tools with statistical tests available through FaDA with recommendations.

  
## How to modify the app :
UI or independent functions in a specific tab can be modified in the corresponding R file (to modify the sidebar, to create a sidebar function and/or to put it into the ui in app.R).
To add a function that needs an input, a function has to be created in the corresponding tab, which returned a reactive value. Then, the reactive value has to be called in the app.R ui and in app.R output part (server).


Version 17-nov-2021
