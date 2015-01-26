#This code checks the user's installed packages against a list of packages (that we've manually compiled) 
#   necessary for the graphs to be rendered. Missing packages are installed, while existing packages are not.
#   If anyone sees a package that should be on there, please tell me.
rm(list=ls(all=TRUE)) #Clear the memory for any variables set from any previous runs.

packagesToInstall <- c(
   "arm" # for working with model objects, by Gelman & Hill
  , "colorspace" #Explicit control over the HCL color scheme
  , "devtools" #package development
  , "dplyr" # for general data manipulation
  , "ggplot2" #Graphing
  , "ggthemes" #Extra themes, scales and geoms for ggplot
  , "ggmap" #Maps & graphics, based on ggplot
  , "googleVis" #JavaScript-based visualizations, like scrollable tables
  , "grid" #The underlying framework for the graphics 
  , "gridExtra" #for table FERE graphs
  , "gridBase" #Additional grid functions
  , "knitr" #For creating dynamic reports
  , "lme4" # used for Random Coefficient Modeling  
  , "lubridate" #Consistent/convienent function signatures for manipulating dates
  , "minqa"
  , "nlme" # used for Fixed Effect modeling  
  , "plyr" #Important for most of our data manipulation
  , "psych" # data summary + other useful function
  , "RColorBrewer" #Explicit control over the Color Brewer colors.  See http://colorbrewer2.org/
  , "reshape2" #Data manipulation not covered in plyr,  wide <-> long
  , "RJSONIO" # for processing jason files
  , "roxygen2" #Creates documentation Rd file from (well-formed) comments
  , "RODBC"
  , "sas7bdat" # Imports SAS formatted data files
  , "stringr"
  , "testit" #has the useful `assert()` function
  , "testthat" #Heavier testing framework that's good for package development
  , "yaml" #for gh-pages production

) 





for( packageName in packagesToInstall ) {
  available <- require(packageName, character.only=TRUE) #Loads the packages, and indicates if it's available
  if( !available ) {
    install.packages(packageName, dependencies=TRUE)
    require( packageName, character.only=TRUE)
  }
}

update.packages(ask="graphics", checkBuilt=TRUE)

#There will be a warning message for every  package that's called but not installed.  It will look like:
#    Warning message:
#        In library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
#        there is no package called 'bootstrap'
#If you see the message (either in here or in another piece of the project's code),
#   then run this again to make sure everything is installed.  You shouldn't get a warning again.