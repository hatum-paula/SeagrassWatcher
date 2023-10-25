################################################################################
# global.R
# 
# Anything you want shared between your ui and server, define here.
################################################################################

# Libraries. -------------------------------------------------------------------
# Core Shiny Libraries for Basic Functionality
library(shiny)              # Core package for building Shiny apps
library(shinydashboard)     # Provides a dashboard layout for Shiny apps
# Additional Shiny Libraries for UI Enhancement
library(shinyjs)            # Allows for the use of JavaScript functions in Shiny
library(shinyWidgets)       # Adds additional widgets like pickers and custom inputs
library(semantic.dashboard) # Alternative dashboard layout based on Semantic UI
library(shinyBS)            # Adds Bootstrap components like modals
library(shinycssloaders)    # Adds loading spinners and other CSS effects
library(bslib)              # Provides Bootstrap utilities for enhanced theming
# Data Handling and Manipulation Libraries
library(dplyr)              # For data manipulation and transformation
library(lubridate)          # For date-time manipulation
# Data Visualization Libraries
library(ggplot2)            # For creating plots
library(leaflet)            # For rendering interactive maps
# Libraries for Data Presentation
library(DT)                 # For rendering data tables
library(gridExtra)          # To arrange multiple plots in a grid
 
# DBN model. -------------------------------------------------------------------
load('highratio.RData')
load('zeroratio.RData')
load('baselines.RData')
load('Seagrass_v9o_ss.RData')
source('dbfive.R')
source('DBFIVEwrapper.R')
source('customiseplot.R')
source('customiseplot2.R')

# Shiny App. -------------------------------------------------------------------
source('./ui.R')
source('./server.R')
shinyApp(ui, server)

# Nodes table. -----------------------------------------------------------------
source("nodes.R")