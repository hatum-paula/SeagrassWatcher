################################################################################
# sidebar.R
# 
# Create the sidebar menu options for the ui.
################################################################################

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Seagrass Tool", tabName = "seagrass", selected = TRUE, icon = icon("seedling", style="font-size: 20px; color: #33CC99;")),
    menuItem("DBN Model", tabName = "model", icon = icon("cogs", style="font-size: 20px; color: #FF99CC;")),
    menuItem("Case Study", tabName = "zostera", icon = icon("map", style="font-size: 20px; color: #99CCFF;")),
    menuItem("Preset Cases", tabName = "examples", icon = icon("book", style="font-size: 20px; color: #99CCCC;")),
    menuItem("Light & Heat", tabName = "events", icon = icon("sun", style="font-size: 20px; color: #FFCC66;")),
    menuItem("SSP-Scenario", tabName = "scenarios", icon = icon("options", style="font-size: 20px; color: #FF9999;"))
  )
)


