################################################################################
# body.R
# 
# Create the body for the ui.
################################################################################
body <- dashboardBody(
  
  tabItems(
    
    # **************************************************************************
    # 1) First tab content. ----------------------------------------------------
    # **************************************************************************
    
    tabItem(
      tabName = "seagrass",
      fluidRow(
        column(16, 
               uiOutput("loadingContent"),
        )
      )
    ),
    
    # **************************************************************************
    # 2) Second tab content. ---------------------------------------------------
    # **************************************************************************
    
    tabItem(
      tabName = "model",
      
      # Adding empty space before the title
      tags$br(),
      
      # Adding title at the top of the tab
      h1(style = "color: #333333;", "Seagrass Dynamic Bayesian Network Model"),
      
      fluidRow(
        
        column(
          width = 5, style = "height: 100px;",
          
          ## Seagrass DBN model. -----------------------------------------------
          box(
            selectInput("image_choice", "Choose an image to display:", 
                        choices = c("Overall DBN model" = "DBN.png",
                                    "Resistance subnetwork" = "Resistance.png",
                                    "Recovery subnetwork" = "Recovery.png",
                                    "Shoot Density subnetwork" = "ShootDensity.png"),
                        multiple = FALSE, selected = "Overall DBN model")
          ),
          
          tags$br(),
          
          ## Legend box. -------------------------------------------------------
          box(
            style = "height: 307px;",
            title = "Seagrass DBN Model",
            tags$div(
              style = "font-family: Arial, sans-serif; line-height: 1.6; margin: 15px; padding: 10px; border: 1px solid #ccc; border-radius: 5px; background-color: #f9f9f9;",
              tags$div(
                "\u2192 = Parent-child relationship"
              ),
              tags$div(
                tags$div(style = "display: inline-block; width: 20px; height: 20px; background-color: #fff; border: 1px solid #000; border-radius: 50%;", ""),
                " = Nodes"
              ),
              tags$div(
                tags$div(style = "display: inline-block; width: 20px; height: 20px; background-color: #fff; border: 1px solid #000; border-radius: 5px;", ""),
                " = Subnetworks"
              ),
              tags$br(),
              p(style = "color: #666;", "Node colors:"),
              tags$ul(
                tags$li(tags$div(style = "display: inline-block; width: 20px; height: 20px; background-color: white; border: 1px solid #000;", ""), " = Site condition"),
                tags$li(tags$div(style = "display: inline-block; width: 20px; height: 20px; background-color: #CC99FF; border: 1px solid #000;", ""), " = Recovery"),
                tags$li(tags$div(style = "display: inline-block; width: 20px; height: 20px; background-color: #00FF99; border: 1px solid #000;", ""), " = Resistance"),
                tags$li(tags$div(style = "display: inline-block; width: 20px; height: 20px; background-color: #99CCFF; border: 1px solid #000;", ""), " = Environment"),
                tags$li(tags$div(style = "display: inline-block; width: 20px; height: 20px; background-color: #FFFF99; border: 1px solid #000;", ""), " = Biomass")
              )
            )
          )
        ),
          
        ## Image display. ------------------------------------------------------
        box( 
          #uiOutput("images"),
          div(style = "height: 500px;", uiOutput("images")),
          width = 11
          ),
        
        # Empty space
        column(
          width = 16,
          div(style = "height: 20px;")
        ),
        
        ## Table display. ------------------------------------------------------
        box(
          DTOutput("table"),
          width = 16
        )
        
      )
    ),
    
    # **************************************************************************
    # 3) Third tab content. ----------------------------------------------------
    # **************************************************************************
    
    ## Gladstone map. ----------------------------------------------------------
    tabItem(
      tabName = "zostera",
      fluidRow(
        column(16,
          box(
            style = "height: 400px;",
            title = "Gladstone Case Study",
            leafletOutput("mymap", height = 400)  
          )
        )
      ),
      
      ## Text. -----------------------------------------------------------------
      fluidRow(
        column(16,
          uiOutput("dynamicContent"),
          tags$br(),
          div(
            style = "text-align: right;",
            actionButton("prevpage", "Previous", style = "width: 100px; margin-right: 10px;"),
            actionButton("nextpage", "Next", style = "width: 100px;")
          )
        )
      )
      
    ),
    
    # **************************************************************************
    # 4) Fourth tab content. ---------------------------------------------------
    # **************************************************************************
    
    tabItem(
      tabName = "examples",
      
      # Adding empty space before the title
      tags$br(),

      # Adding title above the table
      h1(style = "color: #333333;", "Preset Cases: Examples of Light & Heat Events"),
      
      fluidRow(
        
        ## Scenarios and Events. -----------------------------------------------
        column(6,
               
               # Scenario Selection
               wellPanel(
                 selectInput("exscenario", "Choose Scenario:", 
                             choices = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5", 
                                         "Scenario 6", "Scenario 7", "Scenario 8", "Scenario 9", "Scenario 10"))
               ),
               
               # Light Event Box
               box(
                 title = 'Light Event', status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                 selectInput("exdredgestart", "Start:", 
                             choices = c("None" = 0, "Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6, 
                                         "Jul" = 7, "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12), 
                             selected = "None"),
                 sliderInput("exdredgeduration", "Duration:", min = 0, max = 10, value = 0),
                 numericInput("exdredgelight", "Optimal Light:", value = 1, min = 0, max = 1, step = 1),
                 tags$br(),
                 actionButton("toggleLightInfo", "Help?"),
                 hidden(
                   div(id = "lightInfo",
                       HTML("<ul>
                    <li><b>Start:</b> Event Start Month (1-12, Jan-Dec)</li>
                    <li><b>Duration:</b> Event Duration in Months</li>
                    <li><b>Optimal Light:</b> Light Intensity (1, optimal light)</li>
                   </ul>")
                   )
                 )
               ),
               
               # Heat Event Box
               box(
                 title = 'Heat Event', status = 'primary', solidHeader = TRUE, collapsible = TRUE,
                 selectInput("exheatstart", "Start:", 
                             choices = c("None" = 0, "Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6, 
                                         "Jul" = 7, "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12), 
                             selected = "None"),
                 sliderInput("exheatduration", "Duration:", min = 0, max = 10, value = 3),
                 numericInput("exheatstress", "No Effec:", value = 1, min = 0, max = 1, step = 1),
                 tags$br(),
                 actionButton("toggleHeatInfo", "Help?"),
                 hidden(
                   div(id = "heatInfo",
                       HTML("<ul>
                    <li><b>Start:</b> Event Start Month (1-12, Jan-Dec)</li>
                    <li><b>Duration:</b> Event Duration in Months</li>
                    <li><b>No Effect:</b> No Effect Intensity (1, no heat stress)</li>
                   </ul>")
                   )
                 )
               )
               
        ),
        
        ## Image and Resilience Metrics Table. ---------------------------------
        column(10,
               
               box(
                 title = "Scenario selected", status = 'primary', solidHeader = TRUE, width = 12,
                 uiOutput("dynamicText")
               ),
               # Image Box
               box(
                 title = "Scenario Image", status = 'primary', solidHeader = TRUE, width = 12,
                 div(imageOutput("scenario_img")
                     ),
                 p(HTML("<b>Figure:</b> The model predicted-state probabilities for realised shoot density for <i>Zostera muelleri</i> located at Gladstone, Australia.
                    The initial 24 months are used for initialisation to allow the system to enter the baseline pattern. 
                    The top plots are the probability of each realised shoot density state, and the bottom plots show the weighted mean of the expected value and the interquartile range."),
                    style = "margin-top: 10px; font-size: 18px;")
                 ),
               
               # Resilience Metrics Table Box
               box(
                 title = "Resilience Metrics", status = 'primary', solidHeader = TRUE, width = 12,
                 tableOutput("resilience_table"),
                 HTML("<ol>
                 <li>Resistance to stress, measured as a ratio (ideal = 1)</li>
                 <li>Recovery from stress, measured in months (ideal = 0)</li>
                 <li>Persistence or probability of local extinction, measured as a ratio (ideal = 1)</li>
                 <ol>")
               )
        )
      )
    ),
    
    # **************************************************************************
    # 5) Fifth tab content. ---------------------------------------------------
    # **************************************************************************
    tabItem(
      tabName = "events",
      
      # Adding empty space before the title
      tags$br(),
      
      # Adding title at the top of the tab
      h1(style = "color: #333333;", "Short-Term Impact: Light & Heat Disturbance Event"),
      
      # Heat and Light Events. -------------------------------------------------
      fluidRow(
        column(8,
               box(title = "Light Event",
                   selectInput("dredgestart", "Start:", 
                               choices = c("None" = 0, "Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6, 
                                           "Jul" = 7, "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12), 
                               selected = "None"),
                   sliderInput("dredgeduration", "Duration:", min = 0, max = 12, value = 0, step = 1),
                   tags$div(style = "margin-top: 20px;"),
                   numericInput("dredgelight", "Optimal Light:", value = 1, min = 0, max = 1, step = 1)
               )
        ),
        column(8,
               box(title = "Heat Event",
                   selectInput("heatstart", "Start:", 
                               choices = c("None" = 0, "Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6, 
                                           "Jul" = 7, "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12), 
                               selected = "None"),
                   sliderInput("heatduration", "Duration:", min = 0, max = 12, value = 0, step = 1),
                   tags$div(style = "margin-top: 20px;"),
                   numericInput("heatstress", "No Effect:", value = 1, min = 0, max = 1, step = 1)
               )
        )
      ),
      
      ## Variable Selection and Resilience Metrics. ----------------------------
      fluidRow(
        column(8,
               box(title = "Variable Selection",
                   selectInput("variable", "Select Variables (up to 4):",
                               choices = c("Sediment_Quality", "Accumulated_Light", "Accumulated_Burial",
                                           "Salinity", "Temperature", "Physiological_Status_of_Plants",
                                           "Immigrant_Seed_Density", "Immigrant_Seed_Quality", "Baseline_Shoot_Density",
                                           "Above_to_Below_Ground_Biomass_Ratio", "Ability_to_Resist_Hazard",
                                           "Loss_in_Shoot_Density", "Seed_Density", "Seed_Quality",
                                           "Recruitment_Rate_from_Seeds", "Immigrant_Vegetative_Fragments",
                                           "Lateral_Growth_from_Existing_Individuals", "Overall_Lateral_Growth",
                                           "Rate_of_Recovery_in_Shoot_Density", "Net_Change_Shoot_Density",
                                           "Realised_Shoot_Density", "Ability_to_Recover", "Heat_Stress"),
                               multiple = TRUE, selected = "Realised_Shoot_Density"),
                   tags$div(style = "display: flex; justify-content: flex-end;",
                            actionButton("submit", "Plot Graph", class = "btn btn-primary btn-custom"))
                   )
               ),
        column(8,
               box(
                 title = "Resilience metrics",
                 tableOutput("results_table"), 
                 HTML("<ol>
                 <li>Resistance: Baseline = 1</li>
                 <li>Recovery: Baseline = 0</li>
                 <li>Persistence: Baseline = 1</li>
                 </ol>")
                 )
               )
        ),
      
      ## Graphs and Title. -----------------------------------------------------
      fluidRow(
        column(16,
          box(
            uiOutput("dynamic_title"),
            tags$div(style = "margin-top: 10px;"),

            actionButton("helpButton", label = "", icon = icon("question")),
            
            tags$br(),
            
            conditionalPanel(
              condition = "input.helpButton % 2 == 1",
              tags$div(
                tags$ul(
                  tags$li("The initial 24 months are used for initialisation to allow the system to enter the baseline pattern."),
                  tags$li("Top plots are the probability of each node state."),
                  tags$li("The bottom plots show the weighted mean of the expected value and the interquartile range.")
                )
              )
            ),
            
            useShinyjs(),  
            div(id = "loading1", style = "display: none; text-align: center; position: absolute; top: calc(50% - 45px); left: 50%; transform: translate(-50%, 0);",
                img(src = "turtle.gif", style = "width: 100px; height: 100px; position: relative;")
            ),
            div(id = "loading2", style = "display: none; text-align: center; position: absolute; top: calc(50% + 15px); left: 50%; transform: translate(-50%, 0);",
                img(src = "loading.gif", style = "width: 100px; height: 100px; position: relative;")
            ),
            tags$div(style = "text-align:center;",
                     uiOutput("dynamic_plot")
           )
          )
        )
      )
      
    ),
    
    # **************************************************************************
    # 6) Sixth tab content. ----------------------------------------------------
    # **************************************************************************
    tabItem(
      tabName = "scenarios",
      
      shinyjs::useShinyjs(),
      
      # Adding empty space before the title
      tags$br(),
      
      # Adding title above the table
      h1(style = "color: #333333;", "Long-Term Impact: Projected Temperature Effect on Seagrass"),
      
      ## Main Variable and Decade. ---------------------------------------------
      fluidRow(
        column(6,
               box(
                 title = "Variable",
                   selectInput("main_variable", "Select Variable:",
                               choices = c("None selected" = "", "Sediment_Quality", "Accumulated_Light", "Accumulated_Burial",
                                           "Salinity", "Temperature", "Physiological_Status_of_Plants",
                                           "Immigrant_Seed_Density", "Immigrant_Seed_Quality", "Baseline_Shoot_Density",
                                           "Above_to_Below_Ground_Biomass_Ratio", "Ability_to_Resist_Hazard",
                                           "Loss_in_Shoot_Density", "Seed_Density", "Seed_Quality",
                                           "Recruitment_Rate_from_Seeds", "Immigrant_Vegetative_Fragments",
                                           "Lateral_Growth_from_Existing_Individuals", "Overall_Lateral_Growth",
                                           "Rate_of_Recovery_in_Shoot_Density", "Net_Change_Shoot_Density",
                                           "Realised_Shoot_Density", "Ability_to_Recover", "Heat_Stress"),
                               multiple = TRUE, selected = "Realised_Shoot_Density")
                 )
               ),
        column(5,
               box(
                 title = "Decade",
                 selectInput("main_decade", "Select Decade:",
                             choices = c("None selected" = "", "2030s", "2040s", "2050s", "2060s", "2070s", "2080s", "2090s"),
                             multiple = FALSE, selected = "2030s")
                 )
               ),
        column(5,
               box(
                 title = "Index",
                 selectInput("selected_index", "Choose an Index:",
                             choices = 1:100,
                             multiple = FALSE, selected = 1)
               )
        )
      ),
      
      ## Plot Output --------------------------------------------------------------
      fluidRow(
        column(8,
               box(
                 title = "Scenario-1",
                 selectInput("sspscenario1", "Select SSP for Plot-1:",
                             choices = c("None selected" = "", "ssp119", "ssp126", "ssp370", "ssp585"),
                             multiple = FALSE, selected = NULL)
               )
        ),
        
        column(8,
               box(
                 title = "Scenario-2",
                 selectInput("sspscenario2", "Select SSP for Plot-2:",
                             choices = c("None selected" = "", "ssp119", "ssp126", "ssp370", "ssp585"),
                             multiple = FALSE, selected = NULL)
               )
        )
      ),
      
      fluidRow(
        column(8,  
               box(
                 title = "Plot-1",
                 uiOutput("dynamic_title1"),
                 tags$div(style = "margin-top: 10px;"),
                 actionButton("helpButton1", label = "", icon = icon("question")),
                 tags$br(),
                 conditionalPanel(
                   condition = "input.helpButton1 % 2 == 1",
                   tags$div(
                     tags$ul(
                       tags$li("The initial 24 months are used for initialisation to allow the system to enter the baseline pattern."),
                       tags$li("Top plots are the probability of each node state."),
                       tags$li("The bottom plots show the weighted mean of the expected value and the interquartile range.")
                     )
                   )
                 ),
                 tags$div(style = "text-align:center;",
                          plotOutput("plot1")
                 )
               )
        ),
        column(8,  
               box(
                 title = "Plot-2",
                 uiOutput("dynamic_title2"),
                 tags$div(style = "margin-top: 10px;"),
                 actionButton("helpButton2", label = "", icon = icon("question")),
                 tags$br(),
                 conditionalPanel(
                   condition = "input.helpButton2 % 2 == 1",
                   tags$div(
                     tags$ul(
                       tags$li("The initial 24 months are used for initialisation to allow the system to enter the baseline pattern."),
                       tags$li("Top plots are the probability of each node state."),
                       tags$li("The bottom plots show the weighted mean of the expected value and the interquartile range.")
                     )
                   )
                 ),
                 plotOutput("plot2")
               )
        )
      ),
      
      fluidRow(
        column(16,  
               box(
                 title = "High Shoot Density Ratio",
                 plotOutput("highratio_plot")
                 )
               ),
        column(16,
               box(
                 title = "Zero Shoot Density Ratio",
                 plotOutput("zeroratio_plot")
                 )
               )
        )

    )
    
  )
)

