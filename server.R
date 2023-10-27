################################################################################
# server.R
# 
# For all your server needs 
################################################################################
server <- function(input, output, session) {
  
  # ****************************************************************************
  # 1) First tab content. ------------------------------------------------------
  # ****************************************************************************
  
  output$loadingContent <- renderUI({
    tagList(
      div(
        style = "display: flex; justify-content: center; align-items: center; background-image: url('background3.jpeg'); background-size: cover; background-repeat: no-repeat; width: 100%; height: 100vh;",
        div( 
          style = "text-align:center; margin: 20px; padding: 20px; border: 2px solid #ccc; border-radius: 8px; background-color: white; width: 600px; height: 350px;", # Explicitly set width and height
          ## Landing page. -----------------------------------------------------
          
          tags$img(src = "logo.png", alt = "SeagrassWatcher", height = "80px", width = "200px"),  
          h3("A Tool to Assess Seagrass Response to Disturbance"),
          #p("This application has been developed to bridge the gap between scientific analysis and actionable conservation strategies. By using R Shiny and a probabilistic graphical model, we convert complex data into actionable insights, facilitating informed decision-making."),
          tags$p(id = "special-paragraph", "This application has been developed to bridge the gap between scientific analysis and actionable conservation strategies. By using R Shiny and a probabilistic graphical model, we convert complex data into actionable insights, facilitating informed decision-making."),
          #tags$br(),
          
          ## Seagrass information. ---------------------------------------------
          
          tags$button(id = "go_to_seagrass", class = "ui button", 
                      style = "width: 150px; height: 50px;", "Seagrass"),
          tags$div(
            id = "seagrass_modal",
            class = "ui modal",
            tags$div(
              class = "header",
              "Seagrass Information"
            ),
            tags$div(
              class = "content",
              h2("What is seagrass?"),
              p("Contrary to what their nomenclature may suggest, seagrasses are marine angiosperms, not grasses. They range in size from the diminutive to those with leaves spanning up to seven meters. They absorb nutrients via both roots and leaves and rely heavily on sunlight for photosynthesis."),
              p("These marine angiosperms primarily occupy ecological niches like intertidal and subtidal zones. Some species are found at depths up to 19 meters. They can form dense communities, known as seagrass meadows, resembling underwater carpets."),
              h2("Why do we care about seagrass?"),
              p("Seagrass meadows are crucial to the marine ecosystem for several reasons:",
                tags$ul(
                  tags$li("Primary food sources for marine fauna"),
                  tags$li("Feeding grounds for shorebirds"),
                  tags$li("Habitat and protection for juvenile marine life"),
                  tags$li("Wave action dampening"),
                  tags$li("Sediment stabilization.")
                )
              ),
              h2("The grass isn't always greener underwater..."),
              p("Seagrasses face accelerating rates of loss and degradation. Their recovery capacity after disturbances varies significantly."),
              p("Principal threats include:",
                tags$ul(
                  tags$li("Climatic perturbations"),
                  tags$li("Degraded water quality"),
                  tags$li("Thermal stress"),
                  tags$li("Mechanical disruptions"),
                  tags$li("Injudicious dredging and coastal development.")
                )
              )
            )
          ),
          ## DBN model information. --------------------------------------------
          
          tags$button(id = "go_to_dbn", class = "ui button",
                      style = "width: 150px; height: 50px;", "DBN model"),
          tags$div(
            id = "dbn_modal",
            class = "ui modal",
            tags$div(
              class = "header",
              "DBN Model Information"
            ),
            tags$div(
              class = "content",
              h2("Challenges in Modeling Marine Ecosystems"),
              p("Modeling marine ecosystems is complex due to their dynamic and variable nature. 
                Traditional statistical models often overlook key ecological processes and struggle with missing or incomplete data. 
                These limitations make it challenging to predict how these ecosystems respond to various stressors, both natural and anthropogenic."),
              h2("Dynamic Bayesian Network in environmental modelling"),
              p("Bayesian Networks (BNs) and Dynamic Bayesian Networks (DBNs) offer a more robust way to model complex systems like marine ecosystems. 
                BNs capture causal relationships and can integrate diverse data types, including expert opinions. 
                They are adaptable and can be updated as new information becomes available. 
                DBNs extend these benefits by adding a temporal dimension, allowing for more dynamic modeling and the inclusion of feedback loops, crucial for understanding ecosystem resilience."),
              p("For instance, in the context of seagrass ecosystems, a DBN could model how various environmental factors like water quality, temperature, and human activities impact seagrass health over time while also accounting for the feedback effects of seagrass health on these environmental factors."),
              p("DBNs are particularly useful for ecosystem management in the face of changing environmental conditions. 
                Advantages include:",
                tags$ul(
                  tags$li("Adaptive management alignment"),
                  tags$li("Flexibility and versatility"),
                  tags$li("Uncertainty modeling")
                )
              ),
              h2("Seagrass DBN model"),
              p("The DBN model used in this app  consists of 35 nodes and is organised by processes relevant to the seagrass meadow's resilience. 
                These processes include resistance (e.g., physiological measures such as carbohydrate stores that facilitate resistance to pressures up to a point when the carbohydrate stores are depleted), recovery (e.g., growth as production of new leaves, shoots, and lateral expansion enables recovery of a meadow), site conditions (e.g., genera present as different genera have different relative carbohydrate stores and growth rates), and environmental factors (e.g., light).")
            )
          ),
          tags$script(
            HTML(
              '$("#go_to_seagrass").on("click", function() {
              $("#seagrass_modal").modal("show");
            });
            $("#close_seagrass_modal").on("click", function() {
              $("#seagrass_modal").modal("hide");
            });
            $("#go_to_dbn").on("click", function() {
              $("#dbn_modal").modal("show");
            });
            $("#close_dbn_modal").on("click", function() {
              $("#dbn_modal").modal("hide");
            });'
            )
          )
        )
      )
    )
  })
 
  # ****************************************************************************
  # 2) Second tab content. -----------------------------------------------------
  # ****************************************************************************
  
  ## Overall model structure. --------------------------------------------------
  
  output$images <- renderUI({
    selected_image <- input$image_choice
    tags$img(src = selected_image, alt = "Selected Image", 
             width = 600, height = 500, 
             style = "display: block; margin: auto;")
  })
  
  ## Nodes Description.---------------------------------------------------------
  
  output$table <- renderDT({
    datatable(nodes, options = list(pageLength = 5))
  })
  
  # ****************************************************************************
  # 3) Third tab content. ------------------------------------------------------
  # ****************************************************************************
  
  ## Map Rendering. ------------------------------------------------------------
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles(urlTemplate = "http://tile.stamen.com/watercolor/{z}/{x}/{y}.jpg") %>% 
      setView(lng = 151.30833, lat = -23.76611, zoom = 4) %>% 
      addAwesomeMarkers(
        lng = 151.308333,
        lat = -23.766111,
        icon = awesomeIcons(
          icon = 'ios-close',
          iconColor = 'white',
          library = 'ion',
          markerColor = '#FFCCCC'
        ),
        popup = "<div style='width: 300px; height: 200px; overflow-y: scroll;'><img src='zostera.jpg' width='400px' height='300px'></div>"
      )
  })
  
  ## Handle page navigation. ---------------------------------------------------
  
  currentPage <- reactiveVal(1)

  observeEvent(input$nextpage, {
    if (currentPage() == 1) {
      currentPage(2)
    }
  })

  observeEvent(input$prevpage, {
    if (currentPage() == 2) {
      currentPage(1)
    }
  })

  ## Dynamic Content. ----------------------------------------------------------
  
  output$dynamicContent <- renderUI({
    if (currentPage() == 1) {
      firstPageContent()  # Function call to render first page content
    } else if (currentPage() == 2) {
      secondPageContent()  # Function call to render second page content
    }
  })

  # Function to render first page content
  firstPageContent <- function() {
    tagList(
      h1(style = "color: #333333;", "Explore the Seagrasses of the Gladstone"),
      h2("Gladstone Case-Study"),
      # Case study description
      p("The case study focuses on the", tags$em("Zostera"), "seagrass at Pelican Banks in Gladstone Harbour in Australia, part of the Great Barrier Reef World Heritage Area.
      The area experiences seasonal rainfall and has a subtropical climate, with sea temperatures averaging 24.4 degree Celsius.
      Pelican Banks is notable as the largest seagrass habitat in the harbour, primarily featuring the tropical subspecies", tags$em("Zostera muelleri")), 
      
      h2("App Functionality"),
      # App functionality description
      p("This app offers two distinct tabs for exploring seagrass resilience.
      The 'Heat Stress & Light' tab enables you to conduct short-term analyses by varying light and heat stress factors to assess resilience metrics..
      On the other hand, the 'SSP-Scenario' tab is specifically designed to facilitate long-term analyses using Shared Socioeconomic Pathways (SSP) scenarios." ),
      
      h3("Tab: Preset Cases"),
      p("Explore the resilience of seagrass through 10 preset scenarios. 
        Each scenario provides a unique combination of light and heat stress conditions to offer an understanding of how seagrass responds under varying levels of disturbance.
        The scenarios are designed to vary in:",
        tags$ul(
          tags$li("Duration of the stress event"),
          tags$li("Start month of the event"),
          tags$li("Intensity of light or heat stress")
        )
        ),
      p("For instance, some scenarios commence light disturbances in January, while others it initiates in July. 
        The intensity and duration also differ, thus allowing for a broad range of outcomes."),
      p("Quantifying Resilience:",
        "To evaluate the resilience of seagrass, we concentrated on three pivotal ecological interactions:",
        tags$ul(
          tags$li(tags$strong("Resistance: "), "Captures the ecosystem's ability to withstand increasingly frequent and severe pulse disturbances. 
                              A higher resistance implies a greater capacity to maintain its original state and functionality, even when subjected to disturbances like heatwaves."),
          tags$li(tags$strong("Recovery: "), "Measures the expected time needed for an ecosystem to return to its baseline state after experiencing a disturbance."),
          tags$li(tags$strong("Persistence/Local Extinction Risk: "), "Assesses the probability of an ecosystem being irreversibly lost over time relative to its baseline conditions. 
                              A higher value suggests a heightened risk of complete loss.")
        )
      ),
      p("Model Output Insights:",
        tags$ul(
          tags$li(tags$strong("Simulations: "), "Each scenario undergoes a simulation to analyze system response and state probability trajectories over time."),
          tags$li(tags$strong("Population States: "), "Defined as a percentage of a reference site, these indicate realised shoot density (biomass) in terms of grams of dry matter per square meter. 
                          Realised shoot density (biomass) can be high, moderate, low, or zero."),
          tags$li(tags$strong("Cummulative Response: "), "The biomass at a given time is influenced by factors such as loss and recovery rates, baseline biomass nodes, and environmental changes."),
          tags$li(tags$strong("Adaptive Mechanisms: "), "Despite environmental hazards like heat stress or decrease in water quality, plants mitigate loss through their physiological ability to resist stress."),
          tags$li(tags$strong("Long-Term Effects: "), "Over time, environmental stresses may alter physiological status and other recovery factors like lateral growth and seed recruitment.")
        )
      )
                          
    )
  }
  
  # Function to render second page content
  secondPageContent <- function() {
    tagList(
      h1(style = "color: #333333;", "Explore the Seagrasses of the Gladstone"),
      # Two-column layout
      tags$div(style = "display: flex; justify-content: space-between;",
               
               # First Column
               tags$div(style = "width: 48%;",
                        h3("Tab: Light & Heat"),
                        p("Apply a disturbance event related to heat and/or light by varying the following parameters:"), 
                        tags$div(
                          style = "text-align: left;",
                          tags$img(src = "event.png", alt = "Case Study Image", width = "400px", height = "300px")
                        ),
                        tags$ul(
                          tags$li("Start:",
                                tags$ul(
                                  tags$li("This is the month varying from ", HTML("<strong>1 to 12 (January to December)</strong>"), ", that the light/heat event can occur.")
                                )
                          )
                        ),
                        tags$ul(
                          tags$li("Duration:",
                                  tags$ul(
                                    tags$li("Duration is the time from ", HTML("<strong>1 to 12 months</strong>"), " of a light and/or heat event.")
                                  )
                          )
                        ),
                        tags$ul(
                          tags$li("Heat Intensity Settings:",
                                  tags$ul(
                                    tags$li("\"1\" indicates ", HTML("<strong>No Effect</strong>"), " of heat event"),
                                    tags$li("\"0\" indicates ", HTML("<strong>Effect</strong>"), " of a heat event")
                                  )
                            )
                          ),
                        tags$ul(
                          tags$li("Light Intensity Settings:",
                                  tags$ul(
                                    tags$li("\"1\" indicates a level ", HTML("<strong>Above Saturation</strong>"), ", optimal for growth"),
                                    tags$li("\"0\" indicates a level ", HTML("<strong>Below Saturation</strong>"), ", insufficient for growth")
                                  )
                          )
                        )
               ),
               
               # Second Column
               tags$div(style = "width: 48%;",
                        h3("Tab: SSP-Scenario"),
                        p("Select a variable and a decade to view the corresponding state probabilities projected over time. 
                          Also choose two different socio-economic pathways (SSPs) for comparative analysis."), 
                        tags$div(
                          style = "text-align: left;",
                          tags$img(src = "ssp.png", alt = "SSP Trends Image", width = "500px", height = "300px")
                        ),
                        p("This study focuses on specific Shared Socioeconomic Pathways (SSPs) as outlined in the IPCC's latest report. 
                          The scenarios provide projections of future changes in temperature under different emissions trajectories. 
                          Below are the details of the SSPs considered:"),
                        tags$ul(
                          tags$li(tags$b("SSP1-1.9 & SSP1-2.6:"),
                                  tags$span(" These are low-emission scenarios. ", tags$i("The numbers 1.9 and 2.6 represent peak radiative forcing in W/m2.")),
                                  tags$ul(
                                    tags$li("SSP1-1.9: Predicts a 1.5 degrees Celsiu increase in the near future (2021-2040)."),
                                    tags$li("SSP1-2.6: Also considered a low-emission pathway.")
                                  )
                          ),
                          tags$li(tags$b("SSP3-7.0:"),
                                  tags$span(" This is a medium-to-high-emission scenario. ", tags$i("The number 7.0 represents peak radiative forcing in W/m2.")),
                                  tags$ul(
                                    tags$li("Predicts a significant increase in temperature in the far future (2081-2100).")
                                  )
                          ),
                          tags$li(tags$b("SSP5-8.5:"),
                                  tags$span(" This is a high-emission scenario. ", tags$i("The number 8.5 represents peak radiative forcing in W/m2.")),
                                  tags$ul(
                                    tags$li("Predicts the highest increase in temperature, ranging between 3.3-5.7 degrees Celsius in the far future (2081-2100).")
                                  )
                          )
                        )
               )
      )
    )
  }
  
  # ****************************************************************************
  # 4) Fourth tab content. -----------------------------------------------------
  # ****************************************************************************
  
  # Observe toggleLightInfo event to toggle visibility of "lightInfo" section
  observeEvent(input$toggleLightInfo, {
    toggle("lightInfo")
  })
  
  # Observe toggleHeatInfo event to toggle visibility of "heatInfo" section
  observeEvent(input$toggleHeatInfo, {
    toggle("heatInfo")
  })
  
  ## DBN Plot. -----------------------------------------------------------------
  
  output$scenario_img <- renderImage({
    req(input$exscenario)
    
    scenario_str <- input$exscenario
    # Extract the numerical part from the string "Scenario x"
    img_num <- gsub("Scenario ", "", scenario_str)
    
    # Form the image source string
    img_src <- paste0("www/scenario", img_num, ".png")  # Added "www/" to ensure path
    print(paste("Trying to load image from: ", img_src))
    
    list(src = img_src, contentType = "image/png", 
             width = 560, height = 400, 
             style = "display: block; margin: auto;")
  }, deleteFile = FALSE)
  
  ## Resilience table. ---------------------------------------------------------
  
  output$resilience_table <- renderTable({
    req(input$exscenario)
    scenario_data <- switch(input$exscenario,
                            "Scenario 1" = data.frame(Resistance = 0.5340344, Recovery = 5, Persistence = 1.107596),
                            "Scenario 2" = data.frame(Resistance = 0.0881622, Recovery = 5, Persistence = 1.201749),
                            "Scenario 3" = data.frame(Resistance = 0.1290139, Recovery = 8, Persistence = 1.125553),
                            "Scenario 4" = data.frame(Resistance = 0.009046838, Recovery = 15, Persistence = 2.070801),
                            "Scenario 5" = data.frame(Resistance = 0.06520012, Recovery = 7, Persistence = 1.273046),
                            "Scenario 6" = data.frame(Resistance = 0.004054813, Recovery = 5, Persistence = 1.392474),
                            "Scenario 7" = data.frame(Resistance = 0.03363977, Recovery = 8, Persistence = 1.360809),
                            "Scenario 8" = data.frame(Resistance = 0, Recovery = 5, Persistence = 1.433234),
                            "Scenario 9" = data.frame(Resistance = 0.01728537, Recovery = 9, Persistence = 1.548879),
                            "Scenario 10" = data.frame(Resistance = 0, Recovery = 18, Persistence = 2.787322),
                            data.frame(Resistance = NA, Recovery = NA, Persistence = NA)  # default if not matched
    )
    return(scenario_data)
  })
  
  ## Events description. -------------------------------------------------------
  
  output$dynamicText <- renderUI({
    scenario <- input$exscenario
    scenario_text <- NULL
    
    if (scenario == "Scenario 1") {
      scenario_text <- tags$p(strong("Scenario 1:"), br(),
                              "Light: A light disturbance event begins in January, lasts for 3 months.", br(),
                              "Heat: No heat stress event occurs.")
    } 
    else if (scenario == "Scenario 2") {
      scenario_text <- tags$p(strong("Scenario 2"), br(),
                              "Light: A light disturbance event begins in January, lasts for 6 months.", br(),
                              "Heat: No heat stress event occurs.")
    }
    else if (scenario == "Scenario 3") {
      scenario_text <- tags$p(strong("Scenario 3"), br(),
                              "Light: A light disturbance event begins in Juy, lasts for 3 months.", br(),
                              "Heat: No heat stress event occurs.")
    }
    else if (scenario == "Scenario 4") {
      scenario_text <- tags$p(strong("Scenario 4"), br(),
                              "Light: A light disturbance event begins in July, lasts for 6 months.", br(),
                              "Heat: No heat stress event occurs.")
    }
    else if (scenario == "Scenario 5") {
      scenario_text <- tags$p(strong("Scenario 5"), br(),
                              "Light: No light disturbance event occurs.", br(),
                              "Heat: A heat stress event occurs in January and lasts for 3 months.")
    }
    else if (scenario == "Scenario 6") {
      scenario_text <- tags$p(strong("Scenario 6"), br(),
                              "Light: No light disturbance event occurs.", br(),
                              "Heat: A heat stress event occurs in January and lasts for 6 months.")
    }
    else if (scenario == "Scenario 7") {
      scenario_text <- tags$p(strong("Scenario 7"), br(),
                              "Light: A light disturbance event begins in January, lasts for 3 months.", br(),
                              "Heat: A heat stress event occurs in January and lasts for 3 months.")
    }
    else if (scenario == "Scenario 8") {
      scenario_text <- tags$p(strong("Scenario 8"), br(),
                              "Light: A light disturbance event begins in January, lasts for 6 months.", br(),
                              "Heat: A heat stress event occurs in January and lasts for 6 months.")
    }
    else if (scenario == "Scenario 9") {
      scenario_text <- tags$p(strong("Scenario 9"), br(),
                              "Light: A light disturbance event begins in July, lasts for 3 months.", br(),
                              "Heat: A heat stress event occurs in January and lasts for 3 months.")
    }
    else {
      scenario_text <- tags$p(strong("Scenario 10"), br(),
                              "Light: A light disturbance begins in July, lasts for 6 months.", br(),
                              "Heat: A heat stress event occurs in January and lasts for 6 months.")
    }
    
    return(scenario_text)
  })
  
  ## Update slider inputs. -----------------------------------------------------
  
  observe({
    req(input$exscenario)
    scenario <- input$exscenario
    
    # Initialize variables for select inputs and sliders
    heat_start <- 0
    heat_duration <- 0
    heat_stress <- 1
    dredge_start <- 0
    dredge_duration <- 0
    dredge_light <- 1
    
    if (scenario == "Scenario 1") {
      heat_start <- 0
      dredge_start <- 1
      dredge_duration <- 3
      dredge_light <- 0
    } else if (scenario == "Scenario 2") {
      heat_start <- 0
      dredge_start <- 1
      dredge_duration <- 6
      dredge_light <- 0
    } else if (scenario == "Scenario 3") {
      heat_start <- 0
      dredge_start <- 7
      dredge_duration <- 3
      dredge_light <- 0
    } else if (scenario == "Scenario 4") {
      heat_start <- 0
      dredge_start <- 7
      dredge_duration <- 6
      dredge_light <- 0
    } else if (scenario == "Scenario 5") {
      heat_start <- 1
      heat_duration <- 3
      heat_stress <- 0
      dredge_start <- 0
      dredge_duration <- 0
      dredge_light <- 1
    } else if (scenario == "Scenario 6") {
      heat_start <- 1
      heat_duration <- 6
      heat_stress <- 0
      dredge_start <- 0
      dredge_duration <- 0
      dredge_light <- 1
    } else if (scenario == "Scenario 7") {
      heat_start <- 1
      heat_duration <- 3
      heat_stress <- 0
      dredge_start <- 1
      dredge_duration <- 3
      dredge_light <- 0
    } else if (scenario == "Scenario 8") {
      heat_start <- 1
      heat_duration <- 6
      heat_stress <- 0
      dredge_start <- 1
      dredge_duration <- 6
      dredge_light <- 0
    } else if (scenario == "Scenario 9") {
      heat_start <- 1
      heat_duration <- 3
      heat_stress <- 0
      dredge_start <- 7
      dredge_duration <- 3
      dredge_light <- 0
    } else if (scenario == "Scenario 10") {
      heat_start <- 1
      heat_duration <- 6
      heat_stress <- 0
      dredge_start <- 7
      dredge_duration <- 6
      dredge_light <- 0
    }
    
    # Update select inputs, sliders, and numeric inputs
    updateSelectInput(session, "exheatstart", selected = heat_start)
    updateSliderInput(session, "exheatduration", value = heat_duration)
    updateNumericInput(session, "exheatstress", value = heat_stress)  
    updateSelectInput(session, "exdredgestart", selected = dredge_start)
    updateSliderInput(session, "exdredgeduration", value = dredge_duration)
    updateNumericInput(session, "exdredgelight", value = dredge_light) 
    
    # Disable sliders
    shinyjs::disable("exdredgeduration")
    shinyjs::disable("exheatduration")
  })

  # ****************************************************************************
  # 5) Fifth tab content. -----------------------------------------------------
  # ****************************************************************************
  
  ## Selected variables limited to 4. ------------------------------------------
  
  observe({
    if (length(input$variable) > 4) {
      updateSelectInput(session, "variable", selected = input$variable[1:4])
      showNotification("You can select up to 4 variables only.", type = "warning")
    }
  })
  
  ## Calculate height. ---------------------------------------------------------
  
  output$dynamic_plot <- renderUI({
    num_vars <- length(input$variable)

    # Assuming each plot needs 300 pixels
    if (num_vars == 3 || num_vars == 4) {
      # Two rows for 3 or 4 plots
      calculated_height <- 300 * 2
    } else {
      # One row for each plot otherwise
      calculated_height <- 300 * 1
    }

    # Create a div with a minimum height to hold the plot
    div(
      style = paste0("min-height: ", calculated_height, "px;"),
      plotOutput("plot", height = calculated_height)
    )
    
  })
  
  ## Run scenario and estimate resilience. -------------------------------------

  observeEvent(input$submit, {
    
    # Hide the previous plot and table
    shinyjs::hide("plot")
    shinyjs::hide("results_table") 
    
    shinyjs::show("loading1")  # Show the loading imag
    shinyjs::show("loading2")

    heatstart <- as.numeric(input$heatstart)
    heatduration <- input$heatduration
    heatstress <- input$heatstress

    dredgestart <- as.numeric(input$dredgestart)
    dredgeduration <- input$dredgeduration
    dredgelight <- input$dredgelight

    site <- 1
    tend <- 96

    E <- createEvidScenario(I, tend = tend, heatduration = heatduration, heatstress = heatstress, heatstart = heatstart,
                            dredgeduration = dredgeduration, dredgelight = dredgelight, dredgestart = dredgestart, site = site)

    outputs <- runDBFIVE(E = E, I = I, tend = tend,
                         heatduration = heatduration, heatstart = heatstart,
                         dredgeduration = dredgeduration, dredgestart = dredgestart,
                         pandxbar = pandxbar[[site]])

    # Extract posterior probabilities
    P <- dbfiveJ2dbnsP(dbn, 'scenario 1', outputs$Y$J, I$nnames, I$statenames)
    x <- P$t
    x <- ymd(x)
    d <- min(x) - ymd('2022-1-1')
    P$t <- x - d

    ## Plot posterior probabilities for node name. -----------------------------
    
    output$plot <- renderPlot({

      # Get selected variables
      nodenames <- input$variable

      # Create a list to store individual plots
      plot_list <- list()

      # Loop through each selected variable and create a plot
      for(i in seq_along(nodenames)) {
        nodename <- nodenames[i]
        out <- dbnplot(nodename, P, 'both', '%mm-%YY', 1)
        plot_list[[i]] <- out
      }

      # Combine individual plots into a single plot using grid.arrange
      if (length(plot_list) > 0) {
        if (length(plot_list) <= 2) {
         
          do.call(gridExtra::grid.arrange, c(plot_list, ncol = 2))
        } else if (length(plot_list) <= 4) {
          
          do.call(gridExtra::grid.arrange, c(plot_list, ncol = 2, nrow = 2))
        } else {
         
        }
      }
      
      # Show the plot after it's generated
      shinyjs::show("plot")

    })
    
    shinyjs::hide("loading1") 
    shinyjs::hide("loading2")
    
    t_heat = heatstart + 24 + heatduration - 1
    t_dredge = dredgestart+ 24 + dredgeduration - 1
    t = max(sort(unique(c(t_heat, t_dredge))))
    
    ## Create and render output table. -----------------------------------------
    
    output$results_table <- renderTable({
      results <- data.frame(
        Resistance = outputs$resistance,
        Recovery = ifelse(outputs$recovery > 0, outputs$recovery - t, outputs$recovery),
        Persistence = outputs$persistence
      )
      results
    })
    
    # Show the table after it's generated
    shinyjs::show("results_table")
    
  })
  
  ## Add a title. --------------------------------------------------------------
  
  output$dynamic_title <- renderText({
    selected_vars <- paste(input$variable, collapse = ", ")
    paste0("<strong style='font-size: 16px;'>The model predicted-state probabilities for <span style='background-color: lightgray; padding: 4px; color: black;'>", 
           selected_vars, "</span></strong>")
  })
  
  
  # ****************************************************************************
  # 6) Sixth tab content. ------------------------------------------------------
  # ****************************************************************************
  
  ## Load data. ----------------------------------------------------------------
  
  # For loaded_data1 (Assuming sspscenario1 is the input from Shiny UI)
  loaded_data1 <- reactive({
    # Initialize status variable
    status <- NULL
    
    # Check if the scenario is selected and not "None"
    if (is.null(input$sspscenario1) || input$sspscenario1 == "" || is.na(input$sspscenario1)) {
      status <- "No scenario selected for Data 1."
    } else {
      # Use tryCatch for error handling
      tryCatch({
        filepath <- switch(input$sspscenario1,
                           "ssp119" = "Plistssp119.RData",
                           "ssp126" = "Plistssp126.RData",
                           "ssp370" = "Plistssp370.RData",
                           "ssp585" = "Plistssp585.RData",
                           NULL)
        
        if (is.null(filepath)) {
          status <- "Invalid scenario selected for Data 1."
        } else {
          lapply(filepath, load, envir = .GlobalEnv)
          status <- "Data 1 loaded successfully."
        }
      }, error = function(e) {
        status <- paste("An error occurred while loading Data 1:", e$message)
      })
    }
    
    return(status)
  })
  
  # For loaded_data2 (Assuming sspscenario2 is the input from Shiny UI)
  loaded_data2 <- reactive({
    # Initialize status variable
    status <- NULL
    
    # Check if the scenario is selected and not "None"
    if (is.null(input$sspscenario2) || input$sspscenario2 == "" || is.na(input$sspscenario2)) {
      status <- "No scenario selected for Data 2."
    } else {
      # Use tryCatch for error handling
      tryCatch({
        filepath <- switch(input$sspscenario2,
                           "ssp119" = "Plistssp119.RData",
                           "ssp126" = "Plistssp126.RData",
                           "ssp370" = "Plistssp370.RData",
                           "ssp585" = "Plistssp585.RData",
                           NULL)
        
        if (is.null(filepath)) {
          status <- "Invalid scenario selected for Data 2."
        } else {
          lapply(filepath, load, envir = .GlobalEnv)
          status <- "Data 2 loaded successfully."
        }
      }, error = function(e) {
        status <- paste("An error occurred while loading Data 2:", e$message)
      })
    }
    
    return(status)
  })
  
  ## Selected variables limited to 2. ------------------------------------------
  
  observe({
    if (length(input$main_variable) > 2) {
      updateSelectInput(session, "main_variable", selected = input$main_variable[1:2])
      showNotification("You can select up to 2 variables only.", type = "warning")
    }
  })
  
  # Update Plot Data 1 based on selected_indices
  plot_data1 <- reactive({
    ssp1_plots <- list()
    
    if (loaded_data1() == "Data 1 loaded successfully."
        && !is.null(input$main_variable) && length(input$main_variable) > 0
        && !is.null(input$main_decade) && input$main_decade != "") {
      
      idx <- as.numeric(input$selected_index)
      selected_year <- as.numeric(substr(input$main_decade, 1, 4))
      datarange = as.Date(c(paste(selected_year, '-01-01', sep=''), paste(selected_year + 9, '-12-31', sep='')))
      
      P1 <- get(paste("Plist", input$sspscenario1, sep=""))
      
      for (i in 1:length(input$main_variable)) {
        nodename <- input$main_variable[i]
        ssp1_plots[[i]] <- dbnplot2(nodename, P1[[idx]], 'both', '%mm-%YY', 1, datarange)
      }
    }
    
    return(ssp1_plots)
  })
  
  # Update Plot Data 2 based on selected_indices
  plot_data2 <- reactive({
    ssp2_plots <- list()
    
    if (loaded_data2() == "Data 2 loaded successfully."
        && !is.null(input$main_variable) && length(input$main_variable) > 0
        && !is.null(input$main_decade) && input$main_decade != "") {
      
      idx <- as.numeric(input$selected_index)
      selected_year <- as.numeric(substr(input$main_decade, 1, 4))
      datarange = as.Date(c(paste(selected_year, '-01-01', sep=''), paste(selected_year + 9, '-12-31', sep='')))
      
      P2 <- get(paste("Plist", input$sspscenario2, sep=""))
      
      for (i in 1:length(input$main_variable)) {
        nodename <- input$main_variable[i]
        ssp2_plots[[i]] <- dbnplot2(nodename, P2[[idx]], 'both', '%mm-%YY', 1, datarange)
      }
    }
    
    return(ssp2_plots)
  })
  
  ## Calculate height. ---------------------------------------------------------
  
  # Dynamic UI for Plot 1
  output$dynamic_plot1 <- renderUI({
    num_vars <- length(input$main_variable)
    
    # Assuming each plot needs 300 pixels
    calculated_height <- 300 * num_vars
    
    plotOutput("plot1", height = calculated_height)
  })
  
  # Dynamic UI for Plot 2
  output$dynamic_plot2 <- renderUI({
    num_vars <- length(input$main_variable)
    
    # Assuming each plot needs 300 pixels
    calculated_height <- 300 * num_vars
    
    plotOutput("plot2", height = calculated_height)
  })
  
  ## Graphs. -------------------------------------------------------------------

  output$plot1 <- renderPlot({
    if (!is.null(plot_data1()) && length(plot_data1()) > 0) {
      grid.arrange(grobs = plot_data1(), ncol = 1)
    }
  })
  
  output$plot2 <- renderPlot({
    if (!is.null(plot_data2()) && length(plot_data2()) > 0) {
      grid.arrange(grobs = plot_data2(), ncol = 1)
    }
  })
  
  ## Add a title. --------------------------------------------------------------
  
  output$dynamic_title1 <- renderText({
    selected_vars1 <- paste(input$main_variable, collapse = ", ")
    paste0("<strong style='font-size: 16px;'>The model predicted-state probabilities for <span style='background-color: lightgray; padding: 4px; color: black;'>",
           selected_vars1, "</span></strong>")
  })
  
  output$dynamic_title2 <- renderText({
    selected_vars2 <- paste(input$main_variable, collapse = ", ")
    paste0("<strong style='font-size: 16px;'>The model predicted-state probabilities for <span style='background-color: lightgray; padding: 4px; color: black;'>",
           selected_vars2, "</span></strong>")
  })
  
  ## Resilience Plot. ----------------------------------------------------------
  
  # Reactive function to get filtered data
  highratio_data <- reactive({
    # Ensure the required inputs are available
    if (!is.null(input$sspscenario1) && !is.null(input$sspscenario2) && !is.null(input$main_decade)) {
      # Extract the starting year from the selected decade
      start_decade <- as.numeric(substr(input$main_decade, 1, 4))
      end_decade <- start_decade + 9
      
      # List of selected scenarios
      selected_scenarios <- c(input$sspscenario1, input$sspscenario2)
      
      # Filter the data based on the selected scenarios and decade
      filtered_data <- highratio %>%
        filter(Scenario %in% selected_scenarios, Year >= start_decade, Year <= end_decade)
      
      return(filtered_data)
    } else {
      return(data.frame())  # return an empty data frame if inputs are null
    }
  })
  
  zeroratio_data <- reactive({
    # Ensure the required inputs are available
    if (!is.null(input$sspscenario1) && !is.null(input$sspscenario2) && !is.null(input$main_decade)) {
      # Extract the starting year from the selected decade
      start_decade <- as.numeric(substr(input$main_decade, 1, 4))
      end_decade <- start_decade + 9
      
      # List of selected scenarios
      selected_scenarios <- c(input$sspscenario1, input$sspscenario2)
      
      # Filter the data based on the selected scenarios and decade
      filtered_data <- zeroratio %>%
        filter(Scenario %in% selected_scenarios, Year >= start_decade, Year <= end_decade)
      
      return(filtered_data)
    } else {
      return(data.frame())  # return an empty data frame if inputs are null
    }
  })
  
  # observe({
  #   print("High Ratio Data:")
  #   print(highratio_data())
  # })
  
  # Render plot
  output$highratio_plot <- renderPlot({
    # Ensure the required inputs are available
    if (!is.null(highratio_data()) && nrow(highratio_data()) > 0) {
      # Determine the min and max years from the data
      min_year <- min(highratio_data()$Year, na.rm = TRUE)
      max_year <- max(highratio_data()$Year, na.rm = TRUE)
      
      p <- ggplot(highratio_data(), aes(x = Year, y = Average, color = Scenario)) +
        geom_point(size=0.8) + geom_line() +
        geom_line(aes(y = `5%`, linetype = "5th Percentile")) + 
        geom_line(aes(y = `95%`, linetype = "95th Percentile")) + 
        geom_ribbon(aes(ymin = `25%`, ymax = `75%`), linetype = 1, alpha = 0.1, colour = NA) + 
        labs(y = "High Shoot Density Ratio", x = NULL, color = "SSP Scenario") +
        scale_linetype_manual(values = c("5th Percentile" = "dashed", "95th Percentile" = "dashed")) +
        #scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25)) +
        scale_x_continuous(limits = c(min_year, max_year), breaks = seq(min_year, max_year, by = 1)) + 
        theme_bw() +
        theme(legend.position = "none") + 
        theme(
          axis.text = element_text(size = 16),  # Axis text
          axis.title = element_text(size = 18),  # Axis titles
          title = element_text(size = 20),  # Main title
          legend.text = element_text(size = 14),  # Legend text
          legend.title = element_text(size = 16), # Legend title
          strip.text = element_text(size = 18)  
        )
      
      # Conditional faceting
      if (length(unique(highratio_data()$Scenario)) > 1) {
        p <- p + facet_wrap(~Scenario, ncol = 1)
      }
      print(p)
    }
  })

  
  output$zeroratio_plot <- renderPlot({
    # Ensure the required inputs are available
    if (!is.null(zeroratio_data()) && nrow(zeroratio_data()) > 0) {
      # Determine the min and max years from the data
      min_year <- min(zeroratio_data()$Year, na.rm = TRUE)
      max_year <- max(zeroratio_data()$Year, na.rm = TRUE)
      
      p <- ggplot(zeroratio_data(), aes(x = Year, y = Average, color = Scenario)) +
        geom_point(size=0.8) + geom_line() +
        geom_line(aes(y = `5%`, linetype = "5th Percentile")) + 
        geom_line(aes(y = `95%`, linetype = "95th Percentile")) + 
        geom_ribbon(aes(ymin = `25%`, ymax = `75%`), linetype = 1, alpha = 0.1, colour = NA) + 
        labs(y = "Zero Shoot Density Ratio", x = NULL, color = "SSP Scenario") +
        scale_linetype_manual(values = c("5th Percentile" = "dashed", "95th Percentile" = "dashed")) +
        #scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 1.25, by = 0.25)) +
        scale_x_continuous(limits = c(min_year, max_year), breaks = seq(min_year, max_year, by = 1)) + 
        theme_bw() +
        theme(legend.position = "none") +
        theme(
          axis.text = element_text(size = 16),  # Axis text
          axis.title = element_text(size = 18),  # Axis titles
          title = element_text(size = 20),  # Main title
          legend.text = element_text(size = 14),  # Legend text
          legend.title = element_text(size = 16), # Legend title
          strip.text = element_text(size = 18)  
        )
      
      # Conditional faceting
      if (length(unique(zeroratio_data()$Scenario)) > 1) {
        p <- p + facet_wrap(~Scenario, ncol = 1)
      }
      print(p)
    }
  })
  
}

