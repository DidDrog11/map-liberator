# app.R
# ------------------------------------------------------------------------------
# MAP LIBERATOR v1.0
# Entry point for the application.
# ------------------------------------------------------------------------------

# 1. Load Global Config & Modules
source("global.R")
source("R/mod_controls.R")
source("R/mod_map_engine.R")
source("R/mod_sidecar.R")
source("R/mod_workbench.R")
source("R/mod_state_manager.R")

# 2. UI Definition
ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  # Custom CSS for the image container and progress bar
  tags$head(tags$style(HTML("
    .image-container { 
      width: 100%; height: 85vh; overflow: auto; 
      border: 2px dashed #ccc; background-color: #f9f9f9; 
      display: flex; align-items: center; justify-content: center;
      cursor: grab; 
    }
    .image-container:active { cursor: grabbing; }
    #ref_image { transition: transform 0.1s ease; max-width: 90%; }
    .accordion-button { font-weight: bold; background-color: #f8f9fa; }
    .tutorial-box { background: #f8f9fa; padding: 20px; border-radius: 8px; border-left: 5px solid #2c3e50; margin-bottom: 20px; }
  "))),
  
  titlePanel(div(icon("map-location-dot"), " Map Liberator")),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      state_manager_ui("state"),
      hr(),
      controls_ui("ctrl"), 
      hr(),
      h4("Reference Map"),
      actionButton("toggle_ref", "Show/Hide Image", icon=icon("eye"), class="btn-info btn-sm", width="100%"),
      br(), br(),
      sidecar_ui("sidecar")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(id = "main_tabs",
                  
                  # TAB 1: WORKSPACE
                  tabPanel("Workspace", icon = icon("map"),
                           br(),
                           fluidRow(
                             div(id = "map_col", class = "col-sm-6", map_engine_ui("map")),
                             div(id = "img_col", class = "col-sm-6", uiOutput("sidecar_panel"))
                           )
                  ),
                  
                  # TAB 2: DATA LEDGER
                  tabPanel("Data Ledger", icon = icon("table"),
                           br(),
                           workbench_ui("workbench")
                  ),
                  
                  # TAB 3: TUTORIAL (NEW)
                  tabPanel("Tutorial", icon = icon("graduation-cap"),
                           br(),
                           column(10, offset = 1,
                                  h2("How to Use Map Liberator"),
                                  p("This tool allows you to digitize spatial data from static map images (e.g., Situation Reports) by matching them to official administrative boundaries."),
                                  hr(),
                                  
                                  div(class="tutorial-box",
                                      h4("Step 1: Set Up Geography"),
                                      p("1. In the sidebar under **Geography Scope**, select your country (e.g., **Nigeria**)."),
                                      p("2. Choose the **Target Level** (e.g., **Admin 1** for States)."),
                                      p("3. (Optional) Use the **Filters** to limit the scope to specific regions (e.g., filtering only 'Borno' state)."),
                                      p("4. Ensure **Fast Render** is checked for better performance.")
                                  ),
                                  
                                  div(class="tutorial-box",
                                      h4("Step 2: Load & Visualize"),
                                      p("1. Click **1. Load Data**. A modal window will show the progress (Loading -> Filtering -> Simplifying)."),
                                      p("2. Once ready, click **2. Visualise Map** to draw the boundaries."),
                                      p("3. Use **3. Zoom** to center the map on your data.")
                                  ),
                                  
                                  div(class="tutorial-box",
                                      h4("Step 3: Upload Reference Map"),
                                      p("1. In the sidebar under **Reference Map**, upload your image file (PNG/JPG)."),
                                      p("2. Use the **Rotate** and **Zoom** sliders to orient the image so it matches the interactive map."),
                                      p("3. If the image is taking up too much space, click **Show/Hide Image** to toggle it.")
                                  ),
                                  
                                  div(class="tutorial-box",
                                      h4("Step 4: Trace & Extract (Example: Nigeria CDC)"),
                                      p("Imagine you have a Nigeria CDC SitRep map showing Lassa Fever cases in 3 states."),
                                      tags$ul(
                                        tags$li("Look at your Reference Map to identify the affected states."),
                                        tags$li("Click the corresponding states on the Interactive Map. They will turn **RED**."),
                                        tags$li("In the **Project Metadata** section, enter the details:"),
                                        tags$ul(
                                          tags$li("Year: **2024**"),
                                          tags$li("Month: **02**"),
                                          tags$li("Source ID: **NCDC_SitRep_05**")
                                        ),
                                        tags$li("Click **Add Selection to Project**.")
                                      )
                                  ),
                                  
                                  div(class="tutorial-box",
                                      h4("Step 5: Review & Export"),
                                      p("1. Go to the **Data Ledger** tab to review your rows."),
                                      p("2. If you made a mistake, select the row and click **Delete Selected**."),
                                      p("3. When finished, click **Download Final CSV** to save your work.")
                                  )
                           )
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # 1. Controls Module
  controls_out <- controls_server("ctrl")
  
  # 2. Map Engine
  map_out <- map_engine_server("map", controls_output = controls_out)
  
  # 3. State Management (Circular Bridge)
  bridge_data <- reactiveVal(data.frame())
  state_out <- state_manager_server("state", data_to_save = bridge_data)
  
  # 4. Workbench
  wb_out <- workbench_server("workbench", 
                             map_output = map_out, 
                             controls_output = controls_out,
                             loaded_state = state_out)
  
  observe({ req(wb_out()); bridge_data(wb_out()) })
  
  # 5. Sidecar & Layout Logic
  sidecar_server("sidecar")
  output$sidecar_panel <- renderUI({ ns <- NS("sidecar"); uiOutput(ns("image_ui")) })
  
  observeEvent(input$toggle_ref, {
    toggle("img_col")
    toggleClass("map_col", "col-sm-6")
    toggleClass("map_col", "col-sm-12")
    runjs("setTimeout(function() { window.dispatchEvent(new Event('resize')); }, 200);")
  })
}

shinyApp(ui, server)