# App which walks through the process of creating a model
# The tabs are as follows:
# 1. Welcome page: describes the app and the process
# 2. Model design: the user creates a graph representing the model structure including compartment names and expressions for transition rates
# 3. Model code: the app uses the model structure to construct code which the user reviews
# 4. Parameter values: the user enters parameter values for the model
# 5. Model output: the app runs the model and displays sample output
# 6. Project download: the app creates a full R project including the model file, app file, parameters, etc downloadable as a zip file
# 7. About page with a link to the issue tracker

# Load libraries
library(shiny)
library(shinyjs)
library(shinyAce)
library(visNetwork)
library(tidyverse)
library(bslib)
library(DT)
library(sortable)
library(withr)
library(config)

source('R/CompartmentalModel.R')
source('R/visnetworkUtils.R')
source('R/logging.R')
source("R/config.R")

# Note: deploy with source("deployment/deployApp.R")
#              then deployAppDev()

conf = getConfig(localProfile = 'dev')

LOG <- makeLogger(default_level = LEVEL$TRACE)
setLogLevel(conf$LogLevel)
LOG("LOG level set to {conf$LogLevel}.", level = LEVEL$INFO)
LOG("App starting", level = LEVEL$INFO)

theme <- bs_theme(version = 5, 
                  preset = "simplex",
                  primary = "#6E82B4",
                  font_scale = NULL)

model_types <- c(
  "Generalized compartmental model",
  "Age structured model",
  "Multi-patch model",
  "Age and Multi-patch model"
)
model_types <- c("Generalized compartmental model")

# > UI ####
ui <- fluidPage(
  title = "Compartmental Model Boostrap",
  useShinyjs(),
  theme = theme,
  # style:
  tags$style(HTML(paste('#primaryTabs { background-image: url("img.png");}',
                        '#primaryTabs > li > a:not(.active) {background: #f4f4f6DD;border-top-left-radius: 10px;border-top-right-radius: 10px;}',
                        '',sep='\n'))),
  tabsetPanel(
    id="primaryTabs",
    ## 1. "Welcome" ####
    tabPanel("Welcome", 
             h1("Welcome to the Compartmental Model Bootstrap App"),
             p("This app is designed to walk you through the process of creating a compartmental model in R."),
             p("The app will guide you through the process of creating a model, writing the code, entering parameter values, running the model, and downloading the project."),
             p("The app is designed to be a starting point for working with your compartmental model in R. It is not a full-featured model building tool, but rather a way to get started."),
             actionButton(inputId = "startModelBtn", "Lets go!")
    ),
    ## 2. "Model Design" ####
    tabPanel("Model Design",
             fluidRow(column(12, h1("Model Design"))),
             fluidRow(column(12, p("In this tab, you will create a graph representing the model structure including compartment names and expressions for transition rates."))),
             fluidRow(column(12, p("The graph will be used to create the model code in the next tab"))),
             fluidRow(column(8,h3("Model Structure")),
                      column(2,h3("Initial Conditions")),
                      column(2,actionButton(inputId = "updateCompartmentOrder", "Reorder"))),
             fluidRow(
               column(8, visNetworkOutput(outputId="codeVisNetwork", width="100%", height="400px")),
               column(4, 
                      fluidRow(
                        column(12, dataTableOutput("initialConditionsTable"))
                      ))
             ),
             fluidRow(column(12,actionButton(inputId = "acceptModelDesignBtn", "Update Model")))
    ),
    ## 3. "Model Code" ####
    tabPanel("Model Code",
             h1("Model Code"),
             p("In this tab, the app will use the model structure to construct code which you will review."),
             p("The code will be used to create the model in the next tab"),
             selectInput("modelType", "Model Type", choices = model_types),
             verbatimTextOutput("modelCode"),
             actionButton(inputId = "acceptModelCodeBtn", "Update")
    ),
    ## 4. "Parameter Values" ####
    tabPanel("Parameter Values",
             h1("Parameter Values"),
             p("In this tab, you will enter parameter values for the model."),
             p("The parameter values will be used to run the model in the next tab"),
             # I thought about how to build on parameters like lambda
             # Instead of a number, the Value column in the parameter table can include
             # an expression using other parameters, eg `beta*(zeta1*.Ia+.Ic)/.pop`
             dataTableOutput("parameterTable"),
             actionButton(inputId = "acceptModelParmsBtn", "Update")
    ),
    ## 5. "Model Output" ####
    tabPanel("Model Output",
             h1("Model Output"),
             p("In this tab, the app will run the model and display sample output."),
             p("The output will be used to download the project in the next tab"),
             actionButton(inputId = "acceptModelOutputBtn", "Accept"),
             # I decided the timesteps can be from code, eg `seq(2020,2025,1/365)`
             # And the initial conditions can be from code, eg `c(S=0,E=0,...)` reactive to what the user entered before
             # There should be a `Run Model` button which generates the plot
             h3("Timesteps:"),
             aceEditor(outputId = 'modelTimestepsText',
                       value = glue::glue('timesteps <- seq({year(Sys.Date())}, {year(Sys.Date())+2}, 1/365)'),
                       autoComplete = 'enabled',
                       minLines = 2,
                       maxLines = 3,
                       autoScrollEditorIntoView = TRUE
                       ),
             actionButton(inputId = "runModelBtn", "Run Model"),
             plotOutput("modelOutput")
    ),
    ## 6. "Project Download" ####
    tabPanel("Project Download",
             h1("Project Download"),
             p("In this tab, the app will create a full R project including the model file, app file, parameters, etc downloadable as a zip file."),
             p("The project will be a starting point for working with your compartmental model in R."),
             downloadButton("downloadProject", "Download Project")
    ),
    ## 7. "About" ####
    tabPanel("About",
             h1("About"),
             p("This app was created by Jared Norman at the Modelling and Simulation Hub, Africa (MASHA)."),
             p("If you have any questions or issues, please visit the issue tracker"),
             a("Issue Tracker", href = "x"),
             p("This app is licensed under the MIT License.")
    )
  )
)

# > Server ####
server <- function(input, output, session) {
  LOG("New session started")
  # Hide some primary tabs initially
  hideTab("primaryTabs", "Model Code")
  hideTab("primaryTabs", "Parameter Values")
  hideTab("primaryTabs", "Model Output")
  hideTab("primaryTabs", "Project Download")
  
  # Setup the cascade/wizard for tabs
  observeEvent(input$startModelBtn,{
                       updateTabsetPanel(session, "primaryTabs", "Model Design")
                     })
  
  observeEvent(input$acceptModelDesignBtn, {
    LOG('observeEvent(acceptModelDesignBtn) called')
    # Update reactives given the current visNetwork state
    visNetworkProxy("codeVisNetwork") %>%
      visGetEdges(input='graphEdges') %>%
      visGetNodes(input='graphNodes')
    # The above triggers input$graphNodes which can do further processing
    mod <- cm()
    # Update parameters
    parameters <- mod$parameters
    LOG("Model parameters currently are:")
    print(parameters)
    tbParameters <- tibble(parameter=names(parameters), value=as.numeric(unname(parameters)))
    LOG("Updating modelParameters() reactiveVal with:")
    print(tbParameters)
    modelParameters(tbParameters)
    # Update initial conditions
    ics <- mod$initialConditions
    icsTxt <- paste0('y0 <- ',
                     paste0('c(',
                            paste0(names(ics),
                                   '=',
                                   unname(ics),
                                   collapse=',\n     '),
                            ')'))
    
    LOG('observeEvent(acceptModelDesignBtn) showing tabs...')
    showTab("primaryTabs", "Model Code")
    updateTabsetPanel(session, "primaryTabs", "Model Code")
  })
  
  observeEvent(input$acceptModelCodeBtn, {
    LOG('observeEvent(acceptModelCodeBtn) called')
    mod <- cm()
    parameters <- mod$parameters
    tbParameters <- tibble(parameter=names(parameters), value=as.numeric(unname(parameters)))
    modelParameters(tbParameters)
    showTab("primaryTabs", "Parameter Values")
    updateTabsetPanel(session, "primaryTabs", "Parameter Values")
  })
  
  observeEvent(input$acceptModelParmsBtn, {
    showTab("primaryTabs", "Model Output")
    updateTabsetPanel(session, "primaryTabs", "Model Output")
  })
  
  observeEvent(input$acceptModelOutputBtn, {
    showTab("primaryTabs", "Project Download")
    updateTabsetPanel(session, "primaryTabs", "Project Download")
  })
  
  ## 2. Model Design ####
  cm <- reactiveVal(CompartmentalModel())
  
  output$codeVisNetwork <- renderVisNetwork({
    LOG('renderVisNetwork() called')
    vn <- CM2VN(CompartmentalModel())
    vn %>% 
      visEdges(smooth = FALSE, arrows = 'to') %>%
      visNodes(shape = 'box', color = list(background = 'white', border = 'black'), font = list(color = 'black', face = 'monospace', size = 20)) %>%
      visPhysics(enabled = FALSE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = list(from=0, to=1),
                                         labelOnly = TRUE, hover = TRUE),
                 clickToUse = TRUE,
                 manipulation = list(enabled = TRUE,
                                     # "text" = c("title", "label", "isalive", "isInfectious")
                                     addNodeCols = list(
                                       "text" = c("label")
                                     ),
                                     addEdgeCols = list(
                                       "text" = c("label")
                                     ),
                                     editEdgeCols = list(
                                       "text" = c("label")
                                     ),
                                     editNodeCols = list(
                                       "text" = c("label")
                                     )))
  })
  
  observeEvent(input$graphNodes, {
    # This is updated by the visNetworkProxy,
    # eg when acceptModelDesignBtn is clicked
    # compartmentIDs <- seq_along(input$graphNodes)
    # transitionIDs <- seq_along(input$graphEdges)
    # tbCompartments <- map(compartmentIDs, ~{
    #   node <- input$graphNodes[[.x]]
    #   list(compartment=.x,
    #        x=node$x, 
    #        y=node$y)}) %>%
    #   bind_rows() %>%
    #   rowid_to_column(var='ID') %>%
    #   mutate(x=x-min(x)+50, y=y-min(y)+50)
    # tbTransitions <- map(transitionIDs, ~{
    #   edge <- input$graphEdges[[.x]]
    #   list(transitionID=.x,
    #        src=edge$from, 
    #        dst=edge$to)}) %>%
    #   bind_rows() %>%
    #   left_join(tbCompartments %>% rename(src.ID=ID, src.x=x, src.y=y), by=join_by(src==compartment)) %>%
    #   left_join(tbCompartments %>% rename(dst.ID=ID, dst.x=x, dst.y=y), by=join_by(dst==compartment))
    LOG('observeEvent(input$graphNodes) called, updating nodes...')
    # Node - we're forcing a new order on the ids here
    # Maybe in future we should ask the user at this point to order their compartments
    # then we can continue creating the model with the correct ordering
    nodeID_to_compartmentID <- seq_along(input$graphNodes) %>% set_names(names(input$graphNodes))
    nodes <- map_df(seq_along(input$graphNodes),
                    function(nodeID) {
                      node = input$graphNodes[[nodeID]]
                      tibble(id=nodeID,
                             label=node$label,
                             x=node$x,
                             y=node$y)
                    }) |>
      as.data.frame()
    LOG('observeEvent(input$graphNodes) updating edges...')
    edges <- map_df(input$graphEdges,
                    function(edge) {
                      tibble(from=nodeID_to_compartmentID[edge$from],
                           to=nodeID_to_compartmentID[edge$to],
                           label=edge$label)
                    }) |>
      as.data.frame()
    LOG('observeEvent(input$graphNodes) updating cm...')
    cmNew <- updateCMFromVN(cm = cm(), vn = visNetwork(nodes, edges))
    cm(cmNew)
  })
  # bindEvent({input$codeVisNetwork_graphChange}, {
  #   print(1)
  #   visNetworkProxy("network_proxy_nodes")
  # })
  
  ### >initialConditions Reorder
  observeEvent(input$updateCompartmentOrder, {
    showModal(modalDialog(
      title = "Reorder Compartments",
      # Drag and drop using sortable
      rank_list(
        input_id = "sortable",
        text="Set order",
        labels = names(cm()$initialConditions),
        orientation = "horizontal"
      ),
      # To make this work we also need to update the initial conditions table
      # and the visualisation of the model
      footer = tagList(
        modalButton("Cancel"),
        actionButton("acceptCompartmentOrder", "Accept")
      )
    ))
  })
  
  observeEvent(input$acceptCompartmentOrder, {
    LOG('observeEvent(input$acceptCompartmentOrder) called')
    cmNew <- cm()
    compartments <- input$sortable
    cmNew$tbTransitions$src <- fct_relevel(cmNew$tbTransitions$src, compartments)
    cmNew$tbTransitions$dst <- fct_relevel(cmNew$tbTransitions$dst, compartments)
    cmNew$initialConditions <- cmNew$initialConditions[compartments]
    cm(cmNew)
    removeModal()
  })
  
  ### >initialConditionsTable ####
  output$initialConditionsTable <- renderDT({
    LOG('renderDT(initialConditionsTable) called')
    mod <- cm()
    ics <- mod$initialConditions
    tibble(Compartment=names(ics), Value=unname(ics)) %>%
      datatable(editable = list(target = "cell", disable = list(columns = 0)),
                options = list(searching = FALSE,
                               paging = FALSE,
                               info = FALSE,
                               ordering = FALSE),
                selection = "none",
                # better theming for the table
                class = 'compact cell-border stripe',
                rownames = FALSE)
  })
  
  observeEvent(input$initialConditionsTable_cell_edit, {
    info = input$initialConditionsTable_cell_edit
    i = info$row
    j = info$col+1
    v = info$value
    tbICs <- cm()$initialConditions
    tbICs[i] <- as.numeric(v)
    cmNew <- cm()
    cmNew$initialConditions <- tbICs
    cm(cmNew)
  })
  
  ## 3. Model Code ####
  ## 4. Parameter Values ####
  modelParameters <- reactiveVal()
  output$parameterTable <- renderDT({
    LOG('renderDT(parameterTable) called')
    modelParameters() %>%
      datatable(editable = list(target = "cell", disable = list(columns = 0)), rownames = FALSE)
  })
  # When user edits the parameterTable, update the model
  observeEvent(input$parameterTable_cell_edit, {
    LOG('observeEvent(parameterTable_cell_edit) called')
    info = input$parameterTable_cell_edit
    str(info)
    i = info$row
    j = info$col+1
    v = info$value
    LOG('observeEvent(parameterTable_cell_edit) updating model...')
    tbParameters <- modelParameters()
    tryCatch({
      tbParameters[i,j] <- as.numeric(v)
      cmNew <- cm()
      cmNew$parameters <- pull(tbParameters, value, name = parameter)
      cm(cmNew)
      modelParameters(tbParameters)
    }, error = function(e) {
      LOG('observeEvent(parameterTable_cell_edit) caught error')
      LOG(e)
      showNotification("Invalid value", type = "error")
    })
    LOG('observeEvent(parameterTable_cell_edit) updated model...')
  })
  ## 5. Model Output ####

  output$modelOutput <- renderPlot({
    
    req(input$runModelBtn)
    LOG('observeEvent(runModelBtn) called')
    mod <- cm()
    print("IC")
    print(mod$initialConditions)
    print("Rates")
    print(makeRatesFunction(mod))
    print("Parameters")
    print(mod$parameters)
    print("Times")
    print(input$timesteps)
    print("Simulate & plot")
    plot(mod)
  })
  ## 6. Project Download ####
  output$downloadProject <- downloadHandler(
    filename = function() {
      paste0("project-", Sys.Date(), ".zip")
    },
    contentType = "application/zip",
    content = function(file) {
      LOG('downloadProject() called')
      mod <- cm()
      # Create a temporary directory, save files to it, zip it and send the result
      with_tempdir(
        clean = TRUE,
        pattern = 'shweshweModel',
        code = {
          # We need .Rproj, model.R, shweshwe.rds and README.md
          # .Rproj
          LOG('* Writing .Rproj')
          projectName <- "myModel" # TODO: Let user specify project name
          fnameProject <- file.path(getwd(), paste0(projectName,".Rproj"))
          txtProject <- paste0("Project: ",projectName)
          writeLines(txtProject, fnameProject)
          
          # model.R
          cm2R <- function(cm) {
            modelTimestepsText <- input$modelTimestepsText # FIXME: There is no validation done on input$modelTimestepsText
            txt <- paste0(c(
              "library(deSolve)\n",
              "library(tidyverse)\n",
              "\n",
              "rates <- ", makeRatesFunction(cm),
              "\n",
              "runModel <- function(timesteps, initialConditions, parameters) {\n",
              "  mod <- ode(y = initialConditions, times = timesteps, func = rates, parms = parameters) |>\n",
              "    as.data.frame() |>\n",
              "    as_tibble() |>\n",
              '    tidyr::pivot_longer(cols = !time, names_to = "compartment", values_to = "population") |>\n',
              '    dplyr::mutate(compartment = factor(compartment, levels=names(initialConditions)))\n',
              "  return(mod)\n",
              "}\n",
              "\n",
              modelTimestepsText,
              "\n",
              "plotModel <- function(mod) {\n",
              "               ggplot2::ggplot(mod) +\n",
              "                 ggplot2::aes(x = time, y = population, color = compartment) +\n",
              "                 ggplot2::geom_line() +\n",
              '                 labs(title = "Compartmental Model",\n',
              '                      color = "Compartment",\n',
              '                      x = "Time",\n',
              '                      y = "Population")\n',
              "}\n",
              "\n",
              "# Example usage\n",
              "\n",
              "initialConditions <- c(",
              paste0(names(cm$initialConditions),'=',cm$initialConditions,collapse=", "),
              ")\n",
              "parameters <- c(",
              paste0(names(cm$parameters),'=',cm$parameters,collapse=", "),
              ")\n",
              "mod <- runModel(timesteps, initialConditions, parameters)\n",
              "plotModel(mod)\n"
            ), collapse = "")
            return(txt)
          }
          LOG("* Writing model.R")
          fnameModel <- file.path(getwd(), "model.R")
          txtModel <- cm2R(mod)
          writeLines(txtModel, fnameModel)
          
          # shweshwe.rds
          LOG("* Writing shweshwe.rds")
          fnameShweshwe <- file.path(getwd(), "shweshwe.rds")
          list(cm=mod,
               modelParameters=modelParameters()) |>
            saveRDS(file = fnameShweshwe)
          
          # README.md
          LOG("* Writing README.md")
          fnameReadme <- file.path(getwd(), "README.md")
          txtReadme <- paste0(c(
            "# Project: ", projectName, "\n",
            "\n",
            "This project was created using the shweshwe app.\n",
            "\n",
            "## Files\n",
            "\n",
            "- `model.R`: R script containing the model code and example usage\n",
            "- `shweshwe.rds`: RDS file containing the model and model parameters\n",
            "- `README.md`: This file\n"
          ), collapse = "")
          writeLines(txtReadme, fnameReadme)
          
          # Zip the files
          LOG("* Zipping files")
          zip(file, files = map_chr(c(fnameProject, fnameModel, fnameShweshwe, fnameReadme),
                                    basename))
        }
      )
    }
  )
}

shinyApp(ui, server)