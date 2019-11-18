# ui function
library(shiny)
library(shinythemes)

ui <- fluidPage(
  titlePanel("WATLAS Utilities App"),

  fluidRow(
    column(5,
      h4("Residence patches"),
      # textOutput("this_map_label"),
      plotOutput("patch_map", height = "600px")
      ),

    column(3,
     h4("Residence time ~ time"),
     # textOutput("this_map_label"),
     plotOutput("resTime_time", height = "300px", width = "800px"),
     h4("Patch summary"),
     tableOutput("patchSummary")
     )
    ),
  hr(),

  fluidRow(
    column(2,
     h4("Patch construction data"),
     fileInput("revfile", p("Recurse data")),
     fileInput("htfile", p("Tide data")),
     actionButton(inputId = "go", label = "Run")
     ),

    column(2,
     # h4("inferResidence: inferring residence points"),
     numericInput("infResTime",
      p("res time inferred pts (min)"),
      value = 2.0),
     numericInput("infPatchTimeDiff",
      p("min time gap missing data (s)"),
      value = 1800.0),
     numericInput("infPatchSpatDiff",
      p("max spatial gap missing data (m)"),
      value = 100.0)
     ),

    column(2,
     # h4("classifyPath: classify as stationary"),
     numericInput("resTimeLimit",
      p("residence time limit (min)"),
      value = 2.0),
     textInput("resTimeCol",
       p("residence time column"),
       value = "resTime"),
     numericInput("travelSeg",
      p("travel segment length (fixes)"),
      value = 5.0)
     ),

    column(2,
     # h4("getPatches: construct residence patches"),
     numericInput("bufferSize",
      p("spatial buffer size (m)"),
      value = 10.0),
     numericInput("tempIndepLimit",
      p("temp indep limit (s)"),
      value = 1800),
     numericInput("spatIndepLimit",
      p("spat indep limit (m)"),
      value = 100.0)
     )
    )
  )

# ends here
