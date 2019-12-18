# ui function
library(leaflet)
library(shiny)
library(watlasUtils)

ui <- fluidPage(
  titlePanel("WATLAS Utilities App"),

  fluidRow(
    column(5,
           h4("Residence patches"),
           # textOutput("this_map_label"),
           leafletOutput("patch_map", height = "600px")
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
           fileInput("datafile", p("Movement data")),
           actionButton(inputId = "go", label = "Run")
    ),

    column(2,
           h4("inferResidence: inferring residence points"),
           numericInput("infPatchTimeDiff",
                        p("min time gap missing data (min)"),
                        value = 30.0),
           numericInput("infPatchSpatDiff",
                        p("max spatial gap missing data (m)"),
                        value = 100.0)
    ),

    column(2,
           h4("classifyPath: classify as stationary"),
           numericInput("resTimeLimit",
                        p("residence time limit (min)"),
                        value = 10.0)
    ),

    column(2,
           h4("getPatches: basic patch filters"),
           numericInput("bufferSize",
                        p("spatial buffer size (m)"),
                        value = 10.0),
           numericInput("minfixes",
                        p("min. fixes in patch (#)"),
                        value = 3),
           numericInput("lim1",
                        p("tide limit 1 (hrs)"),
                        value = 4),
           numericInput("lim2",
                        p("tide limit 2 (hrs)"),
                        value = 10)
    ),
    column(2,
           h4("getPatches: patch independence"),
           numericInput("tempIndepLimit",
                        p("temp indep limit (min)"),
                        value = 30),
           numericInput("spatIndepLimit",
                        p("spat indep limit (m)"),
                        value = 100.0),
           numericInput("restIndepLimit",
                        p("res. time indep limit (min)"),
                        value = 30.0)
    )
  )
)

# ends here
