# load packages
library(shiny)

ui <- fluidPage(
    titlePanel("WATLAS Utilities App"),

    fluidRow(
        column(3,
               plotOutput("patch_map")
        ),
        column(3,
               plotOutput("resTime_time")
        ),
        column(3,
               tableOutput("patchSummary"))
    ),
    hr(),

    fluidRow(
        column(2,
               h4("Patch construction parameters"),
               fileInput("revfile", p("recurse data")),
               fileInput("htfile", p("tide data")),
        ),
        column(3,
               h4("inferResidence: inferring residence points"),
               numericInput("infResTime",
                            p("res time inferred pts (min)"),
                            value = 2.0),
               numericInput("infPatchTimeDiff",
                            p("min time gap missing data (min)"),
                            value = 2.0),
               numericInput("infPatchSpatDiff",
                            p("max spatial gap missing data (min)"),
                            value = 2.0)
        ),
        column(3,
               h4("classifyPath: classify as stationary"),
               numericInput("resTimeLimit",
                            p("residence time limit (min)"),
                            value = 2.0),
               textInput("resTimeCol",
                         p("residence time column"),
                         value = "resTime"),
               numericInput("travelSeg",
                            p("travel segment length (fixes)"),
                            value = 5.0),
        ),
        column(3,
               h4("getPatches: construct residence patches"),
               numericInput("bufferSize",
                            p("spatial buffer size (m)"),
                            value = 10.0),
               numericInput("tempIndepLimit",
                            p("temp indep limit (s)"),
                            value = 1800),
               numericInput("spatIndepLimit",
                            p("spat indep limit (m)"),
                            value = 100.0),
               actionButton(inputId = "go",
                            label = "Nuke data")
        )
    )
)
