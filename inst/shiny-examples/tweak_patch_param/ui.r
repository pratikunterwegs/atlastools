# load packages
library(shiny)

ui <- fluidPage(
    titlePanel("WATLAS Utilities App"),

    sidebarLayout(
        sidebarPanel(
            h1("Function parameters"),
            fileInput("revfile", p("recurse data")),
            fileInput("htfile", p("tide data")),

            
            p("inferResidence: inferring residence points"),
            numericInput("infResTime",
                p("res time inferred pts (min)"),
                value = 2.0),
            p("classifyPath: classify as stationary"),
            numericInput("resTimeLimit",
                p("residence time limit (min)"),
                value = 2.0),
            textInput("resTimeCol",
                p("residence time column"),
                value = "resTime"),
            numericInput("travelSeg",
                p("travel segment length (fixes)"),
                value = 5.0),
            p("getPatches: construct residence patches"),
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
            ),

        mainPanel(
            textOutput("selected_id")
            )
        )

    )
