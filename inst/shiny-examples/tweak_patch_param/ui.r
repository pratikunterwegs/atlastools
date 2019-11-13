# load packages
library(shiny)

ui <- fluidPage(
    titlePanel("WATLAS Utilities App"),

    sidebarLayout(
        sidebarPanel(
            h1("Function parameters"),
            fileInput("file", p("recurse data")),
            fileInput("file", p("tide data")),
            p("inferResidence"),
            numericInput("resTimeLimit",
                p("residence time limit"),
                value = 2.0),
            p("classifyPath"),
            numericInput("resTimeLimit",
                p("residence time limit"),
                value = 2.0),
            textInput("resTimeCol",
                p("residence time column"),
                value = "resTime"),
            numericInput("travelSeg",
                p("travel segment length"),
                value = 5.0),
            p("getPatches"),
            submitButton("Nuke data")
            ),

        mainPanel(
            h1("Function output"),
            p("This is some function output"),
            p("Residence time ~ time"),
            p("Residence patches")
            )
        )

    )
