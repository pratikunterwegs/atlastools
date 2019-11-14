# server func
library(glue)
library(ggplot2)
library(data.table)
library(RColorBrewer)
options(scipen=1000)

server <- function(input, output) {

  # get data

  # read in the data
  recdata <- reactive({
    print(input$revfile$datapath)
    data <- data.table::fread(input$revfile$datapath)
    return(data)
    })


  htdata <-  reactive({
    print(input$htfile$datapath)
    data <- data.table::fread(input$htfile$datapath)
    return(data)
    })

  # print the selected bird for now to check that it works
  output$everything <- renderPlot(
    {
      # get data
      recdata <- recdata()
      htdata <- htdata()

      # run the inference func
      inference_output <-
        funcInferResidence(
          revdata = recdata,
          htdata = htdata,
          infResTime = input$infResTime,
          infPatchTimeDiff = input$infPatchTimeDiff,
          infPatchSpatDiff = input$infPatchSpatDiff)

      # run the classification func
      classified_output <-
        funcClassifyPath(
          somedata = inference_output,
          restimeCol = input$resTimeCol,
          resTimeLimit = input$resTimeLimit,
          travelSeg = input$travelSeg
        )

      # run patch construction
      patch_output <-
       funcGetResPatch(
          somedata = classified_output,
          bufferSize = input$bufferSize,
          spatIndepLim = input$spatIndepLimit,
          tempIndepLim = input$tempIndepLimit,
          makeSf = "TRUE"
        )

      # get patch outlines
      patch_outline_output <-
        funcGetPatchData(
          resPatchData = patch_output,
          dataColumn = "data",
          whichData = "spatial"
        )

      # get classified points
      patch_classified_output <-
        funcGetPatchData(
          resPatchData = patch_output,
          dataColumn = "data",
          whichData = "points"
        )

      return(
        ggplot()+
          geom_sf(data = patch_outline_output,
                  aes(fill = factor(patch)),
                  alpha = 0.7, col = 'transparent')+
          # geom_sf(data = patch_traj, col = "red", size = 0.2)+
          # geom_text(aes(x_mean, y_mean, label = patch))+
          facet_wrap(~tidalcycle, ncol = 1, labeller = label_both)+
          scale_fill_manual(values = pals::kovesi.rainbow(14))+
          ggthemes::theme_few()+
          labs(x = "long", y = "lat", fill = "patch",
               title = "patches in space")
      )

    }
    # render table ends
  )
}

# ends here

