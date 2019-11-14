# server func
library(glue)
library(ggplot2)
library(data.table)
library(pals)

server <- function(input, output) {

  #### general data handling ####
  dataOut <- eventReactive(input$go,{
    # reads in data
    revdata <- data.table::fread(input$revfile$datapath)
    htdata <- data.table::fread(input$htfile$datapath)

    # run the inference func
    inference_output <-
      funcInferResidence(
        revdata = revdata,
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

    return(patch_output)
  })

  #### patch summary ####
  output$patchSummary <- renderTable(
    {
      patchSummary <- sf::st_drop_geometry(funcGetPatchData(resPatchData = dataOut(),
                                            dataColumn = "data",
                                            whichData = "spatial"))
      return(patchSummary)
    }
  )

  #### patches map plot ####
  output$patch_map <- renderPlot(
    {

      # get patch outlines
      {patch_outline_output <-
        funcGetPatchData(
          resPatchData = dataOut(),
          dataColumn = "data",
          whichData = "spatial"
        )}

      return(
        ggplot()+
          geom_sf(data = patch_outline_output,
                  aes(fill = factor(patch)),
                  alpha = 0.7, col = 'transparent')+
          #geom_sf(data = patch_traj, col = "red", size = 0.2)+
          facet_wrap(~tidalcycle, ncol = 1, labeller = label_both)+
          scale_fill_manual(values = pals::kovesi.rainbow(max(patch_outline_output$patch)))+
          ggthemes::theme_few()+
          theme(axis.text = element_blank())+
          labs(x = "long", y = "lat", fill = "patch")
      )

    }, res = 150, width = 400, height = 400)

  #### restime time plot ####
  output$resTime_time <- renderPlot({
    # get patch outlines
    {patch_outline_output <-
      funcGetPatchData(
        resPatchData = dataOut(),
        dataColumn = "data",
        whichData = "spatial"
      )}

    return(
      ggplot()+
        geom_sf(data = patch_outline_output,
                aes(fill = factor(patch)),
                alpha = 0.7, col = 'grey90')+
        #geom_sf(data = patch_traj, col = "red", size = 0.2)+
        facet_wrap(~tidalcycle, ncol = 1, labeller = label_both)+
        scale_fill_manual(values = pals::kovesi.rainbow(max(patch_outline_output$patch)))+
        ggthemes::theme_few()+
        theme(axis.text = element_blank())+
        labs(x = "long", y = "lat", fill = "patch")
    )

  }, res = 150, width = 400, height = 400)
}

# ends here

