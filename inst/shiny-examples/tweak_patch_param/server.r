# server func
library(glue)
library(ggplot2)
library(data.table)
library(pals)

server <- function(input, output) {

  #### general data handling ####
  dataOut <- eventReactive(input$go,
                           {
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
      patchSummary <- dplyr::select(patchSummary,
                                    id, tidalcycle, patch,
                                    type,
                                    tidaltime_mean,
                                    distInPatch,
                                    distBwPatch,
                                    propfixes,
                                    area)
      return(patchSummary)
    })

  #### patches map plot ####
  output$map_label <- renderText(
    paste("bird tag id = ", unique((dataOut())$id),
          "tidal cycle = ", unique((dataOut())$tidalcycle)))

  output$patch_map <- renderPlot(
    {
      # get patch outlines
      {
        patch_outline_output <- funcGetPatchData(
          resPatchData = dataOut(),
          dataColumn = "data",
          whichData = "spatial")
      }
      # get trajectories
      {
        patchtraj <- funcPatchTraj(df = patch_outline_output)
      }
      # # get points
      # {
      #   patchdata <- funcGetPatchData(resPatchData = dataOut(),
      #                                 dataColumn = "data",
      #                                 whichData = "points")
      # }

      return(
        ggplot()+
          geom_sf(data = patch_outline_output,
                  aes(fill = (patch)),
                  alpha = 0.7, col = 'transparent')+
          # geom_point(data = patchdata,
          #            aes(x, y), size = 0.1, alpha = 0.5)+
          geom_sf(data = patchtraj, col = "black", size = 0.2)+
          scale_fill_gradientn(colours = pals::kovesi.rainbow(max(patch_outline_output$patch)))+
          ggthemes::theme_few()+
          theme(axis.text = element_blank(),
                axis.title = element_text(size = rel(0.5)),
                legend.title = element_text(size = rel(0.5)),
                legend.text = element_text(size = rel(0.5)),
                legend.position = "bottom",
                legend.key.width = unit(0.05, "cm"))+
          labs(x = "long", y = "lat", fill = "patch")
      )

    }, res = 150)

  #### restime time plot ####
  output$resTime_time <- renderPlot(
    {
    # get patch outlines
    {
      patch_summary_data <- funcGetPatchData(
        resPatchData = dataOut(),
        dataColumn = "data",
        whichData = "points")
    }

    return(
      ggplot()+
        #geom_hline(yintercept = 2, col = 2)+
        geom_line(data = patch_summary_data,
                  aes(time, resTime, group = tidalcycle), col = "grey", size = 0.3)+
        geom_point(data = patch_summary_data,
                   aes(time, resTime, shape = type, col = factor(patch)),
                   alpha = 0.2)+
        # facet_wrap(~tidalcycle, ncol = 1, scales = "free_x",
        #            labeller = "label_both")+
        scale_x_time(labels = scales::time_format(format = "%Y-%m-%d\n %H:%M"))+
        # geom_text(aes(time_mean, 100, label = patch))+
        # geom_vline(aes(xintercept = time_end), lty = 3, size = 0.2)+
        scale_color_manual(values = pals::kovesi.rainbow(max(patch_summary_data$patch)))+
        ggthemes::theme_few()+
        theme(legend.position = 'none')+
        labs(x = "time", y = "residence time (mins)", col = "patch")
    )

  }, res = 100)
}

# ends here
