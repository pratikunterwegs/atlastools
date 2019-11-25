# server func
library(glue)
library(ggplot2)
library(data.table)
library(plotly)

server <- function(input, output) {

  #### general data handling ####
  dataOut <- eventReactive(input$go, {
    # reads in data
    revdata <- data.table::fread(input$revfile$datapath)
    htdata <- data.table::fread(input$htfile$datapath)

    # run the inference func
    inference_output <-
      funcInferResidence(
        revdata = revdata,
        htdata = htdata,
        infResTime = input$restIndepLimit,
        infPatchTimeDiff = input$infPatchTimeDiff,
        infPatchSpatDiff = input$infPatchSpatDiff)

    # run the classification func
    classified_output <-
      funcClassifyPath(
        somedata = inference_output,
        resTimeLimit = input$resTimeLimit
      )

    # run patch construction
    patch_output <-
      funcGetResPatch(
        somedata = classified_output,
        bufferSize = input$bufferSize,
        spatIndepLim = input$spatIndepLimit,
        tempIndepLim = input$tempIndepLimit,
        restIndepLim = input$restIndepLimit,
        minFixes = input$minfixes,
        tideLims = c(input$lim1, input$lim2)
      )

    return(patch_output)
  })

  #### raw data ####
  dataRaw <- eventReactive(input$go, {
    # reads in data
    revdata <- data.table::fread(input$revfile$datapath)
  })

  ### patch summary ####
  output$patchSummary <- renderTable(
    {
      patchSummary <- sf::st_drop_geometry(funcGetPatchData(resPatchData = dataOut(),
                                                            dataColumn = "data",
                                                            whichData = "spatial"))

      patchSummary <- dplyr::mutate(patchSummary, duration = duration/60)

      patchSummary <- dplyr::select(patchSummary,
                                    id, tidalcycle, patch,
                                    type,
                                    tidaltime_mean,
                                    duration,
                                    distInPatch,
                                    distBwPatch,
                                    nfixes,
                                    area,
                                    circularity)
      return(patchSummary)
    })

  #### patches map plot ####
  output$this_map_label <- renderText(
    {paste("bird tag id = ", unique((dataOut())$id),
           "tidal cycle = ", unique((dataOut())$tidalcycle))}
  )

  output$patch_map <- renderPlotly(
    {
      # get patch outlines
      patchSummary <- funcGetPatchData(resPatchData = dataOut(),
                                       dataColumn = "data",
                                       whichData = "spatial")
      # get trajectories
      {
        patchtraj <- funcPatchTraj(df = patchSummary)
      }
      # get points
      {
        patchdata <- funcGetPatchData(resPatchData = dataOut(),
                                      dataColumn = "data",
                                      whichData = "points")
      }
      # make plot
      {
        map_plot <- 
        ggplot()+
          
          geom_path(data = dataRaw(), aes(x,y), col = "grey60", 
                     size = 0.1, alpha = 0.3)+
          geom_point(data = dataRaw(), aes(x,y), col = "grey20", 
                     size = 0.2, shape = 4, alpha = 0.3)+
          geom_sf(data = patchSummary,
                  aes(geometry = polygons, fill = patch),
                  alpha = 0.5, col = 'black', lwd = 0.1)+

          geom_sf(data = patchtraj, col = "black", size = 0.3)+
          scale_fill_distiller(palette = "Spectral", na.value = "grey")+
          theme_bw()+
          theme(axis.text = element_blank(),
                axis.title = element_text(size = rel(0.5)),
                legend.title = element_text(size = rel(0.5)),
                legend.text = element_text(size = rel(0.5)),
                legend.position = "bottom",
                legend.key.height = unit(0.05, "cm"),
                plot.title = element_text(size = rel(0.5)),
                panel.grid = element_blank())+
          labs(x = "long", y = "lat", fill = "patch",
               title = paste("bird tag = ",
                             unique((dataRaw())$id),
                             "tidal cycle = ",
                             unique((dataRaw())$tidalcycle)))
      }

      return(
        ggplotly(map_plot)  
      )

    }
    )

  ### restime time plot ####
  output$resTime_time <- renderPlot(
    {
      # get patch points and join to raw data
      {
        patch_point_data <- funcGetPatchData(
          resPatchData = dataOut(),
          dataColumn = "data",
          whichData = "points")

        patch_point_data <- (dataRaw())

        # patch_point_data <- dplyr::filter(patch_point_data, type != "inferred")

        # patch_point_data <- dplyr::arrange(patch_point_data, time)
      }
      # get patch summary for vert lines
      {
        # get patch outlines
        patchSummary <- funcGetPatchData(resPatchData = dataOut(),
                                         dataColumn = "data",
                                         whichData = "spatial")

        patchSummary <- sf::st_drop_geometry(patchSummary)
      }
      # make plot
      {
        

        plot1 <- ggplot()+
          geom_hline(yintercept = input$resTimeLimit, col = 2, lty = 2)+
          geom_rect(data = patchSummary, aes(xmin = time_start, xmax = time_end,
            ymin = 0, ymax = max(patch_point_data$resTime), fill = patch), alpha = 0.2)+
          geom_line(data = patch_point_data,
                    aes(time, resTime, group = tidalcycle), col = "grey50", size = 0.1)+
          geom_point(data = patch_point_data,
                     aes(time, resTime),
                     alpha = 0.2, size = 0.2)+
          scale_x_time(labels = scales::time_format(format = "%Y-%m-%d\n %H:%M"))+

          geom_label(data = patchSummary, aes(time_mean, max(patch_point_data$resTime), label = patch))+
          geom_vline(data = patchSummary, aes(xintercept = time_end), col = 2, lty = 3, size = 0.2)+
          geom_vline(data = patchSummary, aes(xintercept = time_start), col = 4, lty = 3, size = 0.2)+

          # scale_color_manual(values = somecolours, na.value = "grey")+
          scale_fill_distiller(palette = "Spectral",na.value = "grey")+
          theme_bw()+
          ylim(0, max(patch_point_data$resTime))+
          theme(legend.position = 'none',
                axis.title = element_text(size = rel(0.6)),
                panel.grid = element_blank())+
          labs(x = "time", y = "raw (mins)", col = "patch")
      }

      return((plot1))

    }, res = 100
    )
}
# ends here
