# server func
library(glue)
library(data.table)

server <- function(input, output) {
	
	datalist <- eventReactive(input$go, {
		revdata <- data.table::fread(input$revfile)
		htdata <- data.table::fread(input$htfile)

		list(revdata, htdata)

	})

	bird_id <- names(datalist)

	output$selected_id <- renderText({
		paste("selected bird = ", bird_id)
	})  
}