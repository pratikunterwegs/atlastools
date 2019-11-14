# server func
library(glue)

server <- function(input, output) {
	output$selected_var <- renderText({
		paste("selected col", input$resTimeCol)
	})  
}