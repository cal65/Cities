library(shiny)
library(ggplot2)
library(leaflet)
library(ggmap)
library(ggrepel)
library(raster)
library(RColorBrewer)

E <- read.csv('TotalSet.csv')
ui <- fluidPage(
	title = "Urban Clusters",
	fixedRow(
	column(12,
		titlePanel(h1("Urban Clusters", align = "center")),
		fixedRow(
			column(3,
				sliderInput(inputId = "size", label = "Choose a size", value = 5, min = 1, max = 10, width='200px')
				), 
			column(9,
				leafletOutput("map", width = "80%", height = "500px") 
				)
			)
		)
	)
)

server <- function(input, output, session) {
	pal<-brewer.pal(6, 'Set1')
	output$map <- renderLeaflet( {
		leaflet(E) %>% addProviderTiles("CartoDB.Positron") %>%
		setView(0, 0, zoom = 3) 
		})
		#reactive({
		#	pal <- colorBin(input$Pal, domain = openrice_data$color_type, n=7)
		#})
					
	observe({	
		
		proxy <- leafletProxy("map", data=E)
		proxy %>% clearShapes() %>% addCircles(lng = ~long, lat = ~lat, radius = ~(45*as.numeric(input$size))^2, color='black', weight=1,  fillOpacity=0.7, popup = ~City) 
	})	
	
	# observe({
		# proxy <- leafletProxy("map", data= E)
		# #Remove any existing legend
		# proxy %>% clearControls() %>% addLegend("topright", pal=pal, title= "Urban Clusters", opacity=1)
	# })
}
shinyApp(ui = ui, server = server)
