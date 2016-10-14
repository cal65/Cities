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
				#place slider on side panel to adjust size of cities
				), 
			column(9,
				leafletOutput("map", width = "80%", height = "500px") 
				)
			)
		)
	)
)

server <- function(input, output, session) {
	pal<-brewer.pal(4, 'Set1')
	output$map <- renderLeaflet( {
		leaflet(E) %>% addProviderTiles("CartoDB.Positron") %>%
		setView(0, 0, zoom = 2) 
		})
					
	observe({	
		
		proxy <- leafletProxy("map", data=E)
		proxy %>% clearShapes() %>% addCircles(lng = ~long, lat = ~lat, radius = ~(65*as.numeric(input$size))^2, color=~pal[Cluster], weight=1,  fillOpacity=0.7, popup = ~paste(City,  paste("University Attainment %", Educated, sep=': '), paste("Foreign Born %", Foreign_born, sep=': '), paste("Metro Usage %", Usage, sep=': '),  paste("Number of Comedy Clubs", Number_of_comedy, sep=': '), paste("Number of Restaurants", Number_of_restaurants, sep=': '), paste("Number of Starbucks", Starbucks, sep=': '), sep='<br/>')) 
	})	
	
	observe({
		proxy <- leafletProxy("map", data=E)
		#Remove any existing legend
		proxy %>% clearControls() %>% addLegend("topright", pal=colorFactor(pal, domain=E$Cluster), values= ~Cluster, title= "Urban Clusters", labels=c('Asian', 'European', 'American', 'Other'), opacity=1)
	})
}
shinyApp(ui = ui, server = server)

#rsconnect::deployApp('~/Documents/CAL/Real_Life/Cities')