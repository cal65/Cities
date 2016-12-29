setwd('/Users/christopherlee/Documents/CAL/Real_Life/Cities/')
library(shiny)
library(ggplot2)
library(leaflet)
library(ggmap)
library(ggrepel)
library(raster)
library(RColorBrewer)

E <- read.csv('TotalSet.csv', stringsAsFactors=F)
combo <- read.csv('Data/top_combo3.csv')
ui <- fluidPage(
	title = "Urban Clustering",
	fixedRow(
	column(12,
		titlePanel(h1("Urban Clustering", align = "center")),
		fixedRow(
			column(3,
				sliderInput(inputId = "size", label = "Choose a size", value = 5, min = 1, max = 10, width='200px'),
				#place slider on side panel to adjust size of cities
				selectInput(inputId = 'city', label = 'City', choices=E$City, width='200px', selected='Mumbai'),
				plotOutput('dens', height = "400px")
				), 
			column(9,
				leafletOutput("map", width = "80%", height = "550px") 
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
		cluster_text_options = list(c("University Attainment %", "Foreign Born %", "Metro Usage %", "Number of Comedy Clubs", "Number of Restaurants", "Number of Starbucks"), c("University Attainment %", "Foreign Born %", "Metro Usage %", "GDP", "Number of Restaurants", "Working Age %"))
		proxy <- leafletProxy("map", data=E)
		proxy %>% clearShapes() %>% addCircles(lng = ~long, lat = ~lat, radius = ~(65*as.numeric(input$size))^2, color=~pal[Cluster], weight=1,  fillOpacity=0.7, layerId = ~City, popup = ~paste(City,  paste("University Attainment %", sprintf("%.2f", Educated), sep=': '), paste("Foreign Born %", sprintf("%.2f", Foreign_born), sep=': '), paste("Metro Usage %", Usage, sep=': '),  paste("Number of Comedy Clubs", Number_of_comedy, sep=': '), paste("Number of Restaurants", Number_of_restaurants, sep=': '), paste("Number of Starbucks", Starbucks, sep=': '), sep='<br/>')) 
	})	
	
	observe({
		proxy <- leafletProxy("map", data=E)
		#Remove any existing legend
		proxy %>% clearControls() %>% addLegend("topright", colors=pal, title= "Urban Clusters", labels=c('Asian', 'European', 'American', 'Other'), opacity=1)
	})
	
	# density_data <- reactiveValues(clickedMarker=NULL)
	observeEvent(input$Map_marker_click,{ 
		p <- input$map_marker_click
		if(!is.null(p$id)){
			if(is.null(input$city)) updateSelectInput(session, "city", selected=p$id)
			if(!is.null(input$city) && input$city!=p$id) updateSelectInput(session, "city", selected=p$id)
	}
})

	observe({
		output$dens <- renderPlot({
			event <- input$map_shape_click		
			ggplot(combo) + geom_density(aes(Educated), fill='darkslateblue') + xlim(0,1) + facet_grid(cluster ~ .) + ggtitle(paste('Data', input$city, sep=' - '))
		})
	})
}
shinyApp(ui = ui, server = server)

#rsconnect::deployApp('~/Documents/CAL/Real_Life/Cities')