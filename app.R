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
		  tags$head(
      tags$style(type='text/css', 
                 ".nav-tabs {font-size: 5px} ")),

		fixedRow(
			column(3,
				sliderInput(inputId = "size", label = "Choose a size", value = 5, min = 1, max = 10, width='250px'),
				#place slider on side panel to adjust size of cities
				selectInput(inputId = 'city', label = 'City', choices=E$City, width='250px', selected='Mumbai'),
				tabsetPanel(
					tabPanel("Education", plotOutput('dens', height = "360px", width="250px")),
					tabPanel("Metro Usage", size='5', plotOutput('metro', height="360px", width="250px")),
					tabPanel("Number of Cinemas", plotOutput('cinema', height="360px", width="250px")),  
					tabPanel("Number of Restaurants", plotOutput('restaurants', height="360px", width="250px")),  
					tabPanel("Working Age Proportion", plotOutput('working', height="360px", width="250px")),  
					tabPanel("Service Firms/Capita", plotOutput('service', height="360px", width="250px")),
					style='width: 300px')
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
		cluster_text_options = list(c("University Attainment %", "Foreign Born %", "Metro Usage %", "Number of Comedy Clubs", "Number of Restaurants", "Number of Starbucks"), c("University Attainment %", "Foreign Born %", "Metro Usage %", "GDP", "Number of Restaurants", "Working Age %"))
		proxy <- leafletProxy("map", data=E)
		proxy %>% clearShapes() %>% addCircles(lng = ~long, lat = ~lat, radius = ~(65*as.numeric(input$size))^2, color=~pal[Cluster], weight=1,  fillOpacity=0.7, layerId = ~City, popup = ~paste(City,  paste("University Attainment %", sprintf("%.2f", Educated), sep=': '), paste("Foreign Born %", sprintf("%.2f", Foreign_born), sep=': '), paste("Metro Usage %", Usage, sep=': '),  paste("Number of Comedy Clubs", Number_of_comedy, sep=': '), paste("Number of Restaurants", Number_of_restaurants, sep=': '), paste("Number of Starbucks", Starbucks, sep=': '), sep='<br/>')) 
	})	
	
	observe({
		proxy <- leafletProxy("map", data=E)
		#Remove any existing legend
		proxy %>% clearControls() %>% addLegend("topright", colors=pal, title= "Urban Clusters", labels=c('Asian', 'European', 'Other', 'American'), opacity=1)
		#addLegend("topright", pal=colorFactor(pal, domain=E$Cluster), values= ~Cluster, title= "Urban Clusters", labels=c('Asian', 'European', 'American', 'Other'), opacity=1)
	})
	
	# density_data <- reactiveValues(clickedMarker=NULL)
	observeEvent(input$map_shape_click,{ 
		p <- input$map_shape_click
		if(!is.null(p$id)){
			if(is.null(input$city)) updateSelectInput(session, "city", selected=p$id)
			if(!is.null(input$city) && input$city!=p$id) updateSelectInput(session, "city", selected=p$id)
	}
})

	observe({
		output$dens <- renderPlot({
			event <- input$map_shape_click
			clicked_subset <- subset(combo, City==input$city)
			#xplot is helpful to find value in density plot
			xplot = ifelse(!is.na(clicked_subset$Educated),clicked_subset$Educated, 0)
			ggplot(combo) + geom_density(aes(Educated), fill='darkslateblue') + xlim(0,1) + facet_grid(cluster ~ .) + ggtitle(paste('Data', input$city, sep=' - ')) + 
			#add an orange line that shows the data of the selected city
			geom_segment(data= clicked_subset, aes(x= xplot, xend= xplot, y=0,
			#call density function to determine the y-value 
			yend = density(subset(combo, cluster == clicked_subset$cluster)$Educated, na.rm=T, from = xplot, to = xplot, n=1)$y), color='orange')
		})
	})
	
	observe({
		output$metro <- renderPlot({
			event <- input$map_shape_click
			clicked_subset <- subset(combo, City==input$city)
			#avoid missing data crashing the plot
			xplot = ifelse(!is.na(clicked_subset$Usage),clicked_subset$Usage, 0)
			ggplot(combo) + geom_density(aes(Usage), fill='darkslateblue') + xlim(0,1) + facet_grid(cluster ~ .) + ggtitle(paste('Data', input$city, sep=' - ')) + geom_segment(data= clicked_subset, aes(x= xplot, xend= xplot,
			y=0, yend = density(subset(combo, cluster == clicked_subset$cluster)$Usage, na.rm=T, from = xplot, to = xplot, n=1)$y), color='orange')
		})		
	})
	#density plot for cinema
	observe({
		output$cinema <- renderPlot({
			event <- input$map_shape_click
			clicked_subset <- subset(combo, City==input$city)
			xplot = ifelse(!is.na(clicked_subset$Cinemas),clicked_subset$Cinemas, 0)
			ggplot(combo) + geom_density(aes(Cinemas), fill='darkslateblue') + facet_grid(cluster ~ .) + ggtitle(paste('Data', input$city, sep=' - ')) + geom_segment(data= clicked_subset, aes(x= xplot, xend= xplot,
			y=0, yend = density(subset(combo, cluster == clicked_subset$cluster)$Cinemas, na.rm=T, from = xplot, to = xplot, n=1)$y), color='orange')
		})		
	})
	
	#density plot for restaurants
	observe({
		output$restaurants <- renderPlot({
			event <- input$map_shape_click
			clicked_subset <- subset(combo, City==input$city)
			xplot = ifelse(!is.na(clicked_subset$Number_of_restaurants),clicked_subset$Number_of_restaurants, 0)
			ggplot(combo) + geom_density(aes(Number_of_restaurants), fill='darkslateblue') + facet_grid(cluster ~ .) + ggtitle(paste('Data', input$city, sep=' - ')) + geom_segment(data= clicked_subset, aes(x= xplot, xend= xplot,
			y=0, yend = density(subset(combo, cluster == clicked_subset$cluster)$Number_of_restaurants, na.rm=T, from = xplot, to = xplot, n=1)$y), color='orange')
		})		
	})	
	#density plot for working age pop
		observe({
		output$working <- renderPlot({
			event <- input$map_shape_click
			clicked_subset <- subset(combo, City==input$city)
			xplot = ifelse(!is.na(clicked_subset$Working_age),clicked_subset$Working_age, 0)
			ggplot(combo) + geom_density(aes(Working_age), fill='darkslateblue') + facet_grid(cluster ~ .) + ggtitle(paste('Data', input$city, sep=' - ')) + geom_segment(data= clicked_subset, aes(x= xplot, xend= xplot,
			y=0, yend = density(subset(combo, cluster == clicked_subset$cluster)$Working_age, na.rm=T, from = xplot, to = xplot, n=1)$y), color='orange')
		})		
	})	
	#density plot for service firms
		observe({
		output$service <- renderPlot({
			event <- input$map_shape_click
			clicked_subset <- subset(combo, City==input$city)
			xplot = ifelse(!is.na(clicked_subset$service.firms),clicked_subset$service.firms, 0)
			ggplot(combo) + geom_density(aes(service.firms), fill='darkslateblue') + facet_grid(cluster ~ .) + ggtitle(paste('Data', input$city, sep=' - ')) + geom_segment(data= clicked_subset, aes(x= xplot, xend= xplot,
			y=0, yend = density(subset(combo, cluster == clicked_subset$cluster)$service.firms, na.rm=T, from = xplot, to = xplot, n=1)$y), color='orange')
		})		
	})
}
shinyApp(ui = ui, server = server)

#rsconnect::deployApp('~/Documents/CAL/Real_Life/Cities')