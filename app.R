# Require the package
if (!require(devtools)) {
  install.packages("devtools")
  library(devtools)
}

if (!require(leaflet.extras)) {
  devtools::install_github('rstudio/leaflet')
  devtools::install_github('bhaskarvk/leaflet.extras')
  library(leaflet.extras)
}
if (!require(shiny)) {
  install.packages("shiny")
  library(shiny, character.only=T)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(shinydashboard)) {
  install.packages("shinydashboard")
  library(shinydashboard)
}

if (!require(rgdal)) {
  install.packages("rgdal")
  library(rgdal)
}

if (!require(plyr)) {
  install.packages("plyr")
  library(plyr)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(reshape2)) {
  install.packages("reshape2")
  library(reshape2)
}

if (!require(treemapify)) {
  devtools::install_github("wilkox/ggfittext")
  devtools::install_github("wilkox/treemapify")
  library(treemapify)
}

# Read the data
sydData <- read.csv("Sydney/listings_Sydney.csv")
sydData <- sydData[sydData$availability_365>0,]
sydMap <- readOGR("Sydney/neighbourhoods/neighbourhoods.shp")
melData <- read.csv("Melbourne/listings_Melbourne.csv")
melData <- melData[melData$availability_365>0,]
melMap <- readOGR("Melbourne/neighbourhoods/neighbourhoods.shp")

# Calculate the mean price of each neighbourhood and count the number of listings
melListNum <- ddply(melData,.(neighbourhood),summarize,price = mean(price, na.rm = TRUE),number=length(neighbourhood))
sydListNum <- ddply(sydData,.(neighbourhood),summarize,price = mean(price, na.rm = TRUE),number=length(neighbourhood))
# Rename the column 1
names(sydListNum)[1] <- "neighbourh"
names(melListNum)[1] <- "neighbourh"

# Count the number of listing by different room type
mel <- ddply(melData, .(room_type), summarise, number = length(id))
syd <- ddply(sydData, .(room_type), summarise, number = length(id))

# Combine the shape file with the price
melRates <- merge(melMap, melListNum, by="neighbourh")
sydRates <- merge(sydMap, sydListNum, by="neighbourh")

# Prepare the color for the leaflet
bins <- c(0,100, 200, 400, 500, 1000, 1500, 2000, 3000, 4000)
# Color for choropleth map
melCpal <- colorBin("YlOrRd", domain = melRates$number, bins = bins)
sydCpal <- colorBin("YlOrRd", domain = melRates$number, bins = bins)
# Color for dot density map
melfactpal <- colorFactor(topo.colors(3), melData$room_type)
sydfactpal <- colorFactor(topo.colors(3), sydData$room_type)

# Average price for different room type
melRoomType <- ddply(melData,.(neighbourhood, room_type),summarize,price = mean(price, na.rm = TRUE),number=length(neighbourhood))
sydRoomType <- ddply(sydData, .(neighbourhood, room_type), summarise,price = mean(price, na.rm = TRUE), number=length(neighbourhood))


# Melbouren review score rating and reviews per month for each neighbourhood
melScore <- ddply(melData,.(neighbourhood, room_type), summarise, review_scores_rating = mean(review_scores_rating, na.rm = TRUE), reviews_per_month= mean(reviews_per_month, na.rm = TRUE))
# Exculde the missing value
melScore <- na.omit(melScore)
# combine two data frame by neighobourhood and room type
meltree <- merge(melScore,melRoomType,by=c("neighbourhood", "room_type"))


# Sydney review score rating and reviews per month for each neighbourhood
sydScore <- ddply(sydData, .(neighbourhood,room_type), summarise,review_scores_rating = mean(review_scores_rating, na.rm = TRUE), reviews_per_month= mean(reviews_per_month, na.rm = TRUE))
# Exculde the missing value
sydScore <- na.omit(sydScore)
# combine two data frame by neighobourhood and room type
sydtree <- merge(sydScore,sydRoomType,by=c("neighbourhood", "room_type"))


# Dot density map popup
melpop <- paste("ID", melData$id, ":", melData$price, "$/night")
sydpop <- paste("ID", sydData$id, ":", sydData$price, "$/night")

# Choropleth map label
melLabels <- sprintf(
  "<strong>%s</strong><br/>%g listings</sup><br/>averge price: %g$/night</sup>",
  melRates$neighbourh, melRates$number, melRates$price
) %>% lapply(htmltools::HTML)

sydLabels <- sprintf(
  "<strong>%s</strong><br/>%g listings</sup><br/>averge price: %g$/night</sup>",
  sydRates$neighbourh, sydRates$number, sydRates$price
) %>% lapply(htmltools::HTML)

# Choice list for check box
choices_list = list("Entire home/apt"  , 
                    "Private room", 
                    "Shared room")

# Shiny UI body
body <- dashboardBody(
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("map", height = 500)
           )
    ),
    column(width = 3,
           box(width = NULL, status = "warning",
               selectInput(
                 "map", h4("Choose a location: "),
                 c(
                   "Melbourne" = "mel",
                   "Sydney" = "syd"
                 )
               ),
               selectInput(
                 "type", h4("Choose a Map Type: "),
                 c(
                   "Choropleth Map" = "ch",
                   "Heat Map" = "heat",
                   "Dot Denisty Map" = "dot"
                 )
               ),
               checkboxGroupInput("checkType", 
                                  label = h4("Room Type for dot density map"), 
                                  choices = choices_list, selected = choices_list
               )
               
           )
  )
),
fluidRow(
  column(width = 9,
         box(width = NULL, solidHeader = TRUE,
             plotOutput("plot", height = 500)
         )
  ),
  column(width = 3,
         box(width = NULL, status = "warning", selectInput(
           "plot", h4("Choose a type of plot: "),
           c(
             "Number of listings" = "num",
             "Average price of listings" = "price",
             "Average room type price of listings" = "room_price",
             "Review scores rating of listings" = "score",
             "Reviews per month of listings" = 'reviews',
             "Compare price and review scores rating of neighbourhood" = 'tree'
           )
         ),
         checkboxGroupInput("checkKind", 
                            label = h4("Room Type"), 
                            choices = choices_list, selected = choices_list
         ),
         sliderInput("score",
                     "Range of Review Scores Rating:",
                     min = 1,  max = 100,  value = 1),
         sliderInput("reviews",
                     "Range of Reviews per month",
                     min = 0,  max = 4,  value = 0.2, step = 0.2)
         
         
         )
        
  )
)
)

# Shiny UI
ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "AirBnb in Melbourne and Sydney",
                  titleWidth = 350),
  dashboardSidebar(disable = TRUE)
  ,
  body
)

# Shiny server
server <- function(input,output,session) {
  #prepare the output map
  output$map <- renderLeaflet({
    if(input$map == "mel"){
      if(input$type == "ch") {
        # Choropleth map
        map <- leaflet(melRates)%>%addTiles()%>%addPolygons(
          fillColor = melCpal(melRates$number),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 2,
            color = "black",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = melLabels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))%>%addLegend(pal = melCpal, values = melRates$number, opacity = 0.7, title = "Number of Listings",
                                             position = "bottomright")
      } else if (input$type == "heat") {
        # Heat map
         map <- leaflet()%>%addProviderTiles(providers$Thunderforest.TransportDark)%>%setView(lat = -37.814, lng = 144.96332,zoom = 11)%>% addWebGLHeatmap(melData$longitude, melData$latitude, size = 500)
      } else if (input$type == "dot") {
        # Filter the data frame by input room type
        m <- melData[melData$room_type%in%input$checkType,]
        # Dot density map
        map <- leaflet(m) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
          setView(lat = -37.814, lng = 144.96332, zoom=11) %>% 
          addCircles(~longitude, ~latitude, weight = 3, radius=40, 
                     color=~sydfactpal(room_type),popup=sydpop ,stroke = TRUE, fillOpacity = 0.8) %>% 
          addLegend("bottomright", colors=topo.colors(3) , labels= c("Entire Home/Apt", "Private Room", "Shared Room"), title="Room Type Available")
        }
      
    } else if(input$map == "syd") {
        if(input$type == "ch"){
          # Choropleth map
          map <- leaflet(sydRates)%>%addTiles()%>%addPolygons(
            fillColor = sydCpal(sydRates$number),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
              weight = 2,
              color = "black",
              dashArray = "",
              fillOpacity = 0.7,
              bringToFront = TRUE),
            label = sydLabels,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"))%>%addLegend(pal = sydCpal, values = sydRates$number, opacity = 0.7, title = "Number of Listings",
                                               position = "bottomright")
        } else if(input$type == "heat"){
          # Heat map
          map <- leaflet()%>%addProviderTiles(providers$Thunderforest.TransportDark)%>%setView(lat = -33.8688, lng = 151.2093,zoom = 10)%>% addWebGLHeatmap(sydData$longitude, sydData$latitude, size = 800)
        } else if(input$type == "dot") {
          # Filter data frame by input room type
          m <- sydData[sydData$room_type%in%input$checkType,]
          # Dot density map
          map <- leaflet(m) %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
            setView(lat = -33.8688, lng = 151.2093,zoom = 10) %>% 
            addCircles(~longitude, ~latitude, weight = 3, radius=40, 
                       color=~sydfactpal(room_type),popup=sydpop ,stroke = TRUE, fillOpacity = 0.8) %>% 
            addLegend("bottomright", colors=topo.colors(3) , labels= c("Entire Home/Apt", "Private Room", "Shared Room"), title="Room Type Available")
        }
         
      
    }
    
  }) 
  # Prepare output plot
  output$plot <- renderPlot({
    # Filter the data frame by input location
    if(input$map == "mel") {
      s <- melScore
      r <- melRoomType
      t <- meltree
      n <- mel
      p <- melListNum
    } else if (input$map == "syd") {
      s <- sydScore
      r <- sydRoomType
      t <- sydtree
      n <- syd
      p <- sydListNum
    }
    
   if(input$plot == "num") {
     ggplot(data=n, aes(x=room_type, y=number, fill=room_type)) +
       geom_bar(colour="black", stat="identity")+ theme(axis.text.x  = element_text(angle=0, vjust=0.5))
   } else if (input$plot == "price") {
    
     ggplot(data=p, aes(x=neighbourh, y=price)) + geom_bar(colour="black", stat="identity")+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))
   } else if(input$plot == "room_price") {
     
     r <- r[r$room_type%in%input$checkKind,]
     ggplot(data=r, aes(x=neighbourhood, y=price, fill=room_type)) +
       geom_bar(colour="black", stat="identity")+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))
     
   } else if(input$plot == "score") {
    
     s <- s[s$room_type%in%input$checkKind,]
     s <- filter(s,  s$review_scores_rating <= input$score)
     ggplot(data=s, aes(x=neighbourhood, y=review_scores_rating, fill=room_type)) +
       geom_bar(colour="black", stat="identity")+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))
     
   } else if(input$plot == "reviews") {
     s <- s[s$room_type%in%input$checkKind,]
     s <- filter(s,  s$reviews_per_month <= input$reviews)
     ggplot(data=s, aes(x=neighbourhood, y=reviews_per_month, fill=room_type)) +
       geom_bar(colour="black", stat="identity")+ theme(axis.text.x  = element_text(angle=90, vjust=0.5))
     
   } else if(input$plot == "tree") {
     t <- t[t$room_type%in%input$checkKind,]
     # Convert the data frame to easily draw a treemap
     # treemap_coords <- treemapify(t, area = "review_scores_rating", fill = "price", group="room_type",label= "neighbourhood")
     # ggplotify(treemap_coords)+ labs(title="Price & Review Scores Rating")
     ggplot(t, aes(area = review_scores_rating, fill = price, label = neighbourhood,
                     subgroup = room_type)) +
       geom_treemap() +
       geom_treemap_subgroup_border() +
       geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                                    "black", fontface = "italic", min.size = 0) +
       geom_treemap_text(colour = "white", place = "topleft", reflow = T)
     
   }
    
  })
  
}
# Run shiny application
shinyApp(ui, server)

