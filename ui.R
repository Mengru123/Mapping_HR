# check the mapping table using shiny -------------
# load packages
library(shiny)
library(tmap)
library(rgdal)
library(leaflet)
library(dplyr)

# load data
mapping_table<-mapping_table<-read.csv("Data/map.table.hr.2016.census.csv", stringsAsFactors = FALSE)
hr13.shp <- readOGR(dsn = "Data", layer = "HRP000b11a_e_Oct2013", encoding = "UTF-8", stringsAsFactors = FALSE)
csd16.shp <- readOGR(dsn = "Data", layer = "lcsd000a16a_e", encoding = "UTF-8", stringsAsFactors = FALSE)
da16.shp <- readOGR(dsn = "Data", layer = "lda_000b16a_e", encoding = "UTF-8", stringsAsFactors = FALSE)

# define UI
ui <- fluidPage(titlePanel("2013 Health Region Mapping to 2016 census boundary Visualization"), 
                # select hr to map
                wellPanel(
                    selectInput(inputId = "type", 
                                label = "select by code or name",
                                choices = c("HR_code", "HR_nom"),
                                selected = "HR_code"),
                    uiOutput("option1")),
                
                mainPanel(
                    fluidRow(
                        column ("view in map", width = 10,leafletOutput(outputId = "plot")),
                        column ("View in table", width = 2,tableOutput("table"))
                    )
                )
                
)


# define the server code
server <- function(input, output){
    output$option1 <- renderUI({
        selectInput(inputId = "option",
                    label = paste("Select", input$type),
                    choices = mapping_table[[input$type]][order(mapping_table[[input$type]])])
    })
    output$plot <- renderLeaflet({
        # find the matching census region
        csd <- as.character(mapping_table$census_id[mapping_table[[input$type]] == input$option & mapping_table$census_type == "CSD"])
        da <- as.character(mapping_table$census_id[mapping_table[[input$type]] == input$option & mapping_table$census_type == "DA"])
        
        if (length(csd) >=1){
            plot1 <- tm_shape(hr13.shp[hr13.shp$HR_UID == input$option,]) +
                tm_polygons(border.col = "black", alpha = 0.1, lwd=1) +
                tm_shape(csd16.shp[csd16.shp$CSDUID %in% c(csd),])+
                tm_polygons(border.col = "red", alpha = 0.2, lwd=2) 
        }
        
        if (length(da) >=1){
            plot1 <- tm_shape(hr13.shp[hr13.shp$HR_UID == input$option,])+
                tm_polygons(border.col = "black", alpha = 0.1, lwd=1) +
                tm_shape(da16.shp[da16.shp$DAUID %in% c(da),])+
                tm_polygons(border.col = "red", alpha = 0.2, lwd=2) 
        }
        
        # if else exist 
        if( exists("plot1") )
        {
            tmap_leaflet(plot1)
        }
    })
    output$table <- renderTable({mapping_table[mapping_table[[input$type]] == input$option,2:ncol(mapping_table)]})
}

#return a shiny app object
shinyApp (ui = ui, server = server)
