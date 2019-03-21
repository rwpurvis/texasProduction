library(shiny)
library(rvest)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(readr)
library(pdftools)
library(zoo)
library(plotly)
library(scales)
library(maps)
library(sp)
library(rgdal)
library(mapdata)
library(maptools)
library(rgeos)
library(broom)

# Backend Startup Definitions

# Helper functions
getRRClinks <- function (){
  #' Generate a list of links referencing the monthly crude oil production PDFs produced by the Texas RRC.
  #' 
  #' @return A vector of link strings
  url <- 'https://www.rrc.texas.gov/oil-gas/research-and-statistics/production-data/monthly-crude-oil-production-by-district-and-field/'
  page <- read_html(url)
  links <- page %>% html_nodes("a") %>% html_attr("href")
  links <- links[str_detect(links,'own423')]
  links <- paste0("https://www.rrc.texas.gov",links)
  return(links)
}

readRRCpdf <- function (link) {
  #' Convert PDF text output to a list containing the date and table values
  #' 
  #' @param link an address of an RRC production record PDF
  #' @return A list containing the date and a vector of lines for the table
  
  # Extract 2nd page of pdf - finalized production
  reportText <- pdf_text(link)[2]
  reportText <- lapply(strsplit(reportText,'\n'),tolower) %>% unlist
  
  # Find the document date
  months <- format(ISOdate(2018,1:12,1),"%B")%>%tolower
  orMonths <- paste0(months,collapse="|")
  docmonth <- str_extract(reportText[1:7],pattern = paste0("(",orMonths,"),\\s\\d{4}"))
  docmonth <- docmonth[!is.na(docmonth)][1]
  date <- zoo::as.yearmon(docmonth,format = "%B, %Y")%>%as.Date()
  
  # Extract data from the table
  tablebounds <- which(str_detect(reportText,pattern="-----"))
  colnames <- reportText[tablebounds[1]-1]%>%str_split("\\s+")%>%unlist()
  tableData <- reportText[(tablebounds[1]+1):(tablebounds[2]-1)]
  
  return(list("date" = date,"data" = tableData))
}

createRRCdataFrame <- function(inputList) {
  #' Creates a dataframe from the output of readRRCpdf
  #'
  #' @param inputList$date The date of the extracted table
  #' @param inputList$data A vector of line strings for the table
  #' @return A dataframe of production data
  
  date <- inputList$date
  tableData <- inputList$data
  
  # Max # of spaces between columns
  spaceThreshold <- 15
  # Detect blank spaces - threshold value can be adjusted if blanks are skipped
  tableDataList <- tableData%>%str_split(paste0("\\s{1,",spaceThreshold,"}"))
  
  # Dealing with missing values errors - matrix should have 15 columns
  if (any(lapply(tableDataList,length)<15)) {
    for (i in which(lapply(tableDataList,length)<15)) {
      #Find the longest whitespace
      whiteSpaceLengths <- str_locate_all(tableData[i],"\\s+")[[1]]
      whiteSpaceLengths <- whiteSpaceLengths[,2]-whiteSpaceLengths[,1]+1
      whiteSpaceMaxIndex <- which.max(whiteSpaceLengths)
      #Assume the following column is NA, move values to the right
      tableDataList[[i]][length(tableDataList[[i]])+1] <- NA
      tableDataList[[i]][(whiteSpaceMaxIndex+2):length(tableDataList[[i]])] <- tableDataList[[i]][(whiteSpaceMaxIndex+1):(length(tableDataList[[i]])-1)]
      tableDataList[[i]][whiteSpaceMaxIndex+1] <- NA
    }
    
  }
  
  
  tableData <- matrix(unlist(tableDataList),ncol = length(tableDataList[[1]]),byrow = TRUE)
  # Erroneous column
  tableData <- tableData[,1:14]
  # Get rid of commas
  tableData <- gsub(tableData,pattern = ",",replacement = "")
  
  manualColNames <- c("rrcDist",
                      "nLeaseRep",
                      "nLeaseDelq",
                      "nWellFlow",
                      "nWellOther",
                      "bblAllowMonth",
                      "bblAllowDelqMonth",
                      "bblReportedMonth",
                      "bblReportedDay",
                      "pctUnderProduced",
                      "pipeline",
                      "trucks",
                      "otherTrans",
                      "bblClosingStock")
  
  # Create dataframe
  df <- as.data.frame(tableData,stringsAsFactors = FALSE)
  colnames(df) <- manualColNames
  # Convert/clean values
  df[,2:14] <- sapply(df[,2:14],as.numeric)
  
  
  # Add date, clean District names
  df <- df %>% mutate(date = date,
                      rrcDist = gsub(rrcDist,pattern = "^0",replacement = ""))
  
  # District 6e? Combine with 6
  district6s <- df %>% filter(rrcDist %in% c("6","6e")) %>%
    select(-rrcDist) %>% group_by(date) %>%
    summarize_all(.funs = sum) %>% mutate(rrcDist = "6",pctUnderProduced = ((1-bblReportedMonth/bblAllowMonth)*100)%>%round(2))
  df <- rbind(df %>% filter(!(rrcDist %in% c("6","6e"))),
              district6s) %>% arrange(rrcDist)
  
  # Fix district order
  df$rrcDist <- factor(df$rrcDist,ordered = TRUE,levels = c('1','2','3','4','5','6','7b','7c','8','8a','9','10'))
  
  # Get totals - similar to extract data above
  ###### Commented out due to typos found in RRC source PDFs
  # totals <- y2[tablebounds[2]+1]
  # totals <- totals%>%str_split("\\s{1,15}")
  # totals <- matrix(unlist(totals),ncol = length(totals[[1]]),byrow = TRUE)
  # totals <- matrix(totals[1,1:14],nrow=1)
  # totals <- gsub(totals,pattern = ",",replacement = "")
  # totalsDf <- as.data.frame(totals,stringsAsFactors = FALSE)
  # colnames(totalsDf) <- manualColNames
  # totalsDf[,2:14] <- sapply(totalsDf[,2:14],as.numeric)
  # totalsDf <- totalsDf %>% mutate(date = date)
  # 
  # #Check totals == summed columns
  # manualTotals <- df %>% summarize_all(.funs = sum)
  
  return(df)
}

# Generate data
# links <- getRRClinks()
# fullData <- do.call(rbind,lapply(lapply(links,readRRCpdf),createRRCdataFrame))
# write_csv(fullData,"texasProduction/fullData.csv")

# Read so I don't have to scrape every time
fullData <- read_csv("fullData.csv")
fullData$rrcDist <- factor(fullData$rrcDist,ordered = TRUE,levels = c('1','2','3','4','5','6','7b','7c','8','8a','9','10'))
# Map data
rrcDistricts <- read_csv("counties.csv") # RRC district table from:
# https://www.rrc.state.tx.us/about-us/organization-activities/rrc-locations/counties-by-dist/
# Some typos fixed manually
rrcDistricts <- rrcDistricts %>% mutate(County = tolower(County),DC = tolower(DC))

createRRCdistrictMap <- function(districtTable) {
  #' Creates a ggplot-friendly dataframe containing polygon coordinates for the Texas
  #' RRC districts. Utilizes county map from the maps package, and combines according to 
  #' the rrcDistricts list.
  #' Based on this StackOverflow post with tweaks:
  #' https://stackoverflow.com/questions/43174769/r-aggregate-county-map-polygons-to-create-custom-borders
  #' 
  #' @param districtTable A Dataframe containing the list of Texas counties and their dist.
  #' @return A dataframe with polygon coordinates corresponding with Texas RRC districts

  county <- maps::map("county",fill = TRUE)
  
  
  county.sp <- map2SpatialPolygons(county, IDs = as.factor(county$names),
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  # Add information data of the polygons
  region <- sapply(strsplit(county$names, ","), function(x) x[1])
  subregion <- sapply(strsplit(county$names, ","), function(x) x[2])
  subregion[is.na(subregion)] <- region[is.na(subregion)]
  
  county.sp.data <- SpatialPolygonsDataFrame(
    county.sp, 
    data = data.frame(names = county$names,
                      region = region,
                      subregion = subregion),
    match.ID = "names")
  
  # Subset texas
  texas.county.sp.data <- county.sp.data
  texas.county.sp.data@polygons <- texas.county.sp.data@polygons[county.sp.data@data$region == 'texas']
  texas.county.sp.data@data <- texas.county.sp.data@data[county.sp.data@data$region == 'texas',]
  
  # Replace region with texas RRC district
  texas.county.dist.df <- data.frame("County" = as.character(texas.county.sp.data@data$subregion),
                                     stringsAsFactors = FALSE) %>%
    # Clean up galveston county entries
    mutate(County = ifelse(str_detect(County,"galveston"),"galveston",County)) %>% 
    left_join(districtTable%>%select(County,DC),by="County")
  texas.new.region <- factor(texas.county.dist.df$DC)
  
  texas.county.sp.data.dist <- texas.county.sp.data
  texas.county.sp.data.dist@data$region <- texas.new.region
  
  # Because of topology problems
  county.sp.data.buffer <- gBuffer(texas.county.sp.data.dist, byid = TRUE, width = 0)
  # Merge polygons according to region
  county.region <-gUnaryUnion(county.sp.data.buffer, 
                              id = county.sp.data.buffer@data$region)
  
  county.tidy <- suppressWarnings(tidy(county.region))
  
  return(county.tidy)
}

texasRRCmap <- createRRCdistrictMap(rrcDistricts)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Monthly Oil Production in Texas"),
   h4("by Texas Railroad Commission District"),
   

   fluidRow(
     column(6,
            plotlyOutput("linePlot")),
     column(6,
            plotlyOutput("choropleth")
            ),
     br(),
     fluidRow(
       column(12,
              sliderInput("selectedDate",label = "",
                 min = min(fullData$date),
                 max = max(fullData$date),
                 value=as.Date("2016-12-01"),
                 timeFormat="%Y-%b",
                 width = '90%'),
              align="center"
       )
     )
   ),
   hr(),
   DT::dataTableOutput("productionTable"),
   hr(),
   h5('Data Source: ',span(a(href="https://www.rrc.texas.gov/oil-gas/research-and-statistics/production-data/monthly-crude-oil-production-by-district-and-field/","Texas RRC")
  ),
  h5('Created by Richard Purvis')
  )
)

server <- function(input, output) {
   
  output$choropleth <- renderPlotly({
    bpColors <- c("#009B00","#98CE00","#ffff00","#FFFFFF")
    selectedDate <- input$selectedDate%>%as.yearmon%>%as.Date
    dataForDate <- fullData %>% filter(date == selectedDate)
    
    chorData <- dataForDate %>% left_join(texasRRCmap,by = c("rrcDist" = "id"))
    
    p <- ggplot() + geom_polygon(data = chorData, 
                                 aes(x=long,y=lat,
                                     group = group,fill=bblReportedMonth,
                                     text = sprintf("District: %s<br>District Monthly Production:%s<br>%s",
                                                    rrcDist,comma(bblReportedMonth),format(date,"%b %Y"))),
                                 col='black') + 
      # coord_fixed(1.3) + # Broken for plotly
      theme_void() + 
      theme(legend.position = "none") +
      scale_fill_gradient2(low = bpColors[4],mid = bpColors[3],high = bpColors[1])
    # p
    ggplotly(p, tooltip = "text")
  })
   
  output$linePlot <- renderPlotly({
    selectedDate <- input$selectedDate%>%as.yearmon%>%as.Date
    
    l <- fullData %>% ggplot(aes(x = date, y = bblReportedMonth, col = rrcDist,
                                 text = sprintf("District: %s<br>%s<br>District Monthly Production:%s<br>",rrcDist,format(date,"%B %Y"),comma(bblReportedMonth)),
                                 group = rrcDist
    )) + 
      geom_line() + geom_point(data = fullData %>% filter(date==selectedDate),aes(x = date, y = bblReportedMonth),col='red') + 
      scale_y_continuous(label = unit_format(scale = 1e-6,unit = NULL)) + 
      labs(col = "District") + xlab("Date") + ylab("Monthly Reported Production (MMbbl)")
    ggplotly(l, tooltip = "text")
   })
  
  output$productionTable <- DT::renderDataTable({selectedDate <- input$selectedDate%>%as.yearmon%>%as.Date
                                        dataForDate <- fullData %>% filter(date == selectedDate)
                                        dataForDate},
                                        options = list(paging = FALSE,searching = FALSE,scrollX='400px'),
                                        colnames = c("RRC District",
                                                     "Num. Reported Leases",
                                                     "Num. Delq. Leases",
                                                     "Reported Wells Flowing",
                                                     "Reported Wells Other",
                                                     "Total Allowed Monthly (bbl)",
                                                     "Delq. Allowed Monthly (bbl)",
                                                     "Reported Monthly Production (bbl)",
                                                     "Reported Daily Production (bbl)",
                                                     "Percent Underproduced",
                                                     "Pipeline Disposition (bbl)",
                                                     "Truck Disposition (bbl)",
                                                     "Other Disposition (bbl)",
                                                     "Closing Stock (bbl)",
                                                     "Date")
                                        )
}

# Run the application 
shinyApp(ui = ui, server = server)

