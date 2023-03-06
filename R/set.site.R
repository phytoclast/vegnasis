#This function takes inputs for plot id, latitude, longitude, and dates to create a simple data frame representing site time and locations.

set.site <- function(plot, lat, lon, date = NA_integer_, year = NA_integer_, month = NA_integer_, day = NA_integer_){
  x <- data.frame(plot = plot,
                  lat = lat,
                  lon = lon,
                  date = date,
                  year = year,
                  month = month,
                  day = day
  )
  x <- x |> mutate(date = as.Date(ifelse(is.na(date),as.character(as.Date(ISOdate(year = year,
                                                                                  month = month,
                                                                                  day = day))),as.character(date)))
                   ,
                   # year = ifelse(is.na(year), as.integer(format(date, format="%Y")),as.integer(year)),
                   # month = ifelse(is.na(month), as.integer(format(date, format="%m")),as.integer(month)),
                   # day = ifelse(is.na(day), as.integer(format(date, format="%d")),as.integer(day)),
                   year = NULL,
                   month = NULL,
                   day = NULL)
  return(x)
}


#This function take two data frames with standardized plot id, coordinate, and date columns, and compares time and location for an approximate match. Used, for example, to match established site locations of soil pedons to independently acquired sites with vegetation plot data. Both sets of data require GPS coordinates in decimal degrees.
match.sites <- function(x,y,maxdist = 30, maxdays = NA){

  xplots <- x$plot
  x$link <- NA_character_

  for(i in 1:length(xplots)){#i=500
    yf <- y
    if(!is.na(maxdays)){
      yf <- subset(yf, abs(date - x[i,]$date)<=maxdays)
    }

    if(nrow(yf) > 0){
      yf$dist <- (((x[i,]$lat - yf$lat)/360*40041.47*1000)^2 +
                    ((x[i,]$lon - yf$lon)/360*40041.47*1000*cos(x[i,]$lat/2/360*2*3.141592))^2)^0.5

      mindist <- min(yf$dist, na.rm = TRUE)
      y0 <- yf %>% subset(dist %in% mindist)
      x[i,] <- x[i,] |> mutate(link = ifelse(mindist <= maxdist,  y0$plot[1], link))
    }
  }
  return(x)
}
