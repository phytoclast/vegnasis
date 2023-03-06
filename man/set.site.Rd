\name{set.site}
\alias{set.site}
\title{Set site coordinates and date columns}
\description{
This function takes inputs for plot id, latitude, longitude, and dates to create a simple data frame representing site time and locations.

}
\usage{
set.site(plot, lat, lon, date = NA, year = NA, month = NA, day = NA)
}
\arguments{
\item{plot}{Plot ids.}
\item{lat}{Latitude or y coordinate.}
\item{lon}{Longitude or x coordinate.}
\item{date}{Date with YYY-MM-DD format (optional if year, month, and day are separate).}
\item{year}{Year if date is missing.}
\item{month}{Month (integers 1-12) if date is missing.}
\item{day}{Day of the month if date is missing.}
}


\examples{
x <-vegnasis::obs
y <- soilDB::get_site_data_from_NASIS_db(SS=F)

x <- set.site(plot=x$Observation_ID,
              lat = x$Latitude,
              lon = x$Longitude,
              year = x$Year,
              month = x$Mon,
              day= x$Day)
y <- set.site(plot=y$site_id,
              lat = y$y,
              lon = y$x,
              date = y$obs_date)

}
