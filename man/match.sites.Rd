\name{match.sites}
\alias{match.sites}
\title{Match two site data frames}
\description{
This function take two data frames with standardized plot id, coordinate, and date columns, and compares time and location for an approximate match. Used, for example, to match established site locations of soil pedons to independently acquired sites with vegetation plot data. Both sets of data require GPS coordinates in decimal degrees. The return data frame has a "link" column with the plot id from the closest matching established site.

}
\usage{
set.site(x, y, maxdist = 30, maxdays = NA)
}
\arguments{
\item{x}{Target site data frame.}
\item{y}{Established site data frame.}
\item{maxdist}{Maximum distance acceptable for a match (default is 30 meters).}
\item{maxdays}{Maximum number of days between inventory dates among the two site data frames.}
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

z = match.sites(x=x, y=y)
}
