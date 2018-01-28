#'Read data
#'
#'This is a function that will read a csv file and transform it into a
#'data frame tbl (using \code{dplyr::tbl_df(data)}) only if the pathway
#'given is valid (\code{!file.exists(filename)})
#'
#'@param filename A character string giving the pathway to the data file
#'
#'@return This function returns a data frame called data with a file given.
#'
#'@examples
#' \dontrun{
#' data_2014<-fars_read("accident_2014.csv.bz2")
#'}
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#'Create filename
#'
#'What this function does is to add the year to a filepath so that it now
#'refers to the data of that year and print the new filepath.
#'
#'@param year A string character or number with the wanted year of the data
#'
#'@return The filepath correspondent to the data of that year.
#'
#'@examples
#'make_filename("2017")
#'make_filename(1999)
#'
#'@export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("./data/accident_%d.csv.bz2", year)
}

#'Read data of a given year
#'
#'When given some years this function will try to read the data of those years
#'and if there is a not valid year it will throw a warning.
#'
#'@param years A list of years
#'
#'@return A file with the correspondent data of those years.
#'
#'@examples
#'\dontrun{
#'my_data <- fars_read_years(1999:2003)}
#'
#'@importFrom dplyr mutate
#'@importFrom dplyr select
#'@importFrom magrittr "%>%"
#'
#'@export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#'Summarize the values
#'
#'Just a function which will create a table with the years as the rows and the
#'number of values for each month as the new value for each pair year/month
#'
#'@param years A list with all the year to examine
#'
#'@return A table with years as rows and months as columns
#'
#'@examples
#' \dontrun{
#' again_data<-fars_summarize_years(2010:2015)
#'}
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom magrittr "%>%"
#'
#'@export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#'Represent the data
#'
#'A function that will plot in a map the point correspondents to the most affected
#'places, or a message if there are no accidents.
#'
#'@param state.num An integer that represent a state
#'@param year An integer of the wanted year
#'@inheritParams make_filename
#'
#'@return A map with relevant points.
#'
#'@importFrom dplyr filter
#'@importFrom maps map
#'@importFrom graphics points
#'
#'@examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' fars_map_state(30, 2015)}
#'
#'@export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

