#' **fars_read**
#'
#' This function reads in data using the readr package. *Requires the **readr** package*
#' or the function will fail.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param *filename*  full name of the file containing data to be imported into R session
#'
#' @return Returns a data.frame object of the class tbl_df (also known as a *tibble*--
#' the structure central data of  tidyverse packages.  If the file is not located, will return an error
#' stating that *filename* does not exist.
#'
#' @note a vector of multiple filenames can be queried.  Output combines files into one tibble.
#'
#' @examples
#' \dontrun{
#' data.13 <- fars_read("accident_2013.csv.bz2")
#' creates a tibble from the file  "accident_2013.csv.bz2"
#'}
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
#'
#'
#'
#' **make_filename**
#'
#' Simple function to create a filename of a compressed csv FARS file for a specified year, with a .bz2 extension.
#'
#' @param *year*  a 4-digit numeric number for a specied data year from US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System
#'
#' @return outputs a character value of "accident_*year*.csv.bz2"
#'
#' @examples
#' make.filename(2013)
#' outputs "accident_2013.csv.bz2"
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}
#'
#'
#'
#' **fars_read_years**
#'
#' Function to read compressed FARS csv files for a range of years and provides a month (1-12) and 4-digit year for each line item
#' included in each file.  *Requires **mutate** and **select** from the dplyr package*
#'
#' @param *years*  vector of 4-digit numeric numbers for a specied data years from US National Highway Traffic
#' Safety Administration'sFatality Analysis Reporting System
#'
#' @return if the file exists, creates a list consisting of a 2-column tibble for each queried year, with month (1-12)
#' and 4-digit year for each row data in the queried file.  If the file is not located, will include a NULL value
#' for that year
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#'}
#'
#' @export
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
#'
#'
#'
#' **fars_summarize_years**
#'
#' Function to summarize results from *fars_read_years* function.  *Requires **group_by** and **summarize** from dplyr* and
#' *Requires **group_by** and **summarize** from dplyr* and spread from **tidyr** packages*
#'
#' @importFrom dplyr group_by summarize
#'
#' @importFrom tidyr spread
#'
#' @importFrom magrittr "%>%"
#'
#' @param *years*  a numeric vector of 4-digit numbers for specified data years from US National Highway Traffic
#' Safety Administration'sFatality Analysis Reporting System
#'
#' @return Creates a tibble summarizing the total number of accidents reported by month and year
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2015)
#'}
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}
#'
#'
#'
#' **fars_map_state**
#'
#' Function to plot fatality locations for a specified year and state. *Requires **map** from maps, **points** from graphics,
#' and **filter** from dplyr packages*.
#'
#' @importFrom maps map
#'
#' @importFrom graphics points
#'
#' @importFrom dplyr filter
#'
#' @param *state.num*  number representing a US state as coded by the US National Highway Traffic
#' Safety Administration'sFatality Analysis Reporting System
#'
#' @param *year*  vector of 4-digit numeric numbers for a specied data years from US National Highway Traffic
#' Safety Administration's Fatality Analysis Reporting System
#'
#' @return if data is available for the queried state and year, opens a  plot window showing the state outline
#' and location of reported accidents across the year.  If no data, will return warning.
#'
#' @examples
#' \dontrun{
#' fars_map_state(16, 2013)
#' }
#'  where 16=Idaho and 2013 is the queried year
#'
#' @export
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
