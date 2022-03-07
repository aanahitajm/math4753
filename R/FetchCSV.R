
#' FetchCSV
#'
#' @param path string that represents directory path
#' @importFrom utils read.table
#' @return Returns a data table using data frame
#' @export
#'
#'
#' @examples
#'    \dontrun{df = FetchCSV("/path/to/file.csv")}
FetchCSV <- function(path) {
  df=read.table(path ,header=TRUE,sep=",")
}


