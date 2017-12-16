#' Modify cell options
#' 
#' Provides options for modifying the cells and the display of their values
#' 
#' @param digits \emph{integer} (Optional) indicating the number of decimal places to be used by \link{round} for 'label'.
#' @param cellnotes \emph{numeric} (Optional) matrix of the same dimensions as \code{x} that has the human-readable version of each value, for displaying to the user on hover. If \code{NULL}, then \code{x} will be coerced using \code{\link{as.character}}.
#' If missing, it will use \code{x}, after rounding it based on the \code{digits} parameter.
#' @param scale \emph{logical} (default is FALSE). If cellnote is missing and x is used, 
#' should cellnotes be scaled if x is also scaled?
#' @param row.label \emph{character} (Optional) Label to display next to the row value when the user hovers over the cell.
#'  If not specified, trys to match the \code{xaxis_title}; if no axis title, defaults to "Row".
#' @param col.label \emph{character} (Optional) Label to display next to the column value when the user hovers over the cell
#'  If not specified, trys to match the \code{yaxis_title}; if no axis title, defaults to "Col".
#' @param value.label \emph{character} (Optional) Label to display next to the cell value when the user hovers over the cell.
#'  Defaults to "Value".
#' @param print \emph{logical} Show the values inside the cells. Defatuls to \code{FALSE}.
#' 
#' @export
hmCells <- function(heatmap
  , digits = 3L
  , cellnotes
  , scale = FALSE
  , row.label = NULL
  , col.label = NULL
  , value.label = "Value"
  , print = FALSE
  , brush.color = "#0000FF"
) {
  if(missing(heatmap)) {
		message("hmLegend: no heatmap provided... returning NULL")
		return(NULL)
	}
  
	opts <- list(
    cellnote_row = row.label
    , cellnote_col = col.label
    , cellnote_val = value.label
    , print_values = print
    , brush_color = brush.color
	)

  heatmap$x$options <- mergeLists(heatmap$x$options, opts)
	return(heatmap)  
}
