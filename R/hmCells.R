#' Modify cell options
#' 
#' Provides options for modifying the cells and the display of their values
#' 
#' @param d3heatmap \emph{Required} A valid \emph{d3heatmap} object
#' 
#' @param digits \emph{integer} (Optional) indicating the number of decimal places to be used by \link{round} for 'label'.
#' 
#' @param cellnotes \emph{numeric} (Optional) matrix of the same dimensions as \code{x} that has the human-readable version of each value, for displaying to the user on hover. If \code{NULL}, then \code{x} will be coerced using \code{\link{as.character}}.
#' If missing, it will use \code{x}, after rounding it based on the \code{digits} parameter.
#' 
#' @param scale \emph{logical} (default is FALSE). If cellnote is missing and x is used, should cellnotes be scaled if x is also scaled?
#' 
#' @param row.label \emph{character} (Optional) Label to display next to the row value when the user hovers over the cell.
#'  If not specified, trys to match the \code{xaxis_title}; if no axis title, defaults to "Row".
#' 
#' @param col.label \emph{character} (Optional) Label to display next to the column value when the user hovers over the cell
#'  If not specified, trys to match the \code{yaxis_title}; if no axis title, defaults to "Col".
#' 
#' @param value.label \emph{character} (Optional) Label to display next to the cell value when the user hovers over the cell.
#'  Defaults to "Value".
#' 
#' @param print \emph{logical} Show the values inside the cells. Defatuls to \code{FALSE}.
#' 
#' @param color \emph{character} name or hex specifying the color of the values printed
#' inside the cells
#' 
#' @param brush_color The base color to be used for the brush. The brush will be
#'   filled with a low-opacity version of this color. \code{"#RRGGBB"} format 
#'   expected.
#' 
#' @param font.size \emph{numeric} the pixel size of printed cell
#' 
#' @return Modified d3heatmap object
#' 
#' @import htmlwidgets
#'   
#' @source 
#' The interface was inspired by \link{dygraphs}
#' 
#' @seealso 
#' \link{heatmap}, \link[gplots]{heatmap.2}
#' 
#' @examples 
#' library(d3heatmap)
#' library(magrittr)
#' 
#' d3heatmap(mtcars, scale = "column", col = "Blues") %>%
#'   hmCells(digits = 0L, print = T)
#' 
#' @export
hmCells <- function(d3heatmap
  , digits
  , cellnotes # cellnote
  , font.size #notecex
  , color #notecol
  , scale # cellnote_scale
  , row.label # cellnote_row
  , col.label # cellnote_col
  , value.label # cellnote_val
  , brush.color # brush_color
  , print # print.values
) {

  if (missing(d3heatmap)) {
		message("hmLegend: no heatmap provided... returning NULL")
		return(NULL)
	}
  
	# grab the input parameters for generating the heatmap
	params <- d3heatmap$x$params
	options <- d3heatmap$x$options

	if (missing(digits)) digits <- params$digits
	if (missing(scale)) scale <- params$cellnote_scale
	if (missing(cellnotes)) cellnotes <- params$cellnote
	if (missing(cellnotes)) cellnotes <- params$cellnote

	
	new <- list(
		digits  = digits
		, scale = scale
		, cellnote = cellnotes
	)

	params <- mergeLists(params, new)

	## call heatmap with the updated params and save
	## the params with the heatmap for later use
  ##==============================================
	hm <- do.call(heatmap, args = params)

	## Process the additional options and then merge
  ##==============================================
	if (missing(row.label)) row.label <- options$cellnote_row
	if (missing(col.label)) col.label <- options$cellnote_col
	if (missing(value.label)) value.label <- options$cellnote_val
	if (missing(brush.color)) brush.color <- options$brush_color
	if (missing(print)) print <- options$print_values
	if (missing(font.size)) font.size <- options$cellnote_fontsize
	if (missing(color)) color <- options$cellnote_color

	opts <- list(
    cellnote_row = row.label
    , cellnote_col = col.label
    , cellnote_val = value.label
    , cellnote_color = color
    , cellnote_fontsize = font.size
    , brush_color = brush.color
    , print_values = print
	)
  
	options <- mergeLists(options, opts)

	d3heatmap$x$matrix <- hm$mtx
	d3heatmap$x$params <- params
	d3heatmap$x$options <- options

  return(d3heatmap)  
}
