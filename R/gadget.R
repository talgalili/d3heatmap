#' @import shiny methods htmlwidgets
NULL



setOldClass('d3heatmap')

#' An S4 class to capture a heatmap
#'
#' @slot x A data.frame
#' @slot ColSideColors (optional) character vector of length ncol(x), or matrix with
#' columns equal to ncol(x), containing the color names for a horizontal side bar that may 
#' be used to annotate the columns of x. \code{ColIndividualColors}, from heatmap.2, can
#' also be used
#'   
#' @slot RowSideColors (optional) character vector of length nrow(x), or matrix with rows
#' equal to nrow(x), containing the color names for a vertical side bar that may be used to
#' annotate the rows of x. \code{RowIndividualColors}, from heatmap.3, can also be used
#'   
#' @slot settings A list of settings to pass to d3heatmap()
#' @slot heatmap A d3heatmap
#' @slot rows Character vector of selected rows from the heatmap
#' @slot cols Character vector of selected columns from the heatmap
#' @slot filter Character string of a valid filter (as in \code{subset()} for the underlying 
#' d3heatmap data; invalid filters will be safely loaded but not applied
#'
setClass('d3heatmapGadget', 
  representation(x = 'data.frame'
    , RowSideColors = 'data.frame'
    , ColSideColors = 'data.frame'
    , settings = 'list'
    , heatmap = 'd3heatmap'
    , rows = 'character'
    , cols = 'character'
    , filter = 'character'
))

#' A shiny gadget for d3heatmaps
#' 
#' Creates a D3.js-based heatmap widget inside a Shiny gadget. Pass into the function 
#' either a d3heatmap-compatible matrix or data.frame, or a list of class 
#' \emph{d3heatmapGadget} to start from the gadget's last saved settings
#' 
#' @param x A numeric matrix or data.frame populated with all numerics, or a list 
#' of class \emph{d3heatmapGadget}
#' 
#' @param ... arguments to be passed to \code{d3heatmap()}
#' 
#' @return An object of class \emph{d3heatmapGadget} containing the original data,
#' gadget settings, filter parameters, and the resulting plot
#' 
#' @examples
#' \dontrun{
#' 
#' hmg <- d3heatmapGadget(mtcars)
#' 
#' class(hmg)
#' # "d3heatmapGadget"
#' 
#' }
#' 
#' @export
d3heatmapGadget <- function(x, ...) {
  if (!requireNamespace('shiny') | !requireNamespace('miniUI')) 
    stop("Shiny or miniUI package not detected, please install first")
 
  params <- as.list(substitute(list(...)))[-1L]
  params <- lapply(params, eval)

  if (class(x) == 'd3heatmapGadget') {
    if(sum(dim(x@RowSideColors)) == 0) .rsc <- NULL
    else .rsc <- x@RowSideColors

    if(sum(dim(x@ColSideColors)) == 0) .csc <- NULL
    else .csc <- x@ColSideColors

    .x <- x@x
    # object class settings will take precedence over ... params
		.settings <- mergeLists(params, x@settings)
		.heatmap <- x@heatmap
    .rows <- x@rows
    .cols <- x@cols
    .filter <- x@filter
    
  } else if (class(x) %in% c('matrix', 'data.frame')) {
    
    # filter x for only numerics
    .x <- x[, sapply(x, is.numeric)]
    
		.rsc <- params$RowSideColors
		.csc <- params$ColSideColors
		.heatmap <- NULL
  	.settings <- list(
			# main
			main = NULL
			, show_grid = TRUE
			, anim_duration = 500
			# heatmap
  		, symm = FALSE
			, scale = 'none'
			, scale.by.range = FALSE
  		, na.rm = TRUE
  		, na.color = "#777777"
			#dendrograms
			, dendrogram = 'both'
			, k_row = NA_integer_
			, k_col = NA_integer_
			, revC = FALSE
			# legend
  		, key = FALSE
  		, keysize = 1
  		, key.title = NULL
  		, key.location = c("fl", "br", "tr", "tl", "bl")
			, density.info = c('histogram', 'none')
			, denscol = NULL
			# cell notes
  		, digits = 3L
  		, cellnote_scale = FALSE
  		, cellnote_val = "Value"
			, print.values = FALSE
			, notecol = '#222222'
  		, brush_color = "#0000FF"
			# axes
  		, xaxis.location = 'bottom'
  		, yaxis.location = 'right'
  		, xlab = NULL
  		, ylab = NULL
			, labColSize = 80
			, labRowSize = 120
			, cexCol = NA_integer_
			, cexRow = NA_integer_
  		, xaxis_title_font_size = 14
  		, yaxis_title_font_size = 14
  		, srtCol = 60
		)
  	
  	.settings <- mergeLists(.settings, params)
  	.rows <- row.names(.x)
  	.cols <- colnames(.x)
  	.filter = NULL
	}

  ui <- tagList(
    miniUI::miniPage(
      miniUI::miniTitleBar("d3heatmap Gadget",
                 left = miniUI::miniTitleBarButton('cancel', "Cancel", primary = FALSE),
                 right = miniUI::miniTitleBarButton('done', "Done", primary = TRUE)
      ),
      miniUI::miniTabstripPanel(
        miniUI::miniTabPanel("Heatmap", icon = icon("bar-chart"), 
          miniUI::miniContentPanel(
            d3heatmapOutput('heatmap', width = '100%', height = '100%')),
          miniUI::miniButtonBlock(
						actionButton('refresh', label = NULL, icon = icon('refresh')))),
        miniUI::miniTabPanel("Data", icon = icon("table"), 
          miniUI::miniContentPanel(
						selectInput('filter.rows', "Select Rows:", 
						            choices = row.names(.x), 
						            selected = .rows, 
						            multiple = TRUE,
						            width = '100%'),
						selectInput('filter.cols', "Select Columns:",
						            choices = colnames(.x), 
						            selected = .cols, 
						            multiple = TRUE,
						            width = '100%'),
						textInput('filter', "Filter Table:", 
						          value = .filter, 
						          placeholder = "(filter)",
						          width = '100%'),
            dataTableOutput('table'), scrollable = TRUE)),
        miniUI::miniTabPanel("Settings", icon = icon("sliders"), 
          miniUI::miniContentPanel(
            uiOutput('settings', width = '100%', 
										 height = '100%'), scrollable = TRUE))
      )
    )
  )
    
  server <- function(input, output, session) {
  
    # for gadget and exporting 
    exp <- reactiveValues()
    exp$x <- .x
		exp$settings <- .settings
		exp$rows <- .rows
		exp$cols <- .cols
		exp$heatmap <- .heatmap
	
		# for internal use only	
		nexp <- reactiveValues()
   
		# force a redraw... most helpful after resizing 
    observeEvent(input$refresh, {  
      exp$heatmap <- reactive({ 
			  params <- exp$settings
			  params$x <- exp$x
			 
        do.call(d3heatmap, args = params) 
      })
    })
   
    # debounce redrawing the heatmap to avoid overloads 
    exp$heatmap <- debounce(reactive({
			  params <- exp$settings
			  params$x <- exp$x
			  hm <- tryCatch({
            do.call(d3heatmap, args = params)
  			  }, 
  			  error = function(e) { NULL })
			  hm
      }), 2000)
    
    observeEvent(input$filter, {
      req(nchar(input$filter) > 0)
      
      # translate the text into an expression 
      expr <- tryCatch({ parse(text = input$filter)[[1]] },
                       error = function(e) { NULL })
      req(!is.null(expr))
     
      # run the filter, errors terminate the observer 
      tmp.x <- tryCatch({ eval(bquote(subset(exp$x, .(expr)))) }, 
                        error = function(e) { e })
      
      req(!any(class(tmp.x) == 'error'))
      
      # if successful save the filter for exporting
      exp$filter <- input$filter
      
      # update the heatmap matrix 
      exp$x <- tmp.x
      
      # cascade the filtered matrix into the row/column filters, keeping
      # any previously selected rows/columns
      rn <- input$'filter.rows'
      rows <- rn[which(rn %in% row.names(exp$x))]
      
      cn <- input$'filter.cols'
      cols <- cn[which(cn %in% colnames(exp$x))]
      
      updateSelectInput(session, 'filter.rows', selected = rows)
      updateSelectInput(session, 'filter.cols', selected = cols)
    })
   
    # filter matrix rows / columns 
    observe({ 
      rowInd <- which(row.names(.x) %in% input$'filter.rows')
      colInd <- which(colnames(.x) %in% input$'filter.cols')

      if (!is.null(.settings$RowSideColors))
        exp$settings$RowSideColors <- .rsc[rowInd,]
      if (!is.null(.settings$ColSideColors))
        exp$settings$ColSideColors <- .csc[,colInd]
      
      exp$x <- .x[rowInd, colInd]
      
      isolate({
        exp$rows <- input$'filter.rows'
        exp$cols <- input$'filter.cols'
      }) 
    })

		# observer loop for each input value
  	observe({
			# get the name of each input that matches one in the settings
			vals <- names(input)[which(names(input) %in% names(exp$settings))]
			req(length(vals) > 0)
	    
			# for each value where there is a corresponding input, create
			# an observeEvent function to update that value	
			for (i in 1:length(vals)) {
		    local({
		      val <- vals[i]
		      observeEvent(input[[ val ]], {
		        exp$settings[[ val ]] <- input[[ val ]]    
		        
		      })
		    })	  
			}
		})
    
    output$heatmap <- renderD3heatmap({ 
      # print out the error statement if an error, otherwise print heatmap
      heatmap <- tryCatch({ exp$heatmap() }, 
                          error = function(e) { e })
      req(!is.null(heatmap))
      
      validate(need(any(class(heatmap) == 'd3heatmap'), heatmap$message))
      heatmap
    })
    
    output$table <- renderDataTable({ 
      df <- exp$x   
      df$row.names <- row.names(df)
      df <- df[,c(ncol(df), 1:(ncol(df) - 1))]
    })
    
    observeEvent(input$cancel, { stopApp(invisible()) })

    observeEvent(input$done, { 
      .ls <- reactiveValuesToList(exp, all.names = TRUE)   
      .ls$heatmap <- exp$heatmap()
      if (is.null( .ls$filter )) .ls$filter <- ""
      out <- new("d3heatmapGadget", 
                 x = .x
                 , RowSideColors = data.frame(.rsc)
                 , ColSideColors = data.frame(.csc)
                 , settings = .ls$settings
                 , heatmap = .ls$heatmap
                 , rows = .ls$rows
                 , cols = .ls$cols
                 , filter = .ls$filter)
      stopApp(out)
    })
    
    output$settings <- renderUI({
      tagList(
        fillRow(
          fillCol(width = '95%', tagList(
  					uiOutput('main.ui')
  					, uiOutput('heatmap.ui')
  					, uiOutput('cells.ui')
  					, uiOutput('legend.ui')
          )),
  				fillCol(width = '95%', tagList(
  					uiOutput('dendros.ui')
  					, uiOutput('axes.ui')
  				))
        )
      )
    })
		
		output$main.ui <- renderUI({
				tagList(h4('Main')
					, textInput('main', "Main title", 
						          value = .settings$'main',
						          placeholder = "(text)",
						          width = '100%')
          , checkboxInput('show_grid', "Show grid", 
                          value = .settings$'show_grid')
					, numericInput('anim_duration', 'Zoom animation duration', 
					             value = .settings$'anim_duration', 
											 min = 0, step = 1)
					, hr()
				)
		})

		output$heatmap.ui <- renderUI({
			tagList(h4('Heatmap')
					, selectInput('scale', label = 'Scale values', 
					            choices = c('None' = 'none',
                                  'Row' = 'row',
                                  'Column' = 'column'))
          , checkboxInput('scale.by.range', "Scale by absolute values", 
                          value = .settings$'scale.by.range')
          , checkboxInput('symm', "Symmetrical heatmap", 
                          value = .settings$'symm')
          , checkboxInput('na.rm', "Remove NA values", 
                          value = .settings$'na.rm')
					, textInput('na.color', 'NA color:', 
											value = .settings$'na.color')
					, hr()
			)
		})

		output$cells.ui <- renderUI({
				tagList(h4('Cell Values')
					, numericInput('digits', 'Number of digits',
					             value = .settings$'digits', min = 0, step = 1)
          , checkboxInput('print.values', "Print values in cell", 
                          value = .settings$'print.values')
          , checkboxInput('dfjkellnote_scale', "Scale values in cells", 
                          value = .settings$'cellnote_scale')
					, textInput('cellnote_val', 'Value label', 
											value = .settings$'cellnote_val')
					, textInput('notecol', 'Value color', 
											value = .settings$'notecol')
					, textInput('brush_color', 'Popup background color', 
											value = .settings$'brush_color')
					, hr()
				)
		})
		
		output$legend.ui <- renderUI({ 
			tagList(h4('Color legend')
          , checkboxInput('key', "Show color legend", 
                          value = .settings$'key')
					, numericInput('keysize', 'Legend size (% of default)',
					             value = .settings$'keysize', min = 0, step = .01)
					, textInput('key.title', 'Legend title', 
											value = .settings$'key.title')
					, selectInput('key.location', label = 'Legend location', 
					            choices = c('Float' = 'fl', 
                                  'Bottom-left' = 'bl',
                                  'Bottom-right' = 'br',
                                  'Top-left' = 'tl',
                                  'Top-right' = 'tr'))
					, selectInput('density.info', label = 'Density info', 
					            choices = c('Histogram' = 'histogram', 
                                  'None' = 'none'))
					, textInput('denscol', 'Density color', 
											value = .settings$'denscol')
					, hr()
			)
		})

		output$dendros.ui <- renderUI({
				tagList(h4('Dendrograms')
					, selectInput('dendrogram', label = 'Dendrograms:', 
					            choices = c('Both' = 'both', 
                                  'Row' = 'row',
                                  'Column' = 'column',
                                  'None' = 'none'))
					, numericInput('k_row', 'Color groups for row dendrogram:',
					             value = .settings$'k_row', min = 0, step = 1)
					, numericInput('k_col', 'Color groups for column dendrogram:',
					             value = .settings$'k_col', min = 0, step = 1)
          , checkboxInput('revC', "Reverse columns", 
                          value = .settings$'revC')
					, hr()
				)
		})

		output$axes.ui <- renderUI({
				tagList(h4('Axes')
					, selectInput('xaxis.location', label = 'X axis location',
					            choices = c('Bottom' = 'bottom',
                                  'Top' = 'top'))
					, selectInput('yaxis.location', label = 'Y axis location',
					            choices = c('Right' = 'right',
                                  'Left' = 'left'))
					, textInput('xlab', 'X axis title',
											value = .settings$'xlab',
											placeholder = "(text)")
					, textInput('ylab', 'Y axis title',
											value = .settings$'ylab',
											placeholder = "(text)")
					, numericInput('labColSize', 'X axis (column) size (# pixels)',
											value = .settings$'labColSize', min = 0, step = 1)
					, numericInput('labRowSize', 'Y axis (row) size (# pixels)',
											value = .settings$'labRowSize', min = 0, step = 1)
					, numericInput('cexCol', 'X axis label font size (# * 12px)',
											value = .settings$'cexCol', min = 0, step = .1)
					, numericInput('cexRow', 'Y axis label font size (# * 12px)',
											value = .settings$'cexRow', min = 0, step = .1)
					, numericInput('xaxis_title_font_size', 'X axis title font size',
					             value = .settings$'xaxis_title_font_size', 
											 min = 0)
					, numericInput('yaxis_title_font_size', 'Y axis title font size',
					             value = .settings$'yaxis_title_font_size', 
											 min = 0)
					, numericInput('srtCol', 'X axis label angle',
					             value = .settings$'srtCol', step = -1)
					, hr()
				)
		})
    
  }
  
  runGadget(ui, server)  
}


#' @export
#' @docType methods
#' @rdname print-methods
setGeneric("print", function(gadget) { standardGeneric("print") })


#' Print a gadget's main output
#'
#' Use this method to print the main component of the d3heatmapGadget to the viewer 
#' pane without intermediate code
#'
#' @param gadget The gadget to print to screen
#' 
#' @examples 
#' \dontrun{
#' 
#' hm <- d3heatmapGadget(mtcars)
#' print(hm)
#' 
#' }
#' 
#'
#' @rdname print-methods
#' @aliases print.d3heatmapGadget
#' @export
setMethod("print",
    signature(gadget = 'd3heatmapGadget'),
    function(gadget) { gadget@heatmap }
)



#' @export
#' @docType methods
#' @rdname save-methods
setGeneric("save", function(gadget, ...) { standardGeneric("save") })


#' Save a gadget's main output
#'
#' Use this method to save the d3heatmap from the d3heatmapGadget
#' without intermediary code.
#'
#' @param gadget The gadget to save to file
#' @param file the name of the file, without extension
#' @param ... arguments to pass to \code{htmlwidgets::saveWidget}
#' 
#' @examples
#' \dontrun{
#' 
#' hm <- d3heatmapGadget(mtcars)
#' save(hm, file = "myHeatmap")
#' 
#' }
#'
#' @rdname save-methods
#' @aliases save.d3heatmapGadget
#' @export
setMethod("save",
    signature(gadget = 'd3heatmapGadget'),
    function(gadget, file = NULL,
             ...)
    {
      params <- as.list(...)
      
      params$widget = gadget@heatmap
      params$file = paste0(file, ".html")

      do.call(saveWidget, args = params)
    }
)
