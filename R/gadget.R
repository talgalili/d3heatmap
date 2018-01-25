#' @import shiny miniUI
#' @importFrom methods new
NULL

setOldClass('d3heatmap')
setClass('d3heatmapGadget', 
  representation(x = 'data.frame'
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
#' \donttest{
#' hmg <- d3heatmapGadget(mtcars)
#' 
#' class(hmg)
#' # "d3heatmapGadget"
#' }
#' 
#' @export
d3heatmapGadget <- function(x, ...) {
  if (!requireNamespace('shiny') | !requireNamespace('miniUI')) 
    stop("Shiny or miniUI packages not detected, please install first")
 
  params <- as.list(substitute(list(...)))[-1L]
  
  if (class(x) == 'd3heatmapGadget') {
    .x <- x@x
    # object class settings will take precedence over ... params 
		.settings <- mergeLists(params, x@settings)
		.heatmap <- x@heatmap
    .rows <- x@rows
    .cols <- x@cols
    .filter <- x@filter
    
  } else if (class(x) %in% c('matrix', 'data.frame')) {
    #filter x for only numerics
    x <- x[,sapply(x, is.numeric)]
    
		.x <- x
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
    miniPage(
      miniTitleBar("d3heatmap Gadget",
                 left = miniTitleBarButton('cancel', "Cancel", primary = FALSE),
                 right = miniTitleBarButton('done', "Done", primary = TRUE)
      ),
      miniTabstripPanel(
        miniTabPanel("Heatmap", icon = icon("bar-chart"), 
          miniContentPanel(
            d3heatmapOutput('heatmap', width = '100%', height = '100%')),
          miniButtonBlock(
						actionButton('refresh', label = NULL, icon = icon('refresh')))),
        miniTabPanel("Data", icon = icon("table"), 
          miniContentPanel(
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
        miniTabPanel("Settings", icon = icon("sliders"), 
          miniContentPanel(
            uiOutput('settings', width = '100%', 
										 height = '100%'), scrollable = TRUE))
      )
    )
  )
    
  server <- function(input, output, session) {
  
    # for gadget and exporting 
    gVals <- reactiveValues()
    gVals$x <- .x
		gVals$settings <- .settings
		gVals$rows <- .rows
		gVals$cols <- .cols
		gVals$heatmap <- .heatmap
	
		# for internal use only	
		nexp <- reactiveValues()
   
		# force a redraw... most helpful after resizing 
    observeEvent(input$refresh, {  
      gVals$heatmap <- reactive({ 
			  params <- gVals$settings
			  params$x <- gVals$x
			  
        do.call(d3heatmap, args = params) 
      })
    })
   
    # debounce redrawing the heatmap to avoid overloads 
    gVals$heatmap <- debounce(reactive({
			  params <- gVals$settings
			  params$x <- gVals$x
        do.call(d3heatmap, args = params)
      }), 2000)
    
    observeEvent(input$filter, {
      req(nchar(input$filter) > 0)
      
      # translate the text into an expression 
      expr <- tryCatch({ parse(text = input$filter)[[1]] },
                       error = function(e) { NULL })
      req(!is.null(expr))
     
      # run the filter, errors terminate the observer 
      tmp.x <- tryCatch({ eval(bquote(subset(gVals$x, .(expr)))) }, 
                        error = function(e) { e })
      
      req(!any(class(tmp.x) == 'error'))
      
      # if successful save the filter for exporting
      gVals$filter <- input$filter
      
      # update the heatmap matrix 
      gVals$x <- tmp.x
      
      # cascade the filtered matrix into the row/column filters, keeping
      # any previously selected rows/columns
      rn <- input$'filter.rows'
      rows <- rn[which(rn %in% row.names(gVals$x))]
      
      cn <- input$'filter.cols'
      cols <- cn[which(cn %in% colnames(gVals$x))]
      
      updateSelectInput(session, 'filter.rows', selected = rows)
      updateSelectInput(session, 'filter.cols', selected = cols)
    })
   
    # filter matrix rows / columns 
    observe({ 
      rowInd <- which(row.names(.x) %in% input$'fitler.rows')
      colInd <- which(colnames(.x) %in% input$'fitler.cols')
      
      if(!is.null(.settings$RowSideColors)) .settings$RowSideColors[rowInd,]
      if(!is.null(.settings$ColSideColors)) .settings$ColSideColors[,colInd]
      
      gVals$x <- .x[rowInd, colInd]
      
      isolate({
        gVals$rows <- input$'filter.rows'
        gVals$cols <- input$'filter.cols'
      }) 
    })

		# observer loop for each input value
  	observe({
			# get the name of each input that matches one in the settings
			vals <- names(input)[which(names(input) %in% names(gVals$settings))]
			req(length(vals) > 0)
	    
			# for each value where there is a corresponding input, create
			# an observeEvent function to update that value	
			for(i in 1:length(vals)) {
		    local({
		      val <- vals[i]
		      observeEvent(input[[ val ]], {
		        gVals$settings[[ val ]] <- input[[ val ]]    
		        
		      })
		    })	  
			}
		})
    
    output$heatmap <- renderD3heatmap({ 
      # print out the error statement if an error, otherwise print heatmap
      heatmap <- tryCatch({ gVals$heatmap() }, 
                          error = function(e) { e })
      validate(need(!any(class(heatmap) == 'error'), heatmap$message))
      
      heatmap
    })
    
    output$table <- renderDataTable({ 
      df <- gVals$x   
      df$row.names <- row.names(df)
      df <- df[,c(ncol(df), 1:ncol(df)-1)]
    })
    
    observeEvent(input$cancel, { stopApp(invisible()) })

    observeEvent(input$done, { 
      .ls <- reactiveValuesToList(gVals, all.names = TRUE)   
      .ls$heatmap <- gVals$heatmap()
      if(is.null( .ls$filter )) .ls$filter <- ""
      out <- new("d3heatmapGadget", 
                 x = .x
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
					, selectInput('desntiy.info', label = 'Density info', 
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
