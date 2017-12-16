## This contains the primary routines to create the d3heatmap.  
## These used to sit in the d3heatmap.R file in the main d3heatmap() 
## call, but needed to be extracted for the new API. Some paramaters 
## in separate functions in the new API have cascading affects on how 
## the heatmap is created, so extracting those interlinked parts and 
## placing in a separate function - called by the main d3heatmap 
## creator - will hopefully shorten processing time when adding 
## options through the new API

heatmap <- function(
	## matrix of data									
	x

	## dendrogram control
  , Rowv
  , Colv
  , distfun
  , hclustfun
  , dendrogram
  , reorderfun 
  , k_row
  , k_col
  , symm
  , revC
  
  ## data scaling
  , scale
  , na.rm
  , na.value

  ## cellnote formatting
  , digits
  , cellnote
  , cellnote_scale

	## axis controls
  , labRow
  , labCol
	  
) {
  
	## x is a matrix!
  ##====================
  if(!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if(!is.matrix(x)) stop("x must be a matrix")
  
  nr <- dim(x)[1]
  nc <- dim(x)[2]
  ### TODO: debating if to include this or not:
  #   if(nr <= 1 || nc <= 1)
  #     stop("`x' must have at least 2 rows and 2 columns")

  ## Labels for Row/Column 
  ##======================
  rownames(x) <- labRow %||% paste(1:nrow(x))
  colnames(x) <- labCol %||% paste(1:ncol(x))

  ## Dendrograms for Row/Column 
  ##=======================
  dendrogram <- match.arg(dendrogram)
  
  # Use dendrogram argument to set defaults for Rowv/Colv
  if (missing(Rowv)) {
    Rowv <- dendrogram %in% c("both", "row")
  }
  if (missing(Colv)) {
    Colv <- dendrogram %in% c("both", "column")
  }


  if (isTRUE(Rowv)) {
    Rowv <- rowMeans(x, na.rm = na.rm)
  }
  if (is.numeric(Rowv)) {
    Rowv <- reorderfun(as.dendrogram(hclustfun(distfun(x))), Rowv)
  }
  if (is.dendrogram(Rowv)) {
    Rowv <- rev(Rowv)
    rowInd <- order.dendrogram(Rowv)
    if(nr != length(rowInd))
      stop("Row dendrogram is the wrong size")
  } else {
    if (!is.null(Rowv) && !is.na(Rowv) && !identical(Rowv, FALSE))
      warning("Invalid value for Rowv, ignoring")
    Rowv <- NULL
    rowInd <- 1:nr
  }
  
  if (identical(Colv, "Rowv")) {
    Colv <- Rowv
  }
  if (isTRUE(Colv)) {
    Colv <- colMeans(x, na.rm = na.rm)
  }
  if (is.numeric(Colv)) {
    Colv <- reorderfun(as.dendrogram(hclustfun(distfun(t(x)))), Colv)
  }
  if (is.dendrogram(Colv)) {
    colInd <- order.dendrogram(Colv)
    if (nc != length(colInd))
      stop("Col dendrogram is the wrong size")
  } else {
    if (!is.null(Colv) && !is.na(Colv) && !identical(Colv, FALSE))
      warning("Invalid value for Colv, ignoring")
    Colv <- NULL
    colInd <- 1:nc
  }
  
  ## revC
  ##=======================
  if(is.null(revC)) {
    if (symm) {
      revC <- TRUE
    } else if(is.dendrogram(Colv) & is.dendrogram(Rowv) & identical(Rowv, rev(Colv))) {
      revC <- TRUE
    } else {
      revC <- FALSE
    }
  }
  if(revC) {
    Colv <- rev(Colv)
    colInd <- rev(colInd)
  }
  
  ## reorder x (and others)
  ##=======================
  x <- x[rowInd, colInd]
  if (!missing(cellnote))
    cellnote <- cellnote[rowInd, colInd]

  ## Dendrograms - Update the labels and change to dendToTree
  ##=======================

  # color branches?
  #----------------
    # Due to the internal working of dendextend, in order to use it we first need
      # to populate the dendextend::dendextend_options() space:
  if(!is.null(k_row) | !is.null(k_col)) dendextend::assign_dendextend_options()
  
  if(is.dendrogram(Rowv) & !is.null(k_row)) {
    Rowv <- dendextend::color_branches(Rowv, k = k_row)
  }
  if(is.dendrogram(Colv) & !is.null(k_col)) {
    Colv <- dendextend::color_branches(Colv, k = k_col)
  }
  
  rowDend <- if(is.dendrogram(Rowv)) dendToTree(Rowv) else NULL
  colDend <- if(is.dendrogram(Colv)) dendToTree(Colv) else NULL

  
  ## Scale the data?
  ##====================
  scale <- match.arg(scale) 
  
	# keep a backup for cellnote
  if(!cellnote_scale) x_unscaled <- x 
  
  if (!is.na(na.value)) {
    na.rm <- TRUE 
    x[which(x == na.value)] <- NA
  }
  
  if(scale == "row") {
    x <- sweep(x, 1, rowMeans(x, na.rm = na.rm))
    x <- sweep(x, 1, apply(x, 1, sd, na.rm = na.rm), "/")
    
  }
  else if(scale == "column") {
    x <- sweep(x, 2, colMeans(x, na.rm = na.rm))
    x <- sweep(x, 2, apply(x, 2, sd, na.rm = na.rm), "/")
  }
  
  # in the instance where all non-NA values are the same (i.e., mtcars$vs or 
  # mtcars$am when na.value == 0 and scaling by column), the functions above
  # will return NaN, which will be translated to NA... 
  # therefor we will replace all NaN's with .5)
  x[is.nan(x)] <- .5
  
  ## cellnote
  ##====================================================
  if(is.null(cellnote)) {
    if(cellnote_scale) {
      cellnote <- round(x, digits = digits)
    } else { # default
      cellnote <- round(x_unscaled, digits = digits)
    }
  }
  
  # Check that cellnote is o.k.:
  if (is.null(dim(cellnote))) {
    if (length(cellnote) != nr*nc) {
      stop("Incorrect number of cellnote values")
    }
    dim(cellnote) <- dim(x)
  }
  if (!identical(dim(x), dim(cellnote))) {
    stop("cellnote matrix must have same dimensions as x")
  }  
  
  ## Final touches before htmlwidgets
  ##=======================
  mtx <- list(data = as.character(t(cellnote)),
							x = as.numeric(t(round(x, digits = digits))),
              dim = dim(x),
              rows = rownames(x),
              cols = colnames(x))
  
	## return package
	##==============================
	heatmap <- list(
		x = x
		, mtx = mtx
		, rowDend = rowDend
		, colDend = colDend
	)

	return(heatmap)
}
