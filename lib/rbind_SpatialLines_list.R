# Modify rbind.SpatialLines -----------------------------------------------

# From https://github.com/edzer/sp/blob/master/R/rbind.R
makeUniqueIDs <- function(lst) {
  ids = sapply(lst, function(i) slot(i, "ID"))
  if (any(duplicated(ids))) {
    ids <- make.unique(as.character(unlist(ids)), sep = "")
    for (i in seq(along = ids))
      lst[[i]]@ID = ids[i]
  }
  lst
}

rbind.SpatialLines.list <- function (..., makeUniqueIDs = TRUE) {
  dots  <- list(...)
  
  # If input has length 1 and is plausibly a list, then unnest it
  if(length(dots) == 1){
    dots <- unlist(dots, recursive = FALSE)
  }
  
  names(dots) <- NULL
  stopifnot(identicalCRS(dots))
  ll = do.call(c, lapply(dots, function(x) slot(x, "lines")))
  if (makeUniqueIDs) 
    ll = makeUniqueIDs(ll)
  SpatialLines(ll, proj4string = CRS(proj4string(dots[[1]])))
}


rbind.SpatialPointsDataFrame.list <- function (...){
  dots = list(...)
  
  # If input has length 1 and is plausibly a list, then unnest it
  if(length(dots) == 1){
    dots <- unlist(dots, recursive = FALSE)
  }
  
  names(dots) <- NULL
  sp = do.call(rbind, lapply(dots, function(x) as(x, "SpatialPoints")))
  df = do.call(rbind, lapply(dots, function(x) x@data))
  SpatialPointsDataFrame(sp, df, coords.nrs = dots[[1]]@coords.nrs)
}

