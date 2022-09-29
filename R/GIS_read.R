#' GIS_read
#'
#' Read in GIS maps. Returns a spatial data frame containing all maps. 2/14/18.
#' @param maps_in Character vector of maps to be read in by the chosen method.
#' @param type Legacy option - now ONLY accepts "raster" or "Raster"
#' @param typepars The file path to a folder containing the rasters specified via template or other args. TO BE RENAMED TO MAP_DIR.
#' @param map_info Two column matrix of map types and names, output of `read_template()``
#' @param seq_patch_IDs TRUE/FALSE should patch map IDs be overwritten with sequential integers.
#' @param output_patch_map TRUE/FALSE should the new patch map with sequential IDs be output to file.
#' @return Returns a LargeSpatialGridDataFrame containing all the maps indicated in read_in
#' @author Will Burke
#' @export
#' @importFrom methods as

GIS_read = function(maps_in, type = "raster", typepars, map_info = NULL, seq_patch_IDs = FALSE, output_patch_map = FALSE) {

  options(scipen = 999) # no scientific notation

  # ---------- Read in spatial data ----------
  cat("Reading in maps...\n")

  # Raster spatial data ----------
  if (! type %in% c("raster", "Raster", "RASTER")) {
    stop("Raster is now the only valid `type` argument.")
  } # end raster spatial data


  # Get file paths and check files exist
  file_paths = vector(mode = "character")
  for (name in maps_in) {
    # try exact match first
    file = list.files(path = typepars, pattern = paste("^",name,"$",sep = ""),full.names = TRUE)
    if (length(file) != 1) {
      if (length(file) == 0) { # if no files match name in template, try with extensions
        file = list.files(path = typepars, pattern = paste("^",name,"\\.",sep = ""),full.names = TRUE)
      }
      if (length(file) == 0) { # if there were no matches
        stop(paste("No file named:",name,"at path:",typepars))
      }
      if (length(file) > 1) { # ignore .xml files
        file = file[!grepl(".xml$",file)]
      }
      if (length(file) > 1) { # if multiple files, use tif or tiff preferentially
        if (any(grepl(".tif$",file))) {
          file = file[grep(".tif$",file)]
        } else if (any(grepl(".tiff$",file))) {
          file = file[grep(".tiff$",file)]
        }
      }
      if (length(file) > 1) { # if STILL multiple files, can only be one file for each name in maps_in
        stop(paste("multiple files containing name:",name,"check directory:",typepars,". Either specify file extensions or change to unique file names."))
      }
    }
    file_paths = c(file_paths,file)
  }

  if (length(file_paths) != length(maps_in)) {
    stop("Something went wrong with parsing the input map names/finding the input maps -
         check they're specified correctly in the template/elsewhere, and it may help to specify the extensions.")
  }

  # TODO redo raster work with terra, still read as stack, etc.

  # NATIVE DRIVERS -- may or may not actually help, future testing maybe
  read_stack = try(raster::stack(x = file_paths))

  if (inherits(read_stack, "try-error")) { # automatic error handling, can be added to as errors are found -----
    if (attr(read_stack,"condition")$message == "different extent") { # check/compare extents
      extents = list()
      for (i in file_paths) { # get extents
        extents[[which(file_paths == i)]] = raster::extent(raster::raster(i))
      }
      for (i in 1:length(unique(extents))) { # print maps and extents, should make outlier maps obvious
        m = maps_in[sapply(extents,FUN = function(x) x == unique(extents)[[i]])]
        print(paste("Maps:",paste(m,collapse = ", ")),quote = FALSE)
        print(paste("Have extent: xmin =",unique(extents)[[i]]@xmin,"xmax =",unique(extents)[[i]]@xmax,
                    "ymin =",unique(extents)[[i]]@ymin,"ymax =",unique(extents)[[i]]@ymax),quote = FALSE)
      }
    } # end if different extents

    # other types of errors go here
    stop("Something went wrong, see previous messages")
  } # end try error handling

  names(read_stack) = maps_in # names lose extensions by default, confused by "."s in names

  # Check projections (read_stack will error if proj is different, but arguments might be different) -----
  p = vector(mode = "character",length = length(read_stack[1]))
  d = p
  for (i in 1:length(read_stack[1])) {
    p[i] = raster::projection(read_stack[[i]])
    d[i] = attr(rgdal::GDALinfo(file_paths[i],silent = TRUE),which = "driver")
  }
  if (length(unique(p)) > 1 & !is.null("map_info")) { # if map_info is present, coerce world level projection, output text for overwritten projections
    for (i in which(p != raster::projection(read_stack[[map_info[map_info[,1] == "world",2]]]))) {
      raster::projection(read_stack[[i]]) = raster::projection(read_stack[[map_info[map_info[,1] == "world",2]]])
      print(paste("Projection arguments for",names(read_stack[[i]]),"coerced to world level projection:",
                  raster::projection(read_stack[[map_info[map_info[,1] == "world",2]]])),quote = FALSE)
    }
  }
  if (length(unique(p)) > 1 & is.null("map_info")) {
    print(paste("Differing projection arguments:",unique(p),"Potential conflicts."),quote = FALSE)
  }

  # Handle NaNs - set to NA
  cat("Setting NaNs to NA.\n")
  raster::values(read_stack)[is.nan(raster::values(read_stack))] = NA

  # Handling grass ascii 0's - get rid of 0's for background/NA -----
  # Ideally this should be handled when reading in files, but I can't find where the default for nodata is set
  if ((any(d == "AAIGrid") | any(d == "GRASSASCIIGrid")) & !is.null("map_info")) {
    # new fix - just set world map(usually basin) 0's to NA, less chance of confusion
    raster::values(read_stack[[map_info[map_info[,1] == "world",2][[1]]]])[raster::values(read_stack[[map_info[map_info[,1] == "world",2]]]) == 0] = NA
    cat("For 'world level map, 0's set to NA.")
  }

  cat("Trimming NAs.\n")
  if (raster::nlayers(read_stack) == 1) {
    read_stack = raster::trim(read_stack[[1]]) #get rid of extra background
  } else {
    read_stack = raster::trim(read_stack) #get rid of extra background
  }

  # Mask all maps by world level map -----
  if (!is.null("map_info")) { # if being run inside RHESSysPreprocess.R will always have map_info - just makes function more versatile
    cat("Masking maps by 'world' layer map.\n")
    read_stack = raster::mask(read_stack,read_stack[[map_info[map_info[,1] == "world",2][[1]]]], progress = "text")# mask by map used for world level
  }

  # trim NAs again to remove rows/cols that mightve been removed via mask
  #cat("Trimming NAs after mask.\n")
  if (read_stack@data@nlayers == 1) {
    read_stack = raster::trim(read_stack[[1]]) #get rid of extra background
  } else {
    read_stack = raster::trim(read_stack) #get rid of extra background
  }

  # Check for missing data (within world map mask) - no fix, just an error since I think this will break things if left unchecked
  cat("Checking for missing data within bounds of world map.\n")
  if (!is.null("map_info")) {
    wrld_vals = !is.na(raster::values(read_stack[[map_info[map_info[, 1] == "world", 2][[1]]]]))
    NAs_in_wrld = lapply(as.data.frame(raster::values(read_stack)), function(X) {sum(is.na( X[wrld_vals]))})
    if ("streams" %in% map_info[, 1]) {
      NAs_in_wrld[[map_info[map_info[, 1] == "streams", 2]]] = NULL
    }
    if (any(NAs_in_wrld > 0) ) {
      cat("Warning: One or more maps have NAs within the bounds of the world map, see maps and counts of NAs below:\n")
      print(NAs_in_wrld[NAs_in_wrld > 0])
      #09282022: LML for some layers, it's okay to have NA values
      #stop("See above and check your input maps.")
    }
  }

  # Convert maps to SpatialGridDataFrame since world_gen.R expects that format
  readmap = methods::as(read_stack,"SpatialGridDataFrame")

  if (seq_patch_IDs & !is.null("map_info")) {
    cat("Overwritting patch IDs to be sequential\n")
    pmap = map_info[map_info[,1] == "patch",2]
    readmap@data[[pmap]][!is.na(readmap@data[[pmap]])] = seq_along(readmap@data[[pmap]][!is.na(readmap@data[[pmap]])])

    if (output_patch_map) {
      pmap_fname = paste0(sub("\\..{3,4}$", "", pmap),"_seqID.tif")
      cat("Writing patch map with new IDs to file:", pmap_fname,"\n")
      raster::writeRaster(x = as(readmap[pmap], "RasterLayer"), filename = file.path(typepars,pmap_fname))
    }
  }

  cat("Finished reading in maps\n")

  return(readmap)

}
