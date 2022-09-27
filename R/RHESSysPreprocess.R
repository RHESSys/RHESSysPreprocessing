#' RHESSysPreprocess
#'
#' Preforms RHESSys Preprocessing, comprised of two main steps: generating a world file, and generating a flow table.
#' World file is generated via world_gen.R, and flow table is generated via create_flownet.R.
#' @param template Template file used to generate worldfile for RHESSys. Generic strucutre is:
#' <state variable> <operator> <value/map>. Levels are difined by lines led by "_", structured
#' <levelname> <map> <count>. Whitespace and tabs are ignored.  Maps referred to must be supplied
#' by your chosen method of data input(raster), set using the "type" arguement.
#' @param name The base name (and potentially, path as well) to be used for your ouput files.
#' This will create a world file called "<name>.world", and a flow table called "<name>.flow".
#' @param type Input file type to be used. Default is raster. "Raster" type will use rasters
#' in GeoTiff or equivalent format (see Raster package), with file names  matching those indicated in the template.
#' ASCII is supported, but 0's cannot be used as values for data.
#' @param typepars Parameters needed based on input data type used. If using raster type, typepars should be a string
#' indicating the path to a folder containing the raster files that are referenced by the template.
#' @param streams Streams map to be used in building the flowtable.
#' @param overwrite Overwrite existing worldfile. FALSE is default and prompts a menu if worldfile already exists.
#' @param roads Roads map, an optional input for flowtable creation.
#' @param impervious Impervious map, an optional input for flowtable creation.
#' @param roofs Roofs map, an optional input for flowtable creation.
#' @param header TRUE/FALSE flag for the creation of a header file. Will have same name (and location) as "name" argument, but with ".hdr" suffix.
#' @param meta TRUE/FALSE flag for the creation of a metadata file. Still in dev.
#' @param asprules The path and filename to the rules file.  Using this argument enables aspatial patches.
#' @param unique_strata_ID Takes input map or value for canopy strata ID and appends either a 1 or 2 dpending on which canopy it is. Defaults to TRUE.
#' @param seq_patch_IDs TRUE/FALSE should patch map IDs be overwritten with sequential integers.
#' @param output_patch_map TRUE/FALSE should the new patch map with sequential IDs be output to file.
#' @param fire_grid_out TRUE/FALSE should the writefire_grids headerless ascii grid outputs of DEM, patch, zone, and hillslope be output. An additional fire grid info
#' file will be created specifying the ncol/nrow and other header info. Will use the input name as the base prefix for the grid files.
#' @param parallel TRUE/FALSE flag to build a flowtable for use in the hilllslope parallelized version of RHESSys. Console may output warnings of
#' automated actions taken to make hillslope parallelization possible, or errors indicating fatal problems in hillslope parallelization.
#' @param make_stream The maximum distance (cell lengths) away from an existing stream that a patch can be automatically coerced to be a stream.
#' Setting to TRUE will include patches at any distance. This is needed for hillslope parallelization, as all hillslopes must have an outlet stream patch.
#' Default is 4.
#' @param skip_hillslope_check TRUE/FALSE to skip the recursive check for segmented hillslopes. Segmented hillslopes will break the routing, but the
#' recursive check can trigger various recursion protections when hillslopes are large.
#' @param wrapper internal argument to track if being run as all-in-one
#' @seealso \code{\link[raster]{raster}}, \code{\link[RHESSysIOinR]{run_rhessys}}
#' @author Will Burke
#' @export

# ---------- Function start ----------
RHESSysPreprocess = function(template,
                             name,
                             type = 'Raster',
                             typepars,
                             streams = NULL,
                             overwrite = FALSE,
                             roads = NULL,
                             impervious = NULL,
                             roofs = NULL,
                             header = FALSE,
                             meta = FALSE,
                             asprules = NULL,
                             unique_strata_ID = TRUE,
                             seq_patch_IDs = FALSE,
                             output_patch_map = FALSE,
                             fire_grid_out = FALSE,
                             parallel = TRUE,
                             make_stream = 4,
                             skip_hillslope_check = FALSE,
                             wrapper = TRUE) {

  # ---------- Check Inputs ----------
  if (!file.exists(template)) { # check if template exists
    cat("Template does not exist or is not located at specified path:",template)
  }

  basename = basename(name) # check Name
  if (startsWith(basename, "World.") |
      startsWith(basename, "world.")) {
    basename = substr(basename, 7, nchar(basename))
  } else if (endsWith(basename, ".world")) {
    basename = substr(basename, 0, nchar(basename) - 6)
  } else if (startsWith(basename, "Flow.") |
             startsWith(basename, "flow.")) {
    basename = substr(basename, 6, nchar(basename))
  } else if (endsWith(basename, ".flow")) {
    basename = substr(basename, 0, nchar(basename) - 5)
  }
  name_clean = file.path(dirname(name), basename)
  worldfile = name_clean
  flownet_name = name_clean

  if (!dir.exists(dirname(name))) { # check if output dir exists, menu to create
    t = utils::menu(
      c("Yes", "No [Exit]"),
      title = paste("Ouput directory path:",dirname(name),"is not valid. Create folder(s)?"))
    if (t == 1) {
      dir.create(dirname(name), recursive = TRUE)
    }
    if (t == 2) {
      stop("RHESSysPreprocess.R exited without completing")
    }
  }

  if (!type %in% c("Raster", "RASTER", "raster", "GRASS", "GRASS6", "GRASS7")) { # check if type is valid
    stop(noquote(paste("Type '", type, "' not recognized.", sep = "")))
  }

  if (!is.logical(overwrite)) { # check overwrite type
    stop("Overwrite must be logical")
  }

  # ---------- Run world_gen ----------
  cat("Begin world_gen.R\n")

  if (file.exists(worldfile) & overwrite == FALSE) { # check for worldfile overwrite
    t = utils::menu(c("Yes", "No [Exit]"), title = noquote(paste(
      "Worldfile", worldfile, "already exists. Overwrite?"
    )))
    if (t == 2) {
      stop("RHESSysPreprocess.R exited without completing")
    }
  }

  #world_gen_out = world_gen(template = template,
  #                          worldfile = worldfile,
  #                          type = type,
  #                          typepars = typepars,
  #                          overwrite = overwrite,
  #                          header = header,
  #                          unique_strata_ID = unique_strata_ID,
  #                          asprules = asprules)

  #readin = world_gen_out[[1]]
  #asp_rules = world_gen_out[[2]]

  # ---------- Run create_flownet ----------
  cat("Begin create_flownet.R")

  if (file.exists(flownet_name) & overwrite == FALSE) { # check for flownet overwrite
    t = utils::menu(c("Yes", "No [Exit]"), title = noquote(paste(
      "Flowtable", flownet_name, "already exists. Overwrite?"
    )))
    if (t == 2) {
      stop("RHESSysPreprocess.R exited without completing")
    }
  }

  create_flownet(flownet_name = flownet_name,
                template = template,
                type = type,
                typepars = typepars,
                asprules = asprules,
                streams = streams,
                overwrite = overwrite,
                roads = roads,
                impervious = impervious,
                roofs = roofs,
                wrapper = wrapper,
                parallel = parallel,
                make_stream = make_stream,
                skip_hillslope_check = skip_hillslope_check)

  # ---------- Run build_meta ----------
  # if (meta) {
  #   build_meta(
  #     name = name_clean,
  #     world = worldfile,
  #     flow = flownet_name,
  #     template = template,
  #     type = type,
  #     typepars = typepars,
  #     cf_maps = readin,
  #     streams = streams,
  #     roads = roads,
  #     impervious = impervious,
  #     roofs = roofs,
  #     asp_rule = asprules
  #   )
  # }

  if (fire_grid_out) {
    cat("Writing fire grid files")
    write_fire_grids(name = name, template = template, map_dir = typepars, seq_patch_IDs = seq_patch_IDs)
  }


} # end function
