#' create_flownet
#'
#' Creates the flow networkd table used by RHESSys
#' @param flownet_name The name of the flow network file to be created.  Will be coerced to have ".flow" extension if not already present.
#' @param asprules List of aspatial structure and inputs. Also can be path to rules file - must be used along with template input.
#' @param road_width >0, defaults to 1.
#' @inheritParams RHESSysPreprocess
#' @author Will Burke
#' @export

create_flownet = function(flownet_name,
                         template = NULL,
                         map_dir = NULL,
                         asprules = NULL,
                         streams = NULL,
                         overwrite = FALSE,
                         roads = NULL,
                         road_width = NULL,
                         impervious = NULL,
                         roofs = NULL,
                         parallel = TRUE,
                         make_stream = 4,
                         skip_hillslope_check = FALSE,
                         wrapper = FALSE) {

  # ------------------------------ Read and check inputs ------------------------------
  cfbasename = basename(flownet_name) # Coerce .flow extension
  if (startsWith(cfbasename,"Flow.") | startsWith(cfbasename,"flow.")) {
    cfbasename = paste(substr(cfbasename,6,nchar(cfbasename)),".flow",sep = "")
  } else if (!endsWith(cfbasename,".flow")) {
    cfbasename = paste(cfbasename,".flow",sep = "")
  }
  flownet_name = file.path(dirname(flownet_name),cfbasename)

  if (!is.logical(overwrite)) {stop("overwrite must be logical")} # check overwrite inputs
  if (file.exists(flownet_name) & overwrite == FALSE) {stop(noquote(paste("Flowtable",flownet_name,"already exists.")))}

  template_list = template_read(template)
  template_clean = template_list[[1]] # template in list form
  var_names = template_list[[2]] # names of template vars
  map_info = template_list[[5]] # tables of maps and their inputs/names in the template
  cfmaps = rbind(map_info,c("cell_length","none"), c("streams","none"), c("roads","none"), c("impervious","none"),c("roofs","none"))

  # Check for streams map, menu allows input of stream map
  if (is.null(streams) & (cfmaps[cfmaps[,1] == "streams",2] == "none" | is.na(cfmaps[cfmaps[,1] == "streams",2]))) {
    t = utils::menu(c("Specify map","Abort function"),
             title = "Missing stream map. Specify one now, or abort function and edit cf_maps file/readin input?")
    if (t == 2) {stop("Function aborted")}
    if (t == 1) {
      streams = readline("Stream map:")
    }
  }
  # add stream map to cfmaps if it's not there already
  if ((cfmaps[cfmaps[,1] == "streams",2] == "none" | is.na(cfmaps[cfmaps[,1] == "streams",2]))) {
    cfmaps[cfmaps[,1] == "streams",2] = streams
  }

  # add road, impervious, and roofs to maps to get
  if (!is.null(roads)) {
    cfmaps[cfmaps[,1] == "roads",2] = roads
    if (is.null(road_width)) {stop("If using roads, road width cannot be 0.")}
  }
  if (!is.null(impervious)) {cfmaps[cfmaps[,1] == "impervious",2] = impervious}
  if (!is.null(roofs)) {cfmaps[cfmaps[,1] == "roofs",2] = roofs}

  # remove tif and tiff extensions for simplicity
  if (all(!is.na(cfmaps[,2]))) {
    if ( any(endsWith(cfmaps[,2],".tif") | endsWith(cfmaps[,2],".tiff")) ) {
      cfmaps[,2] = gsub(".tif$","",cfmaps[,2])
      cfmaps[,2] = gsub(".tiff$","",cfmaps[,2])
    }
  }

  # check inputs are maps or values
  notamap = cfmaps[suppressWarnings(which(!is.na(as.numeric(cfmaps[,2])))),1]
  maps_in = unique(cfmaps[cfmaps[,2] != "none" & !cfmaps[,1] %in% notamap,2])

  # ------------------------------ Use GIS_read to get maps ------------------------------
  readmap = GIS_read(maps_in = maps_in, map_dir = map_dir, map_info = cfmaps)

  # map_list = lapply(readmap@data, matrix, nrow = readmap@grid@cells.dim[1], ncol = readmap@grid@cells.dim[2])
  map_list = lapply(readmap, matrix, nrow = ncol(readmap), ncol = nrow(readmap))
  names(map_list) = names(readmap)

  raw_patch_data = map_list[[cfmaps[cfmaps[, 1] == "patch", 2]]]
  raw_hill_data = map_list[[cfmaps[cfmaps[, 1] == "hillslope", 2]]]
  raw_basin_data = map_list[[cfmaps[cfmaps[, 1] == "basin", 2]]]
  raw_zone_data = map_list[[cfmaps[cfmaps[, 1] == "zone", 2]]]
  raw_stream_data = map_list[[cfmaps[cfmaps[, 1] == "streams", 2]]]

  if ("slope" %in% notamap) {
    raw_slope_data = raw_patch_data
    raw_slope_data[!is.na(raw_slope_data)] = as.numeric(cfmaps[cfmaps[,1] == "slope",2])
  } else {
    raw_slope_data = map_list[[unique(cfmaps[cfmaps[, 1] == "slope", 2])]]
  }
  if ("z" %in% notamap) {
    raw_patch_elevation_data = raw_patch_data
    raw_patch_elevation_data[!is.na(raw_patch_elevation_data)] = as.numeric(cfmaps[cfmaps[,1] == "z",2])
  } else {
    raw_patch_elevation_data = map_list[[unique(cfmaps[cfmaps[, 1] == "z", 2])]]
  }
  cell_length = terra::res(readmap)[1]
  # Roads
  raw_road_data = NULL
  if (!is.null(roads)) {raw_road_data =  map_list[[cfmaps[cfmaps[,1] == "roads",2]]]}
  # Roofs and impervious is not yet implemented - placeholders for now -----
  if (!is.null(roofs) | !is.null(impervious)) {print("Roofs and impervious are not yet working probably i think",quote = FALSE)}
  raw_roof_data = NULL
  if (!is.null(roofs)) {raw_roof_data =  map_list[[cfmaps[cfmaps[,1] == "roofs",2]]]}
  raw_impervious_data = NULL
  if (!is.null(impervious)) {raw_impervious_data =  map_list[[cfmaps[cfmaps[,1] == "impervious",2]]]}

  map_df = as.data.frame(readmap)

  # read aspatial rules if needed
  if (!is.null(asprules)) {
    asp_map = template_clean[[which(var_names == "asp_rule")]][3] # get rule map/value
    patch_map = map_df[[cfmaps[cfmaps[,1] == "patch",2]]] # set for use later - overwrite if using mode

    if (suppressWarnings(is.na(as.numeric(asp_map)))) { # if it's a map
      asp_map = gsub(".tif|.tiff","",asp_map)
      asp_mapdata = map_df[asp_map]

      # --- doing manipulation of the asp map if needed here ---
      if (template_clean[[which(var_names == "asp_rule")]][2] == "mode") {
        # mode for aggregating by mode
        mode_fun = function(x) {
          ux <- unique(x)
          ux[which.max(tabulate(match(x, ux)))]
        }
        # fun to check if rules are all unique per patch (kinda slow but oh well) -----
        check_rules = function(patches, asp_rules) {
          patches_u = unique(patches[!is.na(patches)])
          patches_i = lapply(patches_u, function(x, y) {which(x == y)}, patches)
          outrules = lapply(patches_i, function(x, y) {y[x]}, asp_rules)
          lens = sapply(outrules, function(x) {length(unique(x))})
          lens_i = which(unlist(lens) > 1)
          if (length(lens_i) > 0) {
            cat("There are patches with multiple multiscale rules:\n")
            out = data.frame(patch = patches[lens_i], rules = sapply(outrules[lens_i], function(x) {paste(unique(x), collapse=", ") }) )
            return(out)
          } else {
            return(NULL)
          }
        }

        # level_names = unname(c(map_info[map_info[, 1] == "basin", 2],
        #                        map_info[map_info[, 1] == "hillslope", 2],
        #                        map_info[map_info[, 1] == "zone", 2],
        #                        map_info[map_info[, 1] == "patch", 2],
        #                        map_info[map_info[, 1] == "strata", 2]))
        level_names = unname(c(map_info[map_info[, 1] == "zone", 2],
                               map_info[map_info[, 1] == "patch", 2]))
        level_names = unique(gsub(".tiff|.tif|.asc","",level_names))

        asp_maps = aggregate(map_df[[cfmaps[cfmaps[,1] == "asp_rule",2]]], by = map_df[level_names], FUN = mode_fun, simplify = T)
        names(asp_maps)[which(names(asp_maps) == "x")] = "asprule"

        patch_map = asp_maps[[cfmaps[cfmaps[,1] == "patch",2]]]

        rules_out = check_rules(patches = patch_map, asp_rules = asp_maps$asprule)

        if (!is.null(rules_out)) {
          print(rules_out)
          stop("Mode of rules was attempted but there are still patches with multiple rules. Check input maps.")
        }
        asp_mapdata = asp_maps$asprule
      }

    } else { # if is a single number
      asp_mapdata = map_df[[cfmaps[cfmaps[,1] == "basin",2]]]
      asp_mapdata[!is.na(asp_mapdata)] = as.numeric(asp_map)
    }
    asp_list = aspatial_patches(asprules = asprules, asp_mapdata = asp_mapdata)
  }

  # ------------------------------ Make flownet list ------------------------------
  cat("\nBuilding flowtable\n")
  CF1 = make_flow_list(
    raw_patch_data = raw_patch_data,
    raw_patch_elevation_data = raw_patch_elevation_data,
    raw_basin_data = raw_basin_data,
    raw_hill_data = raw_hill_data,
    raw_zone_data = raw_zone_data,
    raw_slope_data = raw_slope_data,
    raw_stream_data = raw_stream_data,
    raw_road_data = raw_road_data,
    road_width = road_width,
    cell_length = cell_length,
    parallel = parallel,
    make_stream = make_stream,
    skip_hillslope_check = skip_hillslope_check)

  # ------------------------------ Multiscale routing/aspatial patches ------------------------------
  if (!is.null(asprules)) {
    CF1 = multiscale_flow(CF1 = CF1, asp_map = asp_mapdata, patch_map = patch_map, asp_list = asp_list)
  }

  # ---------- Flownet list to flow table file ----------
  cat("Writing flowtable")
  make_flow_table(flw = CF1, output_file = flownet_name, parallel = parallel)

  cat("Created flowtable: ",flownet_name)

}
