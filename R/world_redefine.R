#' World Redefine
#'
#' Generates redefine worldfiles for use in RHESSys specifically to modify exisitng worldfile values via
#' replacing values or multipliers.
#' Currently includes functionality for GRASS GIS (depreciated) and raster data, and works on both unix and windows. 3/4/19.
#' @param template Template file used to generate redefine worldfile for RHESSys. Use "-9999" to retain
#' values of exisintg worldfile. Generic strucutre is:
#' <state variable> <operator> <value/map>. Levels are difined by lines led by "_", structured
#' <levelname> <map> <count>. Whitespace and tabs are ignored.  Maps referred to must be supplied
#' by your chosen method of data input(GRASS or raster), set using the "type" arguement.
#' @param name Name and path of redefine worldfile to be created.
#' @param map_dir Parameters needed based on input data type used. If using raster type, typepars should be a string
#' indicating the path to a folder containing the raster files that are referenced by the template.
#' For GRASS GIS type, typepars is a vector of 5 character strings. GRASS GIS parameters: gisBase, home, gisDbase, location, mapset.
#' Example parameters are included in an example script included in this package. See initGRASS help
#' for more info on parameters.
#' @param overwrite Overwrite existing redefine worldfile. FALSE is default and prompts a menu if worldfile already exists.
#' @seealso \code{\link{readRAST}}, \code{\link{raster}}
#' @author Will Burke
#' @export

world_redefine = function(template,
                          name,
                          map_dir,
                          overwrite = FALSE) {
  world_gen(
    template = template,
    worldfile = name,
    map_dir = map_dir,
    overwrite = overwrite,
    header = FALSE,
    asprules = NULL
  )
}
