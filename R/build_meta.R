#' build_meta
#'
#' Create documentation/metadata for RHESSys Preprocessing. 2/27/18
#' @param name Name and path for metadata. ".meta" will be appended automatically.
#' @param world worldfile
#' @param flow flowtable
#' @param template template
#' @param map_dir dir for spatial input
#' @param cf_maps additional maps input for flownet creation
#' @param streams stream map
#' @param roads roads map
#' @param impervious impervious map
#' @param roofs roofs map
#' @param asp_rule aspatial rule file
#' @return Writes metadata with name and path indicated in name
#' @author Will Burke
#'

# Include: spatial data used for each input, original source? , author, computer used, date time, worldfile and flownet produced

build_meta = function(name,
                      world = NULL,
                      flow = NULL,
                      template,
                      map_dir,
                      cf_maps,
                      streams = NULL,
                      roads = NULL,
                      impervious = NULL,
                      roofs = NULL,
                      asp_rule = NULL) {

  #---------- Build table ----------

  table_name = paste(name,".meta",sep="")

  template_list = template_read(template)

  vars = c(
    "Project Name",
    "Template",
    "Worldfile",
    "Flowtable",
    "Author",
    "Computer",
    "Datetime",
    "Map Path",
    "world",
    "basin",
    "zone",
    "hillslope",
    "patch",
    "strata",
    "streams",
    "roads",
    "impervious",
    "roofs",
    "asp_rule",
    "z",
    "slope",
    "aspect",
    "e_horizon",
    "w_horizon",
    "gw.storage",
    "veg_parm_ID",
    "soil_parm_ID",
    "landuse_parm_ID"
  )

  meta_out = matrix(ncol = 2, nrow = length(vars))
  meta_out[,1] = vars
  colnames(meta_out) = c("varname", "value")
  rownames(meta_out) = vars

  #----------- System info ----------
  get_sys = Sys.info()

  meta_out["Author", 2] = get_sys["user"]
  meta_out["Computer", 2] = get_sys["nodename"]
  meta_out["Datetime", 2] = as.character(Sys.time())

  #---------- Map info ----------

  meta_out["Project Name",2] = basename(name)
  meta_out["Worldfile",2] = world
  meta_out["Template",2] = template
  meta_out["Flowtable",2] = flow
  meta_out["Map Path", 2] = paste(getwd(),map_dir,sep="")

  if(!is.null(roads)){meta_out["roads",2] = roads}
  if(!is.null(impervious)){meta_out["impervious",2] = impervious}
  if(!is.null(roofs)){meta_out["roofs",2] = roofs}
  if(!is.null(streams)){meta_out["streams",2] = streams}
  if(!is.null(asp_rule)){meta_out["asp_rule",2] = asp_rule}

    for (i in which(meta_out[,1] =="z"):length(meta_out[,1])) {
    if(meta_out[i,1] %in% template_list[[5]][,1]){
      meta_out[i,2] = template_list[[5]][template_list[[5]][,1]==meta_out[i,1],2]
    } else if(sum(meta_out[i,1] == template_list[[2]])==1) {
      meta_out[i,2] = template_list[[1]][[which(meta_out[i,1] == template_list[[2]])]][3]
      }
  }

  utils::write.table(meta_out,file = table_name,row.names = FALSE)

  print(paste("Created metadata:",table_name),quote=FALSE)

}
