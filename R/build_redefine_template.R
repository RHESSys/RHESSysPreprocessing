#' Build Redefine Template
#'
#' Builds a template file for use in a redefine world. Pair with world_redefine.
#' @param template Original template file used to generate your worldfile for RHESSys.
#' @param name Name of output redefine template to be created
#' @param overwrite Overwrite existing redefine template? FALSE is default and prompts a menu if worldfile already exists.
#' @author Will Burke
#' @export

build_redefine_template = function(template, name, overwrite) {

  template_list = template_read(template)

  template_clean = template_list[[1]] # template in list form
  level_index = template_list[[3]] # index of level separators in template_clean/var_names
  var_index = template_list[[4]] # index of vars

  redef_template = template_clean

  # Set all vars to -9999
  for (i in var_index) {
    if (length(redef_template[[i]]) == 3){
      redef_template[[i]] = c(redef_template[[i]][1], "value", "-9999")
      }
    if (length(redef_template[[i]]) == 4){
      redef_template[[i]] = c(redef_template[[i]][1], "value", "-9999", "-9999")
      }
  }

  # print template
  template_write = vector(mode = "character")
  for (i in min(level_index):length(redef_template)) {
    indent = rep(x = "\t", sum(level_index < i))
    template_write = c(template_write, indent, redef_template[[i]], "\n")
  }

  sink(name)
  cat(template_write)
  sink()

}
