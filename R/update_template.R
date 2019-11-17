#' update_template
#' 
#' Read and modify an existing template
#' @param template Name and path of templatefile to be created.
#' @param out_file Destination file to write new template
#' @param vars State variable(s) to be modified
#' @param operator Operator to be (optionally) replaced. If  not included the existing operator (e.g. 'value') will be left.
#' @param values Values to replace for `vars`. N args must equal that of vars or be 1 (will be used for all vars).
#' @param level_names Not implamented yet - for rare cases where you only want to replace 1 occurance of a state var (e.g. 'z' at zone but not patch ele)
#' @param overwrite TRUE/FALSE if input file should be overwriten
#' @author Will Burke
#' @export

update_template = function(template, out_file, vars, values, operator = NULL, level_names = NULL, overwrite = FALSE) {
  
  # NOTES
  # - vdouble check all the list options work and check var lengths/numbers when needed
  
  # ---------- Check Aguments ----------
  if (is.null(out_file) & !overwrite) {stop(noquote("No destination file set by 'out_file' and 'overwrite' is FALSE"))}
  if ( file.exists(out_file) & overwrite == FALSE) {stop(noquote(paste0("File '",out_file,"' already exists and 'overwrite' argument is FALSE")))}
  if (length(vars) != length(values) & length(values) != 1) { stop(noquote(paste0("Vars and values have different lengths"))) }
    

  if (!is.character(values)) {
    values = as.character(values)
  }
  
  
  # for (i in length(vars)) {
  #   if (level_names[i] == "all") {level_names[i] = c("world", "basin", "hillslope", "zone", "patch", "canopy_strata")}
  # }
  
  # ---------- Parse templatefile ----------
  options(scipen = 999)
  # parsing the values as characters to retain the exact value/precision
  read_template = readLines(template)
  template =  strsplit(trimws(read_template), "\\s+")
  
  #template = data.frame(matrix(unlist(template), nrow=length(template), byrow=T),stringsAsFactors = FALSE)
  #names(template) = c("values","vars")
  
  # ---------- Find Levels----------
  template_vars = sapply(template,"[[",1)
  index_all = which(template_vars == "_world" | template_vars == "_basin" | template_vars == "_hillslope" | 
                      template_vars == "_zone" | template_vars == "_patch" | template_vars == "_canopy_strata")
  index_names = gsub("^_", "", x = template_vars[index_all])
  index_max = c(index_all[2:length(index_all)]-1, length(read_template))
  template_levels = unname(unlist(mapply(rep,index_names, (index_max - index_all) + 1 )))
  
  # ---------- Find and Replace Vars ----------
  for (i in length(vars)){
    
    if (!is.null(level_names[i])) {
      find_index = template_levels == level_names[i]
    } else {
      find_index = rep(TRUE,length(template_levels))
    }

    replace_index = which(template_vars == vars[i] & find_index)
    
    # if unique values for every instance of var to be replaces were given, do nothing, otherwise repeat to get enough replacement values
    if (length(replace_index) > 1) {stop(noquote("Too many things to replace havet coded this yet"))}
    
    if (template_levels[replace_index] == "canopy_strata") {
      current_value = template[[replace_index]][c(length(template[[replace_index]]) - 1, length(template[[replace_index]]))]
    } else {
      current_value = template[[replace_index]][length(template[[replace_index]])]
    }
    
    if (length(values[i]) != length(replace_index)) {
      new_value = rep(values[i], length(replace_index)/length(values[i]))
    } else {
      new_value = values[i]
    }
    
    if (any(startsWith(new_value,"*"))) {
      new_value = as.numeric(trimws(substr(new_value[startsWith(new_value,"*")],2,nchar(new_value[startsWith(new_value,"*")]))))
      new_value = new_value * as.numeric(current_value)
    } else {
      new_value = as.numeric(trimws(new_value))
    }
    
    # generic sub/gsub
    #sub(paste0("[[:blank:]]",current_value,"$"), paste0("\t",new_value), read_template[replace_index])
    
    read_template[replace_index] = unname(mapply(sub,paste0("[[:blank:]]",current_value,"$"), paste0("\t",new_value),read_template[replace_index]))
    
  }
  
  # ---------- Write file ----------
  writeLines(text = read_template,out_file)
  
  print(noquote(paste("Successfully wrote updated template to",out_file)))
  
  
}
