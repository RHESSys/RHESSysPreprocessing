#' read_in_flow2
#'
#' Read in flow table file and return a R list. Should be faster than the other one.
#' @param input_file Flow table to be read in.
#' @param df should a data frame also be output?
#' @author William Burke
#' @export
#'
read_in_flow <- function(input_file, df = FALSE) {
  cat("Reading in flow table")

  parallel <- length(scan(file = input_file, nlines = 1, skip = 1, quiet = TRUE)) == 2 # check if flow table is parallelized, if 2nd line is 2 elements long

  if (parallel) {
    num_hills <- scan(file = input_file, nlines = 1, quiet = TRUE) # read in number of hillslopes
  }

  if (!parallel) {
    num_patches <- scan(file = input_file, nlines = 1, quiet = TRUE) # read in number of patches
  }

  # flow_raw = readLines(input_file,warn = FALSE) # read in flow table
  flow_list <- strsplit(trimws(readLines(input_file, warn = FALSE)), "\\s+") # split strings into list of char vectors
  len <- sapply(flow_list, length) # get lengths
  p_ind <- len == max(unique(len)) # patches

  # convert to list
  flw_df <- data.frame(matrix(as.numeric(unlist(flow_list[p_ind])), ncol = 11, byrow = TRUE), stringsAsFactors = FALSE)
  names(flw_df) <- c("PatchID", "ZoneID", "HillID", "Centroidx", "Centroidy", "Centroidz", "Accumulated_Area", "Area", "Landtype", "TotalG", "NumAdjacentPatches")
  flw_df <- cbind("Number" = 1:length(flw_df[, 1]), flw_df)

  flw <- split(flw_df, seq(nrow(flw_df)))
  flw <- lapply(flw, FUN = as.list)

  n_ind2 <- rep(1:length(flw_df[, "NumAdjacentPatches"]), flw_df[, "NumAdjacentPatches"]) # factor for neighbors
  n_ind3 <- len == 4 # index of neighbors in flow_list

  neighbors <- data.frame(matrix(as.numeric(unlist(flow_list[n_ind3])), ncol = 4, byrow = TRUE)) # matrix of neighbor
  names(neighbors) <- c("PatchID", "ZoneID", "HillID", "Gamma_i")
  neighbors <- cbind(neighbors, "row" = 1:length(neighbors[, 1])) # row var for sorting
  neighbors <- merge(neighbors, flw_df[, c("Number", "PatchID", "ZoneID", "HillID")], sort = FALSE) # merge to get IDs corrrect
  neighbors <- neighbors[order(neighbors$row), ] # sort by row

  nbr_p <- split(neighbors$Number, f = n_ind2) # neighbor numbers (NOT PATCH NUMBERS)
  nbr_g <- split(neighbors$Gamma_i, f = n_ind2) # gammas

  # Identify patches with no neighbors and add them to neighbors lists
  no_neighbors <- flw_df$Number[!flw_df$Number %in% unique(n_ind2)]
  for (i in no_neighbors){
    nbr_p <- append(nbr_p, list(vector(mode="numeric", length=0)), after = i-1)  # To match the syntax in make_flow_list, patches with no neighbors need be numeric(0)
    names(nbr_p)[i] <- i
    nbr_g <- append(nbr_g, list(vector(mode="numeric", length=0)), after = i-1)  # To match the syntax in make_flow_list, patches with no neighbors need be numeric(0)
    names(nbr_g)[i] <- i
  }
  
  list_append <- function(X, Y, Z) { # to append to the list, but keep the appended vector together, and not split those elements up
    X[[Z]] <- Y
    return(X)
  }

  flw <- mapply(FUN = list_append, flw, nbr_p, "Neighbors", SIMPLIFY = FALSE) # append neighbors
  flw <- mapply(FUN = list_append, flw, nbr_g, "Gamma_i", SIMPLIFY = FALSE) # append gammas

  if (df) {
    return(list(flw, flw_df))
  } else if (!df) {
    return(flw)
  }
}
