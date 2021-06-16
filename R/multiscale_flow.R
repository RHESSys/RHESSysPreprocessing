#' multiscale_flow
#'
#' Modifies an existing flowtable (R list) to work with multisacle routing.
#' @param CF1 Flowtable list object created from make_flow_list
#' @param asp_map Map of the aspatial rules
#' @param patch_map Map of the patches
#' @param asp_list List of aspatial rules
# Will Burke 1/16/19

multiscale_flow = function(CF1, asp_map, patch_map, asp_list) {

  #nbr 121
  #nbr patch 11955
  # no times nbr asp ct is empty
  #which(lapply(CF1,"[[", 9) == 15660)

  # functions for applys
  apply_patches = function(CFp) {
    id = paste0("rule_", unique(asp_map[which(raw_patch_data == CFp$PatchID)]))
    asp_count = ncol(rulevars[[id]]$patch_level_vars[1, ]) - 1 # get number of aspatial patches for current patch
    asp = c(1:asp_count)
    CFasp = lapply(asp, add_asp,CFp, id)
    unlist(CFasp,recursive = F)
    return(CFasp)
  }
  add_asp = function(asp, CFp, id) {
    CFasp = CFp
    CFasp$PatchID = CFp$PatchID * 100 + asp # aspatial patch ID is old patch ID *100 + aspatial number
    CFasp$Number = CFp$Number * 100 + asp # same modification to number
    CFasp$PatchFamilyID = CFp$PatchID # retain old patch ID as patch family ID
    CFasp$Area = CFp$Area * as.numeric(rulevars[[id]]$patch_level_vars[rulevars[[id]]$patch_level_vars[, 1] == "pct_family_area", asp + 1]) # change area
    # this output is an actual mess but whatever
    nbr_out = mapply(add_nbrs, nbr = CFp$Neighbors, gamma = CFp$Gamma_i, slope = CFp$Slopes, border = CFp$Border_perimeter, SIMPLIFY = F)
    CFasp$Neighbors = unlist(lapply(nbr_out, "[[", "new_nbrs"))
    CFasp$Gamma_i = unlist(lapply(nbr_out, "[[", "new_gammas"))
    CFasp$Slopes = unlist(lapply(nbr_out, "[[", "new_slopes"))
    CFasp$Border_perimeter = unlist(lapply(nbr_out, "[[", "new_borders"))

    return(CFasp)
  }
  add_nbrs = function(nbr, gamma, slope, border) {
    nbr_patch = patch_ID[numbers == nbr] # patch ID from number
    nbr_id = paste0("rule_", unique(asp_map[which(raw_patch_data == nbr_patch)])) # get rule ID
    nbr_asp_ct = ncol(rulevars[[nbr_id]]$patch_level_vars[1, ]) - 1
    new_slopes = rep(slope, nbr_asp_ct)
    new_borders = rep(border, nbr_asp_ct)
    new_nbrs = nbr * 100 + c(1:nbr_asp_ct)
    # original gamma is multiplied by respective areas of the new patches - should sum to original
    new_gammas = gamma * as.numeric(rulevars[[nbr_id]]$patch_level_vars[rulevars[[nbr_id]]$patch_level_vars[, 1] == "pct_family_area", 1 + c(1:nbr_asp_ct)])
    return(list("new_nbrs" = new_nbrs, "new_gammas" = new_gammas, "new_slopes" = new_slopes, "new_borders" = new_borders))
  }

  cat("Creating multiscale flowtable - this may take a moment with many patches")

  # ----- Variable setup -----
  patch_ID = unlist(lapply(CF1, "[[", 9)) # patch IDs from cf1
  numbers = unlist(lapply(CF1, "[[", 1)) # flow list numbers
  raw_patch_data = patch_map
  rulevars = asp_list # get rules - state variable overrides
  CF2 = list() # empty list for new flow list

  loop_ver = F
  if (loop_ver) {
    pb = txtProgressBar(min = 0, max = length(patch_ID), style = 3)
    ct = 0

    # ----- iterate through (spatial) patches -----
    #for (p in raw_patch_data[!is.na(raw_patch_data)]) {
    for (p in patch_ID) {
      ct = ct+1
      setTxtProgressBar(pb, ct)

      id = unique(asp_map[which(raw_patch_data == p)]) # get unique rule ID for patch p
      id = paste0("rule_", id)

      if (length(id) > 1) {
        stop(paste("multiple aspatial rules for patch", p))
      } # if multiple rules for a single patch
      asp_count = ncol(rulevars[[id]]$patch_level_vars[1, ]) - 1 # get number of aspatial patches for current patch

      # ----- iterate through aspatial patches -----
      for (asp in 1:asp_count) {
        CF2 = c(CF2, CF1[which(patch_ID == p)]) # copy (aspatial) patch values from old patch (family)
        CF2[[length(CF2)]]$PatchID = CF1[[which(patch_ID == p)]]$PatchID * 100 + asp # aspatial patch ID is old patch ID *100 + aspatial number
        CF2[[length(CF2)]]$Number = CF1[[which(patch_ID == p)]]$Number * 100 + asp # same modification to number
        CF2[[length(CF2)]]["PatchFamilyID"] = CF1[[which(patch_ID == p)]]$PatchID # retain old patch ID as patch family ID XXXXXXXXX IF CF2 DOESNT WORK REMOVE THIS
        CF2[[length(CF2)]]$Area = CF1[[which(patch_ID == p)]]$Area *
          as.numeric(rulevars[[id]]$patch_level_vars[rulevars[[id]]$patch_level_vars[, 1] == "pct_family_area", asp + 1]) # change area

        # Changes for each neighbor
        old_nbrs = CF1[[which(patch_ID == p)]]$Neighbors
        old_gammas = CF1[[which(patch_ID == p)]]$Gamma_i
        old_slope = CF1[[which(patch_ID == p)]]$Slope
        old_border = CF1[[which(patch_ID == p)]]$Border_perimeter

        new_nbrs = vector(mode = "numeric")
        new_gammas = vector(mode = "numeric")
        new_slope = vector(mode = "numeric")
        new_border = vector(mode = "numeric")

        for (nbr in old_nbrs) {
          # loop through old neighbors - neighbors are numbers not patches
          nbr_patch = patch_ID[numbers == nbr]
          nbr_id = asp_map[which(raw_patch_data == nbr_patch)]
          nbr_id = unique(paste0("rule_", nbr_id))
          if (length(nbr_id) > 1) {
            stop(paste("multiple aspatial rules for patch", nbr_patch))
          } # if multiple rules for a single patch
          nbr_asp_ct = ncol(rulevars[[nbr_id]]$patch_level_vars[1, ]) - 1
          gamma = old_gammas[which(old_nbrs == nbr)]
          new_slope = c(new_slope, rep(old_slope[old_nbrs == nbr], nbr_asp_ct))
          new_border = c(new_border, rep(old_border[old_nbrs == nbr], nbr_asp_ct))
          for (nbr_asp in 1:nbr_asp_ct) {
            # for each asp for each neighbor
            new_nbrs = c(new_nbrs, nbr * 100 + nbr_asp) # use same convention as above
            new_gammas = c(new_gammas,
                           gamma * as.numeric(rulevars[[nbr_id]]$patch_level_vars[rulevars[[nbr_id]]$patch_level_vars[, 1] == "pct_family_area", nbr_asp + 1]))
          }
        }

        CF2[[length(CF2)]]$Neighbors = new_nbrs
        CF2[[length(CF2)]]$Gamma_i = new_gammas
        CF2[[length(CF2)]]$Slopes = new_slope
        CF2[[length(CF2)]]$border = new_border

        # if (length(CF2) != 1 &
        #     length(unique(sapply(CF2, "[[", "PatchFamilyID"))) != 1) {
        #   for (i in 1:length(CF2[[length(CF2)]])) {
        #     if (is.na(CF2[[length(CF2)]])[[i]]) {
        #       stop("shouldn't have NAs")
        #     }
        #     if (length(CF2[[length(CF2)]][[i]]) == 0) {
        #       stop("shouldn't have numeric(0)'s")
        #     }
        #   }
        # }

      } # end aspatial patch loop
    } # end spatial patch loop
    close(pb)
  }

  # ---------- patch and aspatial apply ----------
  if (!loop_ver) {
    CF2 = lapply(CF1, apply_patches)
    CF2 = unlist(CF2, recursive = F)
  }

  return(CF2)
}
