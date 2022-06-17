#' patch_data_analysis
#'
#' This function is the main component of the larger create_flownet function.
#' Function finds neighbors, calculates flow, accounts for pits, roads, streams etc.
#' cell_length is size of cell in meters.
#' @param raw_patch_data patch matrix
#' @param raw_patch_elevation_data DEM matrix
#' @param raw_hill_data hillslope matrix
#' @param raw_basin_data basin matrix
#' @param raw_zone_data zone matrix
#' @param raw_slope_data slope matrix
#' @param raw_stream_data stream map matrix
#' @param raw_road_data road map matrix
#' @param cell_length cell length in meters
#' @inheritParams RHESSysPreprocess
#' @inheritParams create_flownet
#' @author Daniel Nash
#' @author Will Burke
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar

make_flow_list <- function(raw_patch_data,
                           raw_patch_elevation_data,
                           raw_hill_data,
                           raw_basin_data,
                           raw_zone_data,
                           raw_slope_data,
                           raw_stream_data,
                           raw_road_data,
                           cell_length,
                           road_width = NULL,
                           parallel,
                           make_stream,
                           skip_hillslope_check) {

  # -------------------- Error checking and NULL handling --------------------
  if (cell_length <= 0) {stop("Cell length is <=0")}
  if (is.null(raw_road_data) & length(raw_patch_data) != 1) {
    raw_road_data = matrix(0,ncol = ncol(raw_patch_data),nrow = nrow(raw_patch_data))
  } else if (is.null(raw_road_data) & length(raw_patch_data) == 1) {
    raw_road_data = 0
  }
  if (is.null(road_width)) {road_width = 0}

  # area_conv = cell_length*cell_length   #meters^2 per patch (need actual number)

  options(scipen = 999) # IF DEBUGGING YOU WILL HAVE ERRORS ON LARGE BASINS WITHOUT THIS - comes from numeric to character conversion

  # -------------------- Build unique patch IDs --------------------
  cat("Generating unique patch IDs")

  id_data = data.frame("Basin" = as.vector(raw_basin_data[!is.na(raw_basin_data)]),
                       "Hill" = as.vector(raw_hill_data[!is.na(raw_basin_data)]),
                       "Zone" = as.vector(raw_zone_data[!is.na(raw_basin_data)]),
                       "Patch" = as.vector(raw_patch_data[!is.na(raw_basin_data)]),
                       "Row" = 1:length(as.vector(raw_basin_data[!is.na(raw_basin_data)])))

  id_data_unique = unique(id_data[-5]) # unique except for row num
  id_data_unique$Number = 1:length(id_data_unique$Basin) # unique ID number
  id_merge = merge(id_data,id_data_unique,sort = FALSE ) # merge unique and original id data
  id_merge = id_merge[ order(id_merge$Row),] # order by original row order
  unique_patch = raw_basin_data
  unique_patch[!is.na(unique_patch)] = id_merge$Number

  # ----- Conversion and formatting -----
  patch_data = unique_patch # unique patch IDs
  patch_data[is.na(patch_data)] <- 0 #replace NAs w 0
  patch_data = as.matrix(patch_data)
  patch_elevation_data = raw_patch_elevation_data
  patch_elevation_data[is.na(patch_elevation_data)] = 0   #Replace NA's with 0
  patch_slope_data = raw_slope_data
  patch_slope_data[is.na(patch_slope_data)] = 0   #Replace NA's with 0
  patch_stream_data = raw_stream_data
  patch_stream_data[is.na(patch_stream_data)] = 0   #Replace NA's with 0
  patch_road_data = raw_road_data
  patch_road_data[is.na(patch_road_data)] <- 0   #Replace NA's with 0
  y <- as.vector(patch_data)
  z <- as.vector(patch_elevation_data)
  w <- as.vector(patch_slope_data)
  x <- as.vector(patch_stream_data)
  x1 <- as.vector(patch_road_data)
  patches <- as.vector(unique(y))     #find individual patches
  patches <- sort(patches)
  patch_mean_elev <- tapply(z,y,mean)  #create vector of patch elevation means
  patch_mean_elev <- as.vector(patch_mean_elev)
  patch_mean_slope <- tapply(w,y,mean)  #create vector of patch slope means
  patch_mean_slope <- as.vector(patch_mean_slope)
  patch_landtype <- tapply(x,y,max)  #create vector of 1's if there is a stream in the patch
  patch_landtype <- as.vector(patch_landtype)
  patch_landtype[patch_landtype > 0] <- 1
  patch_roadtype <- tapply(x1,y,max)  #create vector of 1's if there is a road in the patch
  patch_roadtype <- as.vector(patch_roadtype)
  patch_roadtype[patch_roadtype > 0] <- 1

  # this might not be needed, i think a legacy from old unique ID gen - test eventually
  if (patches[1] == 0) {
    patches <- patches[-1]       #throw out patch 0
    patch_mean_elev <- patch_mean_elev[-1]     #throw out patch 0
    patch_mean_slope <- patch_mean_slope[-1]     #throw out patch 0
    patch_landtype <- patch_landtype[-1]     #throw out patch 0
    patch_roadtype <- patch_roadtype[-1]     #throw out patch 0
  }

  # -------------------- Patch centroid calc --------------------
  # Find the average row and column of each patch, Variables are, patch_data: all patches w unique id and NA's replaced
  # by 0, num_patches: number of unique pathces, cell_length: size of individual cells.
  num_patches = length(patches)
  patch_coord <- matrix(0,nrow = num_patches,ncol = 3)   #x,y positions of each patch, third col is total count.
  for (i in 1:nrow(patch_data)) {
    for (j in 1:ncol(patch_data)) {
      if (patch_data[i,j] != 0) {
        patch_coord[patch_data[i,j],1] = patch_coord[patch_data[i,j],1] + j # sum of x coordinate of cell
        patch_coord[patch_data[i,j],2] = patch_coord[patch_data[i,j],2] + i # sum of y coordinate of cell
        patch_coord[patch_data[i,j],3] = patch_coord[patch_data[i,j],3] + 1# count of cells
      }
    }
  }
  patch_coord[,1:2] <- patch_coord[,1:2]/patch_coord[,3]
  patch_coord[,3] <- cell_length^2*patch_coord[,3]
  xy_data = patch_coord

  flw_struct <- data.frame(patches,xy_data,patch_mean_elev,patch_mean_slope,patch_landtype,patch_roadtype)
  colnames(flw_struct) <- c("Number","Centroidx","Centroidy","Area","Centroidz","Mean_Slope","Landtype","Roadtype")
  flw_struct <- cbind(flw_struct,id_data_unique[-5])

  # -------------------- find neighbors cell by cell (previously find border row) --------------------
  # patch_data is patch data with NA replaced by 0, patches is a vector of ordered patch numbers
  # patch_borders is an array, initailly 0 but will be filled with the number of times patch i
  # touches patch j. the diagonal will be the number of times patch i touches anything.
  # -------------------- D8 neighbor search and border count --------------------
  cat("Finding patch neighbors")

  # new - list instead of matrix
  patch_borders = list(list("Total" = 0))[rep(1,length(patches))]
  p_rows <- nrow(patch_data)
  p_cols <- ncol(patch_data)
  # <><><> this is the modifier for diagonal borders. scales inversely with cell size <><><>
  diag_border = 1/sqrt(2*cell_length)

  pb = txtProgressBar(min = 0,max = sum(patch_data != 0),style = 3)
  ct = 0
  for (i in 1:p_rows) { # loop through all rows and cols of input patch data
    for (j in 1:p_cols) {
      if (patch_data[i,j] != 0) { # only look for neighbors if current cell is actually a patch
        ct = ct + 1
        setTxtProgressBar(pb,ct)
        if (j < p_cols) { # ----- all rows, all cols except last
          if (patch_data[i,j] != patch_data[i,j + 1] & patch_data[i,j + 1] != 0) { # east - is different patch and is not 0
            p1 <- which(patches == patch_data[i,j])   #index of patch i,j
            p2 <- which(patches == patch_data[i,j + 1])  #index of patch i,j+1
            patch_borders[[p1]]$Total = patch_borders[[p1]]$Total + 1
            patch_borders[[p1]][[as.character(p2)]] = sum(patch_borders[[p1]][[as.character(p2)]]) + 1
            patch_borders[[p2]]$Total = patch_borders[[p2]]$Total + 1
            patch_borders[[p2]][[as.character(p1)]] = sum(patch_borders[[p2]][[as.character(p1)]]) + 1

          } # end east
        }
        if (i < p_rows & j < p_cols) { # ----- all rows/cols except last
          if (patch_data[i,j] != patch_data[i + 1,j + 1] & patch_data[i + 1,j + 1] != 0) { # southeast - is different patch and is not 0
            p1 <- which(patches == patch_data[i,j])   #index of patch i,j
            p2 <- which(patches == patch_data[i + 1,j + 1])  #index of patch i+1,j+1
            patch_borders[[p1]]$Total = patch_borders[[p1]]$Total + diag_border
            patch_borders[[p1]][[as.character(p2)]] = sum(patch_borders[[p1]][[as.character(p2)]]) + diag_border
            patch_borders[[p2]]$Total = patch_borders[[p2]]$Total + diag_border
            patch_borders[[p2]][[as.character(p1)]] = sum(patch_borders[[p2]][[as.character(p1)]]) + diag_border
          } # end southeast
        }
        if (i < p_rows) { # ----- all rows except last, all cols
          if (patch_data[i,j] != patch_data[i + 1,j] & patch_data[i + 1,j] != 0) { # south - is different patch and is not 0
            p1 <- which(patches == patch_data[i,j])   #index of patch i,j
            p2 <- which(patches == patch_data[i + 1,j])  #index of patch i+1,j
            patch_borders[[p1]]$Total = patch_borders[[p1]]$Total + 1
            patch_borders[[p1]][[as.character(p2)]] = sum(patch_borders[[p1]][[as.character(p2)]]) + 1
            patch_borders[[p2]]$Total = patch_borders[[p2]]$Total + 1
            patch_borders[[p2]][[as.character(p1)]] = sum(patch_borders[[p2]][[as.character(p1)]]) + 1
          } # end south
        }
        if (i < p_rows & j > 1) { # ----- all rows except last, all cols except first
          if (patch_data[i,j] != patch_data[i + 1,j - 1] & patch_data[i + 1,j - 1] != 0) { # southwest - is different patch and is not 0
            p1 <- which(patches == patch_data[i,j])   #index of patch i,j
            p2 <- which(patches == patch_data[i + 1,j - 1])  #index of patch i+1,j
            patch_borders[[p1]]$Total = patch_borders[[p1]]$Total + diag_border
            patch_borders[[p1]][[as.character(p2)]] = sum(patch_borders[[p1]][[as.character(p2)]]) + diag_border
            patch_borders[[p2]]$Total = patch_borders[[p2]]$Total + diag_border
            patch_borders[[p2]][[as.character(p1)]] = sum(patch_borders[[p2]][[as.character(p1)]]) + diag_border
          } # end southwest
        }

      } # end if not 0
    } # end p_cols loop
  } # end p_rows loop
  close(pb)

  # check if no streams of any kind - set lowest patch to stream
  if (all(flw_struct$Landtype == 0)) {
    flw_struct[flw_struct$Centroidz == min(flw_struct$Centroidz),"Landtype"] = 1
  }

  # ---------- Hillslope parallelization checks ----------
  if (parallel) {
    # ----- Find and fix hillslopes without stream outlets -----
    # outlet patches missing streams that were fixed
    no_stream_fix = NULL
    # outlet patches missing streams that were NOT fixed
    no_stream = NULL
    # keep track of distances form fixed/unfixed patches to the existing streams
    print_dist_fix = NULL
    print_dist = NULL

    # find hillslopes without outlet patches - lowest elev with streams
    min_hill_patch = stats::aggregate(flw_struct$Centroidz,by = list(flw_struct$Hill),FUN = which.min) # min elev patch for each hillslope
    # this is patch index actually, was patchID, now patch_ind
    names(min_hill_patch) = c("hillID", "patch_ind")
    hill_no_outlets = matrix(0, nrow = length(unique(flw_struct$Hill)), ncol = 2)
    colnames(hill_no_outlets) = c("hillID", "min_patch_landtype")
    hill_no_outlets[,1] = unique(flw_struct$Hill)
    for (h in hill_no_outlets[,1]) {
      hill_no_outlets[hill_no_outlets[,1] == h,2] = flw_struct[flw_struct$Hill == h,][min_hill_patch[min_hill_patch[,1] == h,2],"Landtype"]
    }

    # if single patch world without outlet
    if (length(flw_struct[,1]) == 1 & all(hill_no_outlets[,2] == 0)) {
      flw_struct$Landtype = 1
      hill_no_outlets[1,2] = 1
    }

    if (any(hill_no_outlets[,2] == 0) & length(flw_struct[,1]) != 1) { # if there are any hillslopes without streams
      cat("Correcting for hillslopes missing stream outlets")
      streams = flw_struct[flw_struct$Landtype == 1,] # make var of streams
      for (i in hill_no_outlets[hill_no_outlets[,2] == 0,1]) {
        hill_patches = flw_struct[flw_struct$Hill == i,] # get patches from problem hillslope
        min_patch = hill_patches[which.min(hill_patches$Centroidz),]# find lowest elevation patch
        dist2stream = sqrt(abs(streams$Centroidx - min_patch$Centroidx)^2 + abs(streams$Centroidy - min_patch$Centroidy)^2) # distance to streams from min elev patch

        if (make_stream) { # if TRUE
          flw_struct[min_patch$Number,"Landtype"] = 1
          no_stream_fix = c(no_stream_fix,min_patch$Number)
          print_dist_fix = c(print_dist_fix,min(dist2stream))
        } else if (is.numeric(make_stream)) { # if make_stream is a number
          if (min(dist2stream) <= make_stream) { # if within make_stream var distance of stream, make min patch a stream
            flw_struct[min_patch$Number,"Landtype"] = 1
            no_stream_fix = c(no_stream_fix,min_patch$Number)
            print_dist_fix = c(print_dist_fix,min(dist2stream))
          } else if (min(dist2stream) > make_stream) { # if outside of make_stream distance threshold
            no_stream = c(no_stream,min_patch$Number)
            print_dist = c(print_dist,min(dist2stream))
          }
        } # end else if make stream is num
      } # end for hills w/o outlets
    }
    if (!is.null(no_stream_fix)) {
      cat(paste(length(no_stream_fix),"hillslopes had their lowest elevation patches set to streams, at a max distance from existing streams of",
                  max(print_dist_fix),"cell lengths."))
    }
    if (!is.null(no_stream)) { # output hillslopes that weren't corrected
      cat("The following outlet patches were outside of the",make_stream,"cell length distance threshold set by the 'make_stream' argument.")
      cat("Dist2Stream" = print_dist,flw_struct[no_stream,])
      stop(noquote("The above hillslopes must have stream patch outlets, either increase the value of the 'make_stream' argument, or fix via GIS."))
    }


    find_connected = function(patch_borders, start, history = NULL, found = NULL, missing, hill, itr_ct = NULL, itr_max = 10000) {
      # add start to found, only for first itr tho
      if (is.null(history)) {
        found = start
      }
      # update history to include current patch
      history = c(history, start)
      # find the neighbors
      neighbors = names(patch_borders[[start]])[-1][names(patch_borders[[start]])[-1] %in% flw_struct[flw_struct$Hill == hill, "Number"]]
      neighbors = as.numeric(neighbors)
      # update the found vector to include those neighbors
      found = c(found, neighbors)
      # remove the found from missing
      missing = missing[!missing %in% found]
      # check if we have any missing left
      if (length(missing) == 0) {
        return(NULL)
      }
      # if our history is the same as the patches we've found, then give up there's a segmented patch
      if (all(found %in% history)) {
        return(missing)
      }

      if (!is.null(itr_ct)) {
        itr_ct = itr_ct + 1
        if (itr_ct == itr_max) {
          cat("Reached iteration limit of ",itr_max)
          return(list(missing, found, history, start))
        }
      }

      # if we haven't found everything, keep iterating through found patches not in the history
      opts = as.numeric(found[!found %in% history])
      next_patch = opts[round(runif(1,min = 1, max = length(opts)))]
      #next_patch = as.numeric(found[!found %in% history][1])

      find_connected(
        patch_borders = patch_borders,
        start = next_patch,
        history = history,
        found = found,
        missing = missing,
        hill = hill,
        itr_ct = itr_ct,
        itr_max = itr_max
      )
    }

    if (!skip_hillslope_check) {
      # check for segmented hillslopes - this is going to be so slow but oh well
      cat("Checking for segmented hillslopes")
      segmented_hills = NULL
      segmented_patch_ct = NULL
      segmented_patch_list = list()

      pb = txtProgressBar(min = 0,max = length(unique(flw_struct$Hill)),style = 3)
      ct = 0

      # for testing:
      i = unique(flw_struct$Hill)[4]

      for (i in unique(flw_struct$Hill)) {
        ct = ct + 1
        setTxtProgressBar(pb,ct)
        # start from outlet
        min_patch = flw_struct[flw_struct$Hill == i,][min_hill_patch[min_hill_patch$hillID == i, "patch_ind"],]
        all_patches = flw_struct[flw_struct$Hill == i, "Number"]

        missing = find_connected(
          patch_borders = patch_borders,
          start = min_patch$Number,
          history = NULL,
          found = NULL,
          missing = all_patches,
          hill = i,
          itr_ct = 0,
          itr_max = length(all_patches)
        )

        if (!is.null(missing)) {
          segmented_hills = c(segmented_hills, i)
          segmented_patch_ct = c(segmented_patch_ct, length(missing))
          segmented_df = data.frame("Hillslope ID" = segmented_hills, "Segmented patch count" = segmented_patch_ct)
          segmented_patch_list[[as.character(i)]] = missing
          warning(
            "One or more hillslopes are disconnected with segmented segmented patches and do not reach the outlet patch.\n",
            "This will lead to problems in your simulaion. See table below for hilslope IDs and segmented patch counts.\n",
            "Regenerate hillslopes with a higher threshold for accumulated upslope area to correct this.\n"
          )
          print(segmented_df)
          #error("")

        }

      } # end for segmented hills
      close(pb)
    }


  } # end parallel if

  # -------------------- Build list --------------------
  # Build list for output. Turn border count into probabilities and lists of neighbors
  lst <- list()

  cat("Buildling flowtable list")
  pb = txtProgressBar(min = 0,max = length(flw_struct$Number),style = 3)

  for (i in 1:length(flw_struct$Number)) {
    setTxtProgressBar(pb,i)

    # changed for new patch_borders list
    neighbor_index <- as.numeric(names(patch_borders[[i]])[-1])
    # neighbor_index<-which((patch_borders[i,]>0)) #find neighbors
    # neighbor_index<-neighbor_index[-which(neighbor_index==i)] #remove self from neigbor list

    if (parallel) {
      neighbor_index = neighbor_index[flw_struct[i, ]$Hill == flw_struct[neighbor_index, ]$Hill] # remove neighbors that are in different hillslopes
      #if(length(neighbor_index)<1){stop("Something went wrong in fixing neighbors for parallelization")}
    }

    # changed for new patch_borders list
    tp_perimeter <- cell_length*as.numeric(patch_borders[[i]][as.character(neighbor_index)])
    #tp_perimeter<-cell_length*patch_borders[i,neighbor_index] # total perimeter in map units (meters,etc)
    tp_neighbors <-
      flw_struct$Number[neighbor_index]  # vector of neighboring patches
    tp_xi <- flw_struct$Centroidx[i]     #patch i x position
    tp_yi <- flw_struct$Centroidy[i]     #patch i y position
    tp_zi <- flw_struct$Centroidz[i]     #patch i y position
    tp_xj <- flw_struct$Centroidx[neighbor_index]   #list of neighbors x positions
    tp_yj <- flw_struct$Centroidy[neighbor_index]   #list of neighbors y positions
    tp_zj <- flw_struct$Centroidz[neighbor_index]   #list of neighbors z positions
    dist <- cell_length*sqrt((tp_xj - tp_xi)^2 + (tp_yj - tp_yi)^2)   #list of distances to neighbors (centroid to centroid)
    slope_i <- (tp_zi - tp_zj)/dist
    if (any(dist == 0)) { # if distance is 0 because of weird patch setup, maybe because zones cut patches exactly in half
      if (tp_zi == tp_zj[dist == 0]) {slope_i[dist == 0] = 0 # if elev is the same set slope to 0 --check if this works in fill_pit
      } else{slope_i[dist == 0] = (tp_zi - tp_zj[dist == 0])/cell_length}
    }
    tp_gamma <- tp_perimeter*slope_i # slope * border length (m^3)
    perim_sum <- sum(tp_perimeter[tp_gamma > 0]) #sum downslope boarders of patches with positive gammas
    tp_gamma[tp_gamma < 0] <- 0 # set negative gammas to 0
    gamma_tot <- sum(tp_gamma) # sum perim * slope of all neighbors

    if (gamma_tot != 0) { # if there's a downslope neighbor
      tp_gamma <- tp_gamma/gamma_tot # normalize gamma by total (% or proportion)
      tp_TotalG <- (gamma_tot/perim_sum)*flw_struct$Area[i] # gamma_tot/perim_sum = sum of slopes * area = volume
    } else {
      if (is.null(slope_i) | length(slope_i) == 0) { # if there is only one patch slope_i will be null,
        tp_TotalG <- flw_struct$Area[i]
      } else {#if all upslope, take slope from closest neighbor in height
        tp_TotalG <- -max(slope_i)*flw_struct$Area[i]
      }
    }

    # fixes for no neighbors
    if (is.infinite(tp_TotalG)) {tp_TotalG = NULL}

    # build list
    lst[[i]] <- list(
      Number = flw_struct$Number[i],
      Area = flw_struct$Area[i],
      Centroidx = flw_struct$Centroidx[i],
      Centroidy = flw_struct$Centroidy[i],
      Centroidz = flw_struct$Centroidz[i],
      BasinID = flw_struct$Basin[i],
      HillID = flw_struct$Hill[i],
      ZoneID = flw_struct$Zone[i],
      PatchID = flw_struct$Patch[i],
      Landtype = flw_struct$Landtype[i],
      Roadtype = flw_struct$Roadtype[i],
      Neighbors = tp_neighbors,
      Border_perimeter = tp_perimeter,
      Slopes = tp_perimeter * slope_i,
      TotalG = tp_TotalG,
      Gamma_i = tp_gamma
    )
  }
  close(pb)

  # -------------------- Single Patch World --------------------
  # If there's only 1 patch, build list manually and return
  if (length(raw_patch_data) == 1 | length(flw_struct$Number) == 1) {
    lst[[1]]$Slopes = 0
    lst[[1]]$TotalG = flw_struct$Area
    return(lst)
  }

  # -------------------- Pit filling --------------------
  lst = fix_all_pits(lst,flw_struct,parallel)

  # -------------------- streams and roads --------------------
  lst <- find_stream(lst,road_width) # if there are roads, find the streams that are near

  # ----- End function -----
  return(lst)
}
