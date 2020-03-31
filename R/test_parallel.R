#' test_parallel
#'
#' Checks for flow across hillslopes, and returns a table indicating patches that flow across hillslopes,
#' how many patches they flow to, and the combined gamma of that patch going across hillslopes
#' @param flw List version of flow table, either produced via patch_data_analysis.R or read_in_flow2.R
#' @author Will Burke

test_parallel = function(flw){

cross_hill = matrix(0,nrow = length(flw),ncol = 3) # matrix for patchID, number of patches, and sum of gammas crossing hills
colnames(cross_hill) = c("Patch ID","Number of patches","Percent flow across hillslope")
for(i in 1:length(flw)){
  for(n in flw[[i]]$Neighbors)
    if(flw[[i]]$HillID != flw[[n]]$HillID & flw[[i]]$Gamma_i[flw[[i]]$Neighbors==n] != 0){
      cross_hill[i,1] = flw[[i]]$PatchID
      cross_hill[i,2] = cross_hill[i,2] + 1
      cross_hill[i,3] = cross_hill[i,3] + flw[[i]]$Gamma_i[flw[[i]]$Neighbors==n]
    }
}
x_ind = cross_hill[,1]!= 0 & cross_hill[,2] != 0 & cross_hill[,3] != 0
cross_hill = cross_hill[x_ind,]
return(cross_hill)
}
