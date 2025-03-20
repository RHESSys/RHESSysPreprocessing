#' @export
soilvals2texture = function(basin, sand, clay, plot_soils = T, writeplots = T, plot_out = "soils_plot") {
  # currently assumes polaris input data
  fill.na <- function(x, i=5) {
    if(is.na(x)[i]) {
      return(mean(x, na.rm = T))
    } else {
      return(x[i])
    }
  }

  # mask_map = rast(file.path(output_dir, "basin.tif"))
  clay = rast(clay)
  sand = rast(sand)

  mask_map = rast(basin)

  clay_proj = project(clay, mask_map, method = "bilinear")
  clay_crop = crop(clay_proj, mask_map)
  clay_mask = mask(clay_crop, mask_map)

  sand_proj = project(sand, mask_map, method = "bilinear")
  sand_crop = crop(sand_proj, mask_map)
  sand_mask = mask(sand_crop, mask_map)

  if (any(is.na(values(clay_crop)))) {
    clay_crop = focal(clay_crop, w = 3, fun = fill.na, na.policy = "only")
  }
  if (any(is.na(values(sand_crop)))) {
    sand_crop = focal(sand_crop, w = 3, fun = fill.na, na.policy = "only")
  }

  # silt = "preprocessing/spatial_source/POLARISOut/mean/" # not needed since should be remainder
  texturedata = data.frame(CLAY = unname(values(clay_crop)), SILT = NA, SAND = unname(values(sand_crop)))
  texturedata$ind = seq(1,nrow(texturedata))
  texturedata$SILT = 100 - (texturedata$CLAY + texturedata$SAND)
  # summary(texturedata)

  # this should be the same as the grass function, getting texture from soil components
  texturedata$TextureName = soiltexture::TT.points.in.classes(tri.data  = texturedata, class.sys = "USDA.TT", PiC.type  = "t")

  usdaID = data.frame(name = c("clay","silty-clay", "silty-clay-loam", "sandy-clay", "sandy-clay-loam", "clay-loam",
                              "silt","silt-loam","loam","sand","loamy-sand","sandy-loam"),
                      ID = c(1:12),
                      TextureName = c("Cl","SiCl", "SiClLo","SaCl","SaClLo","ClLo","Si","SiLo","Lo","Sa","LoSa","SaLo"))
  # to do the conversion between names, to make the above df
  # TT.classes.tbl(class.sys = "USDA.TT")
  texturedata = merge(texturedata,usdaID, by = "TextureName", allx = T, sort = F)
  texturedata = texturedata[order(texturedata$ind),]

  soil_texture = clay_crop
  names(soil_texture) = "soil_texture"
  nrow(texturedata) == length(values(soil_texture))
  values(soil_texture) = texturedata$ID

  soil_texture = mask(soil_texture, mask_map)

  if (plot_soils) {
    soil_texture_plot(soil_texture, plot_out = plot_out, writeplots = writeplots)
  }

  return(soil_texture)
}


#' @export
soil_texture_plot = function(soil_texture, plot_out, writeplots = T) {
  usdaID = data.frame(name = c("clay","silty-clay", "silty-clay-loam", "sandy-clay", "sandy-clay-loam", "clay-loam", "silt","silt-loam","loam","sand","loamy-sand","sandy-loam"),ID = c(1:12))

  soiltab = unique(usdaID[usdaID$ID %in% unique(values(soil_texture, rm.na=T)),c("name","ID")])
  tmp = summary(factor(values(soil_texture, na.rm=T)))
  tmp = data.frame(ID = names(tmp),count = tmp)
  soiltab = merge(soiltab,tmp)

  rownames(soiltab) = NULL
  text_rep <- capture.output(print(soiltab))
  
  par(mfrow = c(1, 1))
  plot(soil_texture, main = "Soil Textures")
  tl = textlocTL(soil_texture)
  textlab = paste(text_rep, collapse = "\n")
  text(x = tl[1], y = tl[2], labels =textlab , col = "black", cex = 1, adj = c(0,1))

  if (writeplots) {
    dev.copy2pdf(file = plot_out, width = 8, height = 6)
  }
}