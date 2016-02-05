

# sample session

library(raster)
rasterOptions(format = 'GTiff')
library(gdalUtils)
# gdal_chooseInstallation(...)









# author: Benjamin Brede
# date: 2016-01-16

rm(list=ls())

setwd('D:/S2_Para/')

library(gdalUtils)
gdal_setInstallation('C:/Program Files/GDAL/', rescan = TRUE)
library(raster)
rasterOptions(tmpdir = 'D:/Temp/raster/', format = 'GTiff')
library(snowfall)

# possible S2 bands
bands <- c(paste0('B', c(formatC(width = 2, flag = '0', 1:12), '8A')), 'AOT', 'VIS', 'WVP', 'SCL', 'CLD')

S2_folders <- grep('USER.*SAFE', list.dirs('D:/', full.names = TRUE, recursive = FALSE), value = TRUE) 

bs <- list.dirs('D:/S2_Para/', recursive = FALSE, full.names = FALSE)

# S2_L2A_translate(S2_folders[1], band = 'CLD', resolution = 20, out_folder = 'D:/S2_Para/CLD/')

sfInit(TRUE, 8)
sfExportAll()
sfLibrary(XML)
sfLibrary(gdalUtils)
sfLibrary(raster)
sfClusterEval(setwd(getwd()))

for (b in bs) {
  sfExport('b')
  l <- sfClusterApplyLB(S2_folders, function(f)
    S2_L2A_translate(f, band = b, resolution = 20, out_folder = b)
  )
}
sfStop()


#### mosaicing ####

target_crs <- '+proj=utm +zone=20 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'

spectral_bands <- bands[c(2:7, 11:13)]
band <- 'SCL' #spectral_bands[8]
fs <- list.files(path = band, pattern = 'S2_.*20m.tif$', full.names = TRUE)

sfInit(TRUE, 8)
sfExportAll()
sfLibrary(gdalUtils)
sfLibrary(raster)
sfClusterEval(rasterOptions(tmpdir = 'D:/Temp/raster/', format = 'GTiff'))
sfClusterEval(gdal_setInstallation('C:/Program Files/GDAL/', rescan = TRUE))

l <- sfClusterApplyLB(fs, function(f) {
  crs <- raster(f)@crs@projargs
  if (crs == target_crs) 
    f
  else {
    tmp <- rasterTmpFile()
    gdalwarp(f, tmp, t_srs = target_crs, ot = "Byte")#, r = 'near', ot = "UInt16"
    tmp
  }
})

mosaic_rasters(unlist(l), dst_dataset = paste0('S2_mosaic_', band, '.tif'), co = 'BIGTIFF=YES')

# IRECI
#10000 * (("S2_mosaic_B07@1" - "S2_mosaic_B04@1" ) / ( "S2_mosaic_B05@1" / "S2_mosaic_B06@1"))

# NDVI7
#10000 * (("S2_mosaic_B07@1" - "S2_mosaic_B04@1" ) / ( "S2_mosaic_B07@1" + "S2_mosaic_B04@1"))

# NDVI8
#10000 * (("S2_mosaic_B8A@1" - "S2_mosaic_B04@1" ) / ( "S2_mosaic_B8A@1" + "S2_mosaic_B04@1"))

# calc VI, INT2U, compress


cloudMask <- function(cld, scl) {
  ifelse(cld > 0 | scl %in% c(3, 7:11), NA, 1)
}

rasterOptions(progress = 'text')
cm <- overlay(raster('S2_mosaic_CLD.tif'), raster('S2_mosaic_SCL.tif'), fun = cloudMask, filename = 'S2_mosaic_CLM.tif', datatype = 'INT1U')

# a wrapper for gdal_calc.py
# gdal_calc <- function(infiles, outfile, calc, ...) {
#   system(paste('python C:/"Program Files"/GDAL/gdal_calc.py',
#                paste(infiles, collapse = ' '),
#                paste0('--outfile=', outfile),
# #                paste0('--type=', ot),
#                paste0('--calc="', calc, '"'), 
#                ...))
# }


# CLM = cloud mask
# gdal_calc(infiles = '-A S2_mosaic_CLD.tif -B S2_mosaic_SCL.tif',
#           outfile = 'S2_mosaic_CLM.tif', ot = 'Byte', 
#           calc = '"(A==0)*(B>3)*(B<7)"', '--NoDataValue=0')
# 
# # CLMi = inverted cloud mask (for presentation/search purposes)
# gdal_calc(infiles = '-A S2_mosaic_CLD.tif -B S2_mosaic_SCL.tif',
#           outfile = 'S2_mosaic_CLMi.tif', ot = 'Byte', 
#           calc = '"(A!=0)|(B<=3)|(B>=7)"', '--NoDataValue=0')


#### operating in QGIS raster calc ####
# > gdal_calc did not give good results (errors)

# IRECI
# ( Float("S2_mosaic_B07@1")/10000-Float("S2_mosaic_B04@1")/10000) / (Float("S2_mosaic_B05@1")/10000/(Float("S2_mosaic_B06@1"/10000))) * 10000

# NDVI7
# ( Float("S2_mosaic_B07@1") -"S2_mosaic_B04@1") / ("S2_mosaic_B07@1"+"S2_mosaic_B04@1") * 10000

# NDMI11
# ( Float("S2_mosaic_B07@1") -"S2_mosaic_B11@1") / ("S2_mosaic_B07@1"+"S2_mosaic_B11@1") * 10000

# NDMI12
# ( Float("S2_mosaic_B07@1") -"S2_mosaic_B12@1") / ("S2_mosaic_B07@1"+"S2_mosaic_B12@1") * 10000

# S2REP
# (705.0 + 35 * (((Float("S2_mosaic_B07@1") + "S2_mosaic_B04@1")/2) - "S2_mosaic_B05@1")/("S2_mosaic_B06@1" - "S2_mosaic_B05@1")) * 10

# CI (Clevers & Kooistra, 2012)
# ( Float("S2_mosaic_B07@1") / "S2_mosaic_B05@1" - 1) * 10000

#### compression ####

# gdal_translate(src_dataset = 'S2_mosaic_NDVI8.tif', 
#                dst_dataset = 'S2_mosaic_NDVI8_compress.tif', 
#                co=c("COMPRESS=LZW", 'NUM_THREADS=ALL_CPUS', 'BIGTIFF=YES'),
#                ot = 'UInt16')


gdal_translate(src_dataset = 'NDMI12.tif', 
               dst_dataset = 'S2_mosaic_NDMI12.tif', ot = 'Int16',
               co=c("COMPRESS=DEFLATE", 'ZLEVEL=9', 'NUM_THREADS=ALL_CPUS', 'BIGTIFF=YES'))
# gdal_translate(src_dataset = 'S2_mosaic_SCL.tif', 
#                dst_dataset = 'S2_mosaic_SCL_c.tif', ot = 'Byte',
#                co=c("COMPRESS=DEFLATE", 'ZLEVEL=9', 'NUM_THREADS=ALL_CPUS', 'BIGTIFF=YES'))



#### stack RGB together ####

s <- stack(c('S2_mosaic_B02.tif', 
             'S2_mosaic_B03.tif',
             'S2_mosaic_B04.tif'))
rasterOptions(progress = 'text', chunksize = 1e+08)
writeRaster(s, filename = 'S2_mosaic_brick.tif', datatype = 'INT2U')
# 

# !!!!!!! RAM explodes !!!!!
# system(paste('python C:/"Program Files"/GDAL/gdal_merge.py', 
#              '-o D:/S2_Para/S2_mosaic_brick.tif',
#              '-separate -co COMPRESS=DEFLATE -co ZLEVEL=9 -co BIGTIFF=YES -co NUM_THREADS=ALL_CPUS', 
#              'D:/S2_Para/S2_mosaic_B02.tif', 
#              'D:/S2_Para/S2_mosaic_B03.tif',
#              'D:/S2_Para/S2_mosaic_B04.tif'))
# !!!!!!! RAM explodes !!!!!

gdal_translate('S2_mosaic_brick.tif', 'S2_thumbnail.png', co = 'QUALITY=10')

#### extraction ####


indices <- c('NDVI7', 'NDVI8', 'NDMI11', 'NDMI12', 'IRECI', 'CI', 'S2REP')

s <- subset(shapefile('AOI.shp'), Category == 'Undisturbed')

df <- do.call(cbind, lapply(indices, function(index) {
  r <- raster(paste0('S2_mosaic_', index, '.tif'))
  l <- extract(r, s)[[1]]
  scaling <- ifelse(index == 'S2REP', 10, 10000)
  d <- data.frame(l)
  names(d) <- index
  d
}))

# df <- do.call(rbind, lapply(index) {
#   r <- raster(paste0('S2_mosaic_', index, '.tif'))
#   l <- extract(r, s)
#   
# })


library(ggplot2)

ggplot(df, aes(x = NDVI7, y = NDVI8)) +
  geom_point()

summary(lm(NDVI7 ~ NDVI8, data = df))




#### cloud masking ####
# spectral_bands <- bands[c(2:7, 11:13)]
# 
# S2_cloudMasking <- function(spectral, cld, scl) {
#   ifelse(spectral == 0 | cld > 0 | scl %in% c(3, 7:11), NA, spectral)
# }
# l <- lapply(spectral_bands, function(b) {
#   spectral <- list.files(file.path(out_folder, b), pattern = 'S2_.*20m.tif$', full.names = TRUE)
#   cld <- list.files(file.path(out_folder, 'CLD'), pattern = 'S2_.*20m.tif$', full.names = TRUE)
#   scl <- list.files(file.path(out_folder, 'SCL'), pattern = 'S2_.*20m.tif$', full.names = TRUE)
#   
#   sfClusterApplyLB(seq_along(spectral), function(i) {
#     s <- stack(spectral[i], cld[i], scl[i])
#     out <- paste0(file_path_sans_ext(spectral[i]), '_CM.tif')
#     if (!file.exists(out)) 
#       overlay(s, fun = S2_cloudMasking, filename = out, datatype = 'INT2U')
#     else
#       raster(out)
#   })
# })




