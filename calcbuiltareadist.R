rm(list=ls())
dir.create("E:/tmpd/output")
require(rgdal)
require(raster)
require(foreach)

shp = readOGR('E:/Dropbox (Yale_FES)/Personal/Malaria Project/Data/District Shapefile/Districts','Map_of_Districts_216',verbose=FALSE)

ghsl14 = raster("E:/Data/Global Data/GHSL/GHSL_38m/GHS_BUILT_LDS2014_GLOBE_R2016A_3857_38_v1_0/GHS_BUILT_LDS2014_GLOBE_R2016A_3857_38_v1_0.vrt")
shp_merc <- spTransform(shp, CRS(proj4string(ghsl14)))

ghslkmsq = foreach(i=1:216)%do%{
  require(rgdal)
  poly=shp_merc[i,]
  writeOGR(obj=poly, dsn="E:/tmpd", layer=as.character(i), driver="ESRI Shapefile")
  shpoutname = paste0("E:/tmpd/",i,".shp",sep="")
  inpvrt = "E:/Data/Global Data/GHSL/GHSL_38m/GHS_BUILT_LDS2014_GLOBE_R2016A_3857_38_v1_0/GHS_BUILT_LDS2014_GLOBE_R2016A_3857_38_v1_0.vrt"
  outfile = paste0("E:/tmpd/",i,".tif",sep="")
  
  command = 'gdalwarp -crop_to_cutline -cutline'
  command = paste(command,shQuote(shpoutname,type="cmd"))
  command = paste(command,shQuote(inpvrt,type="cmd"))
  command = paste(command,shQuote(outfile,type="cmd"))
  system(command)
  
  command = "pkstat -i"
  command = paste(command,shQuote(outfile,type="cmd"))
  command = paste(command,"-hist")
  out = system(command,intern=T)
  write.csv(out,paste0("E:/tmpd/output/",i,".csv",sep=''))
  print(i)
  flush.console()
}


builtup = rep(NA,216)
nonbuilt = rep(NA,216)
for(i in 1:216){
  fname = paste0("E:/tmpd/output/",i,".csv",sep='')
  csv = read.csv(fname,stringsAsFactors = F)[,2]
  out = strsplit(csv,split=" ")
  out = t(as.data.frame(out))
  rownames(out) = NULL
  out = apply(out,2,as.numeric)
  colnames(out) = c("Pixel","Count")
  out = as.data.frame(out)
  builtup[i] = out[which(out$Pixel==101),2] * 38.21851 * 38.21851 / 1000000
  nonbuilt[i] = out[which(out$Pixel==1),2] * 38.21851 * 38.21851 / 1000000
}

shp$BuiltArea = builtup
shp$noBuiltArea = nonbuilt
shp$TotalArea = builtup + nonbuilt
writeOGR(obj=shp, dsn="E:/Dropbox (Yale_FES)/Personal/Malaria Project/Data/District Shapefile/Districts", layer="Map_of_Districts_216_ghsl2014", driver="ESRI Shapefile",layer_options = "RESIZE=YES",overwrite_layer=T)

system("rm E:/tmpd/*")
system(paste0("rm -r E:/tmpd/output"))
