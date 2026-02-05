library(inlabru)
library(INLA)
library(sp)
library(sf)
library(raster)
library(rgeos)
library(RColorBrewer)
library(tmap)
library(ggplot2)
library(maps)
library(rgdal)
library(dplyr)
library(viridis)
library(ggstar)
library(stringr)
source("utils/inlabru_import_functions.R")
source('utils/code_for_etas_FINAL.R')

area.oef <- readOGR('shape_Area/rectangle_for_OEF.shp')

square.bdy <- square_poly_from_bbox(area.oef@bbox, area.oef@proj4string, 0)
inner.bdy <- square_poly_from_bbox(matrix(c(12.9, 13.75, 42.1, 43.25), byrow = TRUE, ncol = 2),
                                   area.oef@proj4string, 0)

italy.map <- map_data("world", region = 'Italy')

list.input.aquila <- input.file.to.list('user_input_Aquila.txt')
list.input.amatrice <- input.file.to.list('user_input_Amatrice.txt')

map.aquila <- ggplot(italy.map, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group), color = 'darkgrey', alpha = 0) +
  gg(square.bdy, alpha = 0, color = 'orange') +
  gg(inner.bdy, alpha = 0, color = 'green') +
  geom_point(data = list.input.aquila$catalog, aes(x = Lon, y = Lat), size = 0.1) + 
  theme(legend.position = 'none') + 
  theme_classic() + 
  annotate('text', x = 16.5, y = 46.5, label = '2009-2010') +
  annotate('text', x = 7, y = 38, label = '(a)') + 
  xlab('Longitude') + 
  ylab('Latitude') +
  coord_equal() 

map.amatrice <- ggplot(italy.map, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group), color = 'darkgrey', alpha = 0) +
  gg(square.bdy, alpha = 0, color = 'orange') + 
  gg(inner.bdy, alpha = 0, color = 'green') +
  geom_point(data = list.input.amatrice$catalog, aes(x = Lon, y = Lat), size = 0.1) + 
  theme(legend.position = 'none') + 
  theme_classic() + 
  annotate('text', x = 16.5, y = 46.5, label = '2016-2018') + 
  annotate('text', x = 7, y = 38, label = '(b)') +
  xlab('Longitude') + 
  ylab('Latitude') +
  coord_equal() 



zoom.aquila <- ggplot(list.input.aquila$catalog, aes(x = Lon, y = Lat)) + 
  geom_point(size = 0.1) +
  geom_star(data = list.input.aquila$catalog[list.input.aquila$catalog$Mw > 5, ],
            mapping = aes(x = Lon, y = Lat), fill = 'red', size = 3) +
  gg(inner.bdy, alpha = 0, color = 'green') +
  ylim(42.1,43.25) + 
  xlim(12.9, 13.75) + 
  annotate('text', x = 13, y = 42.3, label = '(c)') +
  theme_classic() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  coord_equal()

zoom.amatrice <- ggplot(list.input.amatrice$catalog, aes(x = Lon, y = Lat)) + 
  geom_point(size = 0.1) +
  geom_star(data = list.input.amatrice$catalog[list.input.amatrice$catalog$Mw > 5, ],
            mapping = aes(x = Lon, y = Lat), fill = 'red', size = 3) +
  gg(inner.bdy, alpha = 0, color = 'green') +
  ylim(42.1,43.25) + 
  xlim(12.9, 13.75) +
  annotate('text', x = 13, y = 42.3, label = '(d)') + 
  theme_classic() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  coord_equal()

pdf('figure1.spatdata.pdf', width = 480/80, height = 480/80)
multiplot(map.aquila, map.amatrice, 
          zoom.aquila, zoom.amatrice, 
          layout = matrix(1:4, byrow = TRUE, ncol = 2))
dev.off()

## cumulative plot 
CumSum.aq <- vapply(list.input.aquila$T12[1]:list.input.aquila$T12[2], 
                 \(x) sum(list.input.aquila$catalog.bru$ts < x), 0)
CumSum5.aq <- vapply(list.input.aquila$T12[1]:list.input.aquila$T12[2],
                     \(x) sum(list.input.aquila$catalog.bru$ts[list.input.aquila$catalog.bru$magnitudes >= 5] < x), 0)

cumulative.aquila <- ggplot() + 
  geom_vline(data = list.input.aquila$catalog.bru[list.input.aquila$catalog.bru$magnitudes >= 5,], mapping = aes(xintercept = ts), 
             linetype = 2, color = 'darkgrey', size = 0.2) + 
  geom_line(aes(x = list.input.aquila$T12[1]:list.input.aquila$T12[2], y = CumSum5.aq*50), color = 'red', linetype = 2) + 
  geom_line(aes(x = list.input.aquila$T12[1]:list.input.aquila$T12[2], y = CumSum.aq)) + 
  geom_star(data = list.input.aquila$catalog.bru[list.input.aquila$catalog.bru$magnitudes >= 5,], 
            mapping = aes(x = ts, y = 0), size = 2, fill = 'red') + 
  xlab('days') + 
  annotate('text', x = 300, y = 250, label = '(a)') +
  annotate('text', x = 300, y = 900, label = 'Mw > 2.5') +
  annotate('text', x = 300, y = 500, label = 'Mw > 5', color = 'red') +
  theme_classic() +
  scale_y_continuous("cumulative count (Mw > 2.5)",
                     limits = c(0,1380), 
                     sec.axis = sec_axis(~ . /50, name = 'cumulative count (Mw > 5)'))


CumSum.ama <- vapply(list.input.amatrice$T12[1]:list.input.amatrice$T12[2], 
                    \(x) sum(list.input.amatrice$catalog.bru$ts < x), 0)
CumSum5.ama <- vapply(list.input.amatrice$T12[1]:list.input.amatrice$T12[2],
                     \(x) sum(list.input.amatrice$catalog.bru$ts[list.input.amatrice$catalog.bru$magnitudes >= 5] < x), 0)

cumulative.amatrice <- ggplot() + 
  geom_vline(data = list.input.amatrice$catalog.bru[list.input.amatrice$catalog.bru$magnitudes >= 5,], 
             mapping = aes(xintercept = ts), 
             linetype = 2, color = 'darkgrey', size = 0.2) + 
  geom_line(aes(x = list.input.amatrice$T12[1]:list.input.amatrice$T12[2], y = CumSum5.ama*50), color = 'red', linetype = 2) + 
  geom_line(aes(x = list.input.amatrice$T12[1]:list.input.amatrice$T12[2], y = CumSum.ama)) + 
  geom_star(data = list.input.amatrice$catalog.bru[list.input.amatrice$catalog.bru$magnitudes >= 5,], 
            mapping = aes(x = ts, y = 0), size = 2, fill = 'red') + 
  xlab('days') + 
  annotate('text', x = 600, y = 250, label = '(b)') +
  annotate('text', x = 600, y = 1200, label = 'Mw > 3') +
  annotate('text', x = 600, y = 550, label = 'Mw > 5', color = 'red') +
  theme_classic() + 
  scale_y_continuous("cumulative count (Mw > 3)", 
                     limits = c(0,1380), 
                     sec.axis = sec_axis(~ . /50, name = 'cumulative count (Mw > 5)'))


pl.mgs.aquila <- ggplot(list.input.aquila$catalog[list.input.aquila$catalog$Mw < 5,], 
                        aes(x = time_date, y = Mw)) + 
  geom_point(size = 0.2) + 
  geom_star(data = list.input.aquila$catalog[list.input.aquila$catalog$Mw > 5,], 
            mapping = aes(x = time_date, y = Mw), size = 2, fill = 'red') + 
  xlab('time') + 
  ylab('Mw (> 2.5)') + 
  annotate('text', x = max(list.input.aquila$catalog$time_date) - 10*60*60*24, 
           y = 5.5, label = '(c)') +
  ylim(2.5, 7) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  

pl.mgs.amatrice <- ggplot(list.input.amatrice$catalog[list.input.amatrice$catalog$Mw < 5,], 
                        aes(x = time_date, y = Mw)) + 
  geom_point(size = 0.2) + 
  geom_star(data = list.input.amatrice$catalog[list.input.amatrice$catalog$Mw > 5,], 
            mapping = aes(x = time_date, y = Mw), size = 2, fill = 'red') + 
  xlab('time') + 
  ylab('Mw (> 3)') + 
  annotate('text', x = max(list.input.amatrice$catalog$time_date) - 20*60*60*24, 
           y = 5.5, label = '(d)') +
  ylim(2.5, 7) + 
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


pdf('figure2.tempdata.pdf', width = 480/80, height = 480/80)
multiplot(
          cumulative.aquila, cumulative.amatrice,
          pl.mgs.aquila, pl.mgs.amatrice,
          layout = matrix(c(1:4), byrow = T, ncol = 2))
dev.off()

###############################
#### BACKGROUND
##############################

list.output.bkg.aq <- background.model(list.input.aquila)
list.output.bkg.ama <- background.model(list.input.amatrice)

# list.input.merge <- list.input.aquila
# list.input.merge$catalog.bru <- rbind(list.input.aquila$catalog.bru, list.input.amatrice$catalog.bru)
# list.output.bkg.merged <- background.model(list.input.merge)

save(list.output.bkg.aq, file = 'list.output.blg.aq.Rds')
save(list.output.bkg.ama, file = 'list.output.blg.ama.Rds')
# save(list.output.bkg.merged, file = 'list.output.blg.merged.Rds')

load('list.output.blg.aq.Rds')
load('list.output.blg.ama.Rds')

mesh.pix <- list.output.bkg.ama$mesh
# mesh.pix$crs <- NULL
pix <- pixels(mesh.pix, nx = 60, ny = 60)

# check if reasonable
loglambda.plot.aq <- predict(list.output.bkg.aq$fit.bkg, 
                          pix, 
                          ~ Intercept + Smooth)
loglambda.plot.ama <- predict(list.output.bkg.ama$fit.bkg, 
                             pix, 
                             ~ Intercept + Smooth)



# loglambda.plot.merged <- predict(list.output.bkg.merged$fit.bkg, 
#                               pix, 
#                               ~ Intercept + Smooth)

inner.bdy.km <- spTransform(inner.bdy, loglambda.plot.ama@proj4string)
col.lims <- range(c(loglambda.plot.aq$mean, 
                    loglambda.plot.ama$mean))
col.lims <- c(floor(col.lims[1]), ceiling(col.lims[2]))

bkg.field.plot.aq <- 
  ggplot() + gg(loglambda.plot.aq['mean']) + 
  gg(list.output.bkg.aq$catalog.bru.km, size = 0.1) + 
  gg(list.output.bkg.aq$bdy.km, color = 'orange', alpha = 0) + 
  xlim(list.output.bkg.aq$bdy.km@bbox[1,]) +
  ylim(list.output.bkg.aq$bdy.km@bbox[2,]) +
  scale_fill_viridis(limits = col.lims,
                     breaks = seq(col.lims[1], col.lims[2], by = 4)) + 
  coord_equal() +
  theme_classic() + 
  theme(legend.position = 'bottom') + 
  xlab('Easting') + 
  ylab('Northing')

bkg.field.plot.ama <- 
  ggplot() + gg(loglambda.plot.ama['mean']) + 
  gg(list.output.bkg.ama$catalog.bru.km, size = 0.1) + 
  gg(list.output.bkg.ama$bdy.km, color = 'orange', alpha = 0) + 
  xlim(list.output.bkg.ama$bdy.km@bbox[1,]) +
  ylim(list.output.bkg.ama$bdy.km@bbox[2,]) +
  scale_fill_viridis(limits = col.lims,
                     breaks = seq(col.lims[1], col.lims[2], by = 4)) + 
  coord_equal() + 
  theme_classic() + 
  theme(legend.position = 'bottom') + 
  xlab('Easting') + 
  ylab('Northing')


bkg.field.sd.plot.aq <- 
  ggplot() + gg(loglambda.plot.aq['sd']) + 
  gg(list.output.bkg.aq$catalog.bru.km, size = 0.1) + 
  gg(list.output.bkg.aq$bdy.km, color = 'orange', alpha = 0) + 
  xlim(list.output.bkg.aq$bdy.km@bbox[1,]) +
  ylim(list.output.bkg.aq$bdy.km@bbox[2,]) +
  scale_fill_viridis(option = 'plasma') + 
  coord_equal() +
  theme_classic() + 
  theme(legend.position = 'bottom') + 
  xlab('Easting') + 
  ylab('Northing')

bkg.field.sd.plot.ama <- 
  ggplot() + gg(loglambda.plot.ama['sd']) + 
  gg(list.output.bkg.ama$catalog.bru.km, size = 0.1) + 
  gg(list.output.bkg.ama$bdy.km, color = 'orange', alpha = 0) + 
  xlim(list.output.bkg.ama$bdy.km@bbox[1,]) +
  ylim(list.output.bkg.ama$bdy.km@bbox[2,]) +
  scale_fill_viridis(option = 'plasma') + 
  coord_equal() + 
  theme_classic() + 
  theme(legend.position = 'bottom') + 
  xlab('Easting') + 
  ylab('Northing')

pdf('figure5.bkgmeansd.pdf', width = 480/50, height = 480/50)
multiplot(bkg.field.plot.aq, bkg.field.plot.ama, 
          bkg.field.sd.plot.aq, bkg.field.sd.plot.ama,cols = 2)
dev.off()

samples.bkg.aq <- generate(list.output.bkg.aq$fit.bkg,
                            pix, 
                            ~ Intercept + Smooth, 
                            n.samples = 3)

dim(samples.bkg.aq)
pix$sample.1.aq <- samples.bkg.aq[,1]
pix$sample.2.aq <- samples.bkg.aq[,2]
pix$sample.3.aq <- samples.bkg.aq[,3]

bkg.sample.1.aq <- ggplot() + 
  gg(pix['sample.1.aq']) + 
  #gg(list.output.bkg.aq$catalog.bru.km, size = 0.1) + 
  gg(list.output.bkg.aq$bdy.km, color = 'orange', alpha = 0) + 
  xlim(list.output.bkg.ama$bdy.km@bbox[1,]) +
  ylim(list.output.bkg.ama$bdy.km@bbox[2,]) +
  scale_fill_viridis() + 
  coord_equal() + 
  theme_classic() + 
  theme(legend.position = 'none') + 
  xlab('Easting') + 
  ylab('Northing')

bkg.sample.2.aq <- ggplot() + 
  gg(pix['sample.2.aq']) + 
  #gg(list.output.bkg.aq$catalog.bru.km, size = 0.1) + 
  gg(list.output.bkg.aq$bdy.km, color = 'orange', alpha = 0) + 
  xlim(list.output.bkg.ama$bdy.km@bbox[1,]) +
  ylim(list.output.bkg.ama$bdy.km@bbox[2,]) +
  scale_fill_viridis() + 
  coord_equal() + 
  theme_classic() + 
  theme(legend.position = 'none') + 
  xlab('Easting') + 
  ylab('Northing')

bkg.sample.3.aq <- ggplot() + 
  gg(pix['sample.3.aq']) + 
  #gg(list.output.bkg.aq$catalog.bru.km, size = 0.1) + 
  gg(list.output.bkg.aq$bdy.km, color = 'orange', alpha = 0) + 
  xlim(list.output.bkg.ama$bdy.km@bbox[1,]) +
  ylim(list.output.bkg.ama$bdy.km@bbox[2,]) +
  scale_fill_viridis() + 
  coord_equal() + 
  theme_classic() + 
  theme(legend.position = 'none') + 
  xlab('Easting') + 
  ylab('Northing')

samples.bkg.ama <- generate(list.output.bkg.ama$fit.bkg,
                           pix, 
                           ~ Intercept + Smooth, 
                           n.samples = 3)

pix$sample.1.ama <- samples.bkg.ama[,1]
pix$sample.2.ama <- samples.bkg.ama[,2]
pix$sample.3.ama <- samples.bkg.ama[,3]

bkg.sample.1.ama <- ggplot() + 
  gg(pix['sample.1.ama']) + 
  #gg(list.output.bkg.aq$catalog.bru.km, size = 0.1) + 
  gg(list.output.bkg.aq$bdy.km, color = 'orange', alpha = 0) + 
  xlim(list.output.bkg.ama$bdy.km@bbox[1,]) +
  ylim(list.output.bkg.ama$bdy.km@bbox[2,]) +
  scale_fill_viridis() + 
  coord_equal() + 
  theme_classic() + 
  theme(legend.position = 'none') + 
  xlab('Easting') + 
  ylab('Northing')

bkg.sample.2.ama <- ggplot() + 
  gg(pix['sample.2.ama']) + 
  #gg(list.output.bkg.aq$catalog.bru.km, size = 0.1) + 
  gg(list.output.bkg.aq$bdy.km, color = 'orange', alpha = 0) + 
  xlim(list.output.bkg.ama$bdy.km@bbox[1,]) +
  ylim(list.output.bkg.ama$bdy.km@bbox[2,]) +
  scale_fill_viridis() + 
  coord_equal() + 
  theme_classic() + 
  theme(legend.position = 'none') + 
  xlab('Easting') + 
  ylab('Northing')

bkg.sample.3.ama <- ggplot() + 
  gg(pix['sample.3.ama']) + 
  #gg(list.output.bkg.aq$catalog.bru.km, size = 0.1) + 
  gg(list.output.bkg.aq$bdy.km, color = 'orange', alpha = 0) + 
  xlim(list.output.bkg.ama$bdy.km@bbox[1,]) +
  ylim(list.output.bkg.ama$bdy.km@bbox[2,]) +
  scale_fill_viridis() + 
  coord_equal() + 
  theme_classic() + 
  theme(legend.position = 'none') + 
  xlab('Easting') + 
  ylab('Northing')


pdf('figure6.bkgsamples.pdf', width = 480/50, height = 480/50)
multiplot(bkg.field.plot.aq, bkg.sample.1.aq, bkg.sample.2.aq, bkg.sample.3.aq, 
          bkg.field.plot.ama, bkg.sample.1.ama, bkg.sample.2.ama, bkg.sample.3.ama, 
          layout = matrix(1:8, ncol = 2, byrow = FALSE))
dev.off()


# plotting grid example 
Plot_grid(xx = mean(list.output.bkg.ama$bdy.km@bbox[1,]),
          yy = mean(list.output.bkg.ama$bdy.km@bbox[2,]),
          delta_ = 20, n.layer = 5, 
          bdy_ = list.output.bkg.ama$bdy.km, min.edge = 1, displaypl = TRUE) + 
  coord_equal() + 
  xlab('Easting') + 
  ylab('Northing')

grid.to.plot <- Plot_grid(xx = mean(list.output.bkg.ama$bdy.km@bbox[1,]),
                          yy = mean(list.output.bkg.ama$bdy.km@bbox[2,]),
                          delta_ = 20, n.layer = 5, 
                          bdy_ = list.output.bkg.ama$bdy.km, min.edge = 1, displaypl = FALSE)

grid.sf <- st_as_sf(grid.to.plot)

pdf('figure.spatgrid.pdf', width = 480/50, height = 480/50)
ggplot(grid.sf) + #, aes(x = long, y = lat, group = ID)) + 
  geom_sf(fill = 'white', color = 'black') + 
  theme_classic() + 
  coord_sf()
dev.off()

list.output.bkg.ama$delta.sp



Find_sp_grid(xx = mean(list.output.bkg.ama$bdy.km@bbox[1,]),
             yy = mean(list.output.bkg.ama$bdy.km@bbox[2,]),
             delta_ = 20, n.layer = 5, 
             bdy_ = list.output.bkg.ama$bdy.km, min.edge = 1)
debugonce(space.time.grid)

list.output.bkg.aq$catalog.bru$mags <-list.output.bkg.aq$catalog.bru$magnitudes 
aa <- space.time.grid(data.point = list.output.bkg.aq$catalog.bru[which.max(list.output.bkg.aq$catalog.bru$magnitudes),],
                delta.sp = 1, n.layer. = 5, min.edge. = 1, coef.t = 0.1, delta.t = 0.1, N.max = 1, 
                T2. = 100, bdy. = list.output.bkg.aq$bdy.km)
aa[,c('bin.name', 'ref_layer')]


#######################################
## FAULTS
###########################
# plot fault info

#fault.l <- readOGR('data/DISS_3.3.0_shp/SHP/DSS330.shp')
fault.l <- readOGR('data/DISS_3.3.0_shp/SHP/CSSPLN330.shp')
#fault.iss <- readOGR('data/DISS_3.3.0_shp/SHP/ISS330.shp')

length(fault.l)
ggplot() + gg(fault.l, color = 'orange', fill = 'orange') 

fault.l.crop <- crop(fault.l, inner.bdy)

pl.fault <- ggplot(italy.map, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group), color = 'darkgrey', alpha = 0, linewidth = 1) +
  gg(fault.l, color = 'red', fill = 'red') + 
  gg(square.bdy, color = 'orange', alpha = 0) + 
  gg(inner.bdy, color = 'green', alpha = 0) + 
  theme_classic() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  coord_equal()
  

zoom.fault.aq <- ggplot()+   
  geom_point(data = list.output.bkg.aq$catalog, aes(x = Lon, y = Lat), size = 0.1) + 
  gg(fault.l.crop, color = 'red', fill = 'red') + 
  #gg(fault.iss.crop, color = 'orange', fill = 'orange') +
  gg(inner.bdy, color = 'green', alpha = 0) +
  xlim(inner.bdy@bbox[1,]) + 
  ylim(inner.bdy@bbox[2,]) +
  xlab('Longitude') + 
  ylab('Latitude') + 
  theme_classic()


zoom.fault.ama <- ggplot()  +   
  geom_point(data = list.output.bkg.ama$catalog, aes(x = Lon, y = Lat), size = 0.1) + 
  gg(fault.l.crop, color = 'red', fill = 'red') + 
  #gg(fault.iss.crop, color = 'orange') +
  gg(inner.bdy, color = 'green', alpha = 0) +
  xlim(inner.bdy@bbox[1,]) + 
  ylim(inner.bdy@bbox[2,]) + 
  xlab('Longitude') + 
  ylab('Latitude') +
  theme_classic()
multiplot(pl.fault, zoom.fault.aq, zoom.fault.ama, 
          layout = matrix(c(1,1,2,3), byrow = TRUE, ncol = 2))

pdf('figure3.fault.pdf', width = 480/70, height = 480/70)
pl.fault + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
dev.off()


fault.l.crop <- crop(fault.l, square.bdy)

plot.fault <- function(list.input, fault.poly, bdy_){
  pp <- list.input$catalog
  coordinates(pp) <- c('Lon', 'Lat')
  proj4string(pp) <- fault.poly@proj4string
  pp <- spTransform(pp, fault.poly@proj4string)
  
  dd <- gDistance(pp, fault.poly, byid=TRUE) 
  which.fault <- apply(dd, 2, which.min)
  pp$SourceName <- fault.poly$SourceName[which.fault]
  fault.poly$mean.strike <- (as.numeric(fault.poly$StrikeMin) + 
                               as.numeric(fault.poly$StrikeMax) )/2
  pp$mean.strike <- fault.poly$mean.strike[which.fault]
  
  pp.crop <- crop(pp, bdy_)
  fault.l.crop2 <- crop(fault.poly, bdy_) 
  dd <- gDistance(pp.crop, fault.l.crop2, byid=TRUE) 
  which.fault <- apply(dd, 2, which.min)
  
  ggplot() + 
    gg(pp.crop, aes(color = mean.strike), size = 0.2) + 
    gg(fault.l.crop2, alpha = 0) + 
    gg(fault.l.crop2[unique(which.fault),], aes(fill = mean.strike)) +
    labs(fill = 'Mean Strike', color = 'Mean Strike') + 
    theme_classic() +
    theme(legend.position = c(0.20, 0.15),
          legend.key.width = unit(0.25,"cm"),
          legend.key.height = unit(0.25,"cm"),
          legend.background = element_rect(colour ="darkgrey"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 7)) +
    xlab('Longitude') +
    ylab('Latitude') 
}


fault.l.crop.inner <- crop(fault.l, inner.bdy)
color.limits <- range((as.numeric(fault.l.crop.inner$StrikeMin) + 
                        as.numeric(fault.l.crop.inner$StrikeMin))/2  )
pl.fault.aq <- plot.fault(list.input.aquila, fault.l.crop.inner, inner.bdy) + 
  annotate('text', x = 13.7, y = 43.2, label = '(a)') + 
  scale_color_viridis(limits = color.limits, option = 'plasma') + 
  scale_fill_viridis(limits = color.limits,option = 'plasma') + 
  coord_equal() 
pl.fault.ama <- plot.fault(list.input.amatrice, fault.l.crop.inner, inner.bdy) + 
  annotate('text', x = 13.7, y = 43.2, label = '(b)') + 
  scale_color_viridis(limits = color.limits) + 
  scale_fill_viridis(limits = color.limits) + 
  coord_equal()

pdf('figure4.faultass.pdf', width = 480/80, height = 480/80)
multiplot(pl.fault.aq, pl.fault.ama, cols = 2)
dev.off()

################################
## fit simple isotropic model ##
################################

#debugonce(compute.grid)
#debugonce(isotropic.ETAS.model)
list.output.bkg.aq$bru.opt.list$bru_max_iter <- 50
list.output.bkg.ama$bru.opt.list$bru_max_iter <- 50
fit.etas.aquila <- isotropic.ETAS.model(list.input = list.output.bkg.aq)
save(fit.etas.aquila, file = 'fit.etas.aquila.Rds')
fit.etas.ama <- isotropic.ETAS.model(list.input = list.output.bkg.ama)
save(fit.etas.ama, file = 'fit.etas.ama.Rds')

load('fit.etas.aquila.Rds')
load('fit.etas.ama.Rds')


# depth model
source('utils/code_for_etas_cov.R')
list.output.bkg.aq$catalog.bru.km$depth <- list.output.bkg.aq$catalog$Depth
list.output.bkg.aq$bru.opt.list$bru_max_iter <- 50
fit.etas.aquila.depth <- spatial.ETAS.model.cov(list.input = list.output.bkg.aq, model.type = 'depth')
save(fit.etas.aquila.depth, file = 'fit.etas.aquila.depth.Rds')

## add fault 
pp <- list.output.bkg.aq$catalog
coordinates(pp) <- c('Lon', 'Lat')
proj4string(pp) <- fault.l.crop@proj4string
pp <- spTransform(pp, fault.l.crop@proj4string)

dd <- gDistance(pp, fault.l.crop, byid=TRUE) 
which.fault <- apply(dd, 2, which.min)
pp$SourceName <- fault.l.crop$SourceName[which.fault]
pp$mean.strike <- (as.numeric(fault.l.crop$StrikeMin[which.fault]) + 
                     as.numeric(fault.l.crop$StrikeMax[which.fault]))/2
list.output.bkg.aq$catalog.bru.km$Sourcename <- factor(pp$SourceName)
list.output.bkg.aq$catalog.bru.km$mean.strike <- pp$mean.strike
#list.try <- list.output.bkg.aq
#list.try$catalog.bru.km <- list.try$catalog.bru.km[1,]
source('utils/code_for_etas_cov.R')

fit.etas.aquila.strike <- spatial.ETAS.model.cov(list.input = list.output.bkg.aq, model.type = 'strike')
save(fit.etas.aquila.strike, file = 'fit.etas.aquila.strike.Rds')

fit.etas.aquila.full <- spatial.ETAS.model.cov(list.input = list.output.bkg.aq, 
                                               model.type = 'full')
save(fit.etas.aquila.full, file = 'fit.etas.aquila.full.Rds')


## amatrice model 

list.output.bkg.ama$catalog.bru.km$depth <- list.output.bkg.ama$catalog$Depth
list.output.bkg.ama$bru.opt.list$bru_max_iter <- 50
fit.etas.ama.depth <- spatial.ETAS.model.cov(list.input = list.output.bkg.ama, 
                                             model.type = 'depth')
save(fit.etas.ama.depth, file = 'fit.etas.ama.depth.Rds')

## add fault 
pp <- list.output.bkg.ama$catalog
coordinates(pp) <- c('Lon', 'Lat')
proj4string(pp) <- fault.l.crop@proj4string
pp <- spTransform(pp, fault.l.crop@proj4string)

dd <- gDistance(pp, fault.l.crop, byid=TRUE) 
which.fault <- apply(dd, 2, which.min)
pp$SourceName <- fault.l.crop$SourceName[which.fault]
pp$mean.strike <- (as.numeric(fault.l.crop$StrikeMin[which.fault]) + 
                     as.numeric(fault.l.crop$StrikeMax[which.fault]))/2
list.output.bkg.ama$catalog.bru.km$Sourcename <- factor(pp$SourceName)
list.output.bkg.ama$catalog.bru.km$mean.strike <- pp$mean.strike


fit.etas.ama.strike <- spatial.ETAS.model.cov(list.input = list.output.bkg.ama, 
                                                 model.type = 'strike')
save(fit.etas.ama.strike, file = 'fit.etas.ama.strike.Rds')

fit.etas.ama.full <- spatial.ETAS.model.cov(list.input = list.output.bkg.ama, 
                                            model.type = 'full')
save(fit.etas.ama.full, file = 'fit.etas.ama.full.Rds')


plot(fit.etas.ama.full$marginals.fixed)

list.output.bkg.ama$link.functions$beta <- \(x){x}
a <- get_posterior(list.output.bkg.ama$link.functions, fit.etas.ama.depth, 
              c('mu', 'K', 'alpha', 'c', 'p', 'sigma', 'beta'))
ggplot(a, aes(x,y)) + geom_line() +
  facet_wrap(facets = vars(param), scales = 'free')



################## posterior analysis

## isotropic models
par.names = c('mu', 'K', 'alpha', 'c', 'p', 'sigma^2')
post.iso.aq <- get_posterior(list.output.bkg.aq$link.functions, fit.etas.aquila,
                             par.names = par.names)
post.iso.aq$model = 'basic'
post.iso.aq$sequence = 'Aquila'

post.iso.ama <- get_posterior(list.output.bkg.ama$link.functions, fit.etas.ama,
                             par.names = par.names)
post.iso.ama$model = 'basic'
post.iso.ama$sequence = 'Amatrice'

pdf('figure6.postbasic.pdf', width = 480/60, height = 480/150)
ggplot(rbind(post.iso.aq, post.iso.ama), 
       aes(x,y, color = sequence, linetype = sequence)) + 
  geom_line() + 
  facet_wrap(facets = vars(param), scales = 'free', 
             labeller = label_parsed) + 
  xlab('value') + 
  ylab('posterior') +
  theme_classic() + 
  scale_x_continuous(expand = c(0,0))
dev.off()




## depth models
load('fit.etas.aquila.depth.Rds')
load('fit.etas.ama.depth.Rds')
list.output.bkg.aq$link.functions$beta.depth = \(x){x}
list.output.bkg.ama$link.functions$beta.depth = \(x){x}

par.names.depth = c('mu', 'K', 'alpha', 'c', 'p', 'sigma^2', 'beta[depth]')
post.depth.aq <- get_posterior(list.output.bkg.aq$link.functions, fit.etas.aquila.depth,
                             par.names = par.names.depth)
post.depth.aq$model = 'depth'
post.depth.aq$sequence = 'Aquila'

list.output.bkg.ama$link.functions$beta.depth = \(x){x}
post.depth.ama <- get_posterior(list.output.bkg.ama$link.functions, fit.etas.ama.depth,
                              par.names = par.names.depth)
post.depth.ama$model = 'depth'
post.depth.ama$sequence = 'Amatrice'

ggplot(rbind(post.depth.aq, post.depth.ama), 
       aes(x,y, color = sequence, linetype = sequence)) + 
  geom_line() + 
  facet_wrap(facets = vars(param), scales = 'free', 
             labeller = label_parsed) + 
  xlab('param') + 
  ylab('posterior') +
  theme_classic()

ggplot(rbind(post.depth.aq[post.depth.aq$param != 'beta[depth]',], post.iso.aq), 
       aes(x,y, color = model, linetype = model)) + 
  geom_line() + 
  facet_wrap(facets = vars(param), scales = 'free', 
             labeller = label_parsed) + 
  xlab('param') + 
  ylab('posterior') +
  theme_classic()

ggplot(rbind(post.depth.ama[post.depth.ama$param != 'beta[depth]',], post.iso.ama), 
       aes(x,y, color = model, linetype = model)) + 
  geom_line() + 
  facet_wrap(facets = vars(param), scales = 'free', 
             labeller = label_parsed) + 
  xlab('param') + 
  ylab('posterior') +
  theme_classic()

## strike models

load('fit.etas.aquila.strike.Rds')
load('fit.etas.ama.strike.Rds')
list.output.bkg.aq$link.functions$beta.strike = \(x){x}
list.output.bkg.ama$link.functions$beta.strike = \(x){x}

par.names.strike = c('mu', 'K', 'alpha', 'c', 'p', 'sigma^2', 'beta[ms]')
post.strike.aq <- get_posterior(list.output.bkg.aq$link.functions, fit.etas.aquila.strike,
                               par.names = par.names.strike)
post.strike.aq$model = 'strike'
post.strike.aq$sequence = 'Aquila'

list.output.bkg.ama$link.functions$beta.strike = \(x){x}
post.strike.ama <- get_posterior(list.output.bkg.ama$link.functions, fit.etas.ama.strike,
                                par.names = par.names.strike)
post.strike.ama$model = 'strike'
post.strike.ama$sequence = 'Amatrice'

ggplot(rbind(post.strike.aq, post.strike.ama), 
       aes(x,y, color = dataset, linetype = dataset)) + 
  geom_line() + 
  facet_wrap(facets = vars(param), scales = 'free', 
             labeller = label_parsed) + 
  xlab('param') + 
  ylab('posterior') +
  theme_classic()

ggplot(rbind(post.strike.aq[post.strike.aq$param != 'beta[ms]',], post.iso.aq), 
       aes(x,y, color = model, linetype = model)) + 
  geom_line() + 
  facet_wrap(facets = vars(param), scales = 'free', 
             labeller = label_parsed) + 
  xlab('param') + 
  ylab('posterior') +
  theme_classic()

ggplot(rbind(post.strike.ama[post.strike.ama$param != 'beta[ms]',], post.iso.ama), 
       aes(x,y, color = model, linetype = model)) + 
  geom_line() + 
  facet_wrap(facets = vars(param), scales = 'free', 
             labeller = label_parsed) + 
  xlab('param') + 
  ylab('posterior') +
  theme_classic()

## full model 

load('fit.etas.ama.full.Rds')
load('fit.etas.aquila.full.Rds')

par.names.full = c('mu', 'K', 'alpha', 'c', 'p', 'sigma^2', 'beta[ms]', 'beta[depth]')
post.full.aq <- get_posterior(list.output.bkg.aq$link.functions, fit.etas.aquila.full,
                                par.names = par.names.full)
post.full.aq$model = 'full'
post.full.aq$sequence = 'Aquila'

post.full.ama <- get_posterior(list.output.bkg.ama$link.functions, fit.etas.ama.full,
                                 par.names = par.names.full)
post.full.ama$model = 'full'
post.full.ama$sequence = 'Amatrice'

ggplot(rbind(post.full.aq, post.full.ama), 
       aes(x,y, color = dataset, linetype = dataset)) + 
  geom_line() + 
  facet_wrap(facets = vars(param), scales = 'free', 
             labeller = label_parsed) + 
  xlab('param') + 
  ylab('posterior') +
  theme_classic()


ggplot(rbind(post.full.ama[post.full.ama$param == 'beta[ms]' | 
                             post.full.ama$param == 'beta[depth]',], 
             post.depth.ama[post.depth.ama$param == 'beta[depth]',],
             post.strike.ama[post.strike.ama$param == 'beta[ms]',]), 
       aes(x,y, color = model, linetype = model)) + 
  geom_line() + 
  facet_wrap(facets = vars(param), scales = 'free', 
             labeller = label_parsed) + 
  xlab('param') + 
  ylab('posterior') +
  theme_classic()


ggplot(rbind(post.full.ama, 
             post.depth.ama,
             post.strike.ama), 
       aes(x,y, color = model, linetype = model)) + 
  geom_line() + 
  facet_wrap(facets = vars(param), scales = 'free', 
             labeller = label_parsed) + 
  xlab('param') + 
  ylab('posterior') +
  theme_classic()


post.merged.aq <- rbind(post.iso.aq, 
                        post.full.aq, 
                        post.depth.aq,
                        post.strike.aq)
post.merged.aq$model <- factor(post.merged.aq$model, 
                               levels = c("basic", "depth", "strike", 'full'))
pdf('figure7.aquila.post.pdf', width = 480/60, height = 480/120)
ggplot(post.merged.aq, 
       aes(x,y, color = model, linetype = model)) + 
  geom_line() +
  facet_wrap(facets = vars(param), scales = 'free', 
             labeller = label_parsed) + 
  xlab('value') + 
  ylab('posterior') +
  theme_classic() + 
  scale_x_continuous(expand = c(0,0))
dev.off()


post.merged.ama <- rbind(post.iso.ama, 
                        post.full.ama, 
                        post.depth.ama,
                        post.strike.ama)
post.merged.ama$model <- factor(post.merged.ama$model, 
                               levels = c("basic", "depth", "strike", 'full'))
pdf('figure8.ama.post.pdf', width = 480/60, height = 480/120)
ggplot(post.merged.ama, 
       aes(x,y, color = model, linetype = model)) + 
  geom_line() +
  facet_wrap(facets = vars(param), scales = 'free', 
             labeller = label_parsed) + 
  xlab('value') + 
  ylab('posterior') +
  theme_classic() + 
  scale_x_continuous(expand = c(0,0))
dev.off()







class(list.output.bkg.ama$bdy.km)

#### abundance

Abundance <- function(th.mu, th.K = NULL, th.alpha = NULL, th.c = NULL, th.p = NULL, th.sigma = NULL, 
                      th.beta.depth = NULL, th.beta.strike = NULL,
                      catalogue, T1, T2, bdy, M0, link.fun, model.type = 'basic',
                      mod = 'classic'){
  #catalogue <- catalogue[catalogue$ts >= T1 & catalogue$ts < T2, ]
  if(model.type == 'basic'){
    par.v <- c(link.fun$mu(th.mu[1]),
               link.fun$K(th.K[1]),
               link.fun$alpha(th.alpha[1]),
               link.fun$cc(th.c[1]),
               link.fun$pp(th.p[1]),
               link.fun$sigma(th.sigma[1]))
    eta.v <- par.v[3]*(catalogue$magnitudes - M0)
  } else if(model.type == 'depth'){
    par.v <- c(link.fun$mu(th.mu[1]),
               link.fun$K(th.K[1]),
               link.fun$alpha(th.alpha[1]),
               link.fun$cc(th.c[1]),
               link.fun$pp(th.p[1]),
               link.fun$sigma(th.sigma[1]),
               link.fun$beta.depth(th.beta.depth[1]))
    eta.v <- par.v[3]*(catalogue$magnitudes - M0) + par.v[7]*catalogue$depth
  } else if(model.type == 'strike'){
    par.v <- c(link.fun$mu(th.mu[1]),
               link.fun$K(th.K[1]),
               link.fun$alpha(th.alpha[1]),
               link.fun$cc(th.c[1]),
               link.fun$pp(th.p[1]),
               link.fun$sigma(th.sigma[1]),
               link.fun$beta.strike(th.beta.strike[1]))
    eta.v <- par.v[3]*(catalogue$magnitudes - M0) + par.v[7]*catalogue$mean.strike  
  } else if(model.type == 'full'){
    par.v <- c(link.fun$mu(th.mu[1]),
               link.fun$K(th.K[1]),
               link.fun$alpha(th.alpha[1]),
               link.fun$cc(th.c[1]),
               link.fun$pp(th.p[1]),
               link.fun$sigma(th.sigma[1]),
               link.fun$beta.strike(th.beta.strike[1]),
               link.fun$beta.depth(th.beta.depth[1]))
    eta.v <- par.v[3]*(catalogue$magnitudes - M0) + par.v[7]*catalogue$mean.strike + par.v[8]*catalogue$depth
  } else{
    stop('Unknown model type')
  }
  Sigma.m <- matrix(c(par.v[6], 0, 
                      0, par.v[6]), byrow = TRUE, ncol = 2)
  #print(par.v)
  #print(Sigma.m)
  n.bkg <- par.v[1]*(T2 - T1)
  n.trig <- sum(exp(logLambda.h.vec(theta = par.v, 
                                    eta = eta.v, 
                                    th = catalogue$ts, 
                                    xh = catalogue$x, 
                                    yh = catalogue$y, 
                                    mh = catalogue$magnitudes, 
                                    M0 = M0, 
                                    T1 = T1, 
                                    T2 = T2, 
                                    bdy = bdy, 
                                    Sigma = Sigma.m) ))
  return(data.frame(n.bkg = n.bkg, 
                    n.trig = n.trig,
                    n.full = n.trig + n.bkg))
  
}


# list.output.bkg.aq$link.functions$beta.depth <- \(x){x}
# list.output.bkg.aq$link.functions$beta.strike <- \(x){x}
# list.output.bkg.ama$link.functions$beta.depth <- \(x){x}
# list.output.bkg.ama$link.functions$beta.strike <- \(x){x}


Abundance.aq.full <- predict(fit.etas.aquila.full, data.frame(), 
                             ~ Abundance(th.mu = th.mu, 
                                         th.K = th.K, 
                                         th.alpha = th.alpha, 
                                         th.c = th.c, 
                                         th.p = th.p, 
                                         th.sigma = th.sigma, 
                                         th.beta.depth = th.beta.depth, 
                                         th.beta.strike =  th.beta.strike,
                                         catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                         T1 = list.output.bkg.aq$T12[1],
                                         T2 = list.output.bkg.aq$T12[2],
                                         bdy = list.output.bkg.aq$bdy.km,
                                         M0 = list.output.bkg.aq$M0,
                                         link.fun = list.output.bkg.aq$link.functions,
                                         model.type = 'full'))


N.lims.full <- c(floor(Abundance.aq.full$n.full$q0.025 - 3*Abundance.aq.full$n.full$sd),
                 ceiling(Abundance.aq.full$n.full$q0.975 + 3*Abundance.aq.full$n.full$sd))

N.lims.trig <- c(floor(Abundance.aq.full$n.trig$q0.025 - 3*Abundance.aq.full$n.trig$sd),
                 ceiling(Abundance.aq.full$n.trig$q0.975 + 3*Abundance.aq.full$n.trig$sd))

N.lims.bkg <- c(floor(Abundance.aq.full$n.bkg$q0.025 - 3*Abundance.aq.full$n.bkg$sd),
                 ceiling(Abundance.aq.full$n.bkg$q0.975 + 3*Abundance.aq.full$n.bkg$sd))


##################################
## POSTERIOR ABUNDANCE ###########
##################################

##########
# AQUILA #
##########

##############
# FULL MODEL #
##############

## TOTAL NUMBER 
Abundance.aq.full.distro_full <- predict(fit.etas.aquila.full, data.frame(), 
                             ~ data.frame(x = N.lims.full[1]:N.lims.full[2],
                                          y = dpois(N.lims.full[1]:N.lims.full[2], 
                                                    Abundance(th.mu = th.mu, 
                                                              th.K = th.K, 
                                                              th.alpha = th.alpha, 
                                                              th.c = th.c, 
                                                              th.p = th.p, 
                                                              th.sigma = th.sigma, 
                                                              th.beta.depth = th.beta.depth, 
                                                              th.beta.strike =  th.beta.strike,
                                                              catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                                              T1 = list.output.bkg.aq$T12[1],
                                                              T2 = list.output.bkg.aq$T12[2],
                                                              bdy = list.output.bkg.aq$bdy.km,
                                                              M0 = list.output.bkg.aq$M0,
                                                              link.fun = list.output.bkg.aq$link.functions,
                                                              model.type = 'full')$n.full)))

## TRIGGERED
Abundance.aq.full.distro_trig <- predict(fit.etas.aquila.full, data.frame(), 
                                         ~ data.frame(x = N.lims.trig[1]:N.lims.trig[2],
                                                      y = dpois(N.lims.trig[1]:N.lims.trig[2], 
                                                                Abundance(th.mu = th.mu, 
                                                                          th.K = th.K, 
                                                                          th.alpha = th.alpha, 
                                                                          th.c = th.c, 
                                                                          th.p = th.p, 
                                                                          th.sigma = th.sigma, 
                                                                          th.beta.depth = th.beta.depth, 
                                                                          th.beta.strike =  th.beta.strike,
                                                                          catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                                                          T1 = list.output.bkg.aq$T12[1],
                                                                          T2 = list.output.bkg.aq$T12[2],
                                                                          bdy = list.output.bkg.aq$bdy.km,
                                                                          M0 = list.output.bkg.aq$M0,
                                                                          link.fun = list.output.bkg.aq$link.functions,
                                                                          model.type = 'full')$n.trig)))

## BACKGROUND
Abundance.aq.full.distro_bkg <- predict(fit.etas.aquila.full, data.frame(), 
                                         ~ data.frame(x = N.lims.bkg[1]:N.lims.bkg[2],
                                                      y = dpois(N.lims.bkg[1]:N.lims.bkg[2], 
                                                                Abundance(th.mu = th.mu, 
                                                                          th.K = th.K, 
                                                                          th.alpha = th.alpha, 
                                                                          th.c = th.c, 
                                                                          th.p = th.p, 
                                                                          th.sigma = th.sigma, 
                                                                          th.beta.depth = th.beta.depth, 
                                                                          th.beta.strike =  th.beta.strike,
                                                                          catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                                                          T1 = list.output.bkg.aq$T12[1],
                                                                          T2 = list.output.bkg.aq$T12[2],
                                                                          bdy = list.output.bkg.aq$bdy.km,
                                                                          M0 = list.output.bkg.aq$M0,
                                                                          link.fun = list.output.bkg.aq$link.functions,
                                                                          model.type = 'full')$n.bkg)))

Abundance.aq.full.distro_bkg$quantity = 'background'
Abundance.aq.full.distro_trig$quantity = 'aftershocks'
Abundance.aq.full.distro_full$quantity = 'bkg + after'
Abundance.aq.full.distro <- rbind(Abundance.aq.full.distro_bkg,
                                 Abundance.aq.full.distro_trig,
                                 Abundance.aq.full.distro_full)
Abundance.aq.full.distro$quantity <- factor(Abundance.aq.full.distro$quantity, 
                                            levels = c('background',
                                                       'aftershocks',
                                                       'bkg + after'))
ggplot(Abundance.aq.full.distro, aes(x, mean, color = quantity)) + 
  geom_line() +
  geom_vline(xintercept = nrow(list.output.bkg.aq$catalog.bru)) + 
  theme_bw()
  

###############
# BASIC MODEL #
###############

# TOTAL NUMBER
Abundance.aq.basic.distro_full <- predict(fit.etas.aquila, data.frame(), 
                                    ~ data.frame(x = N.lims.full[1]:N.lims.full[2],
                                                 y = dpois(N.lims.full[1]:N.lims.full[2], 
                                                           Abundance(th.mu = th.mu, 
                                                                     th.K = th.K, 
                                                                     th.alpha = th.alpha, 
                                                                     th.c = th.c, 
                                                                     th.p = th.p, 
                                                                     th.sigma = th.sigma, 
                                                                     catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                                                     T1 = list.output.bkg.aq$T12[1],
                                                                     T2 = list.output.bkg.aq$T12[2],
                                                                     bdy = list.output.bkg.aq$bdy.km,
                                                                     M0 = list.output.bkg.aq$M0,
                                                                     link.fun = list.output.bkg.aq$link.functions,
                                                                     model.type = 'basic')$n.full)))
# TRIGGERED
Abundance.aq.basic.distro_trig <- predict(fit.etas.aquila, data.frame(), 
                                     ~ data.frame(x = N.lims.trig[1]:N.lims.trig[2],
                                                  y = dpois(N.lims.trig[1]:N.lims.trig[2], 
                                                            Abundance(th.mu = th.mu, 
                                                                      th.K = th.K, 
                                                                      th.alpha = th.alpha, 
                                                                      th.c = th.c, 
                                                                      th.p = th.p, 
                                                                      th.sigma = th.sigma, 
                                                                      catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                                                      T1 = list.output.bkg.aq$T12[1],
                                                                      T2 = list.output.bkg.aq$T12[2],
                                                                      bdy = list.output.bkg.aq$bdy.km,
                                                                      M0 = list.output.bkg.aq$M0,
                                                                      link.fun = list.output.bkg.aq$link.functions,
                                                                      model.type = 'basic')$n.trig)))
# BACKGROUND
Abundance.aq.basic.distro_bkg <- predict(fit.etas.aquila, data.frame(), 
                                          ~ data.frame(x = N.lims.bkg[1]:N.lims.bkg[2],
                                                       y = dpois(N.lims.bkg[1]:N.lims.bkg[2], 
                                                                 Abundance(th.mu = th.mu, 
                                                                           th.K = th.K, 
                                                                           th.alpha = th.alpha, 
                                                                           th.c = th.c, 
                                                                           th.p = th.p, 
                                                                           th.sigma = th.sigma, 
                                                                           catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                                                           T1 = list.output.bkg.aq$T12[1],
                                                                           T2 = list.output.bkg.aq$T12[2],
                                                                           bdy = list.output.bkg.aq$bdy.km,
                                                                           M0 = list.output.bkg.aq$M0,
                                                                           link.fun = list.output.bkg.aq$link.functions,
                                                                           model.type = 'basic')$n.bkg)))




Abundance.aq.basic.distro_bkg$quantity = 'background'
Abundance.aq.basic.distro_trig$quantity = 'aftershocks'
Abundance.aq.basic.distro_full$quantity = 'bkg + after'
Abundance.aq.basic.distro <- rbind(Abundance.aq.basic.distro_bkg,
                                  Abundance.aq.basic.distro_trig,
                                  Abundance.aq.basic.distro_full)
Abundance.aq.basic.distro$quantity <- factor(Abundance.aq.basic.distro$quantity, 
                                            levels = c('background',
                                                       'aftershocks',
                                                       'bkg + after'))
ggplot(Abundance.aq.basic.distro, aes(x, mean, color = quantity)) + 
  geom_line() +
  geom_vline(xintercept = nrow(list.output.bkg.aq$catalog.bru)) + 
  theme_bw()


###############
# DEPTH MODEL #
###############

# TOTAL NUMBER
Abundance.aq.depth.distro_full <- predict(fit.etas.aquila.depth, data.frame(), 
                                     ~ data.frame(x = N.lims.full[1]:N.lims.full[2],
                                                  y = dpois(N.lims.full[1]:N.lims.full[2], 
                                                            Abundance(th.mu = th.mu, 
                                                                      th.K = th.K, 
                                                                      th.alpha = th.alpha, 
                                                                      th.c = th.c, 
                                                                      th.p = th.p, 
                                                                      th.sigma = th.sigma,
                                                                      th.beta.depth  = th.beta,
                                                                      catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                                                      T1 = list.output.bkg.aq$T12[1],
                                                                      T2 = list.output.bkg.aq$T12[2],
                                                                      bdy = list.output.bkg.aq$bdy.km,
                                                                      M0 = list.output.bkg.aq$M0,
                                                                      link.fun = list.output.bkg.aq$link.functions,
                                                                      model.type = 'depth')$n.full)))

# TRIGGERED
Abundance.aq.depth.distro_trig <- predict(fit.etas.aquila.depth, data.frame(), 
                                     ~ data.frame(x = N.lims.trig[1]:N.lims.trig[2],
                                                  y = dpois(N.lims.trig[1]:N.lims.trig[2], 
                                                            Abundance(th.mu = th.mu, 
                                                                      th.K = th.K, 
                                                                      th.alpha = th.alpha, 
                                                                      th.c = th.c, 
                                                                      th.p = th.p, 
                                                                      th.sigma = th.sigma,
                                                                      th.beta.depth  = th.beta,
                                                                      catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                                                      T1 = list.output.bkg.aq$T12[1],
                                                                      T2 = list.output.bkg.aq$T12[2],
                                                                      bdy = list.output.bkg.aq$bdy.km,
                                                                      M0 = list.output.bkg.aq$M0,
                                                                      link.fun = list.output.bkg.aq$link.functions,
                                                                      model.type = 'depth')$n.trig)))
# BACKGROUND
Abundance.aq.depth.distro_bkg <- predict(fit.etas.aquila.depth, data.frame(), 
                                     ~ data.frame(x = N.lims.bkg[1]:N.lims.bkg[2],
                                                  y = dpois(N.lims.bkg[1]:N.lims.bkg[2], 
                                                            Abundance(th.mu = th.mu, 
                                                                      th.K = th.K, 
                                                                      th.alpha = th.alpha, 
                                                                      th.c = th.c, 
                                                                      th.p = th.p, 
                                                                      th.sigma = th.sigma,
                                                                      th.beta.depth  = th.beta,
                                                                      catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                                                      T1 = list.output.bkg.aq$T12[1],
                                                                      T2 = list.output.bkg.aq$T12[2],
                                                                      bdy = list.output.bkg.aq$bdy.km,
                                                                      M0 = list.output.bkg.aq$M0,
                                                                      link.fun = list.output.bkg.aq$link.functions,
                                                                      model.type = 'depth')$n.bkg)))


Abundance.aq.depth.distro_bkg$quantity = 'background'
Abundance.aq.depth.distro_trig$quantity = 'aftershocks'
Abundance.aq.depth.distro_full$quantity = 'bkg + after'
Abundance.aq.depth.distro <- rbind(Abundance.aq.depth.distro_bkg,
                                   Abundance.aq.depth.distro_trig,
                                   Abundance.aq.depth.distro_full)
Abundance.aq.depth.distro$quantity <- factor(Abundance.aq.depth.distro$quantity, 
                                             levels = c('background',
                                                        'aftershocks',
                                                        'bkg + after'))
ggplot(Abundance.aq.depth.distro, aes(x, mean, color = quantity)) + 
  geom_line() +
  geom_vline(xintercept = nrow(list.output.bkg.aq$catalog.bru)) + 
  theme_bw()

################
# STRIKE MODEL #
################

# TOTAL NUMBER 

Abundance.aq.strike.distro_full <- predict(fit.etas.aquila.strike, data.frame(), 
                                     ~ data.frame(x = N.lims.full[1]:N.lims.full[2],
                                                  y = dpois(N.lims.full[1]:N.lims.full[2], 
                                                            Abundance(th.mu = th.mu, 
                                                                      th.K = th.K, 
                                                                      th.alpha = th.alpha, 
                                                                      th.c = th.c, 
                                                                      th.p = th.p, 
                                                                      th.sigma = th.sigma, 
                                                                      th.beta.strike  = th.beta,
                                                                      catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                                                      T1 = list.output.bkg.aq$T12[1],
                                                                      T2 = list.output.bkg.aq$T12[2],
                                                                      bdy = list.output.bkg.aq$bdy.km,
                                                                      M0 = list.output.bkg.aq$M0,
                                                                      link.fun = list.output.bkg.aq$link.functions,
                                                                      model.type = 'strike')$n.full)))

# TRIGGERED
Abundance.aq.strike.distro_trig <- predict(fit.etas.aquila.strike, data.frame(), 
                                      ~ data.frame(x = N.lims.trig[1]:N.lims.trig[2],
                                                   y = dpois(N.lims.trig[1]:N.lims.trig[2], 
                                                             Abundance(th.mu = th.mu, 
                                                                       th.K = th.K, 
                                                                       th.alpha = th.alpha, 
                                                                       th.c = th.c, 
                                                                       th.p = th.p, 
                                                                       th.sigma = th.sigma, 
                                                                       th.beta.strike  = th.beta,
                                                                       catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                                                       T1 = list.output.bkg.aq$T12[1],
                                                                       T2 = list.output.bkg.aq$T12[2],
                                                                       bdy = list.output.bkg.aq$bdy.km,
                                                                       M0 = list.output.bkg.aq$M0,
                                                                       link.fun = list.output.bkg.aq$link.functions,
                                                                       model.type = 'strike')$n.trig)))
# BACKGROUND

Abundance.aq.strike.distro_bkg <- predict(fit.etas.aquila.strike, data.frame(), 
                                      ~ data.frame(x = N.lims.bkg[1]:N.lims.bkg[2],
                                                   y = dpois(N.lims.bkg[1]:N.lims.bkg[2], 
                                                             Abundance(th.mu = th.mu, 
                                                                       th.K = th.K, 
                                                                       th.alpha = th.alpha, 
                                                                       th.c = th.c, 
                                                                       th.p = th.p, 
                                                                       th.sigma = th.sigma, 
                                                                       th.beta.strike  = th.beta,
                                                                       catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                                                       T1 = list.output.bkg.aq$T12[1],
                                                                       T2 = list.output.bkg.aq$T12[2],
                                                                       bdy = list.output.bkg.aq$bdy.km,
                                                                       M0 = list.output.bkg.aq$M0,
                                                                       link.fun = list.output.bkg.aq$link.functions,
                                                                       model.type = 'strike')$n.bkg)))

Abundance.aq.strike.distro_bkg$quantity = 'background'
Abundance.aq.strike.distro_trig$quantity = 'aftershocks'
Abundance.aq.strike.distro_full$quantity = 'bkg + after'
Abundance.aq.strike.distro <- rbind(Abundance.aq.strike.distro_bkg,
                                   Abundance.aq.strike.distro_trig,
                                   Abundance.aq.strike.distro_full)
Abundance.aq.strike.distro$quantity <- factor(Abundance.aq.strike.distro$quantity, 
                                             levels = c('background',
                                                        'aftershocks',
                                                        'bkg + after'))
ggplot(Abundance.aq.strike.distro, aes(x, mean, color = quantity)) + 
  geom_line() +
  geom_vline(xintercept = nrow(list.output.bkg.aq$catalog.bru)) + 
  theme_bw()



## MERGING 
Abundance.aq.full.distro$model <- 'full'
Abundance.aq.depth.distro$model <- 'depth'
Abundance.aq.strike.distro$model <- 'strike'
Abundance.aq.basic.distro$model <- 'basic'


Abundance.aq <- rbind(Abundance.aq.full.distro,
                      Abundance.aq.depth.distro,
                      Abundance.aq.strike.distro,
                      Abundance.aq.basic.distro)
Abundance.aq$model <- factor(Abundance.aq$model, 
                             levels = c('basic', 'strike', 'depth', 'full'))
ggplot(Abundance.aq, aes(x,mean, color = quantity)) + 
  geom_line() +   
  geom_vline(xintercept = nrow(list.output.bkg.aq$catalog.bru)) + 
  facet_wrap(facets = vars(model), nrow = 4,
             strip.position = "left") + 
  theme_bw() + 
  xlab('value') + 
  ylab('posterior')

  
########################
# AMATRICE SEQUENCE #### 
########################

Abundance.ama.full <- predict(fit.etas.ama.full, data.frame(), 
                              ~ Abundance(th.mu = th.mu, 
                                          th.K = th.K, 
                                          th.alpha = th.alpha, 
                                          th.c = th.c, 
                                          th.p = th.p, 
                                          th.sigma = th.sigma, 
                                          th.beta.depth = th.beta.depth, 
                                          th.beta.strike =  th.beta.strike,
                                          catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                          T1 = list.output.bkg.ama$T12[1],
                                          T2 = list.output.bkg.ama$T12[2],
                                          bdy = list.output.bkg.ama$bdy.km,
                                          M0 = list.output.bkg.ama$M0,
                                          link.fun = list.output.bkg.ama$link.functions,
                                          model.type = 'full'))


N.lims.full <- c(floor(Abundance.ama.full$n.full$q0.025 - 3*Abundance.ama.full$n.full$sd),
            ceiling(Abundance.ama.full$n.full$q0.975 + 3*Abundance.ama.full$n.full$sd))
N.lims.trig <- c(floor(Abundance.ama.full$n.trig$q0.025 - 3*Abundance.ama.full$n.trig$sd),
            ceiling(Abundance.ama.full$n.trig$q0.975 + 3*Abundance.ama.full$n.trig$sd))
N.lims.bkg <- c(floor(Abundance.ama.full$n.bkg$q0.025 - 3*Abundance.ama.full$n.bkg$sd),
            ceiling(Abundance.ama.full$n.bkg$q0.975 + 3*Abundance.ama.full$n.bkg$sd))


## FULL MODEL 

## TOTAL NUMBER
Abundance.ama.full.distro_full <- predict(fit.etas.ama.full, data.frame(), 
                                    ~ data.frame(x = N.lims.full[1]:N.lims.full[2],
                                                 y = dpois(N.lims.full[1]:N.lims.full[2], 
                                                           Abundance(th.mu = th.mu, 
                                                                     th.K = th.K, 
                                                                     th.alpha = th.alpha, 
                                                                     th.c = th.c, 
                                                                     th.p = th.p, 
                                                                     th.sigma = th.sigma, 
                                                                     th.beta.depth = th.beta.depth, 
                                                                     th.beta.strike =  th.beta.strike,
                                                                     catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                                                     T1 = list.output.bkg.ama$T12[1],
                                                                     T2 = list.output.bkg.ama$T12[2],
                                                                     bdy = list.output.bkg.ama$bdy.km,
                                                                     M0 = list.output.bkg.ama$M0,
                                                                     link.fun = list.output.bkg.ama$link.functions,
                                                                     model.type = 'full')$n.full)))

# TRIGGGERED 
Abundance.ama.full.distro_trig <- predict(fit.etas.ama.full, data.frame(), 
                                          ~ data.frame(x = N.lims.trig[1]:N.lims.trig[2],
                                                       y = dpois(N.lims.trig[1]:N.lims.trig[2], 
                                                                 Abundance(th.mu = th.mu, 
                                                                           th.K = th.K, 
                                                                           th.alpha = th.alpha, 
                                                                           th.c = th.c, 
                                                                           th.p = th.p, 
                                                                           th.sigma = th.sigma, 
                                                                           th.beta.depth = th.beta.depth, 
                                                                           th.beta.strike =  th.beta.strike,
                                                                           catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                                                           T1 = list.output.bkg.ama$T12[1],
                                                                           T2 = list.output.bkg.ama$T12[2],
                                                                           bdy = list.output.bkg.ama$bdy.km,
                                                                           M0 = list.output.bkg.ama$M0,
                                                                           link.fun = list.output.bkg.ama$link.functions,
                                                                           model.type = 'full')$n.trig)))

# BACKGROUND
Abundance.ama.full.distro_bkg <- predict(fit.etas.ama.full, data.frame(), 
                                          ~ data.frame(x = N.lims.bkg[1]:N.lims.bkg[2],
                                                       y = dpois(N.lims.bkg[1]:N.lims.bkg[2], 
                                                                 Abundance(th.mu = th.mu, 
                                                                           th.K = th.K, 
                                                                           th.alpha = th.alpha, 
                                                                           th.c = th.c, 
                                                                           th.p = th.p, 
                                                                           th.sigma = th.sigma, 
                                                                           th.beta.depth = th.beta.depth, 
                                                                           th.beta.strike =  th.beta.strike,
                                                                           catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                                                           T1 = list.output.bkg.ama$T12[1],
                                                                           T2 = list.output.bkg.ama$T12[2],
                                                                           bdy = list.output.bkg.ama$bdy.km,
                                                                           M0 = list.output.bkg.ama$M0,
                                                                           link.fun = list.output.bkg.ama$link.functions,
                                                                           model.type = 'full')$n.bkg)))
Abundance.ama.full.distro_bkg$quantity = 'background'
Abundance.ama.full.distro_trig$quantity = 'aftershocks'
Abundance.ama.full.distro_full$quantity = 'bkg + after'
Abundance.ama.full.distro <- rbind(Abundance.ama.full.distro_bkg,
                                   Abundance.ama.full.distro_trig,
                                   Abundance.ama.full.distro_full)
Abundance.ama.full.distro$quantity <- factor(Abundance.ama.full.distro$quantity, 
                                             levels = c('background',
                                                        'aftershocks',
                                                        'bkg + after'))
ggplot(Abundance.ama.full.distro, aes(x, mean, color = quantity)) + 
  geom_line() +
  geom_vline(xintercept = nrow(list.output.bkg.ama$catalog.bru)) + 
  theme_bw()



## BASIC MODEL

# TOTAL NUMBER
Abundance.ama.basic.distro_full <- predict(fit.etas.ama, data.frame(), 
                                     ~ data.frame(x = N.lims.full[1]:N.lims.full[2],
                                                  y = dpois(N.lims.full[1]:N.lims.full[2], 
                                                            Abundance(th.mu = th.mu, 
                                                                      th.K = th.K, 
                                                                      th.alpha = th.alpha, 
                                                                      th.c = th.c, 
                                                                      th.p = th.p, 
                                                                      th.sigma = th.sigma, 
                                                                      catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                                                      T1 = list.output.bkg.ama$T12[1],
                                                                      T2 = list.output.bkg.ama$T12[2],
                                                                      bdy = list.output.bkg.ama$bdy.km,
                                                                      M0 = list.output.bkg.ama$M0,
                                                                      link.fun = list.output.bkg.ama$link.functions,
                                                                      model.type = 'basic')$n.full)))

# TRIGGERED
Abundance.ama.basic.distro_trig <- predict(fit.etas.ama, data.frame(), 
                                           ~ data.frame(x = N.lims.trig[1]:N.lims.trig[2],
                                                        y = dpois(N.lims.trig[1]:N.lims.trig[2], 
                                                                  Abundance(th.mu = th.mu, 
                                                                            th.K = th.K, 
                                                                            th.alpha = th.alpha, 
                                                                            th.c = th.c, 
                                                                            th.p = th.p, 
                                                                            th.sigma = th.sigma, 
                                                                            catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                                                            T1 = list.output.bkg.ama$T12[1],
                                                                            T2 = list.output.bkg.ama$T12[2],
                                                                            bdy = list.output.bkg.ama$bdy.km,
                                                                            M0 = list.output.bkg.ama$M0,
                                                                            link.fun = list.output.bkg.ama$link.functions,
                                                                            model.type = 'basic')$n.trig)))
# BACKGROUND
Abundance.ama.basic.distro_bkg <- predict(fit.etas.ama, data.frame(), 
                                           ~ data.frame(x = N.lims.bkg[1]:N.lims.bkg[2],
                                                        y = dpois(N.lims.bkg[1]:N.lims.bkg[2], 
                                                                  Abundance(th.mu = th.mu, 
                                                                            th.K = th.K, 
                                                                            th.alpha = th.alpha, 
                                                                            th.c = th.c, 
                                                                            th.p = th.p, 
                                                                            th.sigma = th.sigma, 
                                                                            catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                                                            T1 = list.output.bkg.ama$T12[1],
                                                                            T2 = list.output.bkg.ama$T12[2],
                                                                            bdy = list.output.bkg.ama$bdy.km,
                                                                            M0 = list.output.bkg.ama$M0,
                                                                            link.fun = list.output.bkg.ama$link.functions,
                                                                            model.type = 'basic')$n.bkg)))



Abundance.ama.basic.distro_bkg$quantity = 'background'
Abundance.ama.basic.distro_trig$quantity = 'aftershocks'
Abundance.ama.basic.distro_full$quantity = 'bkg + after'
Abundance.ama.basic.distro <- rbind(Abundance.ama.basic.distro_bkg,
                                   Abundance.ama.basic.distro_trig,
                                   Abundance.ama.basic.distro_full)
Abundance.ama.basic.distro$quantity <- factor(Abundance.ama.basic.distro$quantity, 
                                             levels = c('background',
                                                        'aftershocks',
                                                        'bkg + after'))
ggplot(Abundance.ama.basic.distro, aes(x, mean, color = quantity)) + 
  geom_line() +
  geom_vline(xintercept = nrow(list.output.bkg.ama$catalog.bru)) + 
  theme_bw()


# DEPTH MODEL

# TOTAL NUMBER
Abundance.ama.depth.distro_full <- predict(fit.etas.ama.depth, data.frame(), 
                                          ~ data.frame(x = N.lims.full[1]:N.lims.full[2],
                                                       y = dpois(N.lims.full[1]:N.lims.full[2], 
                                                                 Abundance(th.mu = th.mu, 
                                                                           th.K = th.K, 
                                                                           th.alpha = th.alpha, 
                                                                           th.c = th.c, 
                                                                           th.p = th.p, 
                                                                           th.sigma = th.sigma,
                                                                           th.beta.depth  = th.beta.depth,
                                                                           catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                                                           T1 = list.output.bkg.ama$T12[1],
                                                                           T2 = list.output.bkg.ama$T12[2],
                                                                           bdy = list.output.bkg.ama$bdy.km,
                                                                           M0 = list.output.bkg.ama$M0,
                                                                           link.fun = list.output.bkg.ama$link.functions,
                                                                           model.type = 'depth')$n.full)))

# TRIGGERED
Abundance.ama.depth.distro_trig <- predict(fit.etas.ama.depth, data.frame(), 
                                          ~ data.frame(x = N.lims.trig[1]:N.lims.trig[2],
                                                       y = dpois(N.lims.trig[1]:N.lims.trig[2], 
                                                                 Abundance(th.mu = th.mu, 
                                                                           th.K = th.K, 
                                                                           th.alpha = th.alpha, 
                                                                           th.c = th.c, 
                                                                           th.p = th.p, 
                                                                           th.sigma = th.sigma,
                                                                           th.beta.depth  = th.beta.depth,
                                                                           catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                                                           T1 = list.output.bkg.ama$T12[1],
                                                                           T2 = list.output.bkg.ama$T12[2],
                                                                           bdy = list.output.bkg.ama$bdy.km,
                                                                           M0 = list.output.bkg.ama$M0,
                                                                           link.fun = list.output.bkg.ama$link.functions,
                                                                           model.type = 'depth')$n.trig)))

# BACKGROUND
Abundance.ama.depth.distro_bkg <- predict(fit.etas.ama.depth, data.frame(), 
                                     ~ data.frame(x = N.lims.bkg[1]:N.lims.bkg[2],
                                                  y = dpois(N.lims.bkg[1]:N.lims.bkg[2], 
                                                            Abundance(th.mu = th.mu, 
                                                                      th.K = th.K, 
                                                                      th.alpha = th.alpha, 
                                                                      th.c = th.c, 
                                                                      th.p = th.p, 
                                                                      th.sigma = th.sigma,
                                                                      th.beta.depth  = th.beta.depth,
                                                                      catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                                                      T1 = list.output.bkg.ama$T12[1],
                                                                      T2 = list.output.bkg.ama$T12[2],
                                                                      bdy = list.output.bkg.ama$bdy.km,
                                                                      M0 = list.output.bkg.ama$M0,
                                                                      link.fun = list.output.bkg.ama$link.functions,
                                                                      model.type = 'depth')$n.bkg)))


Abundance.ama.depth.distro_bkg$quantity = 'background'
Abundance.ama.depth.distro_trig$quantity = 'aftershocks'
Abundance.ama.depth.distro_full$quantity = 'bkg + after'
Abundance.ama.depth.distro <- rbind(Abundance.ama.depth.distro_bkg,
                                    Abundance.ama.depth.distro_trig,
                                    Abundance.ama.depth.distro_full)
Abundance.ama.depth.distro$quantity <- factor(Abundance.ama.depth.distro$quantity, 
                                              levels = c('background',
                                                         'aftershocks',
                                                         'bkg + after'))
ggplot(Abundance.ama.depth.distro, aes(x, mean, color = quantity)) + 
  geom_line() +
  geom_vline(xintercept = nrow(list.output.bkg.ama$catalog.bru)) + 
  theme_bw()

# STRIKE MODEL 
# TOTAL NUMBER
Abundance.ama.strike.distro_full <- predict(fit.etas.ama.strike, data.frame(), 
                                      ~ data.frame(x = N.lims.full[1]:N.lims.full[2],
                                                   y = dpois(N.lims.full[1]:N.lims.full[2], 
                                                             Abundance(th.mu = th.mu, 
                                                                       th.K = th.K, 
                                                                       th.alpha = th.alpha, 
                                                                       th.c = th.c, 
                                                                       th.p = th.p, 
                                                                       th.sigma = th.sigma, 
                                                                       th.beta.strike  = th.beta.strike,
                                                                       catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                                                       T1 = list.output.bkg.ama$T12[1],
                                                                       T2 = list.output.bkg.ama$T12[2],
                                                                       bdy = list.output.bkg.ama$bdy.km,
                                                                       M0 = list.output.bkg.ama$M0,
                                                                       link.fun = list.output.bkg.ama$link.functions,
                                                                       model.type = 'strike')$n.full)))

# TRIGGERED
Abundance.ama.strike.distro_trig <- predict(fit.etas.ama.strike, data.frame(), 
                                            ~ data.frame(x = N.lims.trig[1]:N.lims.trig[2],
                                                         y = dpois(N.lims.trig[1]:N.lims.trig[2], 
                                                                   Abundance(th.mu = th.mu, 
                                                                             th.K = th.K, 
                                                                             th.alpha = th.alpha, 
                                                                             th.c = th.c, 
                                                                             th.p = th.p, 
                                                                             th.sigma = th.sigma, 
                                                                             th.beta.strike  = th.beta.strike,
                                                                             catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                                                             T1 = list.output.bkg.ama$T12[1],
                                                                             T2 = list.output.bkg.ama$T12[2],
                                                                             bdy = list.output.bkg.ama$bdy.km,
                                                                             M0 = list.output.bkg.ama$M0,
                                                                             link.fun = list.output.bkg.ama$link.functions,
                                                                             model.type = 'strike')$n.trig)))
# BACKGROUND
Abundance.ama.strike.distro_bkg <- predict(fit.etas.ama.strike, data.frame(), 
                                            ~ data.frame(x = N.lims.bkg[1]:N.lims.bkg[2],
                                                         y = dpois(N.lims.bkg[1]:N.lims.bkg[2], 
                                                                   Abundance(th.mu = th.mu, 
                                                                             th.K = th.K, 
                                                                             th.alpha = th.alpha, 
                                                                             th.c = th.c, 
                                                                             th.p = th.p, 
                                                                             th.sigma = th.sigma, 
                                                                             th.beta.strike  = th.beta.strike,
                                                                             catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                                                             T1 = list.output.bkg.ama$T12[1],
                                                                             T2 = list.output.bkg.ama$T12[2],
                                                                             bdy = list.output.bkg.ama$bdy.km,
                                                                             M0 = list.output.bkg.ama$M0,
                                                                             link.fun = list.output.bkg.ama$link.functions,
                                                                             model.type = 'strike')$n.bkg)))




Abundance.ama.strike.distro_bkg$quantity = 'background'
Abundance.ama.strike.distro_trig$quantity = 'aftershocks'
Abundance.ama.strike.distro_full$quantity = 'bkg + after'
Abundance.ama.strike.distro <- rbind(Abundance.ama.strike.distro_bkg,
                                    Abundance.ama.strike.distro_trig,
                                    Abundance.ama.strike.distro_full)
Abundance.ama.strike.distro$quantity <- factor(Abundance.ama.strike.distro$quantity, 
                                              levels = c('background',
                                                         'aftershocks',
                                                         'bkg + after'))
ggplot(Abundance.ama.strike.distro, aes(x, mean, color = quantity)) + 
  geom_line() +
  geom_vline(xintercept = nrow(list.output.bkg.ama$catalog.bru)) + 
  theme_bw()

# MERGING
Abundance.ama.full.distro$model <- 'full'
Abundance.ama.depth.distro$model <- 'depth'
Abundance.ama.strike.distro$model <- 'strike'
Abundance.ama.basic.distro$model <- 'basic'


Abundance.ama <- rbind(Abundance.ama.full.distro,
                      Abundance.ama.depth.distro,
                      Abundance.ama.strike.distro,
                      Abundance.ama.basic.distro)
Abundance.ama$model <- factor(Abundance.ama$model, 
                             levels = c('basic', 'strike', 'depth', 'full'))
Abundance.ama$sequence <- 'Amatrice'
Abundance.ama$observed <- nrow(list.output.bkg.ama$catalog.bru)
Abundance.aq$sequence <- 'Aquila'
Abundance.aq$observed <- nrow(list.output.bkg.aq$catalog.bru)

Abundance.merged <- rbind(Abundance.aq, Abundance.ama)
Abundance.merged$sequence <- factor(Abundance.merged$sequence, levels = c('Aquila', 'Amatrice'))


pdf('figure11.abundance.pdf', width = 480/60, height = 480/120)
ggplot(Abundance.merged, aes(x,mean, color = quantity, linetype = quantity)) + 
  geom_line() +   
  geom_vline(aes(xintercept = observed)) + 
  facet_grid(vars(model), vars(sequence), switch = 'y') + 
  labs(color = '', linetype = '') + 
  theme_bw() + 
  theme(legend.position = 'bottom') +
  xlab('value') + 
  ylab('posterior')
dev.off()
?facet_grid

## abundance and time
t.points <- seq(list.output.bkg.ama$T12[1], list.output.bkg.ama$T12[2], length.out = 10)
# Abundance.ama.depth.time <- predict(fit.etas.ama.depth, data.frame(), 
#                                       ~ vapply(2:length(t.points), 
#                                                function(idx) 
#                                                  Abundance(th.mu = th.mu, 
#                                                            th.K = th.K, 
#                                                            th.alpha = th.alpha, 
#                                                            th.c = th.c, 
#                                                            th.p = th.p, 
#                                                            th.sigma = th.sigma,
#                                                            th.beta.depth  = th.beta.depth,
#                                                            catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
#                                                            T1 = list.output.bkg.ama$T12[1],
#                                                            T2 = t.points[idx],
#                                                            bdy = list.output.bkg.ama$bdy.km,
#                                                            M0 = list.output.bkg.ama$M0,
#                                                            link.fun = list.output.bkg.ama$link.functions,
#                                                            model.type = 'depth')$n.full, 0)) 

Abundance.time <- function(model.fit, list.input){
  t.points <- list.input$catalog.bru$ts[-1]
  ab.df.time <- data.frame()
  for(idx in 1:length(t.points)){
    sel.cat <- as.data.frame(list.input$catalog.bru.km[list.input$catalog.bru.km$ts < t.points[idx],])
    
    Ab.time.mean <-  
      Abundance(th.mu = model.fit$summary.fixed$mean[1], 
                th.K = model.fit$summary.fixed$mean[2], 
                th.alpha = model.fit$summary.fixed$mean[3], 
                th.c = model.fit$summary.fixed$mean[4], 
                th.p = model.fit$summary.fixed$mean[5], 
                th.sigma = model.fit$summary.fixed$mean[6],
                th.beta.depth  = model.fit$summary.fixed$mean[7],
                catalogue = sel.cat,
                T1 = list.input$T12[1],
                T2 = t.points[idx],
                bdy = list.input$bdy.km,
                M0 = list.input$M0,
                link.fun = list.input$link.functions,
                model.type = 'depth')
    Ab.time.median <-  
      Abundance(th.mu = model.fit$summary.fixed$`0.5quant`[1], 
                th.K = model.fit$summary.fixed$`0.5quant`[2], 
                th.alpha = model.fit$summary.fixed$`0.5quant`[3], 
                th.c = model.fit$summary.fixed$`0.5quant`[4], 
                th.p = model.fit$summary.fixed$`0.5quant`[5], 
                th.sigma = model.fit$summary.fixed$`0.5quant`[6],
                th.beta.depth  = model.fit$summary.fixed$`0.5quant`[7],
                catalogue = sel.cat,
                T1 = list.input$T12[1],
                T2 = t.points[idx],
                bdy = list.input$bdy.km,
                M0 = list.input$M0,
                link.fun = list.input$link.functions,
                model.type = 'depth')
    Ab.time.lower <-  
      Abundance(th.mu = model.fit$summary.fixed$`0.025quant`[1], 
                th.K = model.fit$summary.fixed$`0.025quant`[2], 
                th.alpha = model.fit$summary.fixed$`0.025quant`[3], 
                th.c = model.fit$summary.fixed$`0.025quant`[4], 
                th.p = model.fit$summary.fixed$`0.025quant`[5], 
                th.sigma = model.fit$summary.fixed$`0.025quant`[6],
                th.beta.depth  = model.fit$summary.fixed$`0.025quant`[7],
                catalogue = sel.cat,
                T1 = list.input$T12[1],
                T2 = t.points[idx],
                bdy = list.input$bdy.km,
                M0 = list.input$M0,
                link.fun = list.input$link.functions,
                model.type = 'depth')
    Ab.time.upper <-  
      Abundance(th.mu = model.fit$summary.fixed$`0.975quant`[1], 
                th.K = model.fit$summary.fixed$`0.975quant`[2], 
                th.alpha = model.fit$summary.fixed$`0.975quant`[3], 
                th.c = model.fit$summary.fixed$`0.975quant`[4], 
                th.p = model.fit$summary.fixed$`0.975quant`[5], 
                th.sigma = model.fit$summary.fixed$`0.975quant`[6],
                th.beta.depth  = model.fit$summary.fixed$`0.975quant`[7],
                catalogue = sel.cat,
                T1 = list.input$T12[1],
                T2 = t.points[idx],
                bdy = list.input$bdy.km,
                M0 = list.input$M0,
                link.fun = list.input$link.functions,
                model.type = 'depth')
    df.row <- data.frame(obs = nrow(sel.cat),
                         mean.full = Ab.time.mean$n.full,
                         median.full = Ab.time.median$n.full,
                         lower.full = Ab.time.lower$n.full,
                         upper.full = Ab.time.upper$n.full,
                         mean.bkg = Ab.time.mean$n.bkg,
                         median.bkg = Ab.time.median$n.bkg,
                         lower.bkg = Ab.time.lower$n.bkg,
                         upper.bkg = Ab.time.upper$n.bkg,
                         mean.trig = Ab.time.mean$n.trig,
                         median.trig = Ab.time.median$n.trig,
                         lower.trig = Ab.time.lower$n.trig,
                         upper.trig = Ab.time.upper$n.trig)
    ab.df.time <- rbind(ab.df.time, df.row)
    
  }
  
  return(cbind(ab.df.time, times = t.points))
  
}


Abundance.time.pred <- function(model.fit, list.input, reduced = FALSE, by.red = 0){
  t.points <- list.input$catalog.bru$ts[-1]
  if(reduced){
    t.points <- c(seq(t.points[1], t.points[length(t.points)], by = by.red),
                  list.input$T12[2])
  }
  ab.df.time <- data.frame()
  for(idx in 1:length(t.points)){
    sel.cat <- as.data.frame(list.input$catalog.bru.km[list.input$catalog.bru.km$ts < t.points[idx],])
    cat('perc : ', idx/length(t.points), '\n')
    Ab.time.pred <-predict(model.fit, data.frame(), 
                           ~  Abundance(th.mu = th.mu, 
                                        th.K = th.K, 
                                        th.alpha = th.alpha, 
                                        th.c = th.c, 
                                        th.p = th.p, 
                                        th.sigma = th.sigma,
                                        th.beta.depth  = th.beta.depth,
                                        catalogue = sel.cat,
                                        T1 = list.input$T12[1],
                                        T2 = t.points[idx],
                                        bdy = list.input$bdy.km,
                                        M0 = list.input$M0,
                                        link.fun = list.input$link.functions,
                                        model.type = 'depth'))
    
    df.row <- data.frame(obs = nrow(sel.cat),
                         mean.full = Ab.time.pred$n.full$mean,
                         median.full = Ab.time.pred$n.full$q0.5,
                         lower.full = Ab.time.pred$n.full$q0.025,
                         upper.full = Ab.time.pred$n.full$q0.975,
                         mean.bkg = Ab.time.pred$n.bkg$mean,
                         median.bkg = Ab.time.pred$n.bkg$q0.5,
                         lower.bkg = Ab.time.pred$n.bkg$q0.025,
                         upper.bkg = Ab.time.pred$n.bkg$q0.975,
                         mean.trig = Ab.time.pred$n.trig$mean,
                         median.trig = Ab.time.pred$n.trig$q0.5,
                         lower.trig = Ab.time.pred$n.trig$q0.025,
                         upper.trig = Ab.time.pred$n.trig$q0.975)
    ab.df.time <- rbind(ab.df.time, df.row)
    
  }
  
  return(cbind(ab.df.time, times = t.points))
  
}

#debugonce(Abundance.time)
Ab.time.depth.ama <- Abundance.time(fit.etas.ama.depth, list.output.bkg.ama)
Ab.time.depth.ama.pred <- Abundance.time.pred(fit.etas.ama.depth, list.output.bkg.ama)


ggplot(Ab.time.depth.ama.pred, aes(times, mean.full)) + 
  geom_line(aes(color = 'exp')) + 
  geom_ribbon(aes(xmin = times, xmax = times, ymin = lower.full, ymax = upper.full,
                  fill = 'exp', color = 'exp'), 
              alpha = 0.2) + 
  geom_line(aes(y = obs, color = 'true')) 

Ab.time.depth.ama.pred$mean.full.cum <- vapply(Ab.time.depth.ama.pred$mean.full,
                                          \(x) sum(Ab.time.depth.ama.pred$mean.full < x),0)
Ab.time.depth.ama.pred$lower.full.cum <- vapply(Ab.time.depth.ama.pred$mean.full,
                                          \(x) sum(Ab.time.depth.ama.pred$lower.full < x),0)
Ab.time.depth.ama.pred$upper.full.cum <- vapply(Ab.time.depth.ama.pred$mean.full,
                                          \(x) sum(Ab.time.depth.ama.pred$upper.full < x),0)


ggplot(Ab.time.depth.ama.pred, aes(mean.full, mean.full.cum)) +
  geom_line() + 
  geom_ribbon(aes(xmin = mean.full, xmax = mean.full,
                  ymin = lower.full.cum, ymax = upper.full.cum),
              alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2)

  


# abundance in space
pix2 <- pixels(list.output.bkg.ama$mesh, nx = 70, ny = 70, 
               mask = list.output.bkg.ama$bdy.km)

pred.bkg.pix <- predict(list.output.bkg.ama$fit.bkg, 
                         pix2, ~ exp(Smooth + Intercept))
bin.bkg.perc <- pred.bkg.pix$mean/sum(pred.bkg.pix$mean)

# add a bkg column to the data
aa <- data.frame(pix2@coords)
dist.x <- diff(unique(abs(aa[,1])))[1]/2
dist.y <- diff(unique(abs(aa[,2])))[1]/2

pix.abundance.df.ama.depth <- data.frame()

for(i in 1:nrow(aa)){
  cat('row number:', i, '\n')
  cat('pix perc:', i/nrow(aa), '\n')
  bbox <- matrix(c(aa[i,1] - dist.x, aa[i,1] + dist.x,
                   aa[i,2] - dist.y, aa[i,2] + dist.y), byrow = TRUE, ncol = 2)
  bdy.pix <- square_poly_from_bbox(bbox, crs.obj = list.output.bkg.ama$bdy.km@proj4string)
  # pred.n.pix <- predict(fit.etas.ama.depth, data.frame(), 
  #         ~   Abundance(th.mu = th.mu, 
  #                       th.K = th.K, 
  #                       th.alpha = th.alpha, 
  #                       th.c = th.c, 
  #                       th.p = th.p, 
  #                       th.sigma = th.sigma, 
  #                       th.beta.depth  = th.beta.depth,
  #                       catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
  #                       T1 = list.output.bkg.ama$T12[1],
  #                       T2 = list.output.bkg.ama$T12[2],
  #                       bdy = bdy.pix,
  #                       M0 = list.output.bkg.ama$M0,
  #                       link.fun = list.output.bkg.ama$link.functions,
  #                       model.type = 'depth'))
  # 
  mean.pix.ab <- Abundance(th.mu = fit.etas.ama.depth$summary.fixed$mean[1], 
                           th.K = fit.etas.ama.depth$summary.fixed$mean[2], 
                           th.alpha = fit.etas.ama.depth$summary.fixed$mean[3], 
                           th.c = fit.etas.ama.depth$summary.fixed$mean[4], 
                           th.p = fit.etas.ama.depth$summary.fixed$mean[5], 
                           th.sigma = fit.etas.ama.depth$summary.fixed$mean[6], 
                           th.beta.depth  = fit.etas.ama.depth$summary.fixed$mean[7],
                           catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                           T1 = list.output.bkg.ama$T12[1],
                           T2 = list.output.bkg.ama$T12[2],
                           bdy = bdy.pix,
                           M0 = list.output.bkg.ama$M0,
                           link.fun = list.output.bkg.ama$link.functions,
                           model.type = 'depth')
  med.pix.ab <- Abundance(th.mu = fit.etas.ama.depth$summary.fixed$`0.5quant`[1], 
                           th.K = fit.etas.ama.depth$summary.fixed$`0.5quant`[2], 
                           th.alpha = fit.etas.ama.depth$summary.fixed$`0.5quant`[3], 
                           th.c = fit.etas.ama.depth$summary.fixed$`0.5quant`[4], 
                           th.p = fit.etas.ama.depth$summary.fixed$`0.5quant`[5], 
                           th.sigma = fit.etas.ama.depth$summary.fixed$`0.5quant`[6], 
                           th.beta.depth  = fit.etas.ama.depth$summary.fixed$`0.5quant`[7],
                           catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                           T1 = list.output.bkg.ama$T12[1],
                           T2 = list.output.bkg.ama$T12[2],
                           bdy = bdy.pix,
                           M0 = list.output.bkg.ama$M0,
                           link.fun = list.output.bkg.ama$link.functions,
                           model.type = 'depth')
  lower.pix.ab <- Abundance(th.mu = fit.etas.ama.depth$summary.fixed$`0.025quant`[1], 
                            th.K = fit.etas.ama.depth$summary.fixed$`0.025quant`[2], 
                            th.alpha = fit.etas.ama.depth$summary.fixed$`0.025quant`[3], 
                            th.c = fit.etas.ama.depth$summary.fixed$`0.025quant`[4], 
                            th.p = fit.etas.ama.depth$summary.fixed$`0.025quant`[5], 
                            th.sigma = fit.etas.ama.depth$summary.fixed$`0.025quant`[6], 
                            th.beta.depth  = fit.etas.ama.depth$summary.fixed$`0.025quant`[7],
                            catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                            T1 = list.output.bkg.ama$T12[1],
                            T2 = list.output.bkg.ama$T12[2],
                            bdy = bdy.pix,
                            M0 = list.output.bkg.ama$M0,
                            link.fun = list.output.bkg.ama$link.functions,
                            model.type = 'depth')
  
  upper.pix.ab <- Abundance(th.mu = fit.etas.ama.depth$summary.fixed$`0.975quant`[1], 
                            th.K = fit.etas.ama.depth$summary.fixed$`0.975quant`[2], 
                            th.alpha = fit.etas.ama.depth$summary.fixed$`0.975quant`[3], 
                            th.c = fit.etas.ama.depth$summary.fixed$`0.975quant`[4], 
                            th.p = fit.etas.ama.depth$summary.fixed$`0.975quant`[5], 
                            th.sigma = fit.etas.ama.depth$summary.fixed$`0.975quant`[6], 
                            th.beta.depth  = fit.etas.ama.depth$summary.fixed$`0.975quant`[7],
                            catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                            T1 = list.output.bkg.ama$T12[1],
                            T2 = list.output.bkg.ama$T12[2],
                            bdy = bdy.pix,
                            M0 = list.output.bkg.ama$M0,
                            link.fun = list.output.bkg.ama$link.functions,
                            model.type = 'depth')
  # if(is.null(pred.n.pix$n.trig)){
  #   out <- pred.n.pix$n.bkg[,c('mean', 'q0.025', 'q0.5', 'q0.975')]*bin.bkg.perc[i] 
  # } else{
  #   out <- pred.n.pix$n.bkg[,c('mean', 'q0.025', 'q0.5', 'q0.975')]*bin.bkg.perc[i] + 
  #     pred.n.pix$n.trig[,c('mean', 'q0.025', 'q0.5', 'q0.975')]
  # }
  mean.pix.ab$n.bkg <- mean.pix.ab$n.bkg*bin.bkg.perc[i]
  mean.pix.ab$n.full <- mean.pix.ab$n.bkg + mean.pix.ab$n.trig
  mean.pix.ab$quantity <- 'mean'
  med.pix.ab$n.bkg <- med.pix.ab$n.bkg*bin.bkg.perc[i]
  med.pix.ab$n.full <- med.pix.ab$n.bkg + med.pix.ab$n.trig
  med.pix.ab$quantity <- 'median'
  lower.pix.ab$n.bkg <- lower.pix.ab$n.bkg*bin.bkg.perc[i]
  lower.pix.ab$n.full <- lower.pix.ab$n.bkg + lower.pix.ab$n.trig
  lower.pix.ab$quantity <- '0.025q'
  upper.pix.ab$n.bkg <- upper.pix.ab$n.bkg*bin.bkg.perc[i]
  upper.pix.ab$n.full <- upper.pix.ab$n.bkg + lower.pix.ab$n.trig
  upper.pix.ab$quantity <- '0.975q'
  out <- rbind(mean.pix.ab, med.pix.ab, lower.pix.ab, upper.pix.ab)
  cat('---', '\n')
  pix.abundance.df.ama.depth <- rbind(pix.abundance.df.ama.depth, out)
}

#pix.abundance.ama.depth <- mclapply(1:nrow(aa), function(i) 
 # torun(i), mc.cores = 4)

pix2$mean.full <- pix.abundance.df.ama.depth$n.full[pix.abundance.df.ama.depth$quantity == 'mean']
pix2$logmean.full <- log(pix2$mean.full)
pix2$mean.trig <- pix.abundance.df.ama.depth$n.trig[pix.abundance.df.ama.depth$quantity == 'mean']
pix2$logmean.trig <- log(pix2$mean.trig)
pix2$mean.bkg <- pix.abundance.df.ama.depth$n.bkg[pix.abundance.df.ama.depth$quantity == 'mean']
pix2$logmean.bkg <- log(pix2$mean.bkg)

pix2$med.full <- pix.abundance.df.ama.depth$n.full[pix.abundance.df.ama.depth$quantity == 'median']
pix2$logmed.full <- log(pix2$med.full)
pix2$med.trig <- pix.abundance.df.ama.depth$n.trig[pix.abundance.df.ama.depth$quantity == 'median']
pix2$logmed.trig <- log10(pix2$mean.trig)
pix2$med.bkg <- pix.abundance.df.ama.depth$n.bkg[pix.abundance.df.ama.depth$quantity == 'median']
pix2$logmed.bkg <- log(pix2$med.bkg)


ggplot() + gg(pix2['logmed.full']) + 
  gg(list.output.bkg.ama$catalog.bru.km, size = 0.05) + 
  scale_fill_viridis()

## aquila spatial map   


head(list.output.bkg.aq$catalog.bru[order(list.output.bkg.aq$catalog.bru$magnitudes,
                                          decreasing = TRUE),], 3)

pixel.abundance <- function(list.input, model.fit, T1, T2, nxx, nyy, prospective = FALSE){
  pix.out <- pixels(list.input$mesh, nx = nxx, ny = nyy, 
                   mask = list.input$bdy.km)
  
  pred.bkg.pix <- predict(list.input$fit.bkg, 
                          pix.out, ~ exp(Smooth + Intercept))
  bin.bkg.perc <- pred.bkg.pix$mean/sum(pred.bkg.pix$mean)
  
  # add a bkg column to the data
  aa <- data.frame(pix.out@coords)
  dist.x <- diff(unique(abs(aa[,1])))[1]/2
  dist.y <- diff(unique(abs(aa[,2])))[1]/2
  
  pix.ab <- data.frame()
  true.pix <- c()
  catalogue_ <- as.data.frame(list.input$catalog.bru.km)
  catalogue_ <- catalogue_[catalogue_$ts < T2,]
  if(prospective){
    catalogue_ <- catalogue_[catalogue_$ts < T1,] 
    pp.true <- list.input$catalog.bru.km[list.input$catalog.bru.km$ts >= T1 &
                                           list.input$catalog.bru.km$ts < T2,]
  } else {
    pp.true <- list.input$catalog.bru.km
  }
  
  for(i in 1:nrow(aa)){
    cat('row number:', i, '\n')
    cat('pix perc:', i/nrow(aa), '\n')
    bbox <- matrix(c(aa[i,1] - dist.x, aa[i,1] + dist.x,
                     aa[i,2] - dist.y, aa[i,2] + dist.y), byrow = TRUE, ncol = 2)
    bdy.pix <- square_poly_from_bbox(bbox, crs.obj = list.input$bdy.km@proj4string)
    true.cropped <- crop(pp.true, bdy.pix)
    true.pix <- c(true.pix, length(true.cropped))
    cat('OBS :', length(true.cropped), '\n')
    mean.pix.ab <- Abundance(th.mu = model.fit$summary.fixed$mean[1], 
                             th.K = model.fit$summary.fixed$mean[2], 
                             th.alpha = model.fit$summary.fixed$mean[3], 
                             th.c = model.fit$summary.fixed$mean[4], 
                             th.p = model.fit$summary.fixed$mean[5], 
                             th.sigma = model.fit$summary.fixed$mean[6], 
                             th.beta.depth  = model.fit$summary.fixed$mean[7],
                             catalogue = catalogue_,
                             T1 = T1,
                             T2 = T2,
                             bdy = bdy.pix,
                             M0 = list.input$M0,
                             link.fun = list.input$link.functions,
                             model.type = 'depth')
    med.pix.ab <- Abundance(th.mu = model.fit$summary.fixed$`0.5quant`[1], 
                            th.K = model.fit$summary.fixed$`0.5quant`[2], 
                            th.alpha = model.fit$summary.fixed$`0.5quant`[3], 
                            th.c = model.fit$summary.fixed$`0.5quant`[4], 
                            th.p = model.fit$summary.fixed$`0.5quant`[5], 
                            th.sigma = model.fit$summary.fixed$`0.5quant`[6], 
                            th.beta.depth  = model.fit$summary.fixed$`0.5quant`[7],
                            catalogue = catalogue_,
                            T1 = T1,
                            T2 = T2,
                            bdy = bdy.pix,
                            M0 = list.input$M0,
                            link.fun = list.input$link.functions,
                            model.type = 'depth')
    # lower.pix.ab <- Abundance(th.mu = model.fit$summary.fixed$`0.025quant`[1], 
    #                           th.K = model.fit$summary.fixed$`0.025quant`[2], 
    #                           th.alpha = model.fit$summary.fixed$`0.025quant`[3], 
    #                           th.c = model.fit$summary.fixed$`0.025quant`[4], 
    #                           th.p = model.fit$summary.fixed$`0.025quant`[5], 
    #                           th.sigma = model.fit$summary.fixed$`0.025quant`[6], 
    #                           th.beta.depth  = model.fit$summary.fixed$`0.025quant`[7],
    #                           catalogue = catalogue_,
    #                           T1 = T1,
    #                           T2 = T2,
    #                           bdy = bdy.pix,
    #                           M0 = list.input$M0,
    #                           link.fun = list.input$link.functions,
    #                           model.type = 'depth')
    # 
    # upper.pix.ab <- Abundance(th.mu = model.fit$summary.fixed$`0.975quant`[1], 
    #                           th.K = model.fit$summary.fixed$`0.975quant`[2], 
    #                           th.alpha = model.fit$summary.fixed$`0.975quant`[3], 
    #                           th.c = model.fit$summary.fixed$`0.975quant`[4], 
    #                           th.p = model.fit$summary.fixed$`0.975quant`[5], 
    #                           th.sigma = model.fit$summary.fixed$`0.975quant`[6], 
    #                           th.beta.depth  = model.fit$summary.fixed$`0.975quant`[7],
    #                           catalogue = catalogue_,
    #                           T1 = T1,
    #                           T2 = T2,
    #                           bdy = bdy.pix,
    #                           M0 = list.input$M0,
    #                           link.fun = list.input$link.functions,
    #                           model.type = 'depth')
    # # if(is.null(pred.n.pix$n.trig)){
    #   out <- pred.n.pix$n.bkg[,c('mean', 'q0.025', 'q0.5', 'q0.975')]*bin.bkg.perc[i] 
    # } else{
    #   out <- pred.n.pix$n.bkg[,c('mean', 'q0.025', 'q0.5', 'q0.975')]*bin.bkg.perc[i] + 
    #     pred.n.pix$n.trig[,c('mean', 'q0.025', 'q0.5', 'q0.975')]
    # }
    mean.pix.ab$n.bkg <- mean.pix.ab$n.bkg*bin.bkg.perc[i]
    mean.pix.ab$n.trig <- mean.pix.ab$n.trig
    mean.pix.ab$n.full <- mean.pix.ab$n.bkg + mean.pix.ab$n.trig
    mean.pix.ab$quantity <- 'mean'
    med.pix.ab$n.bkg <- med.pix.ab$n.bkg*bin.bkg.perc[i]
    med.pix.ab$n.trig <- med.pix.ab$n.trig
    med.pix.ab$n.full <- med.pix.ab$n.bkg + med.pix.ab$n.trig
    med.pix.ab$quantity <- 'median'
    # lower.pix.ab$n.bkg <- lower.pix.ab$n.bkg*bin.bkg.perc[i]
    # lower.pix.ab$n.full <- lower.pix.ab$n.bkg + lower.pix.ab$n.trig
    # lower.pix.ab$quantity <- '0.025q'
    # upper.pix.ab$n.bkg <- upper.pix.ab$n.bkg*bin.bkg.perc[i]
    # upper.pix.ab$n.full <- upper.pix.ab$n.bkg + lower.pix.ab$n.trig
    # upper.pix.ab$quantity <- '0.975q'
    # out <- rbind(mean.pix.ab, med.pix.ab, lower.pix.ab, upper.pix.ab)
    out <- rbind(mean.pix.ab, med.pix.ab)
    cat('---', '\n')
    pix.ab <- rbind(pix.ab, out)
  }
  
  #pix.abundance.ama.depth <- mclapply(1:nrow(aa), function(i) 
  # torun(i), mc.cores = 4)
  
  pix.out$mean.full <- pix.ab$n.full[pix.ab$quantity == 'mean']
  pix.out$logmean.full <- log(pix.out$mean.full)
  pix.out$mean.trig <- pix.ab$n.trig[pix.ab$quantity == 'mean']
  pix.out$logmean.trig <- log(pix.out$mean.trig)
  pix.out$mean.bkg <- pix.ab$n.bkg[pix.ab$quantity == 'mean']
  pix.out$logmean.bkg <- log(pix.out$mean.bkg)
  
  pix.out$med.full <- pix.ab$n.full[pix.ab$quantity == 'median']
  pix.out$logmed.full <- log(pix.out$med.full)
  pix.out$med.trig <- pix.ab$n.trig[pix.ab$quantity == 'median']
  pix.out$logmed.trig <- log10(pix.out$mean.trig)
  pix.out$med.bkg <- pix.ab$n.bkg[pix.ab$quantity == 'median']
  pix.out$logmed.bkg <- log(pix.out$med.bkg)
  
  pix.out$true <- true.pix
  pix.out$logtrue <- log(pix.out$true)
  return(list(pixel = pix.out,
              point.obs = pp.true))
}


large.sel.aq <- head(list.output.bkg.aq$catalog.bru[order(list.output.bkg.aq$catalog.bru$magnitudes,
                                                          decreasing = TRUE),],3)
#debugonce(pixel.abundance)
#undebug(pixel.abundance)
ab.pix.ama  <- pixel.abundance(list.input = list.output.bkg.ama,
                              model.fit = fit.etas.ama.depth,
                              T1 = list.output.bkg.ama$T12[1],
                              T2 = list.output.bkg.ama$T12[2],#list.output.bkg.aq$T12[2],
                              nxx = 90,
                              nyy = 90,
                              prospective = FALSE)

ab.pix.aq  <- pixel.abundance(list.input = list.output.bkg.aq,
                           model.fit = fit.etas.aquila.depth,
                           T1 = list.output.bkg.aq$T12[1],
                           T2 = list.output.bkg.aq$T12[2],#list.output.bkg.aq$T12[2],
                           nxx = 90,
                           nyy = 90,
                           prospective = FALSE)


pix.aq <- ab.pix.aq$pixel


df.ab.aq <- as.data.frame(pix.aq)

df.ab.aq.comp <- rbind(data.frame(x = df.ab.aq$x, y = df.ab.aq$y, 
                                  `logN` = df.ab.aq$logmean.full,
                                  quantity = 'bkg + after'),
                       data.frame(x = df.ab.aq$x, y = df.ab.aq$y, 
                                  `logN` = df.ab.aq$logtrue,
                                  quantity = 'observed'),
                       data.frame(x = df.ab.aq$x, y = df.ab.aq$y, 
                                  `logN` = df.ab.aq$logmean.trig,
                                  quantity = 'aftershocks'),
                       data.frame(x = df.ab.aq$x, y = df.ab.aq$y, 
                                  `logN` = df.ab.aq$logmean.bkg,
                                  quantity = 'background'))

df.ab.aq.comp$quantity = factor(df.ab.aq.comp$quantity, 
                                levels = c('background', 'aftershocks', 'bkg + after', 'observed'))
aq.plot <- as.data.frame(list.output.bkg.aq$catalog.bru.km)
pdf('figure13.aq.spatial.pdf', width = 480/60, height = 480/60)
ggplot(df.ab.aq.comp, aes(x = x, y = y, fill = `logN`)) + 
  geom_tile() + 
  coord_equal() +
  geom_star(data = aq.plot[aq.plot$magnitudes >= 5,],
            mapping = aes(x,y), fill = 'red', size = 2) + 
   scale_fill_viridis(limits = c(-10,6), 
                      name = expression(paste("log(", Lambda, ")"))) + 
  facet_wrap(facets = vars(quantity), nrow = 2) + 
  theme_bw() + 
  xlab('Northing') + 
  ylab('Easting') 
dev.off()


# ab.pix.ama  <- pixel.abundance(list.input = list.output.bkg.ama,
#                               model.fit = fit.etas.ama.depth,
#                               T1 = list.output.bkg.ama$T12[1],
#                               T2 = list.output.bkg.ama$T12[2],#list.output.bkg.aq$T12[2],
#                               nxx = 70,
#                               nyy = 70,
#                               prospective = FALSE)

pix.ama <- ab.pix.ama$pixel

df.ab.ama <- as.data.frame(pix.ama)

df.ab.ama.comp <- rbind(data.frame(x = df.ab.ama$x, y = df.ab.ama$y, 
                                  `logN` = df.ab.ama$logmean.full,
                                  quantity = 'bkg + after'),
                       data.frame(x = df.ab.ama$x, y = df.ab.ama$y, 
                                  `logN` = df.ab.ama$logtrue,
                                  quantity = 'observed'),
                       data.frame(x = df.ab.ama$x, y = df.ab.ama$y, 
                                  `logN` = df.ab.ama$logmean.trig,
                                  quantity = 'aftershocks'),
                       data.frame(x = df.ab.ama$x, y = df.ab.ama$y, 
                                  `logN` = df.ab.ama$logmean.bkg,
                                  quantity = 'background'))

df.ab.ama.comp$quantity = factor(df.ab.ama.comp$quantity, 
                                levels = c('background', 'aftershocks', 'bkg + after', 'observed'))
ama.plot <- as.data.frame(list.output.bkg.ama$catalog.bru.km)


pdf('figure14.ama.spatial.pdf', width = 480/60, height = 480/60)
ggplot(df.ab.ama.comp, aes(x = x, y = y, fill = `logN`)) + 
  geom_tile() + 
  coord_equal() +
  geom_star(data = ama.plot[ama.plot$magnitudes >= 5,],
            mapping = aes(x,y), fill = 'red', size = 2) + 
  scale_fill_viridis(limits = c(-10,6), 
                     name = expression(paste("log(", Lambda, ")"))) + 
  facet_wrap(facets = vars(quantity), nrow = 2) + 
  theme_bw() + 
  xlab('Northing') + 
  ylab('Easting') 
dev.off()

# fill_lims <- range(c(pix.try$logmean.full, pix.try$logtrue[is.finite(pix.try$logtrue)]))
ggplot() + gg(pix.try['logtrue']) +
  scale_fill_viridis() 

ggplot() + gg(pix.try['logmean.full']) +
  scale_fill_viridis() 

# try2 <- ggplot() + gg(pix.try['logtrue']) + 
#   scale_fill_viridis(limits = fill_lims) + 
#   gg(ab.list.aq$point.obs[ab.list.aq$point.obs$magnitudes>5,],
#      color = 'red')
# multiplot(try1, try2, cols = 2)

# abundance in space
pix.aq <- pixels(list.output.bkg.aq$mesh, nx = 70, ny = 70, 
               mask = list.output.bkg.aq$bdy.km)

pred.bkg.pix <- predict(list.output.bkg.aq$fit.bkg, 
                        pix.aq, ~ exp(Smooth + Intercept))
bin.bkg.perc <- pred.bkg.pix$mean/sum(pred.bkg.pix$mean)

# add a bkg column to the data
aa <- data.frame(pix.aq@coords)
dist.x <- diff(unique(abs(aa[,1])))[1]/2
dist.y <- diff(unique(abs(aa[,2])))[1]/2

pix.abundance.df.aq.depth <- data.frame()

for(i in 1:nrow(aa)){
  cat('row number:', i, '\n')
  cat('pix perc:', i/nrow(aa), '\n')
  bbox <- matrix(c(aa[i,1] - dist.x, aa[i,1] + dist.x,
                   aa[i,2] - dist.y, aa[i,2] + dist.y), byrow = TRUE, ncol = 2)
  bdy.pix <- square_poly_from_bbox(bbox, crs.obj = list.output.bkg.ama$bdy.km@proj4string)
  # pred.n.pix <- predict(fit.etas.ama.depth, data.frame(), 
  #         ~   Abundance(th.mu = th.mu, 
  #                       th.K = th.K, 
  #                       th.alpha = th.alpha, 
  #                       th.c = th.c, 
  #                       th.p = th.p, 
  #                       th.sigma = th.sigma, 
  #                       th.beta.depth  = th.beta.depth,
  #                       catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
  #                       T1 = list.output.bkg.ama$T12[1],
  #                       T2 = list.output.bkg.ama$T12[2],
  #                       bdy = bdy.pix,
  #                       M0 = list.output.bkg.ama$M0,
  #                       link.fun = list.output.bkg.ama$link.functions,
  #                       model.type = 'depth'))
  # 
  mean.pix.ab <- Abundance(th.mu = fit.etas.aquila.depth$summary.fixed$mean[1], 
                           th.K = fit.etas.aquila.depth$summary.fixed$mean[2], 
                           th.alpha = fit.etas.aquila.depth$summary.fixed$mean[3], 
                           th.c = fit.etas.aquila.depth$summary.fixed$mean[4], 
                           th.p = fit.etas.aquila.depth$summary.fixed$mean[5], 
                           th.sigma = fit.etas.aquila.depth$summary.fixed$mean[6], 
                           th.beta.depth  = fit.etas.aquila.depth$summary.fixed$mean[7],
                           catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                           T1 = list.output.bkg.aq$T12[1],
                           T2 = list.output.bkg.aq$T12[2],
                           bdy = bdy.pix,
                           M0 = list.output.bkg.aq$M0,
                           link.fun = list.output.bkg.aq$link.functions,
                           model.type = 'depth')
  med.pix.ab <- Abundance(th.mu = fit.etas.aquila.depth$summary.fixed$`0.5quant`[1], 
                          th.K = fit.etas.aquila.depth$summary.fixed$`0.5quant`[2], 
                          th.alpha = fit.etas.aquila.depth$summary.fixed$`0.5quant`[3], 
                          th.c = fit.etas.aquila.depth$summary.fixed$`0.5quant`[4], 
                          th.p = fit.etas.aquila.depth$summary.fixed$`0.5quant`[5], 
                          th.sigma = fit.etas.aquila.depth$summary.fixed$`0.5quant`[6], 
                          th.beta.depth  = fit.etas.aquila.depth$summary.fixed$`0.5quant`[7],
                          catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                          T1 = list.output.bkg.aq$T12[1],
                          T2 = list.output.bkg.aq$T12[2],
                          bdy = bdy.pix,
                          M0 = list.output.bkg.aq$M0,
                          link.fun = list.output.bkg.aq$link.functions,
                          model.type = 'depth')
  lower.pix.ab <- Abundance(th.mu = fit.etas.aquila.depth$summary.fixed$`0.025quant`[1], 
                            th.K = fit.etas.aquila.depth$summary.fixed$`0.025quant`[2], 
                            th.alpha = fit.etas.aquila.depth$summary.fixed$`0.025quant`[3], 
                            th.c = fit.etas.aquila.depth$summary.fixed$`0.025quant`[4], 
                            th.p = fit.etas.aquila.depth$summary.fixed$`0.025quant`[5], 
                            th.sigma = fit.etas.aquila.depth$summary.fixed$`0.025quant`[6], 
                            th.beta.depth  = fit.etas.aquila.depth$summary.fixed$`0.025quant`[7],
                            catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                            T1 = list.output.bkg.aq$T12[1],
                            T2 = list.output.bkg.aq$T12[2],
                            bdy = bdy.pix,
                            M0 = list.output.bkg.aq$M0,
                            link.fun = list.output.bkg.aq$link.functions,
                            model.type = 'depth')
  
  upper.pix.ab <- Abundance(th.mu = fit.etas.aquila.depth$summary.fixed$`0.975quant`[1], 
                            th.K = fit.etas.aquila.depth$summary.fixed$`0.975quant`[2], 
                            th.alpha = fit.etas.aquila.depth$summary.fixed$`0.975quant`[3], 
                            th.c = fit.etas.aquila.depth$summary.fixed$`0.975quant`[4], 
                            th.p = fit.etas.aquila.depth$summary.fixed$`0.975quant`[5], 
                            th.sigma = fit.etas.aquila.depth$summary.fixed$`0.975quant`[6], 
                            th.beta.depth  = fit.etas.aquila.depth$summary.fixed$`0.975quant`[7],
                            catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                            T1 = list.output.bkg.aq$T12[1],
                            T2 = list.output.bkg.aq$T12[2],
                            bdy = bdy.pix,
                            M0 = list.output.bkg.aq$M0,
                            link.fun = list.output.bkg.aq$link.functions,
                            model.type = 'depth')
  # if(is.null(pred.n.pix$n.trig)){
  #   out <- pred.n.pix$n.bkg[,c('mean', 'q0.025', 'q0.5', 'q0.975')]*bin.bkg.perc[i] 
  # } else{
  #   out <- pred.n.pix$n.bkg[,c('mean', 'q0.025', 'q0.5', 'q0.975')]*bin.bkg.perc[i] + 
  #     pred.n.pix$n.trig[,c('mean', 'q0.025', 'q0.5', 'q0.975')]
  # }
  mean.pix.ab$n.bkg <- mean.pix.ab$n.bkg*bin.bkg.perc[i]
  mean.pix.ab$n.full <- mean.pix.ab$n.bkg + mean.pix.ab$n.trig
  mean.pix.ab$quantity <- 'mean'
  med.pix.ab$n.bkg <- med.pix.ab$n.bkg*bin.bkg.perc[i]
  med.pix.ab$n.full <- med.pix.ab$n.bkg + med.pix.ab$n.trig
  med.pix.ab$quantity <- 'median'
  lower.pix.ab$n.bkg <- lower.pix.ab$n.bkg*bin.bkg.perc[i]
  lower.pix.ab$n.full <- lower.pix.ab$n.bkg + lower.pix.ab$n.trig
  lower.pix.ab$quantity <- '0.025q'
  upper.pix.ab$n.bkg <- upper.pix.ab$n.bkg*bin.bkg.perc[i]
  upper.pix.ab$n.full <- upper.pix.ab$n.bkg + lower.pix.ab$n.trig
  upper.pix.ab$quantity <- '0.975q'
  out <- rbind(mean.pix.ab, med.pix.ab, lower.pix.ab, upper.pix.ab)
  cat('---', '\n')
  pix.abundance.df.aq.depth <- rbind(pix.abundance.df.aq.depth, out)
}

#pix.abundance.ama.depth <- mclapply(1:nrow(aa), function(i) 
# torun(i), mc.cores = 4)

pix.aq$mean.full <- pix.abundance.df.aq.depth$n.full[pix.abundance.df.aq.depth$quantity == 'mean']
pix.aq$logmean.full <- log(pix.aq$mean.full)
pix2$mean.trig <- pix.abundance.df.ama.depth$n.trig[pix.abundance.df.ama.depth$quantity == 'mean']
pix2$logmean.trig <- log(pix2$mean.trig)
pix2$mean.bkg <- pix.abundance.df.ama.depth$n.bkg[pix.abundance.df.ama.depth$quantity == 'mean']
pix2$logmean.bkg <- log(pix2$mean.bkg)

pix2$med.full <- pix.abundance.df.ama.depth$n.full[pix.abundance.df.ama.depth$quantity == 'median']
pix2$logmed.full <- log(pix2$med.full)
pix2$med.trig <- pix.abundance.df.ama.depth$n.trig[pix.abundance.df.ama.depth$quantity == 'median']
pix2$logmed.trig <- log10(pix2$mean.trig)
pix2$med.bkg <- pix.abundance.df.ama.depth$n.bkg[pix.abundance.df.ama.depth$quantity == 'median']
pix2$logmed.bkg <- log(pix2$med.bkg)


pix.aq$logmean.full <- log10(pix.aq$mean.full)
  

pl.ab.am <- ggplot() + gg(pix2['logmean.full']) + 
  #gg(list.output.bkg.ama$catalog.bru.km, size = 0.05) + 
  geom_star(data = as.data.frame(list.output.bkg.ama$catalog.bru.km[list.output.bkg.ama$catalog.bru.km$magnitudes >=5,]),
            mapping = aes(x,y),
            fill = 'red', size = 2) + 
  scale_fill_viridis()  + 
  xlab('Eastings') + 
  ylab('Northings') + 
  labs(fill = expression(paste('log', Lambda))) +
  theme_classic() +
  theme(legend.position = 'bottom') 


multiplot(pl.ab.aq, pl.ab.am, cols = 2)
points.per.pix <- c()
for(i in 1:nrow(aa)){
  cat('row number:', i, '\n')
  cat('pix perc:', i/nrow(aa), '\n')
  bbox <- matrix(c(aa[i,1] - dist.x, aa[i,1] + dist.x,
                   aa[i,2] - dist.y, aa[i,2] + dist.y), byrow = TRUE, ncol = 2)
  bdy.pix <- square_poly_from_bbox(bbox, crs.obj = list.output.bkg.ama$bdy.km@proj4string)
  pp.cropped <- crop(list.output.bkg.aq$catalog.bru.km, bdy.pix)
  points.per.pix <- c(points.per.pix, length(pp.cropped))
}

pix.aq$true <- points.per.pix
pix.aq$logtrue <- log10(pix.aq$true)

pl.aq.lims <- range(c(pix.aq$logmean.full, pix.aq$logtrue[is.finite(pix.aq$logtrue)]))

pl.ab.aq <- ggplot() + gg(pix.aq['logmean.full']) + 
  #gg(list.output.bkg.aq$catalog.bru.km, size = 0.05) + 
  geom_star(data = as.data.frame(list.output.bkg.aq$catalog.bru.km[list.output.bkg.aq$catalog.bru.km$magnitudes >=5,]),
            mapping = aes(x,y),
            fill = 'red', size = 2) + 
  scale_fill_viridis(limits = pl.aq.lims) +
  xlab('Eastings') + 
  ylab('Northings') + 
  labs(fill = expression(paste('log', Lambda))) +
  theme_classic() +
  theme(legend.position = 'bottom') 

pl.true.aq <- ggplot() + gg(pix.aq['logtrue']) + 
  #gg(list.output.bkg.aq$catalog.bru.km, size = 0.05) + 
  geom_star(data = as.data.frame(list.output.bkg.aq$catalog.bru.km[list.output.bkg.aq$catalog.bru.km$magnitudes >=5,]),
            mapping = aes(x,y),
            fill = 'red', size = 2) + 
  scale_fill_viridis(limits = pl.aq.lims) +
  xlab('Eastings') + 
  ylab('Northings') + 
  labs(fill = expression(paste('log', Lambda))) +
  theme_classic() +
  theme(legend.position = 'bottom') 

multiplot(pl.ab.aq, pl.true.aq, cols = 2)

#------------------------------------------
# branching ratio

Branch.Ratio <- function(th.mu, th.K = NULL, th.alpha = NULL, 
                         th.c = NULL, th.p = NULL, 
                         th.sigma = NULL, 
                         th.beta.depth = NULL, th.beta.strike = NULL,
                         catalogue, bdy, M0, link.fun, model.type = 'basic'){
  if(model.type == 'basic'){
    par.v <- c(link.fun$mu(th.mu[1]),
               link.fun$K(th.K[1]),
               link.fun$alpha(th.alpha[1]),
               link.fun$cc(th.c[1]),
               link.fun$pp(th.p[1]),
               link.fun$sigma(th.sigma[1]))
    eta.v <- par.v[3]*(catalogue$magnitudes - M0)
  } else if(model.type == 'depth'){
    par.v <- c(link.fun$mu(th.mu[1]),
               link.fun$K(th.K[1]),
               link.fun$alpha(th.alpha[1]),
               link.fun$cc(th.c[1]),
               link.fun$pp(th.p[1]),
               link.fun$sigma(th.sigma[1]),
               link.fun$beta.depth(th.beta.depth[1]))
    eta.v <- par.v[3]*(catalogue$magnitudes - M0) + par.v[7]*catalogue$depth
  } else if(model.type == 'strike'){
    par.v <- c(link.fun$mu(th.mu[1]),
               link.fun$K(th.K[1]),
               link.fun$alpha(th.alpha[1]),
               link.fun$cc(th.c[1]),
               link.fun$pp(th.p[1]),
               link.fun$sigma(th.sigma[1]),
               link.fun$beta.strike(th.beta.strike[1]))
    eta.v <- par.v[3]*(catalogue$magnitudes - M0) + par.v[7]*catalogue$mean.strike  
  } else if(model.type == 'full'){
    par.v <- c(link.fun$mu(th.mu[1]),
               link.fun$K(th.K[1]),
               link.fun$alpha(th.alpha[1]),
               link.fun$cc(th.c[1]),
               link.fun$pp(th.p[1]),
               link.fun$sigma(th.sigma[1]),
               link.fun$beta.strike(th.beta.strike[1]),
               link.fun$beta.depth(th.beta.depth[1]))
    eta.v <- par.v[3]*(catalogue$magnitudes - M0) + par.v[7]*catalogue$mean.strike + par.v[8]*catalogue$depth
  } else{
    stop('Unknown model type')
  }
  Sigma.m <- matrix(c(par.v[6], 0, 
                      0, par.v[6]), byrow = TRUE, ncol = 2)
  
  out<-exp(logLambda.h.vec(theta = par.v, 
                             eta = eta.v, 
                             th = 0, 
                             xh = mean(bdy@bbox[1,]), 
                             yh = mean(bdy@bbox[2,]), 
                             mh = catalogue$magnitudes, 
                             M0 = M0, 
                             T1 = 0, 
                             T2 = Inf, 
                             bdy = bdy, 
                             Sigma = Sigma.m) )
  return(mean(out))  
}


BR.aq.full <- predict(fit.etas.aquila.full, data.frame(), 
                      ~ Branch.Ratio(th.mu = th.mu, 
                                     th.K = th.K, 
                                     th.alpha = th.alpha, 
                                     th.c = th.c, 
                                     th.p = th.p, 
                                     th.sigma = th.sigma, 
                                     th.beta.depth = th.beta.depth, 
                                     th.beta.strike =  th.beta.strike,
                                     catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                     bdy = list.output.bkg.aq$bdy.km,
                                     M0 = list.output.bkg.aq$M0,
                                     link.fun = list.output.bkg.aq$link.functions,
                                     model.type = 'full'))


BR.ama.full <- predict(fit.etas.ama.full, data.frame(), 
                      ~ Branch.Ratio(th.mu = th.mu, 
                                     th.K = th.K, 
                                     th.alpha = th.alpha, 
                                     th.c = th.c, 
                                     th.p = th.p, 
                                     th.sigma = th.sigma, 
                                     th.beta.depth = th.beta.depth, 
                                     th.beta.strike =  th.beta.strike,
                                     catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                     bdy = list.output.bkg.ama$bdy.km,
                                     M0 = list.output.bkg.ama$M0,
                                     link.fun = list.output.bkg.ama$link.functions,
                                     model.type = 'full'))


BR.ama.basic <- predict(fit.etas.ama, data.frame(), 
                       ~ Branch.Ratio(th.mu = th.mu, 
                                      th.K = th.K, 
                                      th.alpha = th.alpha, 
                                      th.c = th.c, 
                                      th.p = th.p, 
                                      th.sigma = th.sigma, 
                                      #th.beta.depth = th.beta.depth, 
                                      #th.beta.strike =  th.beta.strike,
                                      catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                      bdy = list.output.bkg.ama$bdy.km,
                                      M0 = list.output.bkg.ama$M0,
                                      link.fun = list.output.bkg.ama$link.functions,
                                      model.type = 'basic'))

BR.ama.basic2 <- predict(fit.etas.ama, data.frame(), 
                        ~ Branch.Ratio(th.mu = th.mu, 
                                       th.K = th.K, 
                                       th.alpha = th.alpha, 
                                       th.c = th.c, 
                                       th.p = th.p, 
                                       th.sigma = th.sigma, 
                                       #th.beta.depth = th.beta.depth, 
                                       #th.beta.strike =  th.beta.strike,
                                       catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                       bdy = list.output.bkg.ama$bdy.km,
                                       M0 = list.output.bkg.ama$M0,
                                       link.fun = list.output.bkg.ama$link.functions,
                                       model.type = 'basic'))

rbind(BR.ama.basic,
      BR.ama.basic2)

## ----------------------------

Abundance.aq.full <- predict(fit.etas.aquila.full, data.frame(), 
                             ~ Abundance(th.mu = th.mu, 
                                         th.K = th.K, 
                                         th.alpha = th.alpha, 
                                         th.c = th.c, 
                                         th.p = th.p, 
                                         th.sigma = th.sigma, 
                                         th.beta.depth = th.beta.depth, 
                                         th.beta.strike =  th.beta.strike,
                                         catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                         T1 = list.output.bkg.aq$T12[1],
                                         T2 = list.output.bkg.aq$T12[2],
                                         bdy = list.output.bkg.aq$bdy.km,
                                         M0 = list.output.bkg.aq$M0,
                                         link.fun = list.output.bkg.aq$link.functions,
                                         model.type = 'full'))

######################################
## LOG LIKELIHOOD ##
################################

loglambda.single <- function(th.mu, th.K = NULL, th.alpha = NULL, th.c = NULL, th.p = NULL, th.sigma = NULL, 
                             th.beta.depth = NULL, th.beta.strike = NULL,
                             data.point, h.cat, M0, link.fun, model.type = 'basic'){
  h.cat <- h.cat[h.cat$ts < data.point$ts, ]
  if(model.type == 'basic'){
    par.v <- c(link.fun$mu(th.mu[1]),
               link.fun$K(th.K[1]),
               link.fun$alpha(th.alpha[1]),
               link.fun$cc(th.c[1]),
               link.fun$pp(th.p[1]),
               link.fun$sigma(th.sigma[1]))
    eta.v <- par.v[3]*(h.cat$magnitudes - M0)
  } else if(model.type == 'depth'){
    par.v <- c(link.fun$mu(th.mu[1]),
               link.fun$K(th.K[1]),
               link.fun$alpha(th.alpha[1]),
               link.fun$cc(th.c[1]),
               link.fun$pp(th.p[1]),
               link.fun$sigma(th.sigma[1]),
               link.fun$beta.depth(th.beta.depth[1]))
    eta.v <- par.v[3]*(h.cat$magnitudes - M0) + par.v[7]*h.cat$depth
  } else if(model.type == 'strike'){
    par.v <- c(link.fun$mu(th.mu[1]),
               link.fun$K(th.K[1]),
               link.fun$alpha(th.alpha[1]),
               link.fun$cc(th.c[1]),
               link.fun$pp(th.p[1]),
               link.fun$sigma(th.sigma[1]),
               link.fun$beta.strike(th.beta.strike[1]))
    eta.v <- par.v[3]*(h.cat$magnitudes - M0) + par.v[7]*h.cat$mean.strike  
  } else if(model.type == 'full'){
    par.v <- c(link.fun$mu(th.mu[1]),
               link.fun$K(th.K[1]),
               link.fun$alpha(th.alpha[1]),
               link.fun$cc(th.c[1]),
               link.fun$pp(th.p[1]),
               link.fun$sigma(th.sigma[1]),
               link.fun$beta.strike(th.beta.strike[1]),
               link.fun$beta.depth(th.beta.depth[1]))
    eta.v <- par.v[3]*(h.cat$magnitudes - M0) + par.v[7]*h.cat$mean.strike + par.v[8]*h.cat$depth
  } else{
    stop('Unknown model type')
  }
  Sigma.m <- matrix(c(par.v[6], 0, 
                      0, par.v[6]), byrow = TRUE, ncol = 2)
  
  log(par.v[1] + 
        sum(g.x(theta = par.v, 
                eta = eta.v, 
                tt = data.point$ts, 
                xx = data.point$x, 
                yy = data.point$y, 
                th = h.cat$ts, 
                xh = h.cat$x, 
                yh = h.cat$y, 
                mh = h.cat$magnitudes, 
                M0 = M0, Sigma = Sigma.m)))
}



ETAS.log.lik <- function(th.mu, th.K = NULL, th.alpha = NULL, th.c = NULL, th.p = NULL, th.sigma = NULL, 
                         th.beta.depth = NULL, th.beta.strike = NULL,
                         catalogue, T1, T2, bdy, M0, link.fun, model.type = 'basic'){
  Lambda <- Abundance(th.mu = th.mu, 
                      th.K = th.K, 
                      th.alpha = th.alpha, 
                      th.c = th.c, 
                      th.p = th.p, 
                      th.sigma = th.sigma, 
                      th.beta.depth = th.beta.depth, 
                      th.beta.strike = th.beta.strike,
                      catalogue = catalogue, 
                      T1 = T1, 
                      T2 = T2, 
                      bdy = bdy, 
                      M0 = M0, 
                      link.fun = link.fun, 
                      model.type = model.type)$n.full
  cat('Abundance = ', Lambda, '\n')
  loglambda <- vapply(1:nrow(catalogue), function(idx)
    loglambda.single(th.mu = th.mu, 
                     th.K = th.K, 
                     th.alpha = th.alpha, 
                     th.c = th.c, 
                     th.p = th.p, 
                     th.sigma = th.sigma, 
                     th.beta.depth = th.beta.depth, 
                     th.beta.strike = th.beta.strike,
                     data.point = catalogue[idx,], 
                     h.cat = catalogue, 
                     M0 = M0, 
                     link.fun = link.fun, 
                     model.type = model.type),0)
  cat('SL = ', sum(loglambda), '\n')
  return(-Lambda + sum(loglambda))
}

load('fit.etas.aquila.full.Rds')
load('fit.etas.ama.full.Rds')
loglik.aq.full <- predict(fit.etas.aquila.full, data.frame(), 
                          ~ ETAS.log.lik(th.mu = th.mu, 
                                         th.K = th.K, 
                                         th.alpha = th.alpha, 
                                         th.c = th.c, 
                                         th.p = th.p, 
                                         th.sigma = th.sigma, 
                                         th.beta.depth = th.beta.depth, 
                                         th.beta.strike =  th.beta.strike,
                                         catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                         T1 = list.output.bkg.aq$T12[1],
                                         T2 = list.output.bkg.aq$T12[2],
                                         bdy = list.output.bkg.aq$bdy.km,
                                         M0 = list.output.bkg.aq$M0,
                                         link.fun = list.output.bkg.aq$link.functions,
                                         model.type = 'full'))

loglik.aq.depth <- predict(fit.etas.aquila.depth, data.frame(), 
                          ~ ETAS.log.lik(th.mu = th.mu, 
                                         th.K = th.K, 
                                         th.alpha = th.alpha, 
                                         th.c = th.c, 
                                         th.p = th.p, 
                                         th.sigma = th.sigma, 
                                         th.beta.depth = th.beta, 
                                         catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                         T1 = list.output.bkg.aq$T12[1],
                                         T2 = list.output.bkg.aq$T12[2],
                                         bdy = list.output.bkg.aq$bdy.km,
                                         M0 = list.output.bkg.aq$M0,
                                         link.fun = list.output.bkg.aq$link.functions,
                                         model.type = 'depth'))

loglik.aq.strike <- predict(fit.etas.aquila.strike, data.frame(), 
                          ~ ETAS.log.lik(th.mu = th.mu, 
                                         th.K = th.K, 
                                         th.alpha = th.alpha, 
                                         th.c = th.c, 
                                         th.p = th.p, 
                                         th.sigma = th.sigma, 
                                         th.beta.strike =  th.beta,
                                         catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                         T1 = list.output.bkg.aq$T12[1],
                                         T2 = list.output.bkg.aq$T12[2],
                                         bdy = list.output.bkg.aq$bdy.km,
                                         M0 = list.output.bkg.aq$M0,
                                         link.fun = list.output.bkg.aq$link.functions,
                                         model.type = 'strike'))

loglik.aq.basic <- predict(fit.etas.aquila, data.frame(), 
                          ~ ETAS.log.lik(th.mu = th.mu, 
                                         th.K = th.K, 
                                         th.alpha = th.alpha, 
                                         th.c = th.c, 
                                         th.p = th.p, 
                                         th.sigma = th.sigma, 
                                         catalogue = as.data.frame(list.output.bkg.aq$catalog.bru.km),
                                         T1 = list.output.bkg.aq$T12[1],
                                         T2 = list.output.bkg.aq$T12[2],
                                         bdy = list.output.bkg.aq$bdy.km,
                                         M0 = list.output.bkg.aq$M0,
                                         link.fun = list.output.bkg.aq$link.functions,
                                         model.type = 'basic'))



AIC.aq <- rbind(full = -2*loglik.aq.full + 2*8,
      depth = -2*loglik.aq.depth + 2*7,
      strike = -2*loglik.aq.strike + 2*7,
      basic = -2*loglik.aq.basic + 2*6)

ggplot(aa, aes(x = q0.025, xend = q0.975, y = rownames(aa), yend = rownames(aa))) +
  geom_segment(size = 1) + 
  geom_point(data = aa, aes(x = q0.5, y = rownames(aa), color = 'median'),
             size = 2) + 
  geom_point(data = aa, aes(x = mean, y = rownames(aa), color = 'mean'),
             size = 2)


loglik.ama.full <- predict(fit.etas.ama.full, data.frame(), 
                          ~ ETAS.log.lik(th.mu = th.mu, 
                                         th.K = th.K, 
                                         th.alpha = th.alpha, 
                                         th.c = th.c, 
                                         th.p = th.p, 
                                         th.sigma = th.sigma, 
                                         th.beta.depth = th.beta.depth, 
                                         th.beta.strike =  th.beta.strike,
                                         catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                         T1 = list.output.bkg.ama$T12[1],
                                         T2 = list.output.bkg.ama$T12[2],
                                         bdy = list.output.bkg.ama$bdy.km,
                                         M0 = list.output.bkg.ama$M0,
                                         link.fun = list.output.bkg.ama$link.functions,
                                         model.type = 'full'))

loglik.ama.depth <- predict(fit.etas.ama.depth, data.frame(), 
                           ~ ETAS.log.lik(th.mu = th.mu, 
                                          th.K = th.K, 
                                          th.alpha = th.alpha, 
                                          th.c = th.c, 
                                          th.p = th.p, 
                                          th.sigma = th.sigma, 
                                          th.beta.depth = th.beta.depth, 
                                          catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                          T1 = list.output.bkg.ama$T12[1],
                                          T2 = list.output.bkg.ama$T12[2],
                                          bdy = list.output.bkg.ama$bdy.km,
                                          M0 = list.output.bkg.ama$M0,
                                          link.fun = list.output.bkg.ama$link.functions,
                                          model.type = 'depth'))

loglik.ama.strike <- predict(fit.etas.ama.strike, data.frame(), 
                            ~ ETAS.log.lik(th.mu = th.mu, 
                                           th.K = th.K, 
                                           th.alpha = th.alpha, 
                                           th.c = th.c, 
                                           th.p = th.p, 
                                           th.sigma = th.sigma, 
                                           th.beta.strike =  th.beta.strike,
                                           catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                           T1 = list.output.bkg.ama$T12[1],
                                           T2 = list.output.bkg.ama$T12[2],
                                           bdy = list.output.bkg.ama$bdy.km,
                                           M0 = list.output.bkg.ama$M0,
                                           link.fun = list.output.bkg.ama$link.functions,
                                           model.type = 'strike'))

loglik.ama.basic <- predict(fit.etas.ama, data.frame(), 
                           ~ ETAS.log.lik(th.mu = th.mu, 
                                          th.K = th.K, 
                                          th.alpha = th.alpha, 
                                          th.c = th.c, 
                                          th.p = th.p, 
                                          th.sigma = th.sigma, 
                                          catalogue = as.data.frame(list.output.bkg.ama$catalog.bru.km),
                                          T1 = list.output.bkg.ama$T12[1],
                                          T2 = list.output.bkg.ama$T12[2],
                                          bdy = list.output.bkg.ama$bdy.km,
                                          M0 = list.output.bkg.ama$M0,
                                          link.fun = list.output.bkg.ama$link.functions,
                                          model.type = 'basic'))

loglik.aq.full$sd*2
loglik.aq.depth$sd*2
loglik.aq.strike$sd*2
loglik.aq.basic$sd*2

AIC.ama$q0.975 - AIC.aq$q0.025

AIC.ama <- rbind(full = -2*loglik.ama.full + 2*8,
                 depth = -2*loglik.ama.depth + 2*7,
                 strike = -2*loglik.ama.strike + 2*7,
                 basic = -2*loglik.ama.basic + 2*6)

ggplot(bb, aes(x = q0.025, xend = q0.975, y = rownames(bb), yend = rownames(bb))) +
  geom_segment(size = 1) + 
  geom_point(data = bb, aes(x = median, y = rownames(bb), color = 'median'),
             size = 2) + 
  geom_point(data = bb, aes(x = mean, y = rownames(bb), color = 'mean'),
             size = 2)




## ----------------------------

sample.post.full.aq <- foreach(ss = 1:100, .combine = rbind) %do% {
  print(ss)
  sample.p <- generate(fit.etas.aquila.full, data.frame(), 
                       ~ c(list.output.bkg.aq$link.functions$mu(th.mu),
                           list.output.bkg.aq$link.functions$K(th.K),
                           list.output.bkg.aq$link.functions$alpha(th.alpha),
                           list.output.bkg.aq$link.functions$cc(th.c),
                           list.output.bkg.aq$link.functions$pp(th.p) + 1,
                           list.output.bkg.aq$link.functions$sigma(th.sigma),
                           list.output.bkg.aq$link.functions$beta.depth(th.beta.depth),
                           list.output.bkg.aq$link.functions$beta.strike(th.beta.strike)),
                       n.samples = 100)
  data.frame(mu = sample.p[1,],
             K = sample.p[2,],
             alpha = sample.p[3,],
             c = sample.p[4,],
             p = sample.p[5,],
             sigma2 = sample.p[6,],
             beta.depth = sample.p[7,],
             beta.strike = sample.p[8,])
}
save(sample.post.full.aq, file = 'sample.post.full.aq.Rds')  


sample.post.full.ama <- foreach(ss = 1:100, .combine = rbind) %do% {
  print(ss)
  sample.p <- generate(fit.etas.ama.full, data.frame(), 
                       ~ c(list.output.bkg.aq$link.functions$mu(th.mu),
                           list.output.bkg.aq$link.functions$K(th.K),
                           list.output.bkg.aq$link.functions$alpha(th.alpha),
                           list.output.bkg.aq$link.functions$cc(th.c),
                           list.output.bkg.aq$link.functions$pp(th.p) + 1,
                           list.output.bkg.aq$link.functions$sigma(th.sigma),
                           list.output.bkg.aq$link.functions$beta.depth(th.beta.depth),
                           list.output.bkg.aq$link.functions$beta.strike(th.beta.strike)),
                       n.samples = 100)
  data.frame(mu = sample.p[1,],
             K = sample.p[2,],
             alpha = sample.p[3,],
             c = sample.p[4,],
             p = sample.p[5,],
             sigma2 = sample.p[6,],
             beta.depth = sample.p[7,],
             beta.strike = sample.p[8,])
}
save(sample.post.full.ama, file = 'sample.post.full.ama.Rds')


library(GGally)

colnames(sample.post.full.aq) <- c('mu', 'K', 'alpha', 'c', 'p', 'beta[depth]', 'beta[ms]')
load('sample.post.full.aq.Rds')
sample.post.full.aq2 <- sample.post.full.aq
colnames(sample.post.full.aq2) <- c('mu', 'K', 'alpha', 'c', 'p', 'sigma^2', 'beta[depth]', 'beta[ms]')
colnames(sample.post.full.ama) <- c('mu', 'K', 'alpha', 'c', 'p', 'sigma^2', 'beta[depth]', 'beta[ms]')



pdf('figure9.aq.pairplot.pdf', width = 480/60, height = 480/120)
ggpairs(sample.post.full.aq2[,c('K', 'alpha', 'beta[depth]', 'beta[ms]')], 
        labeller = label_parsed, lower=list(continuous='density'))+ 
  theme_bw()
dev.off()

pdf('figure10.ama.pairplot.pdf', width = 480/60, height = 480/120)
ggpairs(sample.post.full.ama[,c('K', 'alpha', 'beta[depth]', 'beta[ms]')], 
        labeller = label_parsed, lower=list(continuous='density'))+ 
  theme_bw()
dev.off()


#### TRIGGERING FUNCTIONS


tt.seq <- seq(0.05, 1,length.out = 100)


plot.trig.depth <- function(depth, strike, list.input, sample.post, tt.seq, mag = 6){
  df.plot.post.fix.depth <- foreach(idx = 1:nrow(sample.post), .combine = rbind) %do% {
    par.vec <- as.numeric(sample.post[idx,])
    eta.pred <- par.vec[3]*(mag - list.input$M0) + par.vec[7]*depth + par.vec[8]*strike
    g.t.toplot(theta = par.vec,
               eta = eta.pred,
               tt = tt.seq,
               th = 0,
               M0 = list.input$M0)
  }
  
  trig.q0.025 <- apply(df.plot.post.fix.depth, 2, \(x) quantile(x, 0.025))
  trig.q0.5 <- apply(df.plot.post.fix.depth, 2, \(x) quantile(x, 0.5))
  trig.q0.975 <- apply(df.plot.post.fix.depth, 2, \(x) quantile(x, 0.975))
  
  plot.trig.depth <- ggplot()
  for(i in sample(1:nrow(df.plot.post.fix.depth), 500, replace = FALSE)){
    df.p <- data.frame(x = tt.seq,
                       y = df.plot.post.fix.depth[i,])
    plot.trig.depth <- plot.trig.depth + 
      geom_line(data = df.p,
                mapping = aes(x, y),
                color = 'grey', alpha = 0.5)
  }  
  plot.trig.depth + 
    geom_line(aes(x = tt.seq, y = trig.q0.025)) +
    geom_line(aes(x = tt.seq, y = trig.q0.975)) +
    geom_line(aes(x = tt.seq, y = trig.q0.5), color = 'red')
}


## AQUILA
pl.depth5.aq <- plot.trig.depth(depth = 5, 
                                strike = mean(list.output.bkg.aq$catalog.bru.km$mean.strike), 
                                list.input = list.output.bkg.aq, 
                                sample.post = sample.post.full.aq, 
                                tt.seq = tt.seq) + 
  ylim(0,30) + 
  annotate('text', x = 0.75, y = 30*(17/20), label = 'Aquila \n depth = 5 km') + 
  theme_bw() + 
  xlab('days') + 
  ylab(expression(paste('g(t,', eta, ')')))

pl.depth10.aq <- plot.trig.depth(depth = 10, 
                                 strike = mean(list.output.bkg.aq$catalog.bru.km$mean.strike),
                                 list.input = list.output.bkg.aq, 
                                 sample.post = sample.post.full.aq, 
                                 tt.seq = tt.seq) + 
  ylim(0,30) + 
  annotate('text', x = 0.75, y = 30*(17/20), label = 'Aquila \n depth = 10 km') + 
  theme_bw() + 
  xlab('days') + 
  ylab(expression(paste('g(t,', eta, ')')))


pl.depth15.aq <- plot.trig.depth(depth = 15, 
                                 strike = mean(list.output.bkg.aq$catalog.bru.km$mean.strike),
                                 list.input = list.output.bkg.aq, 
                                 sample.post = sample.post.full.aq, 
                                 tt.seq = tt.seq) + 
  ylim(0,30) + 
  annotate('text', x = 0.75, y = 30*(17/20), label = 'Aquila \n depth = 15 km') + 
  theme_bw() + 
  xlab('days') + 
  ylab(expression(paste('g(t,', eta, ')')))

## AMATRICE
load('sample.post.full.ama.Rds')

pl.depth5.ama <- plot.trig.depth(depth = 5, 
                                 strike = mean(list.output.bkg.ama$catalog.bru.km$mean.strike),
                                 list.input = list.output.bkg.ama, 
                                 sample.post = sample.post.full.ama, 
                                 tt.seq = tt.seq) + 
  ylim(0,10) + 
  annotate('text', x = 0.75, y = 10*(17/20), label = 'Amatrice \n depth = 5 km') + 
  theme_bw() + 
  xlab('days') + 
  ylab(expression(paste('g(t,', eta, ')')))

pl.depth10.ama <- plot.trig.depth(depth = 10, 
                                  strike = mean(list.output.bkg.ama$catalog.bru.km$mean.strike),
                              list.input = list.output.bkg.ama, 
                              sample.post = sample.post.full.ama, 
                              tt.seq = tt.seq)+ 
  ylim(0,10) + 
  annotate('text', x = 0.75, y = 10*(17/20), label = 'Amatrice \n depth = 10 km') + 
  theme_bw()+ 
  xlab('days') + 
  ylab(expression(paste('g(t,', eta, ')')))

pl.depth15.ama <- plot.trig.depth(depth = 15, 
                                  strike = mean(list.output.bkg.ama$catalog.bru.km$mean.strike),
                                  list.input = list.output.bkg.ama, 
                                  sample.post = sample.post.full.ama, 
                                  tt.seq = tt.seq)+ 
  ylim(0,10) + 
  annotate('text', x = 0.75, y = 10*(17/20), label = 'Amatrice \n depth = 15 km') + 
  theme_bw()+ 
  xlab('days') + 
  ylab(expression(paste('g(t,', eta, ')')))



pdf('comp.trig.depth.variability.pdf', width = 10, height = 6)
multiplot(pl.depth5.aq, pl.depth10.aq, pl.depth15.aq, 
          pl.depth5.ama, pl.depth10.ama, pl.depth15.ama, 
          layout = matrix(1:6, ncol = 3, byrow = TRUE))
dev.off()

# median comparison
par.med.aq <- apply(sample.post.full.aq, 2, median)
df.plot.trig.aq <- foreach(depth = seq(1,20,by = 2), .combine = rbind) %do% {
  eta.pred <- par.med.aq[3]*(mm - list.output.bkg.aq$M0) + par.med.aq[7]*depth +
    par.med.aq[8]*mean(list.output.bkg.aq$catalog.bru.km$mean.strike)
  data.frame(time.diff = tt.seq,
             value = g.t.toplot(theta = par.med.aq,
                                eta = eta.pred,
                                tt = tt.seq,
                                th = 0,
                                M0 = list.output.bkg.aq$M0),
             depth = depth,
             sequence = 'Aquila'
  )
}

par.med.ama <- apply(sample.post.full.ama, 2, median)
df.plot.trig.ama <- foreach(depth = seq(1,20,by = 2), .combine = rbind) %do% {
  eta.pred <- par.med.ama[3]*(mm - list.output.bkg.ama$M0) + par.med.ama[7]*depth+
    par.med.ama[8]*mean(list.output.bkg.ama$catalog.bru.km$mean.strike)
  data.frame(time.diff = tt.seq,
             value = g.t.toplot(theta = par.med.ama,
                                eta = eta.pred,
                                tt = tt.seq,
                                th = 0,
                                M0 = list.output.bkg.ama$M0),
             depth = depth,
             sequence = 'Amatrice'
  )
}

df.trig.merge <- rbind(df.plot.trig.ama,
                  df.plot.trig.aq)
df.trig.merge$sequence <- factor(df.trig.merge$sequence, levels = c('Aquila', 'Amatrice'))
pdf('comp.trig.depth.median.pdf', width = 10, height = 3)
ggplot(df.trig.merge, aes(x = time.diff, y = value, color = factor(depth))) + 
  geom_line() + 
  facet_wrap(facets = vars(sequence)) + 
  scale_color_viridis(discrete = TRUE) + 
  theme_bw() + 
  theme(legend.position = 'bottom')+
  labs(color = 'depth', x = 'days', y = expression(paste('g(t,', eta, ')')))
dev.off()


## influence of depth 


infl.depth.aq <- foreach(dd = 5:15, .combine = c) %do% {
  den. <- par.med.aq[3]*(6 - list.output.bkg.aq$M0) + abs(par.med.aq[7]*dd) + 
    abs(par.med.aq[8]*mean(list.output.bkg.aq$catalog.bru.km$mean.strike))
  abs(par.med.aq[7]*dd)/den.
}

infl.depth.ama <- foreach(dd = 5:15, .combine = c) %do% {
  den. <- par.med.ama[3]*(6 - list.output.bkg.ama$M0) + abs(par.med.ama[7]*dd) + 
    abs(par.med.ama[8]*mean(list.output.bkg.ama$catalog.bru.km$mean.strike))
  abs(par.med.ama[7]*dd)/den.
}

df.infl.depth <- rbind(data.frame(depth = 5:15,
                                  gamma = infl.depth.aq,
                                  sequence = 'Aquila'),
                       data.frame(depth = 5:15,
                                  gamma = infl.depth.ama,
                                  sequence = 'Amatrice'))
df.infl.depth$sequence <- factor(df.infl.depth$sequence, 
                                 levels = c('Aquila', 'Amatrice'))
pdf('depth.influence.pdf', width = 6, height = 3)
ggplot(df.infl.depth, aes(depth,gamma,
                          color = sequence,
                          linetype = sequence)) + 
  geom_line() + 
  ylab(expression(gamma[d])) +
  theme_bw() + 
  ylim(0,1)
dev.off()



##### STRIKE EFFECT
range(list.output.bkg.aq$catalog.bru.km$mean.strike)
range(list.output.bkg.ama$catalog.bru.km$mean.strike)

plot.trig.strike <- function(depth, strike, list.input, sample.post, tt.seq, mag = 6){
  df.plot.post.fix.strike <- foreach(idx = 1:nrow(sample.post), .combine = rbind) %do% {
    par.vec <- as.numeric(sample.post[idx,])
    eta.pred <- par.vec[3]*(mag - list.input$M0) + par.vec[7]*depth + par.vec[8]*strike
    g.t.toplot(theta = par.vec,
               eta = eta.pred,
               tt = tt.seq,
               th = 0,
               M0 = list.input$M0)
  }
  
  trig.q0.025 <- apply(df.plot.post.fix.strike, 2, \(x) quantile(x, 0.025))
  trig.q0.5 <- apply(df.plot.post.fix.strike, 2, \(x) quantile(x, 0.5))
  trig.q0.975 <- apply(df.plot.post.fix.strike, 2, \(x) quantile(x, 0.975))
  
  pl.trig.strike <- ggplot()
  for(i in sample(1:nrow(df.plot.post.fix.strike), 500, replace = FALSE)){
    df.p <- data.frame(x = tt.seq,
                       y = df.plot.post.fix.strike[i,])
    pl.trig.strike <- pl.trig.strike + 
      geom_line(data = df.p,
                mapping = aes(x, y),
                color = 'grey', alpha = 0.5)
  }  
  pl.trig.strike + 
    geom_line(aes(x = tt.seq, y = trig.q0.025)) +
    geom_line(aes(x = tt.seq, y = trig.q0.975)) +
    geom_line(aes(x = tt.seq, y = trig.q0.5), color = 'red')
}



pl.strike120.aq <- plot.trig.strike(strike = 120, 
                                    depth = 10,
                                    list.input = list.output.bkg.aq, 
                                    sample.post = sample.post.full.aq, 
                                    tt.seq = tt.seq) + 
  ylim(0,30) + 
  annotate('text', x = 0.75, y = 30*(17/20), label = 'Aquila \n strike = 120') + 
  theme_bw() + 
  xlab('days') + 
  ylab(expression(paste('g(t,', eta, ')')))

pl.strike140.aq <- plot.trig.strike(strike = 140, 
                                    depth = 10,
                                    list.input = list.output.bkg.aq, 
                                    sample.post = sample.post.full.aq, 
                                    tt.seq = tt.seq) + 
  ylim(0,40) + 
  annotate('text', x = 0.75, y = 40*(17/20), label = 'Aquila \n strike = 140') + 
  theme_bw() + 
  xlab('days') + 
  ylab(expression(paste('g(t,', eta, ')')))

pl.strike300.aq <- plot.trig.strike(strike = 300, 
                                    depth = 10,
                                    list.input = list.output.bkg.aq, 
                                    sample.post = sample.post.full.aq, 
                                    tt.seq = tt.seq) + 
  ylim(0,40) + 
  annotate('text', x = 0.75, y = 40*(17/20), label = 'Aquila \n strike = 300') + 
  theme_bw() + 
  xlab('days') + 
  ylab(expression(paste('g(t,', eta, ')')))


pl.strike120.ama <- plot.trig.strike(strike = 120,
                                     depth= 10,
                                     list.input = list.output.bkg.ama, 
                                     sample.post = sample.post.full.ama, 
                                     tt.seq = tt.seq) + 
  ylim(0,5) + 
  annotate('text', x = 0.75, y = 5*(17/20), label = 'Amatrice \n strike = 120') + 
  theme_bw() + 
  xlab('days') + 
  ylab(expression(paste('g(t,', eta, ')')))

pl.strike140.ama <- plot.trig.strike(strike = 140,
                                     depth= 10,
                                     list.input = list.output.bkg.ama, 
                                     sample.post = sample.post.full.ama, 
                                     tt.seq = tt.seq) + 
  ylim(0,5) + 
  annotate('text', x = 0.75, y = 5*(17/20), label = 'Amatrice \n strike = 140') + 
  theme_bw() + 
  xlab('days') + 
  ylab(expression(paste('g(t,', eta, ')')))


pl.strike300.ama <- plot.trig.strike(strike = 300, 
                                     depth= 10,
                                     list.input = list.output.bkg.ama, 
                                     sample.post = sample.post.full.ama, 
                                     tt.seq = tt.seq)+ 
  ylim(0,5) + 
  annotate('text', x = 0.75, y = 5*(17/20), label = 'Amatrice \n strike = 300') + 
  theme_bw()+ 
  xlab('days') + 
  ylab(expression(paste('g(t,', eta, ')')))

pdf('comp.trig.strike.variability.pdf', width = 10, height = 6)
multiplot(pl.strike120.aq, pl.strike140.aq, pl.strike300.aq, 
          pl.strike120.ama, pl.strike140.ama, pl.strike300.ama,
          layout = matrix(1:6, byrow = TRUE, ncol = 3))
dev.off()
## median comparison

par.med.aq <- apply(sample.post.full.aq, 2, median)
df.plot.strike.aq <- foreach(strike = seq(100,300,by = 50), .combine = rbind) %do% {
  eta.pred <- par.med.aq[3]*(mm - list.output.bkg.aq$M0) + par.med.aq[8]*strike + par.med.aq[7]*10
  data.frame(time.diff = tt.seq,
             value = g.t.toplot(theta = par.med.aq,
                                eta = eta.pred,
                                tt = tt.seq,
                                th = 0,
                                M0 = list.output.bkg.aq$M0),
             strike = strike,
             sequence = 'Aquila'
  )
}

par.med.ama <- apply(sample.post.full.ama, 2, median)
df.plot.strike.ama <- foreach(strike = seq(100,300,by = 50), .combine = rbind) %do% {
  eta.pred <- par.med.ama[3]*(mm - list.output.bkg.ama$M0) + par.med.ama[7]*10 +par.med.ama[8]*strike
  data.frame(time.diff = tt.seq,
             value = g.t.toplot(theta = par.med.ama,
                                eta = eta.pred,
                                tt = tt.seq,
                                th = 0,
                                M0 = list.output.bkg.ama$M0),
             strike = strike,
             sequence = 'Amatrice'
  )
}

df.strike.merge <- rbind(df.plot.strike.ama,
                         df.plot.strike.aq)
df.strike.merge$sequence <- factor(df.strike.merge$sequence, levels = c('Aquila', 'Amatrice'))
pdf('comp.trig.strike.median.pdf', width = 10, height = 3)
ggplot(df.strike.merge, aes(x = time.diff, y = value, color = factor(strike))) + 
  geom_line() + 
  facet_wrap(facets = vars(sequence)) + 
  scale_color_viridis(discrete = TRUE) + 
  theme_bw() + 
  theme(legend.position = 'bottom')+
  labs(color = 'mean strike') + 
  ylab(expression(paste('g(t,', eta, ')')))+
  xlab('days')
dev.off()

# influence

infl.strike.aq <- foreach(strike = 100:300, .combine = c) %do% {
  den. <- par.med.aq[3]*(6 - list.output.bkg.aq$M0) + abs(par.med.aq[7]*10) + 
    abs(par.med.aq[8]*strike)
  abs(par.med.aq[8]*strike)/den.
}

infl.strike.ama <- foreach(strike = 100:300, .combine = c) %do% {
  den. <- par.med.ama[3]*(6 - list.output.bkg.ama$M0) + abs(par.med.ama[7]*10) + 
    abs(par.med.ama[8]*strike)
  abs(par.med.ama[8]*strike)/den.
}

df.infl.strike <- rbind(data.frame(strike = 100:300,
                                  gamma = infl.strike.aq,
                                  sequence = 'Aquila'),
                       data.frame(strike = 100:300,
                                  gamma = infl.strike.ama,
                                  sequence = 'Amatrice'))
df.infl.strike$sequence <- factor(df.infl.strike$sequence, 
                                 levels = c('Aquila', 'Amatrice'))
pdf('strike.influence.pdf', width = 6, height = 3)
ggplot(df.infl.strike, aes(strike,gamma,
                          color = sequence,
                          linetype = sequence)) + 
  geom_line() +
  xlab('mean strike') + 
  ylab(expression(gamma[ms])) +
  theme_bw() + 
  ylim(0,1)
dev.off()


# depth distribution

horus <- read.table('data/HORUS_Ita_Catalog.txt', header = TRUE, sep = '\t')


pdf('figure.depth.distro.pdf', width = 8, height = 3)
ggplot() + #, linetype = sequence)) + 
  geom_histogram(aes(x = horus$Depth), binwidth = 0.1) + 
  theme_bw() + 
  xlim(0,100) +
  theme(legend.position = 'bottom')
dev.off()








