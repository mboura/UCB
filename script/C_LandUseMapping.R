### Mapping LU for each FUA

for (i in levels(Boundaries$FUA)) {
  load(paste0('../RData/LU/LU_', i, '.RData'))
  
  pdf(paste0('../Maps/LU/LU_', i, '.pdf'))
  par(mai = c(0,.5,.5,0))
  plot(subset(Boundaries, FUA == i), border = 'grey80', main = i, cex.main = 2)
  plot(Residential.UA, border = NA, col = 'grey50', add = T)
  plot(NonResidential.UA, border = NA, col = 'black', add = T)
  plot(Road.UA, border = NA, col = 'orangered', add = T)
  plot(NonRoad.UA, border = NA, col = 'lightpink', add = T)
  plot(Agriculture.UA, border = NA, col = 'wheat', add = T)
  plot(Forest.UA, border = NA, col = 'forestgreen', add = T)
  plot(Wetlands.UA, border = NA, col = 'turquoise4', add = T)
  plot(Herbaceous.UA, border = NA, col = 'limegreen', add = T)
  plot(Green.UA, border = NA, col = 'yellowgreen', add = T)
  plot(Water.UA, border = NA, col = 'turquoise', add = T)
  scalebar(10000, type = 'line', lonlat = F, label = '10 km', cex = 2)
  # legend('topright', fill = c('grey50', 'black', 'orangered', 'lightpink', 'wheat',
  #                             'forestgreen', 'turquoise4', 'limegreen', 'yellowgreen', 'turquoise'),
  #        legend = c('Residential', 'Non Residential', 'Roads', 'Non Roads', 'Agricultural',
  #                   'Forest', 'Wetlands', 'Herbaceous', 'OtherUGI', 'Water'),
  #        title = 'Land Use', border = NA, bty = 'n')
  dev.off()
}