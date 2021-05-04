
mark_netplot_groups<-function(netplot_object,igraph_object,
                              mark.groups=list(),mark.shape= 1/2,
                              mark.border = rainbow(length(mark.groups), 
                                                    alpha = 1),
                              mark.col=rainbow(length(mark.groups),  alpha = 0.3),
                     mark.expand=0.01,
                     vertex.size =1/200 ){
  ### Get vp graph
  print(netplot_object)
  netenv<-netplot_object
  netenv$xlim <- range(netenv$.layout[,1], na.rm=TRUE)
  netenv$ylim <- range(netenv$.layout[,2], na.rm=TRUE)
  
  
  # Creating layout
  # Solution from this answer https://stackoverflow.com/a/48084527
  asp <- grDevices::dev.size()
  lo  <- grid::grid.layout(
    widths  = grid::unit(1, "null"),
    heights = grid::unit(asp[2]/asp[1], "null"),
    respect = TRUE # This forcces it to the aspect ratio
  )
  
  vp_graph <- grid::viewport(
    xscale         = netenv$xlim + .04*diff(netenv$xlim)*c(-1,1),
    yscale         = netenv$ylim + .04*diff(netenv$ylim)*c(-1,1),
    clip           = "off",
    layout.pos.col = 1,
    layout.pos.row = 1,
    name           = "graph-vp"
  )
  
  ### end vp graph
  
  ### start xspline fitting
  draw_poly<-function (points, vertex.size = 15/200, expand.by = 15/200, shape = 1/2, 
                       col = "#ff000033", border = NA,vp_graph=vp_graph) 
  {
    by <- expand.by
    pp <- rbind(points, cbind(points[, 1] - vertex.size - by, 
                              points[, 2]), cbind(points[, 1] + vertex.size + by, points[, 
                                                                                         2]), cbind(points[, 1], points[, 2] - vertex.size - by), 
                cbind(points[, 1], points[, 2] + vertex.size + by))
    cl <- convex_hull(pp)

    grid::grid.xspline(x=cl$rescoords[,1],y=cl$rescoords[,2],default.units = "native",
                       shape = 1/2,gp=gpar(fill=col,col=border),open=F,vp=vp_graph)
  }
  ### end xspline fitting
  
  ### loop through the communities and plot
  mark.shape <- rep(mark.shape, length = length(mark.groups))
  mark.border <- rep(mark.border, length = length(mark.groups))
  mark.col <- rep(mark.col, length = length(mark.groups))
  mark.expand <- rep(mark.expand, length = length(mark.groups))
  for (g in seq_along(mark.groups)) {
    v <- V(igraph_object)[mark.groups[[g]]]
    if (length(vertex.size) == 1) {
      vs <- vertex.size
    }
    else {
      vs <- rep(vertex.size, length = vcount(igraph_object))[v]
    }
    draw_poly(points = netplot_object$.layout[v, , drop = FALSE], vertex.size = vs, 
                   expand.by = mark.expand[g]/200, shape = mark.shape[g], 
                   col = mark.col[g], border = mark.border[g],vp_graph = vp_graph)
  }
  ### end of plotting
}
