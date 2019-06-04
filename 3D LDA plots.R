
library(plot3D)



scatter3D(b$LD1, b$LD2, b$LD3, colvar = as.integer(plda$class), pch=20, cex=2, theta = 15, phi = 20)



# Add small dots on basal plane and on the depth plane
scatter3D_fancy <- function(x, y, z,..., colvar = z)
{
  panelfirst <- function(pmat) {
    XY <- trans3D(x, y, z = rep(min(z), length(z)), pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 2, add = TRUE, colkey = FALSE)
    
    XY <- trans3D(x = rep(min(x), length(x)), y, z, pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = colvar, pch = ".", 
              cex = 2, add = TRUE, colkey = FALSE)
  }
  scatter3D(x, y, z, ..., colvar = colvar, panel.first=panelfirst,
            colkey = list(length = 0.5, width = 0.5, cex.clab = 0.75)) 
}



scatter3D_fancy(b$LD1, b$LD2, b$LD3, colvar = as.integer(plda$class), pch=20, cex=2, theta = 15, phi = 20)
