# tabla, correlacion!
#
# This is an example function named 'kn_tb'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

kn_tb = function(datos){
  require("Hmisc")
  tmp = rcorr(as.matrix(datos))
  n = nrow(tmp$r)*2
  ds = c()
  vp = c()
  for (i in c(1:length(tmp$r))){
    if (tmp$r[i]== 1.000){
      ds = c(ds,NA,NA)
    }else{
      pv = sprintf("%.4f", round(tmp$P[i],4))
      if (pv < 0.05 ){
        vp = paste("(p<",toString(pv),")" ,sep = "")
      }else{
        vp = paste("(p>", toString(pv),")" ,sep = "")
      }
      vr = sprintf("%.4f", round(tmp$r[i],4))
      ds = c(ds,vr,vp )
    }
  }
  tb = as.array(ds)
  dim(tb) <- c(n, ncol(tmp$r))# (rows, cols)
  lb1 = colnames(tmp$P) # cols names 4
  lb2 = rep(NA,length(lb1)*2 )
  lb2[seq(from = 1, length.out = length(lb1), by = 2)] = lb1 #--- falta
  rownames(tb) <- lb2
  colnames(tb) <- lb1
  return(tb)
}
