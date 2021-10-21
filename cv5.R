# balicek Qtools
# install.packages('gtools')
# library('gtools')

# Ukol 1 - Double Digest Problem
xa <- c(2, 3, 5, 10)
xb <- c(3, 7, 10)
xab <- c(1, 2, 2, 5, 5, 5)


DDP <- function(xa, xb, xab){
  # ulozeni delek
  nxa <- length(xa)
  nxb <- length(xb)
  nxab <- length(xab)
  
  #usporadani fragmentu
  perm_xa <- permutations(nxa, nxa, xa)
  perm_xb <- permutations(nxb, nxb, xb)
  
  for (i in 1:nrow(perm_xa)) {
    for (j in 1:nrow(perm_xb)) {
      mapa_xa <- c(0, cumsum(perm_xa[i,]))
      mapa_xb <- c(0, cumsum(perm_xb[j,]))
      sloucena_mapa <- sort(unique(c(mapa_xa,mapa_xb)))
      setrizene_diference <- sort(diff(sloucena_mapa))
      
      if (length(setrizene_diference==length(xab))){
        if (all(setrizene_diference == xab)){
          r_mista_A <- mapa_xa[2:(length(mapa_xa)-1)]
          r_mista_B <- mapa_xb[2:(length(mapa_xb)-1)]
          return(c(list(r_mista_A, r_mista_B), list(r_mista_B, r_mista_A)))
          break
        }
        else {
          mapa_xa <- c()
          mapa_xb <- c()
        }
      }
      else
      mapa_xa <- c()
      mapa_xb <- c()
    }
  }
}