## helper function, via Wolfgang Viechtbauer—should be in metafor package soon
rmat <- function(x, n, upper=TRUE, simplify=TRUE, rtoz=FALSE, data) {
  
  if (class(x) == "formula") {
    
    options(na.action = "na.pass")
    
    if (missing(data))
      stop("Must specify 'data' argument when 'x' is a formula.")
    
    if (!is.data.frame(data))
      data <- data.frame(data)
    
    dat <- get_all_vars(x, data=data)
    
    if (ncol(dat) != 4)
      stop("Incorrect number of variables specified in formula.")
    
    id <- dat[,4]
    dat <- split(dat, id)
    
    res <- list()
    
    for (i in 1:length(dat)) {
      
      ri <- dat[[i]][[1]]
      var1 <- as.character(dat[[i]][[2]])
      var2 <- as.character(dat[[i]][[3]])
      
      vars <- sort(unique(c(var1, var2)))
      
      R <- matrix(NA, nrow=length(vars), ncol=length(vars))
      diag(R) <- 1
      rownames(R) <- colnames(R) <- vars
      
      for (j in 1:length(var1)) {
        R[var1[j],var2[j]] <- R[var2[j],var1[j]] <-  ri[j]
      }
      
      res[[i]] <- R
      
    }
    
    return(rmat(res, n=n, simplify=TRUE, rtoz=FALSE))
    
  }
  
  ### in case x is a list, need to loop through elements
  
  if (is.list(x)) {
    
    k <- length(x)
    
    if (length(x) != length(n))
      stop("Argument 'n' must be of the same length as there are elements in 'x'.")
    
    res <- list()
    
    for (i in 1:k) {
      res[[i]] <- rmat(x[[i]], n[i], upper=upper)
    }
    
    if (simplify) {
      ki <- sapply(res, function(x) ifelse(is.null(x$dat), 0, nrow(x$dat)))
      dat <- cbind(id=rep(1:k, times=ki), do.call(rbind, lapply(res, "[[", "dat")))
      V <- bldiag(lapply(res[ki > 0], "[[", "V"))
      rownames(V) <- colnames(V) <- unlist(lapply(res, function(x) rownames(x$V)))
      return(list(dat=dat, V=V))
    } else {
      return(res)
    }
    
  }
  
  ### check if x is square matrix
  
  if (!is.matrix(x))
    stop("Argument 'x' must be a matrix (or list thereof).")
  
  if (dim(x)[1] != dim(x)[2])
    stop("Argument 'x' must be a square matrix (or list thereof).")
  
  ### set default dimension names
  
  dimsx <- nrow(x)
  dnames <- paste0("x", 1:dimsx)
  
  ### in case x has dimension names, use those
  
  if (!is.null(rownames(x)))
    dnames <- rownames(x)
  if (!is.null(colnames(x)))
    dnames <- colnames(x)
  
  ### in case x is a 1x1 (or 0x0) matrix, return nothing
  
  if (dimsx <= 1L)
    return(list(dat=NULL, V=NULL))
  
  ### make x symmetric, depending on whether we use upper or lower part
  
  if (upper) {
    x[lower.tri(x)] <- t(x)[lower.tri(x)]
  } else {
    x[upper.tri(x)] <- t(x)[upper.tri(x)]
  }
  
  ### check if x is symmetric (can be skipped since x must now be symmetric)
  
  #if (!isSymmetric(x))
  #   stop("x must be a symmetric matrix.")
  
  ### stack upper/lower triangular part of x into a column vector (this is always done column-wise!)
  
  if (upper) {
    ri <- cbind(x[upper.tri(x)])
  } else {
    ri <- cbind(x[lower.tri(x)])
  }
  
  ### apply r-to-z transformation if requested
  
  if (rtoz)
    ri <- 1/2 * log((1 + ri)/(1 - ri))
  
  ### I and J are matrices with 1:dimsx for rows and columns, respectively
  
  I <- matrix(1:dimsx, nrow=dimsx, ncol=dimsx)
  J <- matrix(1:dimsx, nrow=dimsx, ncol=dimsx, byrow=TRUE)
  
  ### get upper/lower triangular elements of I and J
  
  if (upper) {
    I <- I[upper.tri(I)]
    J <- J[upper.tri(J)]
  } else {
    I <- I[lower.tri(I)]
    J <- J[lower.tri(J)]
  }
  
  ### dimensions in V (must be dimsx*(dimsx-1)/2)
  
  dimsV <- length(ri)
  
  ### set up V matrix
  
  V <- matrix(NA, nrow=dimsV, ncol=dimsV)
  
  for (ro in 1:dimsV) {
    for (co in 1:dimsV) {
      
      i <- I[ro]
      j <- J[ro]
      k <- I[co]
      l <- J[co]
      
      ### Olkin & Finn (1995), equation 5, page 157
      
      V[ro,co] <- 1/2 * x[i,j]*x[k,l] * (x[i,k]^2 + x[i,l]^2 + x[j,k]^2 + x[j,l]^2) +
        x[i,k]*x[j,l] + x[i,l]*x[j,k] -
        (x[i,j]*x[i,k]*x[i,l] + x[j,i]*x[j,k]*x[j,l] + x[k,i]*x[k,j]*x[k,l] + x[l,i]*x[l,j]*x[l,k])
      
      ### Steiger (1980), equation 2, page 245 (provides the same result - checked)
      
      #V[ro,co] <- 1/2 * ((x[i,k] - x[i,j]*x[j,k]) * (x[j,l] - x[j,k]*x[k,l]) +
      #                   (x[i,l] - x[i,k]*x[k,l]) * (x[j,k] - x[j,i]*x[i,k]) +
      #                   (x[i,k] - x[i,l]*x[l,k]) * (x[j,l] - x[j,i]*x[i,l]) +
      #                   (x[i,l] - x[i,j]*x[j,l]) * (x[j,k] - x[j,l]*x[l,k]))
      
      ### Steiger (1980), equation 11, page 247 for r-to-z transformed values
      
      if (rtoz)
        V[ro,co] <- V[ro,co] / ((1 - x[i,j]^2) * (1 - x[k,l]^2))
      
    }
  }
  
  ### divide V by (n-1) for raw correlations and by (n-3) for r-to-z transformed correlations
  
  if (rtoz) {
    V <- V/(n-3)
  } else {
    V <- V/(n-1)
  }
  
  ### create matrix with var1 and var2 names and sort rowwise
  
  dmat <- cbind(dnames[I], dnames[J])
  dmat <- t(apply(dmat, 1, sort))
  
  ### set row/column names for V
  
  var1var2 <- paste0(dmat[,1], ".", dmat[,2])
  rownames(V) <- colnames(V) <- var1var2
  
  return(list(dat=data.frame(yi=ri, var1=dmat[,1], var2=dmat[,2], var1var2=var1var2, stringsAsFactors=FALSE), V=V))
  
}
