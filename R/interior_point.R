# Interior Point Linear Programming Algorithm
# Adapted from 'intpoint' package by Alejandro Quintela del Rio (GPL-2)
# Reference: Vanderbei, Meketon and Freedman (1986)

#' Linear programming solver by the interior point method
#'
#' Solves a linear programming problem using a modification of Karmarkar's
#' algorithm developed by Vanderbei, Meketon and Freedman (1986).
#'
#' @param t Problem type: -1 for minimization, 1 for maximization.
#' @param c Objective function coefficient vector.
#' @param bA Vector of right-hand side values for equality constraints. Default NULL.
#' @param A Matrix of equality constraints. Default NULL.
#' @param bm Vector of right-hand side values for >= constraints. Default NULL.
#' @param m Matrix of >= constraints. Default NULL.
#' @param bM Vector of right-hand side values for <= constraints. Default NULL.
#' @param M Matrix of <= constraints. Default NULL.
#' @param e Tolerance for optimality. Default 1e-04.
#' @param a1 Step size parameter 1. Default 1.
#' @param a2 Step size parameter 2. Default 0.97.
#' @return A list containing optimal value, solution vector, and iteration count.
#' @noRd
interior_point <- function(t, c, bA = NULL, A = NULL, bm = NULL, m = NULL,
                           bM = NULL, M = NULL, e = 1e-04, a1 = 1, a2 = 0.97) {
  b <- ip_createb(bA, A, bm, m, bM, M)
  if (!is.null(A)) {
    if (is.vector(A)) {
      aux <- array(0, c(1, length(A)))
      for (i in seq_along(A)) aux[i] <- A[i]
      A <- aux
    }
    A <- ip_check_mm(m, A)
    A <- ip_check_M(M, A)
  } else if (!is.null(m)) {
    A <- ip_add(m, 1)
    A <- ip_check_M(M, A)
  } else {
    A <- ip_add(M, -1)
  }

  c <- ip_type(t, c)
  for (i in seq_along(b)) {
    if (b[i] < 0) {
      b[i] <- (-1) * b[i]
      A <- ip_signA(A, i)
    }
  }

  A <- ip_a_artificial(A, b)
  c <- ip_c_artificial(c, A)
  x <- ip_x_initial(c)
  dx <- ip_Dx(x)
  C <- ip_C_bar(c, A, dx)
  f <- 1
  n <- 1
  failure <- 0
  l <- length(x)

  while (ip_optimal(C, x, e, c) == 0) {
    if (ip_no_bounded(dx, C) == 1) {
      return("Unbounded problem")
    }
    if (f == 1) {
      p <- ip_P(dx, C)
      x2 <- x
      x <- ip_xn(x, C, p, a1)
      if (abs(x[l]) <= 1e-04) {
        x[l] <- 0
        A <- ip_q_a_artificial(A, l)
        c <- ip_q_c_artificial(c)
        f <- 2
      } else {
        x <- ip_xn(x2, C, p, a2)
        if (abs(x[l]) <= 1e-04) {
          x[l] <- 0
          A <- ip_q_a_artificial(A, l)
          c <- ip_q_c_artificial(c)
          f <- 2
        }
      }
      n <- n + 1
      dx <- ip_Dx(x)
      C <- ip_C_bar(c, A, dx)
    } else {
      p <- ip_P(dx, C)
      x <- ip_xn(x, C, p, a2)
      n <- n + 1
      dx <- ip_Dx(x)
      C <- ip_C_bar(c, A, dx)
    }
  }

  if (x[l] > 1e-08) {
    return("The problem has no feasible solutions")
  }

  if (!is.null(m) || !is.null(M)) {
    h <- ip_slacks(m, M) + 1
  } else {
    h <- 1
  }
  xf <- array(0, c(length(x) - h, 1))
  for (i in seq_along(xf)) xf[i] <- x[i]
  if (t == 1) {
    z <- (-1) * ip_Z(c, x)
  } else {
    z <- ip_Z(c, x)
  }
  list(
    `The optimum value of Z is` = z,
    `The optimal solution X` = xf,
    `Number of iterations` = n
  )
}

ip_createb <- function(bA, A, bm, m, bM, M) {
  if (!is.null(A)) {
    lb <- length(bA)
    b <- array(0, c(lb, 1))
    for (i in 1:lb) b[i] <- bA[i]
    if (!is.null(m)) {
      lbm <- length(bm)
      la <- lb + lbm
      ab <- array(0, c(la, 1))
      for (i in 1:lb) ab[i] <- bA[i]
      for (i in (lb + 1):la) ab[i] <- bm[i - lb]
      b <- ab
      if (!is.null(M)) {
        lbM <- length(bM)
        lc <- la + lbM
        aux <- array(0, c(lc, 1))
        for (i in 1:la) aux[i] <- b[i]
        for (i in (la + 1):lc) aux[i] <- bM[i - la]
        b <- aux
      }
    } else if (!is.null(M)) {
      lbM <- length(bM)
      lc <- lb + lbM
      aux <- array(0, c(lc, 1))
      for (i in 1:lb) aux[i] <- b[i]
      for (i in (lb + 1):lc) aux[i] <- bM[i - lb]
      b <- aux
    }
  } else if (!is.null(m)) {
    lb <- length(bm)
    b <- array(0, c(lb, 1))
    for (i in 1:lb) b[i] <- bm[i]
    if (!is.null(M)) {
      lbM <- length(bM)
      la <- lb + lbM
      aux <- array(0, c(la, 1))
      for (i in 1:lb) aux[i] <- b[i]
      for (i in (lb + 1):la) aux[i] <- bM[i - lb]
      b <- aux
    }
  } else if (!is.null(M)) {
    lb <- length(bM)
    b <- array(0, c(lb, 1))
    for (i in 1:lb) b[i] <- bM[i]
  }
  b
}

ip_check_mm <- function(m, A) {
  if (is.vector(m) || is.array(m)) {
    r1 <- nrow(A) + 1
    A <- rbind(A, m)
    r2 <- nrow(A)
    for (i in r1:r2) {
      aux <- array(0, c(r2, 1))
      aux[i] <- 1
      A <- cbind(A, aux)
    }
  }
  A
}

ip_check_M <- function(M, A) {
  if (is.array(M)) {
    r1 <- nrow(A) + 1
    ca <- ncol(A)
    cm <- ncol(M)
    col <- ca - cm
    rm <- nrow(M)
    if (col != 0) {
      aux <- array(0, c(rm, col))
      M <- cbind(M, aux)
    }
    A <- rbind(A, M)
    r2 <- nrow(A)
    for (i in r1:r2) {
      aux <- array(0, c(r2, 1))
      aux[i] <- -1
      A <- cbind(A, aux)
    }
  } else if (is.vector(M)) {
    r1 <- nrow(A) + 1
    ca <- ncol(A)
    cm <- length(M)
    col <- ca - cm
    aux1 <- array(0, c(1, length(M)))
    for (i in seq_along(M)) aux1[i] <- M[i]
    M <- aux1
    if (col != 0) {
      aux <- array(0, c(1, col))
      M <- cbind(M, aux)
    }
    A <- rbind(A, M)
    r2 <- nrow(A)
    for (i in r1:r2) {
      aux <- array(0, c(r2, 1))
      aux[i] <- -1
      A <- cbind(A, aux)
    }
  }
  A
}

ip_add <- function(A, s) {
  if (is.array(A)) {
    r1 <- nrow(A)
    for (i in 1:r1) {
      aux <- array(0, c(r1, 1))
      aux[i] <- s
      A <- cbind(A, aux)
    }
  } else if (is.vector(A)) {
    l <- length(A)
    aux <- array(0, c(1, l + 1))
    for (i in 1:l) aux[i] <- A[i]
    aux[l + 1] <- s
    A <- aux
  }
  A
}

ip_type <- function(t, c) {
  t * (-1) * c
}

ip_signA <- function(A, i) {
  for (j in 1:ncol(A)) A[i, j] <- (-1) * A[i, j]
  A
}

ip_a_artificial <- function(A, b) {
  rows <- nrow(A)
  cols <- ncol(A)
  a <- matrix(0, rows, cols + 1)
  for (i in 1:rows) for (j in 1:cols) a[i, j] <- A[i, j]
  for (i in 1:rows) {
    a[i, cols + 1] <- b[i]
    for (j in 1:cols) a[i, cols + 1] <- a[i, cols + 1] - A[i, j]
  }
  a
}

ip_c_artificial <- function(c, A) {
  col <- ncol(A)
  l <- length(c)
  cn <- array(0, c(col, 1))
  for (i in 1:l) cn[i] <- c[i]
  cn[col] <- 1e+06
  cn
}

ip_x_initial <- function(c) {
  l <- length(c)
  array(1, c(l, 1))
}

ip_Dx <- function(x) {
  l <- length(x)
  dx <- array(0, c(l, l))
  for (i in 1:l) dx[i, i] <- x[i]
  dx
}

ip_C_bar <- function(c, A, Dx) {
  At <- t(A)
  if (nrow(At) <= 40) {
    Dx2 <- Dx^2
    ec <- A %*% Dx2 %*% At
    aux <- svd(ec)
    Positive <- aux$d > max(1e-08 * aux$d[1L], 0)
    if (all(Positive)) {
      inv <- aux$v %*% (1 / aux$d * t(aux$u))
    } else if (!any(Positive)) {
      inv <- array(0, dim(ec)[2L:1L])
    } else {
      inv <- aux$v[, Positive, drop = FALSE] %*% ((1 / aux$d[Positive]) * t(aux$u[, Positive, drop = FALSE]))
    }
    C <- c - At %*% inv %*% A %*% Dx2 %*% c
    return(C)
  } else {
    Dx2 <- Dx^2
    B <- A %*% Dx2 %*% At
    y <- A %*% Dx2 %*% c
    w <- solve(B, y)
    C <- c - At %*% w
    return(C)
  }
}

ip_optimal <- function(C, x, e, c) {
  max_val <- 0
  for (i in seq_along(x)) if (x[i] * C[i] > max_val) max_val <- x[i] * C[i]
  min_val <- max_val
  for (i in seq_along(C)) if (C[i] < min_val) min_val <- C[i]
  l <- length(c)
  if (max_val - min_val * mean(x) < e / l) 1 else 0
}

ip_no_bounded <- function(Dx, C) {
  cp <- Dx %*% C
  zERO <- 1e-18
  zeros <- 0
  negs <- 0
  big <- 0
  for (i in seq_along(cp)) {
    if (abs(cp[i]) > 1e+150 || (abs(cp[i]) < 1e-150 && cp[i] != 0)) big <- 1
    if (abs(cp[i]) < zERO) zeros <- zeros + 1
    if (cp[i] < 0 && abs(cp[i]) >= zERO) negs <- negs + 1
  }
  if ((zeros != length(cp) && (zeros + negs) == length(cp)) || big == 1) 1 else 0
}

ip_P <- function(Dx, C) {
  d2 <- Dx^2
  -d2 %*% C
}

ip_xn <- function(x, C, p, a) {
  max_val <- 0
  for (i in seq_along(x)) if (abs(x[i] * C[i]) > max_val) max_val <- abs(x[i] * C[i])
  div <- a * p / max_val
  x + div
}

ip_q_a_artificial <- function(A, j) {
  rows <- nrow(A)
  for (i in 1:rows) A[i, j] <- 0
  A
}

ip_q_c_artificial <- function(c) {
  c[length(c)] <- 0
  c
}

ip_slacks <- function(m, M) {
  s <- 0
  n <- 0
  if (!is.null(m)) {
    if (is.vector(m)) {
      n <- 1
    } else if (is.array(m)) {
      for (i in seq_along(m)) s <- s + abs(m[i])
      if (s != 0) n <- nrow(m) else n <- 0
    }
  }
  s <- 0
  if (!is.null(M)) {
    if (is.vector(M)) {
      n <- n + 1
    } else if (is.array(M)) {
      for (i in seq_along(M)) s <- s + abs(M[i])
      if (s != 0) n <- n + nrow(M)
    }
  }
  n
}

ip_Z <- function(c, x) {
  sum(c * x)
}
