barycentric <- function(A, cats = 5, con=TRUE)
{
  #Coding of a continuous variable into categorical
  #via barycentric recoding
  
  #Example
  #A = c(40, 33, 32.5, 32, 55.2, 60.1, 32)
  #barycentric(A, 3, con = TRUE)
  
  #converts a continuous variable A into a fuzzy-coded
  #categorical with 3 categories
  
  #use con = TRUE for continuous
  #use con = FALSE for Likert-type ordinal
  
  if (con == TRUE) {
    minA = min(A)
    maxA = max(A)
    absdist = dist(A,"manhattan")
    dmin = min(apply(as.matrix(absdist), 1, FUN = function(x) {min(x[x > 0])}))
    all = round((maxA - minA) / dmin + 1)
    Acat={}
    for (i in 1:length(A)) {
      Acat[i] = trunc((A[i] - minA) / dmin) + 1
    }
    A = Acat
  }
  
  #Apply barycentric coding
  maxA = max(A)
  
  Ni = length(A)
  
  
  pLOW = 1/2
  k = 0
  inVals = cats
  
  R = matrix(0,Ni,inVals)
  p00 = rep(0,inVals+1)
  
  #for (j in 0:(Nj-1)) {
  pHIGH = maxA + 1/2
  p00[1] = pLOW
  for (cnt in 1:cats) {
    p00[cnt+1] = p00[cnt] + (pHIGH - pLOW) / inVals
  }
  
  for (i in 1:Ni) {
    eee = Gen_split_w_center(A[i],p00)
    for (cnt in 1:(inVals)) {
      R[i,cnt] = eee[cnt]
    }
  }
  #}
  return(R)
  
}

Low_level_split <- function(s, sx, a, b) {
  Low_level_split = sx * (b - s) / (b - a)
}


Gen_split_w_center <- function(s, p) {
  x= rep(0,length(p)-1)
  flag = 0
  for (u in 1:(length(p)-1)) {
    
    if (s == p[u]) {
      flag = 1
      Base = u
      base_a = Base - 1
    } else if ((s > p[u]) & (s < p[u+1])) {
      Base = u
      base_a = Base
    }
  }
  
  a = p[base_a]
  b = p[Base + 1]
  xa = Low_level_split(s, 1, a, b)
  xb = 1 - xa
  if (flag == 1) 
    a = p[Base]
  if ((Base +1) <= (length(p)-1)) {
    for (i in (Base + 1):(length(p)-1)) {
      a2 = (b + a) / 2
      b2 = p[i + 1]
      xa2 = Low_level_split(b, xb, a2, b2)
      xb2 = xb - xa2
      x[i-1] = xa2
      a = b
      b = b2
      xb = xb2
    }
  }
  
  x[length(p)-1] = xb
  a = p[base_a]
  b = p[Base + 1]
  xa = Low_level_split(s, 1, a, b)
  xb = 1 - xa
  
  if (flag == 1) 
    b = p[Base]
  
  if (base_a >= 2) {
    for (k in seq(base_a, 2, by=-1))
    {
      b2 = (a + b) / 2
      a2 = p[k-1]
      xa2 = Low_level_split(a, xa, a2, b2)
      xb2 = xa - xa2
      x[k] = x[k] + xb2
      b = a
      a = a2
      xa = xa2
    }
  }
  x[1] = x[1] + xa
  
  return(x)
  
}


