parametersMirt <- function( model, pathDataSet )
{
  library('mirt')
  data = expand.table(LSAT7)
  data = read.table(pathDataSet, sep=" ",header=T)
  if ( model == '1PL_AD')
  {
    fit = mirt(data = data,model = 1,itemtype = "2PL",constrain = list(seq(1,ncol(data)*4,by = 4)))
  }
  else
  {
    fit = mirt(data = data,model = 1,itemtype = model )
  }
  zita=coef(fit)
  zita=unlist(zita)
  n = length(zita)
  zita = zita[-((n-1):n)]
  print(zita)
  zita = matrix(zita,ncol=4,byrow = TRUE)
  zita[,2] = -zita[,2] / zita[,1]
  return (zita[,c(1,2,3)])
}

