# 'RASCH_A1', 'RASCH_A_CONSTAN', 'TWO_PL', 'THREE_PL' 
parametersIRTpp <-function( model , pathDataSet, div)
{
  library('IRTpp')
  data = read.table(pathDataSet, sep=" ",header=T)
  dataForIRTpp = as.matrix(data)
  outputIRTpp = irtpp( dataForIRTpp, model, 1, "ANDRADE" , 0.0001, 200, T)
  outputIRTpp = outputIRTpp[1]
  if ( model == 'RASCH_A1')
  {
    outputIRTpp = matrix(unlist(outputIRTpp), ncol = 1, byrow = FALSE)
  }
  else if( model == 'RASCH_A_CONSTAN')
  {
    v1 = outputIRTpp[1]
    v1 = unlist(v1)
    outputIRTpp = outputIRTpp[-1]
    v1 = rep(v1, times = length(outputIRTpp))
    v2 = unlist(outputIRTpp)
    v1 = c(v1, v2)
    outputIRTpp = matrix( v1, ncol = 2, byrow = FALSE) #TODO
  }
  else if( model == 'TWO_PL')
  {
    outputIRTpp = matrix(unlist(outputIRTpp), ncol = 2, byrow = FALSE)
  }
  else if( model == 'THREE_PL')
  {
    outputIRTpp = matrix(unlist(outputIRTpp), ncol = 3, byrow = FALSE)
  }
  else
  {
    print("Error in parametersIRTpp, model no found")
  }
  return (outputIRTpp)
}
