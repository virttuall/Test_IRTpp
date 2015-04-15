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
    outputIRTpp = matrix(unlist(outputIRTpp), ncol = 1, byrow = FALSE) #TODO
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
