ParametersIRTpp <-function( model , pathDataSet)
{
  library('IRtpp')
  data = read.table(pathDataSet, sep=" ",header=T)
  dataForIRTpp = as.matrix(data)
  outputIRTpp = irtpp( dataForIRTpp, model, 1, "ANDRADE" , 0.0001, 200, T)
  return (outputIRTpp)
}
