rm(list=ls())
source('ParametersIRTpp.R')
source('ParametersMirt.R')

namesInputForIRTpp = c('RASCH_A1', 'RASCH_A_CONSTANT', 'TWO_PL', 'THREE_PL' ) # name of model for input to IRTpp, for is change in package IRTpp
namesInputForMirt = c( 'Rasch', '1PL_AD', '2PL', '3PL')
dir = '/home/Yeison/SICS'
blocks = c('Bloque_1')
models = c('1PL_A_Dist')

namesDataSet = c('Test_10_1_1000.csv')

listBlocks <- list()

for ( block in blocks) # covers all blocks
{
  listModels <- list()
  for( model in models ) # covers all models
  {
    listDataSets <- list()
    for ( nameDataSet in namesDataSet ) # covers all datasets
    {
      l <- list()
      
      
      path = paste(dir, block, model, 'Datasets', '' , sep = "/" ) # to made the path of dataset
      path = paste(path, nameDataSet, sep = "" )
      print(path)
      nameModelForIRTpp = ""
      nameModelForMirt = ""
      if ( model == '1PL')
      {
        nameModelForIRTpp = namesInputForIRTpp[1]
        nameModelForMirt = namesInputForMirt[1]
        columns = 1
      }
      else if( model == '1PL_A_Dist')
      {
        nameModelForIRTpp = namesInputForIRTpp[2]
        nameModelForMirt = namesInputForMirt[2]
        columns = 2
      }
      else if( model == '2PL' )
      {
        nameModelForIRTpp = namesInputForIRTpp[3]
        nameModelForMirt = namesInputForMirt[3]
        columns = 2
      }
      else if( model == '3PL')
      {
        nameModelForIRTpp = namesInputForIRTpp[4]
        nameModelForMirt = namesInputForMirt[4]
        columns = 3
      }
      else
      {
        print("Error, model different of 1pl, 2pl,...")
      }
      
      resultMirt = parametersMirt(nameModelForMirt, path)
      resultIRTpp = parametersIRTpp(nameModelForIRTpp, path)
      print(resultIRTpp)
      print(resultMirt)
      
      
      #mapP <- c( 'a','b','c')
      #names(mapP) <- c(1, 2, 3)
      
      for ( index in 1:columns)
      {
        if (nameModelForMirt == 'Rasch' )
        {
          difference = abs(resultMirt[,(index+1)] - resultIRTpp[,index])
        }
        else
        {
          difference = abs(resultMirt[,index] - resultIRTpp[,index])
        }
        maxDif = max(difference)
        l <- append( l, maxDif)
      }
      listDataSets <- append( listDataSets, list(l))
    } 
    listModels <- append(listModels, list(listDataSets))
  }
  listBlocks <- append(listBlocks, list(listModels) )
}

print(str(listBlocks))
write.table(listBlocks, file = "result3pl.txt")

