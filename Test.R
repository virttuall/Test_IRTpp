#install mirt and sics
library('IRTpp')
library('mirt')


namesInputForIRTpp = c('RASCH_A1', 'RASCH_A_CONSTAN', 'TWO_PL', 'THREE_PL' ) # name of model for input to IRTpp, for is change in package IRTpp
dir = '/home/Yeison/SICS'
blocks = c('Bloque_1')
models = c('1PL','1PL_A_Dist','2PL','3PL')
namesDataSet <- c('Test_10_1_1000.csv', 'Test_10_2_1000.csv')  #this vector to save the names of datasets


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
      
      
      {
        #for irtpp
        
        nameModelForIRTpp = ""
        if ( model == '1PL')
        {
          nameModelForIRTpp = namesInputForIRTpp[1]
        }
        else if( model == '1PL_A_Dist')
        {
          nameModelForIRTpp = namesInputForIRTpp[2]
        }
        else if( model == '2PL' )
        {
          nameModelForIRTpp = namesInputForIRTpp[3]
        }
        else if( model == '3PL')
        {
          nameModelForIRTpp = namesInputForIRTpp[4]
        }
        else
        {
          print("Error, model different of 1pl, 2pl,...")
        }
        outputIRTpp = irtpp( dataForIRTpp, nameModelForIRTpp, 1, "ANDRADE" , 0.0001, 200, T)
      }
        
      {
        #for mirt
      }
  
     
      #generate max difference
      
      #here the parameters of packages
      aMirt <- c( 1.09, 2, 3, 4, 5)
      bMirt <- c( 6, 7, 8, 9, 10)
      cMirt <- c( 11, 12, 13, 14, 15)
      aSics <- c( 21.03434, 22, 23, 34, 45)
      bSics <- c( 9, 2, 0, 1, -1)
      cSics <- c( 3, 2, 3, 3, -5)
      
      
      
      parametersMirt <- list( a = aMirt, b = bMirt, c = cMirt )
      parametersSics <- list( a = aSics, b = bSics, c = cSics )
      
      mapP <- c( 'a','b','c')
      names(mapP) <- c(1, 2, 3)

      for ( index in 1:3)
      {
        difference = abs(parametersMirt[[index]] - parametersSics[[index]])
        #TestDataStructure$block$model$nameDataSet$statistical$difference$mapP[index] = difference;
        maxDif = max(difference)
        l <- append( l, maxDif)
        #print(maxDif)
      }
      listDataSets <- append( listDataSets, list(l))
    } 
    listModels <- append(listModels, list(listDataSets))
  }
  listBlocks <- append(listBlocks, list(listModels) )
}

print(str(listBlocks))


