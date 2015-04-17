datos = read.table("file:///home/Yeison/SICS/Bloque_1/2PL/Datasets/Test_10_1_1000.csv",sep=" ",header=T)
x = as.matrix(datos);
irtpp(x, "TWO_PL", 1, "ANDRADE" , 0.0001, 200, T)
parametersMirt("2PL", "/home/Yeison/SICS/Bloque_1/2PL/Datasets/Test_10_1_1000.csv")
print(x)
x = list('1', '2', '3', '4', '5')
x = unlist(x)
print(x)
