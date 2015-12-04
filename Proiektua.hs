praktika :: IO()
 praktika = do n<- alK
  putStrLn"Adierazi CNF formula:"
  f<-lZbZ
  menu n f

  
  
menu :: Integer -> [Integer] -> IO()
 menu n f = do putStr("Sartutako CNF formula:"
  fAd n f
  aukeratu n f
  
  
  
alK :: IO Integer
 alK = do putStrLn"Adierazi n-ren balioa:"
  n<-lZb
  if n>=1
   then return n
   else alK
  
  
  
aukeratu :: IO ()
 aukeratu n f = do putStr("Hautatu aukera bat:"++"\n"++"\n"++"1. Egiazkotasun-proba"++"\n"++"2. Betegarritasun-proba"++"\n"++"3. Baliozkotasun-proba"++"\n"++"4. Bukatu")
  a<-lZb
  if a==1
   then egP n f
   else if a ==2
    then beP n f
	else if a==3
	 then baP n f
	 else if a==4
	  else do putStrLn"Aukera okerra"
	   aukeratu n f

	   
	  
egP :: Integer -> [Integer] -> IO()



beP :: Integer -> [Integer] -> IO()



baP :: Integer -> [Integer] -> IO()



lZb :: IO Integer
 lZb = do z <- getLine
  return (read z :: Integer)
