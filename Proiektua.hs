praktika :: IO()
 praktika = do n<- alK
  f<-fK n
  menu n f

  
  
menu :: Integer -> [[Integer]] -> IO()
 menu n f = do putStr("Sartutako CNF formula:"
  fAd n f
  aukeratu n f
  
  
  
alK :: IO Integer
 alK = do putStrLn"Adierazi n-ren balioa:"
  n<-lZb
  if n>=1
   then return n
   else alK
  
  
  
aukeratu :: Integer -> [[Integer]] -> IO ()
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

	   
	  
egP :: Integer -> [[Integer]] -> IO()



beP :: Integer -> [[Integer]] -> IO()



baP :: Integer -> [[Integer]] -> IO()



lZb :: IO Integer
 lZb = do z <- getLine
  return (read z :: Integer)


fK :: Integer -> IO [[Integer]]
 fK n = do putStrLn"Adierazi CNF formula:"
 return lZbZZ n


lZbZZ :: Integer -> [[Integer]]
 lZbZZ n = do putStrLn"Formula bat gehitu nahi duzu?"
  if bEE
  	then do  f <- getLine
     read f :: [Integer]
     if konprobatu f n
 	  then (f: lZbZZ n)
 	  else do putStrLn"Azken formula ez da zuzena"
 		      lZbZZ n 
 	else []


bEE :: IO boolean
 bEE = do putStr" B(Bai)-E(Ez)"
 b <- getLine
 read b :: string
 if b == "b"
 	then return true
 	if b =="e"
 		then return false
 		else do putStrLn""
 		 bEE



konprobatu :: [Integer] -> Integer -> boolean

 konprobatu [] i = true
 konprobatu x:s i
  | i < 0 = false
  | x==0 || x==1 || x==(-1) = konprobatu s (i-1)
  | otherwise = false