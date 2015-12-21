--Xabier Lecumberri Mezo
--Andoni Bermo Pérez

--HASKELL-EKO PRAKTIKA: KONPUTATZEAREN KONPLEXUTASUNA


--Aurrebaldintzak:
  --True formula n zero dituen zerrenda baten bidez adieraziko da.
  --Zeroz betetako zerrenda hori sartzean, lehen sartutako klausulak ez dute baliorik izango, eta True izango da zuzenean formula (eta ezin izango dira klausula gehiago sartu).
  --Klausula 0, 1 edo -1 zenbakiez osatutako zerrenda baten bidez adieraziko da.
  --Balorazioa 0 edo 1 digituez osatutako zenbaki baten bidez adieraziko da.
  --Aldagai kopurua adierazteko, 1 edo handiagoa den zenbaki bat erabiliko da.

--Kostuak:
   --Egiazkotasun-proba: Kostua lineala da O(n), n aldagai kopurua izanda.
   --Betegarritasun-proba: Kostua esponentziala da O(2^n), n aldagai kopurua izanda. Kasu honetan, formula True egiten duen balorazio bat aurkitzerakoan amaituko da.
   --Baliozkotasun-proba: Kostua esponentziala da O(2^n), n aldagai kopurua izanda. Kasu honetan, formula False egiten duen balorazio bat aurkitzerakoan amaituko da.
  -- 2^n-ren zergaitia, 2^n balorazio posible daudelako da.


import Data.List

praktika :: IO() --Metodo nagusia da (Main metodoa)
praktika = do
	n <- alK
	f <- (fK n)
	menu n f



menu :: Integer -> [[Integer]] -> IO()  --CNF formula pantailaratu eta menua abiarazi
menu n f = do
	putStr"Sartutako CNF formula:"
	fAd f
	aukeratu n f



alK :: IO Integer  --Aldagai kopurua zuzena izatea egiaztatu
alK = do
	putStrLn"Adierazi n-ren balioa: (>0)"
	n <- lZb
	if n>=1
		then return n
		else do
			putStrLn"Balioa ez da zuzena"
			alK



aukeratu :: Integer -> [[Integer]] -> IO ()  --Menuko aukeraketa egin
aukeratu n f = do
	putStrLn""
	putStr("Hautatu aukera bat:"++"\n"++"\n"++"1. Egiazkotasun-proba"++"\n"++"2. Betegarritasun-proba"++"\n"++"3. Baliozkotasun-proba"++"\n"++"4. Bukatu")
	a <- lZb
	if a==1
		then do
			egP n f
			aukeratu n f
		else if a ==2
			then do
				beP n f
				aukeratu n f
			else if a==3
				then do
					baP n f
					aukeratu n f
				else if a==4
					then putStrLn"Programa bukatu da"
					else do
						putStrLn"Aukera okerra"
						aukeratu n f



egP :: Integer -> [[Integer]] -> IO()  --(Egiazkotasun-probaren metodo nagusia) Balorazioa zuzena dela egiaztatu eta emaitza pantailaratu egiten du
egP n f = do
	putStr"Sartu zure balorazioa zenbaki oso moduan (True = 1, False = 0). Adibidez, 1010 n=4 kasurako. (Soilik azkeneko n digituak hartuko dira kontuan) "
	c <- getLine
	let a = (read c :: Integer)
	if egiaztatu (reverse (zB n a)) n
		then if egPK f (reverse (zB n a))
			then putStrLn"Balorazioak True egiten du formula"
			else putStrLn"Balorazioak False egiten du formula"
	else do
		putStrLn"Balorazioa ez da zuzena"
		egP n f



egiaztatu :: [Integer] -> Integer -> Bool  --Balorazioa zuzena den erantzun
egiaztatu x i = do
		if null x
			then if i == 0
				then True
				else False
			else if i < 0 
				then False
				else if (head x) == 1 || (head x) == (0)
					then egiaztatu (tail x) (i-1)
					else False



egPK :: [[Integer]] -> [Integer] -> Bool  --Formula bat eta balorazio bat jasota, hauen emaitza itzultzen du
egPK x b = do
		if null x || lH (head x)
			then True
			else if egPKMur (head x) b
				then egPK (tail x) b
				else False



egPKMur :: [Integer] -> [Integer] -> Bool  --Emaitza murgilketaren bidez lortzen da
egPKMur x y = do
		if null x
			then False
			else if (head x) == 0
				then egPKMur (tail x) (tail y)
				else if (head x) == 1
					then if (head y) == 0
						then egPKMur (tail x) (tail y)
						else True
					else if (head y) == 0
						then True
						else egPKMur (tail x) (tail y)



beP :: Integer -> [[Integer]] -> IO()  --Betegarritasun-probaren metodo nagusia
beP n f = bePMur f (guztiak n)



bePMur :: [[Integer]] -> [[Integer]] -> IO()  --Formula betegarria den erantzuten du. Betegarria bada, adierazpena pantailaratzen du
bePMur f x = do
		if null x
			then putStrLn"Ez da betegarria"
			else if lH (head f)
				then putStrLn"Betegarria da (True delako)"
				else if egPK f (head x)
					then do
						putStr"("
						adi (head x)
					else bePMur f (tail x)



adi :: [Integer] -> IO()  --Balorazioa pantailaratzen du, True eta False boolearren adierazpen baten moduan
adi x = do
		if null x
			then putStr"   )"
			else if (head x) == 0
				then do
					putStr"   False"
					adi (tail x)
				else do
					putStr"   True"
					adi (tail x)



baP :: Integer -> [[Integer]] -> IO()  --Baliozkotasun-probaren metodo nagusia
baP n f = baPMur f (guztiak n)



baPMur :: [[Integer]] -> [[Integer]] ->IO()  --Formula baliozkoa den erantzuten du
baPMur f x = do
		if null x || lH (head f)
			then putStrLn"Baliozkoa da"
			else if egPK f (head x)
				then baPMur f (tail x)
				else putStrLn"Ez da baliozkoa"



aldatu :: Integer -> Integer -> Integer -> [Integer]  --x zenbakia n digitu erabiliz b oinarrian ipintzen duen metodoa
aldatu x b n = [ (aldatu_lag x b n i) | i <- [1..n] ]



aldatu_lag :: Integer -> Integer -> Integer -> Integer -> Integer  --x zenbakia b oinarrian adierazita, i-garren posizioan duen digitua itzultzen du
aldatu_lag x b n i = (mod (div x (b^(n-i))) b)



guztiak :: Integer -> [[Integer]]  --b zenbaki oso bat emanda, 2 oinarrian eta b digitu erabiliz errepresenta daitezkeen zenbaki guztiak itzultzen ditu
guztiak b = [ (aldatu i 2 b) | i <-[0..((2^b)-1)] ]



lZb :: IO Integer --Zenbaki bat teklatutik jaso
lZb = do
		z <- getLine
		return (read z :: Integer)



fK :: Integer -> IO [[Integer]]  --Formula lortzeko pausoak kudeatzen ditu
fK n = do
		putStrLn"Adierazi CNF formula:"
		ema <- (lZbZZ (n))
		return ema



lZbZZ :: Integer -> IO [[Integer]] --Zerrenda bat teklatutik jasotzeko deia egin
lZbZZ n = lZbZZMur n []



lZbZZMur :: Integer -> [[Integer]] -> IO [[Integer]]  --Formularen klausulak banaka lortu
lZbZZMur n e = do
		putStrLn"Adierazi formularen klausula zerrenda baten bidez ( 0 = Ez dago literala ; 1 = Ukatu gabe ; -1 = Ukatua )"
		f <- getLine
		let g = (read f :: [Integer])
		if konprobatu g n
			then if lH g
				then return [g]
				else do
					putStrLn"Formula bat gehitu nahi duzu?"
					era <- bEE
					if era
						then lZbZZMur n (g:e)
						else return (g:e)
			else do
				putStrLn"Azken formula ez da zuzena"
				lZbZZMur n e



bEE :: IO Bool  --Bai edo Ez-en aukeraketa
bEE = do
		putStr" B(Bai)-E(Ez)"
		b <- getLine
		if b == "B"
			then return True
			else if b == "E"
				then return False
				else do
					putStrLn""
					bEE



konprobatu :: [Integer] -> Integer -> Bool  --Klausula bat zuzena den erantzuten du
konprobatu x i = do
		if null x
			then if i == 0
				then True
				else False
			else if i < 0
				then False
				else if ((head x) == 0 || (head x) == 1 || (head x) == (-1))
					then konprobatu (tail x) (i-1)
					else False



fAd :: [[Integer]] -> IO()  --Formula adierazteko kudeaketa burutzen du
fAd x = do
		if null x
			then error "Ez dago formularik"
			else if lH (head x)
				then putStr"  True"
				else do
					putStr"("
					fAdMur1 (head x) 1
					if null (tail x)
						then putStr""
						else do
							putStr" ^ "
							fAdMur2 (tail x)



fAdMur1 :: [Integer] -> Integer -> IO()  --Klausula bateko termino bat aurkitu arte, zerrendako 0 zenbakiak igarotzen ditu
fAdMur1 x i = do
		if null x
			then putStr""
			else if (head x) == 0
				then fAdMur1 (tail x) (i+1)
				else fAdMur3 x i



fAdMur3 :: [Integer] -> Integer -> IO()  --Klausula bat pantailaratzen du
fAdMur3 x i = do
		if null x
			then putStr""
			else do
				if (head x) == 0
					then putStr""
					else if (head x) == 1
						then do
							putStr(" x"++show i)
						else if (head x) == (-1)
							then do
								putStr(" !x"++show i) -- ¬
							else putStr""
				if null (tail x)
					then putStr" )"
					else if (head (tail x)) == 0
						then putStr""
						else putStr" v"
				fAdMur3 (tail x) (i+1)



fAdMur2 :: [[Integer]] -> IO()  --Klausulen arteko aldaketa kudeatzen du pantailaratze orduan
fAdMur2 x = do
		if null x
			then putStr""
			else do
				putStr"("
				if null (head x)
					then putStr""
					else fAdMur1 (head x) 1
				if null (tail x)
					then putStr""
					else putStr" ^ "
				fAdMur2 (tail x)



zB :: Integer -> Integer -> [Integer]  --Zenbaki baten azken n digituak dituen zerrenda bat eratzen du
zB n a = do
		if n == 0
			then []
			else (a `mod` 10) : (zB (n-1) (a `div` 10))



lH :: [Integer] -> Bool  --Klausula hutsa den erantzuten du
lH x = do
		if null x
			then True
			else if (head x) == 0
				then lH (tail x)
				else False


