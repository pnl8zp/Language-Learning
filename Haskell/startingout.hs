doubleMe x = x * 2
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
						then x
						else x*2

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

rightTriangles = [ (a,b,c) | c <- [1..35], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c <= 100]  
