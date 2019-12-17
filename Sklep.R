

#Instalujemy/wlaczamy wymagane pakiety
#install.packages("GA")
library(GA)
#Pobieramy od u¿ytkownika informacjê ile pieniêdzy ma do wydania podczas zakupów
budzet <- as.integer(readline(prompt = "Ile masz pieniêdzy na zakupy? "))

#Tworzymy bazê danych z³o¿on¹ z trzech kolumn oraz dwudziestu jeden wierszy
asortymentSklepowy = data.frame(
	produkt = c("Mleko", "Jajka", "M¹ka", "Cukier", "Ser w plastrach", "Frytki", "Lody", "Ziemniaki", "Chipsy" , "Woda", "Kawa", "Œmietana", "Mas³o", "Wêdliny", "Sok", "Cola", "Chleb", "Herbata", "Twaróg", "Czekolada", "Jogurt"),
	cena = c(2.50, 7, 5.50, 6.80, 3, 10, 6.50, 8, 4, 1, 15.50, 2.40, 6.50, 3.30, 4.60, 4.20, 1.30, 12.80, 5.40, 4.50, 3.40),
	wagaProduktu = c(1.5, 0.7, 1, 1, 0.3, 1.2, 1, 5, 0.4, 1.5, 0.7, 0.5, 0.2, 0.3, 2, 2, 0.4, 0.4, 0.5, 0.2, 0.5)
)
#Deklarujemy jakie najwiêksze obci¹¿enie mo¿e mieæ koszyk
udzwigKoszyka = 10


#Definiujemy funkcje przystosowania
fitnessFunc = function(chr) {
	cenaProduktuChr = chr %*% asortymentSklepowy$cena
	wagaCalkowitaChr = chr %*% asortymentSklepowy$wagaProduktu
	if (wagaCalkowitaChr > udzwigKoszyka) return(-cenaProduktuChr) 
	else if (wagaCalkowitaChr <= udzwigKoszyka){
	  if (cenaProduktuChr > budzet) return(-cenaProduktuChr)
	  else return(cenaProduktuChr)
	} 
	return(calkowityZyskChr)
}

#Uruchamiamy algorytm genetyczny dla zadanych parametrow
wyniki=ga(type="binary",nBits=21,fitness=fitnessFunc,popSize=100,
           pcrossover=0.85,pmutation=0.05,elitism=5,maxiter=30,seed=10)

#Podsumawanie dzialania algorytmu genetycznego		   
summary(wyniki)

#Prezentacja pojedynczego rozwiazania
decode=function(chr){
	print("Podsumowanie: ")
	print( asortymentSklepowy[chr == 1, ] )
	print( paste("Kupi³eœ tyle towaru =",chr %*% asortymentSklepowy$wagaProduktu) )
	print( paste("Za =",chr %*% asortymentSklepowy$cena ) )
}
decode(wyniki@solution[1,])

