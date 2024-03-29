

#Instalujemy/wlaczamy wymagane pakiety
#install.packages("GA")
library(GA)
#Pobieramy od u�ytkownika informacj� ile pieni�dzy ma do wydania podczas zakup�w
budzet <- as.integer(readline(prompt = "Ile masz pieni�dzy na zakupy? "))

#Tworzymy baz� danych z�o�on� z trzech kolumn oraz dwudziestu jeden wierszy
asortymentSklepowy = data.frame(
	produkt = c("Mleko", "Jajka", "M�ka", "Cukier", "Ser w plastrach", "Frytki", "Lody", "Ziemniaki", "Chipsy" , "Woda", "Kawa", "�mietana", "Mas�o", "W�dliny", "Sok", "Cola", "Chleb", "Herbata", "Twar�g", "Czekolada", "Jogurt"),
	cena = c(2.50, 7, 5.50, 6.80, 3, 10, 6.50, 8, 4, 1, 15.50, 2.40, 6.50, 3.30, 4.60, 4.20, 1.30, 12.80, 5.40, 4.50, 3.40),
	wagaProduktu = c(1.5, 0.7, 1, 1, 0.3, 1.2, 1, 5, 0.4, 1.5, 0.7, 0.5, 0.2, 0.3, 2, 2, 0.4, 0.4, 0.5, 0.2, 0.5)
)
#Deklarujemy jakie najwi�ksze obci��enie mo�e mie� koszyk
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
	print( paste("Kupi�e� tyle towaru =",chr %*% asortymentSklepowy$wagaProduktu) )
	print( paste("Za =",chr %*% asortymentSklepowy$cena ) )
}
decode(wyniki@solution[1,])

