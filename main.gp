printmessage(m) = print(concat([Str(x)|x<-m]));
[N,k,c] = readvec("input.txt");
\\k est la base publique
\\c est le chiffré




B=ceil(sqrt(#k * 2^(#k)));

taille_mat=#k;
M=matid(taille_mat);
M[taille_mat,taille_mat]=-c*B;
for(i=1, taille_mat-1, M[taille_mat,i]=B*k[i+1]; M[i,taille_mat]=-1/2);
\\print(M);
\\la matrice est créée
\\ ATTENTION, qflll ne donne pas la matrice qui contient la base réduite, mais elle renvoie la matrice A telle que M * A est une base réduite du réseau

\\le code ne fonctionnant pas pour la recherche de m=m1,m2,...,m140
\\nous allons faire une sorte de brute force sur m1, ou plutôt une disjonction de cas à cette échelle
\\ceci permettra de travailler sur une matrice plus petite

\\base_reduite= M * (qflll(M));
\\print(base_reduite);

ajustement=Vec(0,taille_mat);
for(i=1,taille_mat, ajustement[i]=1/2);
ajustement[taille_mat]=0;
\\print(ajustement);
\\utilisation de ajustement ok

verif_validite_colonne(vec) = {
	\\print(vec);
	
	s = Set(vec);
	( #s == 2 ) && setsearch(s,0) ;
	\\on vérifie qu'il n'y a que deux valeurs, dont 0
}

\\ fonction ok

verif_solution(vec) = { \\on vérifie que le candidat satisfait bien l'équation du problème sac à dos
	(vec*(k~))%N == c;
}
\\ fonction ok


\\Nous savons que c est le résultat d'une somme de produit de nombres positifs modulo N
\\La somme "hors modulo" est donc du type c+k*N
\\Il faudra donc tester les différentes valeurs possibles de N jusqu'à trouver la valeur qui convient

\\La boucle suivante traite deux cas distincts:
\\     - le cas où k1=0
\\     - le cas où k1=1
\\Dans chacun des cas, on met à jour le coefficient (taille_mat,taille_mat) de la matrice en fonction 
\\de la valeur de N
\\On applique ensuite LLL pour avoir une base réduite
\\On parcours ensuite cette base pour vérifier si elle contient un vecteur court qui serait solution de 
\\notre problème.
\\Pour cela, il faut d'abbord réinsérer la valeur k1 sur laquelle nous avons fait la distinction de cas
\\La dernière coordonnée du vecteur à tester est quant à elle retirée. Elle doit être nulle car c'est 
\\le résultat de la combinaison linaire qui donne : (somme des mi*ki) - (c+k*N)=0 par définition de c
\\Il est possible de ne pas retirer cette valeur, et dans ce cas le test devra être moddifé :
\\     - au lieu de tester l'égalité du produit scalaire avec c, il faudra tester
\\     - l'égalité à 0 du produit scalaire des deux vecteurs qui auront un élément de plus
\\     - que dans le cas que nous avons gardé


i=0;
while(1,												\
	M[taille_mat,taille_mat] = -B * (c + i * N );							\
	base_reduite=M*qflll(M);									\
	for(j = 1, taille_mat,										\
		candidat=(base_reduite[,j])~ + ajustement;						\
		test=concat([0],candidat[1..-2]);							\
		if(verif_validite_colonne(candidat) && verif_solution(test),				\
			print(fromdigits(test));							\
			return;									\
		);											\
	);												\
													\
	M[taille_mat,taille_mat]= - B * ( c - k[1] + i * N );					\
	base_reduite=M*qflll(M);									\
	for(j = 1, taille_mat,										\
		candidat=(base_reduite[,j])~ + ajustement;						\
		test=concat([1],candidat[1..-2]);							\
		if(verif_validite_colonne(candidat) && verif_solution(test),				\
			print(fromdigits(test));							\
			return;									\
		);											\
	);												\
	i++;												\
);
