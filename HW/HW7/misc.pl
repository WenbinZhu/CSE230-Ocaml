%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helpers

%isin(X,L) is true if X appears in L
isin(X,[X|_]).
isin(X,[_|T]) :- isin(X,T).

% zip(L1,L2,L3) is true if L3 is the result of interleaving L1 and L2
% e.g. zip([1,2],[3,4],[1,3,2,4]) is true
zip([],[],[]).
zip([H1|T1],[H2|T2],[H1,H2|T]) :- zip(T1,T2,T).

% assoc(L,K,V) is true if L is a list of 2-element lists and one of them is [K,V]
% e.g. assoc([[key1,value1],['a',1],[3,4]], 3, 4) is true
assoc([[X,Y]|_],X,Y).
assoc([_|T],X,Y) :- assoc(T,X,Y).

% remove_duplicates(L1,L2) is true if L2 is the result of removing all duplicate elements from L1.
% The remaining elements should be in the original order.
% e.g. remove_duplicates([1,1,2,2,3,3,4,4],[1,2,3,4]) is true
clean([],Soln,Y) :- reverse(Y,Soln).
clean([H|T],Soln,Y) :- isin(H,Y),!,clean(T,Soln,Y).
clean([H|T],Soln,Y) :- clean(T,Soln,[H|Y]).
remove_duplicates(L1,L2) :- clean(L1,L2,[]). 

% union(L1,L2,L3) is true if L3 is the set union of L1 and L2. 
% There should be no duplicates in L3.
% e.g. union([1,2,3],[2,3,4],[1,2,3,4]) is true
union(L1,L2,L3) :- append(L1,L2,L),remove_duplicates(L,L3). 

% intersection(L1,L2,L3) is true if L3 is the set intersection of L1 and L2.
% There should be no duplicates in L3.
% e.g. intersection([1,2,3],[2,3,4],[2,3]) is true
its([],_,X,Y) :- reverse(X,Y).
its([H|T],L,X,Y) :- isin(H,L),!,its(T,L,[H|X],Y).
its([_|T],L,X,Y) :- its(T,L,X,Y).
intersection(L1,L2,L3) :- its(L1,L2,[],L3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 1: Facts

cost(carne_asada,3).
cost(lengua,2).
cost(birria,2).
cost(carnitas,2).
cost(adobado,2).
cost(al_pastor,2).
cost(guacamole,1).
cost(rice,1).
cost(beans,1).
cost(salsa,1).
cost(cheese,1).
cost(sour_cream,1).
cost(taco,1).
cost(tortilla,1).
cost(sopa,1).


ingredients(carnitas_taco, [taco,carnitas, salsa, guacamole]).
ingredients(birria_taco, [taco,birria, salsa, guacamole]).
ingredients(al_pastor_taco, [taco,al_pastor, salsa, guacamole, cheese]).
ingredients(guacamole_taco, [taco,guacamole, salsa,sour_cream]).
ingredients(al_pastor_burrito, [tortilla,al_pastor, salsa]).
ingredients(carne_asada_burrito, [tortilla,carne_asada, guacamole, rice, beans]).
ingredients(adobado_burrito, [tortilla,adobado, guacamole, rice, beans]).
ingredients(carnitas_sopa, [sopa,carnitas, guacamole, salsa,sour_cream]).
ingredients(lengua_sopa, [sopa,lengua,beans,sour_cream]).
ingredients(combo_plate, [al_pastor, carne_asada,rice, tortilla, beans, salsa, guacamole, cheese]).
ingredients(adobado_plate, [adobado, guacamole, rice, tortilla, beans, cheese]).

taqueria(el_cuervo, [ana,juan,maria], 
        [carnitas_taco, combo_plate, al_pastor_taco, carne_asada_burrito]).

taqueria(la_posta, 
        [victor,maria,carla], [birria_taco, adobado_burrito, carnitas_sopa, combo_plate, adobado_plate]).

taqueria(robertos, [hector,carlos,miguel],
        [adobado_plate, guacamole_taco, al_pastor_burrito, carnitas_taco, carne_asada_burrito]).

taqueria(la_milpas_quatros, [jiminez, martin, antonio, miguel],  
        [lengua_sopa, adobado_plate, combo_plate]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 1: Rules

% available_at(X, Y) is true if item X is available at taqueria Y
available_at(X,Y) :- taqueria(Y, _, L), isin(X, L).

% find_multi_available(X) is true if X is available in more than one palce
find_multi_available(X) :- available_at(X, Y), available_at(X, Z), Y \= Z.
% multi_available(X) is true if X is available in more than one palce and removes duplicates
multi_available(X) :- bagof(T, find_multi_available(T), L), remove_duplicates(L, L1), isin(X, L1).

% works_at(X, Y) is true if person X works at taqueria Y 
works_at(X, Y) :- taqueria(Y, L, _), isin(X, L).
% find_overworked(X, Y) is true if person X works at more than one taqueria
find_overworked(X) :- works_at(X, Y), works_at(X, Z), Y \= Z.
% overworked(X) is true if person X works at more than one taqueria and removes duplicates
overworked(X) :- bagof(T, find_overworked(T), L), remove_duplicates(L, L1), isin(X, L1).

% sum(S, L) is true if S is the sum of all elements in L
sum(0, []).
sum(S, [H|T]) :- cost(H, C), sum(S1, T), S is S1 + C.
% total_cost(X, K) is true if the sum of the costs of the ingredients of item X is equal to K 
total_cost(X,K) :- ingredients(X, L), sum(K, L).

% has_ingredients(X, L) is true if the item X has all the ingredients listed in L
has_ingredients(X,L) :- remove_duplicates(L, L1), ingredients(X, L2), intersection(L1, L2, L1).

% avoids_ingredients(X, L) is true if the item X does not have any of the ingredients listed in L
avoids_ingredients(X,L) :- ingredients(X, L1), intersection(L, L1, []).

% p1(L, X) is true if L is the list of all items that contain all the ingredientsc in X
p1(L,X) :- bagof(Z, has_ingredients(Z, X), L).
% p2(L, Y) is true if L is the list of all items that do not contain any of the ingredients in Y
p2(L,Y) :- bagof(Z, avoids_ingredients(Z, Y), L).
% find_items(L, X, Y) is true if L is the list of all items that contain 
% all the ingredients in X and do not contain any of the ingredients in Y
find_items(L,X,Y) :- p1(L1,X),p2(L2,Y),intersection(L1,L2,L).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
