:- module(ventanas, [display/0]).

:-use_module(library(socket)).
:-use_module(library(http/http_open)).
:-use_module(pathplanning).
:-use_module(buses).
:-use_module(geographic).
:-use_module(recorrido).
:-use_module(list).
:-use_module(staticmaps).
:-use_module(proxy).

toNumber(X,N):-atom_codes(X,Y),number_chars(N,Y).

succAtom(N,N1):-toNumber(N,Num),Num2 is Num + 1,number_chars(Num2,K),atom_codes(N1,K).
predAtom(N,N1):-toNumber(N,Num),Num2 is Num - 1,number_chars(Num2,K),atom_codes(N1,K).

find_name(N,N1):-atomic_concat(N,'.jpg',F),exists_file(F),!,succAtom(N,N2),find_name(N2,N1).
find_name(N,N1):-atomic_concat(N,'.jpg',N1).

http_save(Url,N):-
	proxy(P),
	http_open(Url, In,P),% [proxy('192.168.57.2',80)]),% esto en la ORT [proxy('192.168.57.2',80)]),
	working_directory(D,D),
	find_name('0',N),
	open(N, write, Fd, [type(binary)]),
	copy_stream_data(In, Fd),
	close(Fd),
	close(In).

display:-
	new(D, dialog('Laboratorio:Prolog Map')),
	send(D, append, new(Co1,text_item('Calle Origen 1'))),
	send(D, append, new(Cd1,text_item('Calle Destino 1')),right),
	send(D, append, new(Co2,text_item('Calle Origen 2')),next_row),
	send(D, append, new(Cd2,text_item('Calle Destino 2')),right),
	send(D, append, new(L,menu('Algoritmo caminando',cycle))),
	send_list(L, append, ['Hill Climbing','A*','Amplitud']),
	send(D, append, new(button('Caminar',message(@prolog, buscar, Co1?selection, Co2?selection, Cd1?selection, Cd2?selection,'15',L?selection)))),
	send(D, append, new(button('Omnibus',message(@prolog, buscar2, Co1?selection, Co2?selection, Cd1?selection, Cd2?selection,'15'))),below),
	send(D, open).

buscar2(Co1,Co2,Cd1,Cd2,Z):-new(D, dialog('Resultado de la busqueda en omnibus')),
	send(D,size,size(670,720)),
	atomic_concat(Co1,' y ',O1),atomic_concat(O1, Co2, Or),
	atomic_concat(Cd1,' y ',D1),atomic_concat(D1, Cd2, De),
	send(D,append, new(label(selection:=Or))),
	send(D,append, new(label(selection:=De)),below),
	findBusCalles(Co1,Co2,Cd1,Cd2,N,Desc,C,PO,PD),
	send(D,append, new(label(selection:=N)),below),
	send(D,append, new(label(selection:=Desc)),right),
	recorrido(N,Desc,C,P),
	sublist(P,P2,PO,PD),
	length(P2,L),
	Div is L div 60,drop(Div,P2,P3),
	coordsUrl(P3,Cs),
	corner(Co1,Co2,Xo),
	corner(Cd1,Cd2,Xd),
	googlePathUrl(Cs,Z,G,Xo,Xd),
	http_save(G,M),
	send(D, append,new(bitmap(M)), below),
	succAtom(Z,SZ),predAtom(Z,PZ),
	send(D, append, new(button('Zoom +',message(@prolog, zoom, D, Cs, SZ, Co1,Co2,Cd1,Cd2))),below),
	send(D, append, new(button('Zoom -',message(@prolog, zoom, D, Cs, PZ, Co1,Co2,Cd1,Cd2))),right),	
	send(D,open).

buscar(Co1,Co2,Cd1,Cd2,Z,Sel):-new(D, dialog('Resultado de la busqueda caminando')),
	send(D,size,size(670,700)),
	atomic_concat(Co1,' y ',O1),atomic_concat(O1, Co2, Or),
	atomic_concat(Cd1,' y ',D1),atomic_concat(D1, Cd2, De),
	send(D,append, new(label(selection:=Or))),
	send(D,append, new(label(selection:=De)),below),
	((Sel == 'Hill Climbing', caminoHC(Co1,Co2,Cd1,Cd2,P)) | (Sel == 'A*', caminoAast(Co1,Co2,Cd1,Cd2,P)) |
	 (Sel == 'Amplitud', caminoAmp(Co1,Co2,Cd1,Cd2,P))),!,
	corner(Co1,Co2,Xo),
	corner(Cd1,Cd2,Xd),
	length(P,L),
	Div is L div 60,drop(Div,P,P2),
	coordsUrl(P2,Cs),
	googlePathUrl(Cs,Z,G,Xo,Xd),
	http_save(G,M),
	send(D, append,new(bitmap(M)), below),
	succAtom(Z,SZ),predAtom(Z,PZ),
	send(D, append, new(button('Zoom +',message(@prolog, zoom, D, Cs, SZ, Co1,Co2,Cd1,Cd2))),below),
	send(D, append, new(button('Zoom -',message(@prolog, zoom, D, Cs, PZ, Co1,Co2,Cd1,Cd2))),right),	
	send(D,open).

buscar_aux(Co1,Co2,Cd1,Cd2,Z,Cs):-new(D, dialog('Resultado de la busqueda')),
	send(D,size,size(670,700)),
	atomic_concat(Co1,' y ',O1),atomic_concat(O1, Co2, Or),
	atomic_concat(Cd1,' y ',D1),atomic_concat(D1, Cd2, De),
	send(D,append, new(label(selection:=Or))),
	send(D,append, new(label(selection:=De)),below),
	corner(Co1,Co2,Xo),
	corner(Cd1,Cd2,Xd),
	googlePathUrl(Cs,Z,G,Xo,Xd),
	http_save(G,M),
	send(D, append,new(bitmap(M)), below),
	succAtom(Z,SZ),predAtom(Z,PZ),
	send(D, append, new(button('Zoom +',message(@prolog, zoom, D, Cs, SZ, Co1,Co2,Cd1,Cd2))),below),
	send(D, append, new(button('Zoom -',message(@prolog, zoom, D, Cs, PZ, Co1,Co2,Cd1,Cd2))),right),	
	send(D,open).

zoom(D,Cs,Z,Co1,Co2,Cd1,Cd2):-
	send(D,destroy),free(D),
	buscar_aux(Co1,Co2,Cd1,Cd2,Z,Cs).

%http_open('http://maps.googleapis.com/maps/api/staticmap?path=color:0x0000ff|weight:5|-34.90461808680903,-56.186989272012255|-34.904705046809752,-56.188115581168539|-34.904778560104184,-56.189212089242062|-34.903852430301768,-56.18931109604042&zoom=17&size=800x600&sensor=false&key=AIzaSyCuCDpFtGwMJzR8g2vCFzQFeVcWocIXUWc', In, []),

%a('COLONIA','EJIDO','COLONIA','YI',P),corner('COLONIA','EJIDO',Xo),corner('COLONIA','YI',Xd),googlePathUrl(P,'15',G,Xo,Xd).

% dibujar constituyente
%findall((X,Y),(tramo('CONSTITUYENTE',


