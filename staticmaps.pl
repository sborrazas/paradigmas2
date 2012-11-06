:- module(staticmaps, [coordsUrl/2,googlePathUrl/5]).

coordsUrl([],'').
coordsUrl([(X,Y)|Cs],R):-
        atomic_concat('|',X,R1),atomic_concat(R1,',',R2),atomic_concat(R2,Y,R3),
        coordsUrl(Cs,R4),atomic_concat(R3,R4,R).

addMarker(Url,UrlRes,Color,Label,Marks):-
        atomic_concat(Url,'&markers=color:',A),
        atomic_concat(A,Color,B),
        atomic_concat(B,'|label:',C),
        atomic_concat(C,Label,D),
        coordsUrl(Marks,M),
        atomic_concat(D,M,UrlRes).

addZoom(Url,UrlRes,Zoom):-
        atomic_concat(Url,'&zoom=',A),
        atomic_concat(A,Zoom,UrlRes).

addPath(Url,UrlRes,Color,P):-
        atomic_concat(Url,'&path=weight:5|color:',A),
        atomic_concat(A,Color,B),
        atomic_concat(B,P,UrlRes).
        
createGoogleUrl('http://maps.googleapis.com/maps/api/staticmap?size=800x600&sensor=false&format=jpg').

googlePathUrl(Cs,Zoom,GP,X,Y):-
        createGoogleUrl(Url),
        addZoom(Url,Url1,Zoom),
        addPath(Url1,Url2,'red',Cs),
        addMarker(Url2,Url3,'red','O',[X]),
        addMarker(Url3,GP,'blue','D',[Y]).
