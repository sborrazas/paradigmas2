:- module(proxy, [proxy/1]).

% máquinas de ORT (no wifi de ORT) o cualquier red con proxy http:
%proxy([proxy('192.168.57.2',80)]).

% wifi ORT o cualquier red sin proxy http:
proxy([]).
