% Routes
%
% Allows you to map requests to different controller modules, doing simple
% transformations to the request object or path as desired.
% 
% It isn't strictly required to put anything in here, as it will return
% 'no_route' by default, at which point zerl will look for a controller that
% automatically matches the first part of the requested path.  If none is found
% or if the given controller doesn't have the a matching handler, a 404 will be
% served up.
%

-module(!project!.routes).

% ------ Add your routes here ------
%
% EXAMPLE:
%
% route('GET', ["users", UserName], Req) ->
%   {controller, user, [name, UserName], Req};
%



% Catch everything else.  Leave this here.
% This can return a special controller or static error though if you don't want
% zerl to do its own search for a suitable controller.
route(_, _, _) ->
  no_route.
