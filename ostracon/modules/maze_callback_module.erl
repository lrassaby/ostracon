-module(maze_callback_module).
% ENSURE NO EXPLICIT CONCURRENCY IN THIS MODULE -- ABSTRACT THE LOGIC TO THE 
% OSTRACON_APP.ERL

% TODO: export:
% updateState/1, takes in vote histogram and attempts to update state ETS, failing 
% if out of bounds
% reset/0, which sets the state ETS to its initial form
