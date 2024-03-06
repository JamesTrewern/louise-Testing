:-use_module(model_src(types)).

:-begin_tests(types, []).

% Testing angle type.

test('angle_0.0',[]):-
        type(angle,0.0).

test('angle_0.1',[]):-
        type(angle,0.1).

test('angle_0',[fail]):-
        type(angle,0).

test('angle_1',[fail]):-
        type(angle,1).

test('angle_var',[fail]):-
        type(angle,_).

test('angle_list',[fail]):-
        type(angle,[_|_]).

test('angle_atom',[fail]):-
        type(angle,stassa).


% Testing coordinate type.

test('coordinate_0.0',[]):-
        type(coordinate,0.0).

test('coordinate_0.1',[]):-
        type(coordinate,0.1).

test('coordinate_0',[fail]):-
        type(coordinate,0).

test('coordinate_1',[fail]):-
        type(coordinate,1).

test('coordinate_var',[fail]):-
        type(coordinate,_).

test('coordinate_list',[fail]):-
        type(coordinate,[_|_]).

test('coordinate_atom',[fail]):-
        type(coordinate,stassa).


% Testing distance type.

test('distance_0.0',[fail]):-
        type(distance,0.0).

test('distance_0.1',[fail]):-
        type(distance,0.1).

test('distance_0',[]):-
        type(distance,0).

test('distance_1',[]):-
        type(distance,1).

test('distance_var',[fail]):-
        type(distance,_).

test('distance_list',[fail]):-
        type(distance,[_|_]).

test('distance_atom',[fail]):-
        type(distance,stassa).


% Testing id type.

test('id_0.0',[fail]):-
        type(id,0.0).

test('id_0.1',[fail]):-
        type(id,0.1).

test('id_0',[]):-
        type(id,0).

test('id_1',[]):-
        type(id,1).

test('id_var',[fail]):-
        type(id,_).

test('id_list',[fail]):-
        type(id,[_|_]).

test('id_atom',[fail]):-
        type(id,stassa).


% Testing length type.

test('length_0.0',[fail]):-
        type(length,0.0).

test('length_0.1',[fail]):-
        type(length,0.1).

test('length_0',[]):-
        type(length,0).

test('length_1',[]):-
        type(length,1).

test('length_var',[fail]):-
        type(length,_).

test('length_list',[fail]):-
        type(length,[_|_]).

test('length_atom',[fail]):-
        type(length,stassa).


% Testing list type.

test('list_0.0',[fail]):-
        type(list,0.0).

test('list_0.1',[fail]):-
        type(list,0.1).

test('list_0',[fail]):-
        type(list,0).

test('list_1',[fail]):-
        type(list,1).

test('list_var',[fail]):-
        type(list,_).

test('list_list',[fail]):-
% List-types are lists of a type!
        type(list,[_|_]).

test('list_atom',[fail]):-
        type(list,stassa).

test('list_of_not_int,atoms',[fail]):-
        type(list(integer),[stassa,ab,c,d]).

test('list_of_not_int_floats',[fail]):-
        type(list(integer),[0.0,0.2,0.0]).

test('list_of_not_ints',[fail]):-
        type(list(integer),[0.0,1,2,0.1,stassa]).

test('list_of_not_ints_vars',[fail]):-
        type(list(integer),[_X,_Y,_Z]).

test('list_of_int_empty',[]):-
        type(list(integer),[]).

test('list_of_int',[]):-
        type(list(integer),[1,2,3,1,3]).

test('list_of_float_empty',[]):-
        type(list(float),[]).

test('list_of_float',[]):-
        type(list(float),[0.1,0.2,0.3,0.1,0.3]).

test('list_of_not_angle',[fail]):-
        type(list(angle),[stassa]).

test('list_of_angle',[]):-
        type(list(angle),[0.0,0.1,0.3545]).

test('list_of_not_coordinate',[fail]):-
        type(list(coordinate),[stassa]).

test('list_of_coordinate',[]):-
        type(list(coordinate),[0.0,0.1,0.3545]).

test('list_of_not_distance',[fail]):-
        type(list(distance),[stassa]).

test('list_of_distance',[]):-
        type(list(distance),[100,2,3,1,400]).

test('list_of_not_id',[fail]):-
        type(list(id),[stassa]).

test('list_of_id',[]):-
        type(list(id),[1,3,1,2,4]).

test('list_of_not_length',[fail]):-
        type(list(length),[stassa]).

test('list_of_not_length_2',[fail]):-
        type(list(length),[0.1,0.2,0.1,0.4]).

test('list_of_length',[]):-
        type(list(length),[1,2,100,40]).

test('list_of_not_speed',[fail]):-
        type(list(speed),[stassa]).

test('list_of_not_speed_2',[fail]):-
        type(list(speed),[1,2,100,40]).

test('list_of_speed',[]):-
        type(list(speed),[0.1,0.23,0.1,0498374957.4]).

test('list_of_not_time',[fail]):-
        type(list(time),[stassa,massa,fassa,1,2,3,0.1,0.3]).

test('list_of_not_time_2',[fail]):-
        type(list(time),[1,2,100,40]).

test('list_of_time',[]):-
        type(list(time),[0.1,0.1232,0.13497,0.6]).

test('list_of_list_of_empty_list',[]):-
        type(list(list(integer)),[[],[]]).

test('list_of_list_of_int',[]):-
        type(list(list(integer)),[[0,1,2,3,4],[4234,875,28,52,1,2]]).

test('list_of_list_of_float_and_int',[fail]):-
        type(list(list(float)),[[0.0,0.1,0.12,0.3,0.4]
                               ,[4234,875,28,52,1,2]
                               ]).

test('list_of_list_of_float',[]):-
        type(list(list(float)),[[0.0,0.1,0.12,0.3,0.4]
                               ,[0.4234,0.875,0.28,0.52,0.1,0.2]
                               ]).

test('list_of_unknown_type',[fail]):-
        type(list(stassa),[1,2,3,4]).

test('list_of_unknown_type_empty',[fail]):-
        type(list(stassa),[]).


% Testing name type.

test('name_0',[fail]):-
        type(name,0).

test('name_1',[fail]):-
        type(name,1).

test('name_0.0',[fail]):-
        type(name,0.0).

test('name_0.1',[fail]):-
        type(name,0.1).

test('name_var',[fail]):-
        type(name,_).

test('name_list',[fail]):-
        type(name,[_|_]).

test('name_atom',[]):-
        type(name,stassa).


% Testing string type.

test('string_0',[fail]):-
        type(string,0).

test('string_1',[fail]):-
        type(string,1).

test('string_0.0',[fail]):-
        type(string,0.0).

test('string_0.1',[fail]):-
        type(string,0.1).

test('string_var',[fail]):-
        type(string,_).

test('string_list',[fail]):-
        type(string,[_|_]).

test('string_atom',[]):-
        type(string,stassa).


:-end_tests(types).
