:-multifile(step_up/2).
:-multifile(step_down/2).
:-multifile(step_left/2).
:-multifile(step_right/2).
:-multifile(testing_instance/3).
testing_instance(solve,4,solve([4,10/6,s],[4,7/6,e])).
step_right([4,6/6,f],[4,7/6,e]).
step_right([4,6/8,f],[4,7/8,f]).
step_up([4,6/8,f],[4,6/9,f]).
step_down([4,6/9,f],[4,6/8,f]).
step_up([4,6/9,f],[4,6/10,f]).
step_right([4,6/10,f],[4,7/10,f]).
step_down([4,6/10,f],[4,6/9,f]).
step_left([4,7/6,e],[4,6/6,f]).
step_right([4,7/6,e],[4,8/6,f]).
step_left([4,7/8,f],[4,6/8,f]).
step_right([4,7/8,f],[4,8/8,f]).
step_left([4,7/10,f],[4,6/10,f]).
step_right([4,7/10,f],[4,8/10,f]).
step_left([4,8/6,f],[4,7/6,e]).
step_up([4,8/6,f],[4,8/7,f]).
step_down([4,8/7,f],[4,8/6,f]).
step_up([4,8/7,f],[4,8/8,f]).
step_left([4,8/8,f],[4,7/8,f]).
step_right([4,8/8,f],[4,9/8,f]).
step_down([4,8/8,f],[4,8/7,f]).
step_left([4,8/10,f],[4,7/10,f]).
step_right([4,8/10,f],[4,9/10,f]).
step_left([4,9/8,f],[4,8/8,f]).
step_right([4,9/8,f],[4,10/8,f]).
step_left([4,9/10,f],[4,8/10,f]).
step_right([4,9/10,f],[4,10/10,f]).
step_up([4,10/6,s],[4,10/7,f]).
step_down([4,10/7,f],[4,10/6,s]).
step_up([4,10/7,f],[4,10/8,f]).
step_left([4,10/8,f],[4,9/8,f]).
step_down([4,10/8,f],[4,10/7,f]).
step_left([4,10/10,f],[4,9/10,f]).
