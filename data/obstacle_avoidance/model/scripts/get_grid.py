import survey_simulation
import numpy as np

# example test run
ss = survey_simulation.SurveySimulation('test',
                                       save_loc='data',
                                       params_loc='params.txt')
for n in range(100):
    rnd_mv = np.random.randint(0,100,size=(2)).tolist()
    t, cov_map, contacts, occ_grid = ss.new_action('move', rnd_mv)
    
# Save the episode log
ss.save_episode()

# Writes to file the occ_grid array rows as prolog terms.
(rows,cols) = occ_grid.shape
arity = cols + 1

np.set_printoptions(linewidth=502)

with open('occ_grid.pl', encoding="utf-8", mode='w') as f:

    print(':-module(occlusion_grid, [occ_grid/'+ str(arity) + ']).\n',file=f)

    for i in range(0,rows):
        print('occ_grid(' + str(i) + ',',file=f,end='')
        for k in range(0,cols):
            print(occ_grid[i][k], file=f, end='')
            if k < cols-1:
                print(',',file=f,end='')
        print(').',file=f)

f.closed
