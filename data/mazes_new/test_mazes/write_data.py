import pandas as pd
import numpy as np
import write_prolog

df = pd.read_csv("./data.csv")
for idx, row in df.iterrows():
    maze = np.load(row["Numpy Name"])
    write_prolog.matrix_to_prolog(maze,idx+1)


# maze = np.array([[0,0.5],[1,0]])
# matrix_to_prolog(maze,1)

# maze = np.load("maze.npy")
# maze = maze[2]
# bucketize(maze)
# maze = cv2.resize(maze, dsize=(16,16), interpolation=cv2.INTER_NEAREST)
# d = maze.shape[0]
# values = {}
# for x in range(d):
#     for y in range(d):
#         if maze[x,y] in values.keys():
#             values[maze[x,y]] += 1
#         else:
#             values[maze[x,y]] = 0
# print(values)
# plt.imshow(maze)
 
# plt.show()