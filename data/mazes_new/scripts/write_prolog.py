
import matplotlib.pyplot as plt
import numpy as np
import math
import cv2

def find_start(maze):
    for x in range(maze.shape[0]):
        for y in range(maze.shape[0]):
            if maze[x,y] == 0.5:
                return x,y

def find_end(maze):
    for x in range(maze.shape[0]):
        for y in range(maze.shape[0]):
            if maze[x,y] == 1:
                return x,y

def closest(a, values):
    res = values[0]
    dis = abs(a-values[0])
    for v in values:
        d = abs(a-v)
        if d < dis:
            dis = d
            res = v
    return res

def get_tile(value):
    match value:
        case 0.0:
            return 'f'
        case 0.25:
            return 'w'
        case 0.5:
            return 's'
        case 1.0:
            return 'e'

def bucketize(image):
    bins = [0,0.25,0.5,1]
    for x in range(image.shape[0]):
        for y in range(image.shape[1]):
            image[x,y] = closest(image[x,y],bins)

def get_moves(maze,x1,y1,id):
    dim = maze.shape[0]
    moves = []
    t1 = get_tile(maze[x1,y1])
    if t1 == 'w': 
        return moves   
    for x2,y2,direction in [(x1-1,y1,"left"),(x1+1,y1,"right"),(x1,y1-1,"down"),(x1,y1+1,"up")]:
        if x2 >= dim or x2 < 0 or y2 >= dim or y2 < 0: continue
        t2 = get_tile(maze[x2,y2])
        if t2 == 'w': continue
        moves.append(f"step_{direction}([{id},{x1}/{y1},{t1}],[{id},{x2}/{y2},{t2}]).\n")
    return moves

# Save matrix to file called "{id}.pl"
def matrix_to_prolog(maze:np.ndarray, id: int, folder: str = "."):
    maze = maze[2]
    bucketize(maze)
    maze = cv2.resize(maze, dsize=(16,16), interpolation=cv2.INTER_NEAREST)
    sx,sy = find_start(maze)
    ex,ey = find_end(maze)
    lines = [
        ":-multifile(step_up/2).\n",
        ":-multifile(step_down/2).\n",
        ":-multifile(step_left/2).\n",
        ":-multifile(step_right/2).\n",
        ":-multifile(testing_instance/3).\n"
        f"testing_instance(solve,{id},solve([{id},{sx}/{sy},s],[{id},{ex}/{ey},e])).\n"
    ]
    for x in range(maze.shape[0]):
        for y in range(maze.shape[1]):
            lines += get_moves(maze,x,y,id)
    file = open(F"{folder}/{id}.pl", "w")
    file.writelines(lines)
    file.close()

