import numpy as np
from PIL import Image, ImageOps
import random

SIZE = 4
DATA_FOLDER = "./Mazes"
N = 10

def get_char(n):
    if n == 1: return 'f'
    else: return 'w'

class Maze:
    def __init__(self,w,h):
        gw = (w * 2) - 1
        gh = (h * 2) - 1
        self.h = h
        self.w = w
        self.grid = np.zeros((gh,gw), np.int32)
        self.visited = np.zeros((h,w))
        for y in range(0,gh,2):
            for x in range(0,gw,2):
                self.grid[y,x] = 1
        self.backtracking((0,0))
        # self.add_border()
        
    def display(self):
        img = Image.fromarray(self.grid*255)
        img = ImageOps.expand(img,border=1,fill='black')
        img = img.resize((500,500), Image.BOX)
        # img.show()
        return img
        
    def remove_wall(self, p1:(int, int), p2:(int,int)):
        (x1, y1) = p1
        (x2, y2) = p2
        if x1 == x2:
            if y1 - 1 == y2:
                #above p1
                self.grid[(y1*2)-1,x1*2] = 1
            elif y1 + 1 == y2:
                #below p1
                self.grid[(y1*2)+1,x1*2] = 1
        elif y1 == y2:
            if x1 - 1 == x2:
                #left of p1
                self.grid[y1*2,(x1*2)-1] = 1
            elif x1 + 1 == x2:
                #right of p1
                self.grid[y1*2,(x1*2)+1] = 1

    def add_border(self):
        self.grid = np.pad(self.grid, pad_width=1, mode='constant', constant_values=0)
        self.grid[0,1] = 1
        self.grid[-1,-2] = 1
        
    def in_bounds(self, p:(int,int)):
        (x,y) = p
        if x < 0 or x >= self.w :
            return False
        if y < 0 or y >= self.h:
            return False
        return True

    def can_visit(self, p:(int, int)):
        if self.in_bounds(p) == False: return False
        (x,y) = p
        return self.visited[y,x] == 0

    def backtracking(self, p:(int,int)):
        (x, y) = p
        self.visited[y,x] = 1
        while True:
            valid_nbrs = []
            if self.can_visit((x,y+1)): valid_nbrs.append((x,y+1))
            if self.can_visit((x,y-1)): valid_nbrs.append((x,y-1))
            if self.can_visit((x+1,y)): valid_nbrs.append((x+1,y))
            if self.can_visit((x-1,y)): valid_nbrs.append((x-1,y))
            if not valid_nbrs: break
            next_p = random.choice(valid_nbrs)
            self.remove_wall(p, next_p)
            self.backtracking(next_p)
    
    def save(self, id: int):
        # TO DO each prolog file should start with 
        img = self.display().convert("RGB")
        img.save(f"{DATA_FOLDER}/Images/{id}.png")
        f = open(f"{DATA_FOLDER}/Code/{id}.pl", "w")
        f.write(":-multifile(maze/2).\n")
        (h,w) = self.grid.shape
        f.write(f"maze({id},[{h},{w}],\n")
        f.write(np.array2string(self.grid,separator=",",prefix="\t", formatter={'int': get_char}))
        f.write("\n).")
        f.close()

f = open(f"{DATA_FOLDER}/load_mazes.pl", "w")
for i in range(N):
    maze = Maze(SIZE,SIZE)
    maze.save(i)
    f.write(f":-['Code/{i}.pl'].\n")
    # TO DO create 1 prolog file with directives load every maze
    # TO DO 
f.close()