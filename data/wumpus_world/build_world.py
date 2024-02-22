import numpy as np
from PIL import Image, ImageDraw
import random
import os

OPTIONS = {"a": (4,1), "b": (8,4), "c": (16,8)}
ICON_FOLDER = "./icons"
DATA_FOLDER = "Worlds"
KEYS = ["wumpus", "pit", "gold", "breeze", "stench"]
N = 5

def get_literal(item,tag):
    (x,y),(cell) = item
    return f"cell({tag},{x}/{y},{list(cell.values())}).\n"
    

class MapGrid:
    def __init__(self,w,h,pit_n):
        gw = (w * 2) - 1
        gh = (h * 2) - 1
        self.h = h
        self.w = w
        self.grid = {k:{key:0 for key in KEYS} for k in [(x,y) for x in range(w) for y in range(h)]}
        for _ in range(pit_n):
            x,y = random.choice(list(self.grid.keys()))
            self.place_pit(x,y)
        x,y = random.choice(list(self.grid.keys()))
        self.place_wumpus(x,y)
        self.place_random_gold()
        
    def place_wumpus(self,x,y):
        self.grid[x,y]["wumpus"] = 1
        for stx,sty in [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]:
            if stx < 0 or stx >= self.w: continue
            if sty < 0 or sty >= self.w: continue
            self.grid[stx,sty]["stench"] = 1
    
    def place_pit(self,x,y):
        self.grid[x,y]["pit"] = 1
        for brx,bry in [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]:
            if brx < 0 or brx >= self.w: continue
            if bry < 0 or bry >= self.w: continue
            self.grid[brx,bry]["breeze"] = 1          
        
    def place_random_gold(self):
        while 1:
            cell = random.choice(list(self.grid.values()))
            if not cell["pit"] and not cell["wumpus"]:
                cell["gold"] = 1
                break
        
        
    def create_img(self):
        image = Image.new('RGB', (self.w*20+1, self.h*20+1), color="white")
        icons = {key: Image.open(f"{ICON_FOLDER}/{key}.png", 'r') for key in KEYS}
        for (x,y),cell in self.grid.items():
           for key in KEYS:
               if cell[key]:
                   image.paste(icons[key],(x*20,y*20),icons[key]) 
        imgd = ImageDraw.Draw(image)
        for line in [[(x,0),(x,self.h*20)]for x in range(0,self.w*20+1,20)]:     
            imgd.line(line,width=0,fill="black")
        for line in [[(0,y),(self.w*20,y)]for y in range(0,self.h*20+1,20)]:     
            imgd.line(line,width=0,fill="black")
        return image
    
    def save(self, id, name):
        f = open(f"{DATA_FOLDER}/Code/{name}_{id}.pl",'w')
        f.write(":-multifile(cell/3).\n")
        f.writelines([get_literal(item,f"{name}_{id}") for item in self.grid.items()])
        f.close()
        img = self.create_img()
        img.save(f"{DATA_FOLDER}/Images/{name}_{id}.png")

try: 
    os.mkdir(DATA_FOLDER)
    os.mkdir(f"{DATA_FOLDER}/images")
    os.mkdir(f"{DATA_FOLDER}/code")
except:
    pass

map = MapGrid(5,5, 2)
map.save(1,"j")
# for item in map.grid.items():
#     print(get_literal(item))
# print(map.grid)
# (name, (dim,pits))
for (name, (dim,pits)) in OPTIONS.items():
    for id in range(N):
        map = MapGrid(dim,dim,pits)
        map.save(id,name)