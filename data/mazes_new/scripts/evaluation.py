
from pandas import DataFrame
import pandas as pd
import matplotlib.pyplot as plt
import random

N = 10
SIZES = ['5x5', '10x10','15x15']

def random_data(n: int = N, sizes: list = [7, 19]) -> DataFrame:
    l = n*len(sizes)
    return DataFrame({
        "maze_id": list(range(l)),
        "size": [size for size in sizes for _ in range(n)],
        "rl": [random.randint(0, 600) for _ in range(l)],
        "louise": [random.randint(0, 600) for _ in range(l)],
        # "metagol": [random.randint(0, 600) for _ in range(l)]
    })


def get_average_times(df: DataFrame, n: int = N, sizes: list = SIZES, keys: list = ["rl", "louise"],) -> DataFrame:
    totals = {key: {size: 0.0 for size in sizes} for key in keys}
    for _, row in df.iterrows():
        size = row["Size"]
        for key in keys:
            totals[key][size] += float(row[key])
    data = {"size": sizes}
    data.update({key: [totals[key][size]/n for size in sizes] for key in keys})
    return DataFrame(data)

def get_accuracy(df: DataFrame, n: int = N, max_time: int = 300.0, sizes: list = SIZES, keys: list = ["rl", "louise"],) -> DataFrame:
    passed = {key: {size: 0.0 for size in sizes} for key in keys}
    for _, row in df.iterrows():
        size = row["Size"]
        for key in keys:
            if max_time > float(row[key]):
                passed[key][size] += 1
    data = {"size": sizes}
    data.update({key: [passed[key][size]/n for size in sizes] for key in keys})
    return DataFrame(data)


df = pd.read_csv("../test_mazes/results.csv")
avg = get_average_times(df)
acc = get_accuracy(df)
print(avg)
print(acc)