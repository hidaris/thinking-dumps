#! /usr/bin/python
import json
import os

current_path = os.getcwd()


def convert_json_to_txt(filename):
    jsonfile = filename + '.json'
    # get data
    with open(jsonfile, 'r') as f:
        data = json.load(f)
        vectors = data['Vector']
        depths = data['Depth'][0]

    txtfile = filename + '.txt'
    # transform data and write in the file
    with open(txtfile, 'w') as f:
        for i in range(0, len(vectors), 3):
            vector = vectors[i:i + 3]
            depth = depths[i / 3]
            vector.append(depth)
            str = '{x} {y} {z} {depth}\n'
            str = str.format(
                x=vector[0], y=vector[1], z=vector[2], depth=vector[3])
            f.write(str)


# iter in current directory
for file in os.listdir(current_path):
    a, b = os.path.splitext(file)
    if b == '.json':
        convert_json_to_txt(a)
    else:
        continue
