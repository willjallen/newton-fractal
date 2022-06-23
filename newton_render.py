from hashlib import new
import sys, random, math
import png

size = width, height = 1920*4, 1080*4


def random_color():
    return (random.randint(0, 255), random.randint(0, 255), random.randint(0, 255)) 

max_itr = 30

# COLOR MODE
# color_divisons = 50
# colors = [random_color() for x in range(0, color_divisons)]

# BLACK AND WHITE
colors = [(x/max_itr)*255 for x in range(0, max_itr+1)]
# WHITE AND BLACK
# colors = [(1-(x/max_itr))*255 for x in range(0, max_itr+1)]


# Each vector is a column vector
def reader(filename):
    with open(filename) as f:
        while True:
            # read next character
            char = f.read(1)
            # if not EOF, then at least 1 character was read, and 
            # this is not empty
            if char:
                yield char
            else:
                return
                

r = reader('renders/[z^3-1]-4k-30itr.txt')


arr = []
temp_arr = []

tmp_number_str = ""

print("Reading file")

for c in r:
    if(c == '\'' or c == '(' or c == ' ' or c == '#'):
        if tmp_number_str:
            temp_arr.append(int(tmp_number_str))
        tmp_number_str = ""
        continue

    elif(c == ')'):
        if tmp_number_str:
            temp_arr.append(int(tmp_number_str))
        tmp_number_str = ""
        arr.append(temp_arr.copy())
        temp_arr.clear()
        continue
    
    elif(c == 'f'):
        temp_arr.append(-1)
        continue

    tmp_number_str = tmp_number_str + str(c)

    

# Delete empty lists
arr = [ele for ele in arr if ele != []]




def colorIterations(arr):
    new_arr = []
    for i in range(0, len(arr)):
        tmp = []
        for j in range(0, len(arr[i])):
            if(arr[i][j] == -1):
                tmp.extend([0, 0, 0])
            else:                
                tmp.extend([int(colors[arr[i][j]]), int(colors[arr[i][j]]), int(colors[arr[i][j]])])
        new_arr.append(tuple(tmp))
    return new_arr

print("Coloring..")
rgb_pixels = colorIterations(arr)

painted = False

print("Writing")
f = open('out.png', 'wb')
w = png.Writer(width, height, greyscale=False)
w.write(f, rgb_pixels)
f.close()
print("Finished")