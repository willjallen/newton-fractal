import sys, pygame
pygame.init()

size = width, height = 1920, 1080
speed = [2, 2]
# black = 0, 0, 0
# red = 255, 0, 0
# green = 0, 225, 0
# blue = 0, 0, 255

max_itr = 20
colors = [(x*255)/20 for x in range(0, 20)]


# ball = pygame.image.load("intro_ball.gif")
# ballrect = ball.get_rect()


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
                

r = reader('out.txt')


arr = []
temp_arr = []

for c in r:
    # c = next(r)    
    # print(c)
    if(c == ' '):
        continue

    if(c == '('):
        continue

    if(c == ')'):
        arr.append(temp_arr.copy())
        temp_arr.clear()
        continue
    
    if(c == '#'):
        continue
    
    if(c == 'f'):
        temp_arr.append(-1)
        continue

    temp_arr.append(int(c))

    
# print(arr)


screen = pygame.display.set_mode(size)


painted = False

while True:
    for event in pygame.event.get():
        if event.type == pygame.QUIT: sys.exit()

    if not painted:
        for i in range(0, width):
            for j in range(0, height):
                itr = arr[i][j]
                color = (colors[itr], colors[itr], colors[itr])
                pygame.draw.rect(screen, color, pygame.Rect(i,j,1,1))
    painted = True
    
    # screen.fill(black)
    # screen.blit(ball, ballrect)
    pygame.display.flip()
    pygame.time.wait(100)