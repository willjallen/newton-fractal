import sys, pygame
pygame.init()

size = width, height = 680, 680
speed = [2, 2]
black = 0, 0, 0
red = 255, 0, 0
green = 0, 225, 0
blue = 0, 0, 255

screen = pygame.display.set_mode(size)

# ball = pygame.image.load("intro_ball.gif")
# ballrect = ball.get_rect()

arr = []

import csv
with open("out.txt", 'r') as file:
    r = csv.reader(file, delimiter=' ')
    for line in r:
        arr = line

print(arr)

while 1:
    for event in pygame.event.get():
        if event.type == pygame.QUIT: sys.exit()

    for i in range(0, height):
        for j in range(0, width):
            if(arr[i + j] == "BLACK"):
                pygame.draw.rect(screen, black, pygame.Rect(i,j,1,1))
            if(arr[i + j] == "RED"):
                pygame.draw.rect(screen, red, pygame.Rect(i,j,1,1))
            if(arr[i + j] == "GREEN"):
                pygame.draw.rect(screen, green, pygame.Rect(i,j,1,1))
            if(arr[i + j] == "BLUE"):
                pygame.draw.rect(screen, blue, pygame.Rect(i,j,1,1))
    # screen.fill(black)
    # screen.blit(ball, ballrect)
    pygame.display.flip()