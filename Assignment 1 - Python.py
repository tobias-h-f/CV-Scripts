# -*- coding: utf-8 -*-
"""
Created on Mon Oct 26 09:40:44 2020

@author: Tobias Hulbæk Fog - 05/04/1996

Assignment 1 - Introduktion til databehandling
"""


"""

Simple Python codes for the coding camp of the first few weeks of the Master of Data Science on SDU

The assignment was rated 14/15 points. The subtracted point was due to the use of while loop
in the last assignment instead of a for loop.

"""

def Pythagoras():
    print("Hello, im m Pythagoras and we are going to calculate the unknown side in a triangle.") 
    print("I will ask you for the length of each size. When asked for the unknown side")
    print("type '?'")
    a = input("write the length of side a: ")
    b = input("write in the length of side b: ")
    c = input("write the length of side c: ") 


    if a == '?':
        b = int(b)
        c = int(c)
        a = (c**2 - b**2)**0.5
        print(f"the length of c is {a}")
    elif b == '?':
        a = int(a)
        c = int(c)
        b = (c**2 - a**2)**0.5
        print(f"the length of c is {b}")
    elif c == '?':
        a = int(a)
        b = int(b)
        c = (a**2 + b**2)**0.5
        print(f"the length of c is {c}")
    else:
        print("I think you made a mistake somewhere.")
    
    
Pythagoras()


#%%


import math

def median(L):
    c = len(L)
    if c%2 == 0:
        c = c/2
        m = int(c)
        m_1 = int(m+1)
        print((L[m-1]+L[m_1-1])/2)
    else:
        m = int(math.ceil(c/2))
        print(L[m-1])


median([1,2,3,4])



#%%


def unique(L):
    c = []
    
    for l in L:
            if L.count(l) == 1:
                c.append(l)
    return c


#%%


raven = open('raven.txt').read()

def characters(textfile):
    c = {}
    #characters = textfile.lower()
    characters = list(textfile)

    for char in characters: 
            if char in c:
                c[char] +=1
            else:
                c[char]= 1
    c.pop('\n')
    
    for char in sorted(c.items()) :
        print(char[0] , " :" , char[1] )


characters(raven)


#%%

import os
from matplotlib import pyplot as plt


def count(term):
    term = str(term)
    frequencies = {}
    c = 1
    while c <51:
        counter = 0
        chapter = open(f'genesis\chapter-{c}.txt').read()
        words = chapter.split(" ")
        for word in words:
            if term == word:
                counter +=1
        frequencies[c]= counter
        c +=1
    return frequencies

frequencies = count('God')
x = list(frequencies.keys())
y = list(frequencies.values())

plt.figure(dpi = 400)
plt.style.use('fivethirtyeight')
plt.plot(x, y)