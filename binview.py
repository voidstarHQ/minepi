#! /usr/bin/env python

# Copyright (c) 2009 Peter Waller <peter.waller@gmail.com>

# MIT License.

#~ Permission is hereby granted, free of charge, to any person
#~ obtaining a copy of this software and associated documentation
#~ files (the "Software"), to deal in the Software without
#~ restriction, including without limitation the rights to use,
#~ copy, modify, merge, publish, distribute, sublicense, and/or sell
#~ copies of the Software, and to permit persons to whom the
#~ Software is furnished to do so, subject to the following
#~ conditions:

#~ The above copyright notice and this permission notice shall be
#~ included in all copies or substantial portions of the Software.

#~ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#~ EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
#~ OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#~ NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
#~ HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
#~ WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
#~ FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
#~ OTHER DEALINGS IN THE SOFTWARE.

from __future__ import division, with_statement

# Controls:
#   Space: start automatically rotating
#   Left-right: switch between files in the file list
#   Scroll wheel up-down: zoom in/out
#   Scroll-wheel drag: translate in XY plane
#   Left mouse drag: arcball rotation
#   Right click: reset arcball rotation

# Configure size of openglpoints (sensible values = 1-10)
pointSize = 4

# List of files to use
inputFiles = [
    #"twain.dll",
    #"System32/slcc.dll",
    #"System32/ssBranded.scr",
    #"System32/Aurora.scr",
    #"System32/spwizimg.dll"
    "binview.py",
]

# Base path to input files
# path = "/mnt/windows/Windows/"
path = "./"

# Number of chunks to split the file into (granularity in "depth")
nChunks = 200

from time import time

from PIL.Image import new
from array import array
from itertools import chain, repeat, izip
from collections import defaultdict
from math import log
from os.path import basename
from contextlib import contextmanager

import pyglet
import pyglet.window.key as key

from pyglet import clock
from pyglet.gl import Config
from pyglet.text import Label
from pyglet.window import Window, mouse
from pyglet.graphics import vertex_list

from OpenGL.GL import (
    glBlendFunc, glClear, glClearColor, glEnable,
    glColor, glVertex, glPointSize,
    glGenLists, glNewList, glEndList, glCallList,
    glLoadIdentity, glMatrixMode, glPushMatrix, glPopMatrix, glViewport, 
    glMultMatrixf, glTranslate, glRotate, glScale,
    GL_COLOR_BUFFER_BIT, GL_COMPILE_AND_EXECUTE, GL_BLEND, GL_DEPTH_BUFFER_BIT, 
    GL_DST_ALPHA, GL_MODELVIEW, GL_POINTS, GL_PROJECTION, GL_QUADS, GL_SRC_ALPHA, 
)

from OpenGL.GLU import gluPerspective

from OpenGL.GLUT import glutInit, glutWireCube
glutInit()
 
try:
    assert hasattr("chain", "from_iterable")
except AssertionError:
    class chain(chain):
        @classmethod
        def from_iterable(cls, iterables):
            # chain.from_iterable(['ABC', 'DEF']) --> A B C D E F
            for it in iterables:
                for element in it:
                    yield element

try:
    from itertools import izip_longest
except ImportError:
    def izip_longest(*args, **kwds):
        # izip_longest('ABCD', 'xy', fillvalue='-') --> Ax By C- D-
        fillvalue = kwds.get('fillvalue')
        def sentinel(counter = ([fillvalue]*(len(args)-1)).pop):
            yield counter()         # yields the fillvalue, or raises IndexError
        fillers = repeat(fillvalue)
        iters = [chain(it, sentinel(), fillers) for it in args]
        try:
            for tup in izip(*iters):
                yield tup
        except IndexError:
            pass

try:
    from itertools import product
except ImportError:
    def product(*args, **kwds):
        # product('ABCD', 'xy') --> Ax Ay Bx By Cx Cy Dx Dy
        # product(range(2), repeat=3) --> 000 001 010 011 100 101 110 111
        pools = map(tuple, args) * kwds.get('repeat', 1)
        result = [[]]
        for pool in pools:
            result = [x+[y] for x in result for y in pool]
        for prod in result:
            yield tuple(prod)    

# From itertools python library documentation (recipies section)
def grouper(n, iterable, fillvalue=None):
    "grouper(3, 'ABCDEFG', 'x') --> ABC DEF Gxx"
    args = [iter(iterable)] * n
    return izip_longest(fillvalue=fillvalue, *args)

def flatten(listOfLists):
    "Flatten a list"
    return list(chain.from_iterable(listOfLists))

def tally(seq):
    "Tally objects in a list, return dictionary of counts"
    d = defaultdict(int)
    for item in seq:
        d[item] += 1
    return dict(d)

@contextmanager
def timer(name):
    "A context manager which spits out how long the block took to execute"
    start = time()
    try:
        yield
    finally:
        end = time()
        print "Took %.2f to do %s" % (end - start, name)

@contextmanager
def SaveMatrix():
    "Save the current opengl matrix over a with block"
    glPushMatrix()
    try:
        yield
    finally:
        glPopMatrix()


class LeaveWith(Exception): pass

displayLists = {}

@contextmanager
def displayListify(name):
    global displayLists
    if name in displayLists:
        # Hey, we've made this display list before!
        # Run the existing list, then break out of the with block
        glCallList(displayLists[name])
        try:
            yield True
        except LeaveWith:
            pass
            
    else:
        # Nope, not made this display list. Wrap the block in "work rememberers"
        displayLists[name] = thisList = glGenLists(1)
        glNewList(thisList, GL_COMPILE_AND_EXECUTE)
        try:
            yield
        finally:
            glEndList()

def PILImageToPyglet(image):
    "Given a PIL image, return a pyglet image"
    import PIL.Image as Image
    from pyglet.image import ImageData
    
    image = image.transpose(Image.FLIP_TOP_BOTTOM)

    # Convert bitmap and palette images to component
    if image.mode in ('1', 'P'):
        image = image.convert()

    if image.mode not in ('L', 'LA', 'RGB', 'RGBA'):
        raise ImageDecodeException('Unsupported mode "%s"' % image.mode)
    type = GL_UNSIGNED_BYTE
    width, height = image.size

    return ImageData(width, height, image.mode, image.tostring())
    
def MakePalette():
    "Generate a palette suitable for PIL.Image.ImageData.putpalette"
    return flatten((i,i,i) for i in xrange(256))
    
    # The below code mimicks the source from the original
    
    # If you want to use this, you need numpy. "easy_install numpy" or whatever 
    # for your distribution
    from numpy import array as narray
    
    # Some possible destination colours (sepia, or blueish, etc)
    #dst = narray([18,74,155])
    #dst = narray([155,74,18])
    #dst = narray([17,51,96])
    dst = narray([96,51,17])
    
    high = narray([255,255,255])
    firsthalf  = [high + (dst-high)/255*i for i in xrange(0,256,2)]
    secondhalf = [dst + (-dst)/255*i for i in xrange(0,256,2)]
    
    result = flatten(firsthalf+secondhalf)
    result = map(int, result)
    return result
    
#
# ================================
#
# Interesting stuff starts here!
#
# ================================
#    

def GeneratePixelCoordinates(data):
    
    # A lot happens here. We first take data and turn it into byte values
    # array("B", data) is exactly like map(ord, data), but faster
    # grouper(2, ..., VALUE) takes [1,2,3,4,5] and returns ((1,2),(3,4),(5,VALUE))
    # Tally counts the number of occurences of (1,2) and returns a dictionary
    # Then we turn the dictionary into a list of [(key,value), ...]
    # Then we sort by value in decending order (the lambda)
    # At the end of this, we end up with the locations of our pixels
    #   and their colour values
    tallied = tally(grouper(2, array("B", data), 0))
    lst = sorted(tallied.iteritems(), key=lambda x: -x[1])

    # Since we ordered the list, the highest is the first element, and the 
    # lowest is the last. Take the logs.
    highest = log(lst[0][1])
    lowest  = log(lst[-1][1])

    if highest == lowest: return None, None

    # Take the logs, scale so that minimum is at 0, and highest is at 255.
    def compute(x):
        return (x[0], 255*(log(x[1]) - lowest) / (highest - lowest))
        
    lst = map(compute, lst)

    # Here, we create a PIL image, and populate the relevent pixels.
    i = new("P", (256, 256))    
    i.putpalette(MakePalette())
    for position, value in lst: 
        i.putpixel(position, value)
        
    return i, lst

def MakePlainPlot(data):
    """Given a 1D length of data, turn it into a 2D plot the 'straightforward'
    way, by mapping bytes onto RGB components"""
    sqrtsize = int(len(data)**0.5)
    i = new("L", (sqrtsize, sqrtsize))
    # Ensure data is a length % 3 = 0. (i.e. exactly fills the number of pixels)
    data = data[:-(len(data)%3)]
    i.fromstring(data)
    i.show()

class AppWindow(Window):

    def __init__(self, *args, **kwargs):
        clock.schedule(self.update)
        self.autoRun = False
        #self.autoRun = True
        self.translatex = self.translatey = 0
        self.rotx = 0
        self.depthtarget = self.depth = -7
        super(AppWindow, self).__init__(*args,**kwargs)
    
    def on_resize(self, width, height):
        # Override the default on_resize handler to create a 3D projection
        glViewport(0, 0, width, height)
        glMatrixMode(GL_PROJECTION)
        glLoadIdentity()
        gluPerspective(60., width / float(height), .1, 1000.)
        
        glMatrixMode(GL_MODELVIEW)
        
        self.rotspeed = 45 # degrees per second
        
        from ArcBall import ArcBallT
        self.arcball = ArcBallT(width, height)

        return pyglet.event.EVENT_HANDLED

    def on_key_press(self, symbol, modifier):
        global counter
        
        if symbol == key.ESCAPE:
            pyglet.app.exit()
            
        elif symbol == key.SPACE:
            self.autoRun = not self.autoRun
            
        elif symbol == key.LEFT:
            
            counter -= 1
            update_vlist()
            
        elif symbol == key.RIGHT:
            
            counter += 1
            update_vlist()
        
    def on_mouse_scroll(self, x, y, scroll_x, scroll_y):
        self.depthtarget += scroll_y / 4
        
    def on_mouse_press(self, x, y, button, modifiers):
        if button == mouse.RIGHT:
            self.arcball.reset()
        
        elif button == mouse.LEFT:
            self.arcball.click((x, y))
        
    def on_mouse_release(self, x, y, button, modifiers):
        if button != mouse.LEFT: return
        
        self.arcball.release()

    def on_mouse_drag(self, x, y, dx, dy, buttons, modifiers):
        if buttons & mouse.MIDDLE:
            self.translatex += dx / 50
            self.translatey += dy / 50
            return
            
        if not buttons & mouse.LEFT: 
            return
        
        self.arcball.move((x, y))
    
    def update(self, dt):
        if abs(self.depth - self.depthtarget) > 1e-4:
            self.depth += (self.depthtarget - self.depth) / 2
        
        if not self.autoRun:
            return
            
        self.rotx += dt * self.rotspeed
        
        if self.rotx > 360:
            
            global counter
            counter += 1
            update_vlist()
            
            self.rotx = 0

    def makeLabel(self, text):
        return Label(text, font_name='Times New Roman', font_size=20, anchor_x='center')

    def on_draw(self):
         
        glBlendFunc(GL_SRC_ALPHA, GL_DST_ALPHA)

        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glLoadIdentity()
        
        glTranslate(self.translatex, self.translatey, self.depth)
        
        if self.autoRun:
            glRotate(self.rotx,  0, 1, 0)
        
        else:
            # Perform arcball rotation.
            glScale(1,-1,1)
            glMultMatrixf(self.arcball.transform)
            glScale(1,-1,1)
        
        with displayListify("cubeTranslate") as shouldLeave:
            if shouldLeave: raise LeaveWith
                
            # Scale the co-ordinates so that they are correct
            glScale(2/128, 2/128, 2/128)
            
            glColor(0.25, 0.25, 0.25, 1)
            glutWireCube(255)
            
            glTranslate(-128, -128, -128)
        
        with displayListify("allPoints") as shouldLeave:
            if shouldLeave: raise LeaveWith
            
            with SaveMatrix():
                # Flip the co-ordinates and translate upwards
                glScale(1,-1,1)
                glColor(0.25, 0.25, 0.25, 0.5)
                glTranslate(0,-255,0)
                glPointSize(pointSize)
                for dat in reversed(vLists):
                    glTranslate(0., 0., 1./nChunks*255)
                    dat.draw(GL_POINTS)

        with displayListify("axisLabels") as shouldLeave:
            if shouldLeave: raise LeaveWith
        #if True:
            with SaveMatrix():
                glTranslate(128,0,0)
                self.makeLabel("End").draw()
                
                glTranslate(0,0,255)
                self.makeLabel("Beginning").draw()
                
                with SaveMatrix():
                    glTranslate(-128,128,0)
                    glRotate(90,0,0,1)
                    self.makeLabel("Byte 1").draw()
                
                glTranslate(0,255,0)
                self.makeLabel("Byte 2").draw()
        
        with displayListify("fileName") as shouldLeave:
            if shouldLeave: raise LeaveWith
            glLoadIdentity()
            
            with SaveMatrix():
                glColor(1,0,0)
                glTranslate(0,-2.2,-4)
                glScale(1/64, 1/64, 1/64)
                l = self.makeLabel(basename(currentInputFile))
                l.color = (0, 128, 230, 255)
                l.draw()
        
        glTranslate(0,0,-1)
        
        if self.autoRun:
        
            if self.rotx > 360 - 45:
                vlist = vertex_list(4, ('v2i', (-1, -1, 1, -1, 1, 1, -1, 1)))
                glColor(0,0,0,(self.rotx-(360-45))/45)
                vlist.draw(GL_QUADS)
                
            if self.rotx < 45:
                vlist = vertex_list(4, ('v2i', (-1, -1, 1, -1, 1, 1, -1, 1)))
                glColor(0,0,0,1-(self.rotx/45))
                vlist.draw(GL_QUADS)

def makevLists(inputFile):
    with open(inputFile) as f:
        data = f.read()

    datLst = []

    with timer("GeneratePixelCoordinates"):
        chunksize = len(data) // nChunks
        
        for i in xrange(nChunks):
            image, dat = GeneratePixelCoordinates(data[i*chunksize:(i+1)*chunksize])
            if not dat: 
                continue
            # throw away image
            datLst.append(dat)


    vLists = []

    with timer("elementTransform"):
        for element in datLst:
            positions, colours = zip(*element)
            positions = flatten(positions)
            colours = map(int, colours)
            
            vertex_list = pyglet.graphics.vertex_list(len(element),
                ('v2f/static', positions),
                ('c4B/static', flatten(map(lambda x: (x,x,x,192), colours)))
            )
            
            vLists.append(vertex_list)
            
    return vLists

counter =  0
vLists = None

try:
    config = Config(sample_buffers=1, samples=4, 
                    depth_size=16, double_buffer=True)
    window = AppWindow(resizable=True, config=config)
    
except pyglet.window.NoSuchConfigException:
    window = AppWindow(resizable=True)

#~ #vLists = makevLists(path + inputFiles[0])
allvLists = [(inputFile, makevLists(path + inputFile)) for inputFile in inputFiles]

def update_vlist():
    global counter, currentInputFile, vLists, displayLists
    
    # Kill the display lists
    displayLists = {}
    
    counter %= len(allvLists)
    if counter < 0:
        counter = len(allvLists)-1
    currentInputFile, vLists = allvLists[counter]
    
update_vlist()
    
glClearColor(0, 0, 0, 1)
glEnable(GL_BLEND)

pyglet.app.run()
