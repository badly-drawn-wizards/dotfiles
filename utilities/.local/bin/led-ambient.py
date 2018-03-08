#!/usr/bin/env python3
import numpy as np
import socket
import atexit

from subprocess import check_output
from time import time, sleep

from pyscreenshot import grab
from PIL import Image
from scipy.misc import imresize
from skimage.color import rgb2hsv, hsv2rgb
from mss import mss


def grab_im(sct, monitor):
    res = sct.grab(monitor)
    im = np.frombuffer(res.rgb, dtype='uint8')
    im.resize((res.size.height, res.size.width, 3))
    return im

# # Amount to reduce the height of the image by before processing
# COLUMN_RED = 300
# Number of leds
NUM_LEDS = 120

def process_im(im):
    # im = imresize(im, (COLUMN_RED, NUM_LEDS)) # Reduce image
    # im = rgb2hsv(im) # Convert to HSV
    # idx_x = np.arange(NUM_LEDS)
    # idx_y = np.arange(COLUMN_RED)
    # im_sat = im[:,:,1]
    # im_val = im[:,:,2]
    # arg_y = np.argsort(im_sat, axis=0)
    # sat_w = (1-arg_y/COLUMN_RED)
    # im_val *= sat_w
    # im = hsv2rgb(im) # Convert to RGB
    im = imresize(im, (1, NUM_LEDS)) # Collapse columns
    im //= 4 # Reduce intensity
    im = im[:,:,[1,0,2]] # Convert to GRB
    return im

IP = "192.168.0.7"
PORT = 4242
SSID = "STEENEKAMP"
FPS = 5

def at_home():
    try:
        return SSID in str(check_output(['iwgetid']))
    except:
        return False

def display_on():
    return True

def turn_off_leds():
    if at_home():
        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        sock.sendto(np.zeros(NUM_LEDS).tobytes(), (IP, PORT))

atexit.register(turn_off_leds)

def frame_limiter(fps):
    period = 1/fps
    then = time()
    while True:
        then += period
        yield
        while True:
            delta = then - time()
            if delta <= 0:
                break
            sleep(delta)


def start_ambient():
    sct = mss()
    monitor = sct.monitors[0]
    try:
        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        for _ in frame_limiter(FPS):
            row = process_im(grab_im(sct, monitor))
            sock.sendto(row.tobytes(), (IP, PORT))
    except KeyboardInterrupt as e:
        turn_off_leds()
        raise e
    except OSError as e:
        print("Disconnected.")
    finally:
        sock.close()

def main():
    while True:
        while not at_home() or not display_on():
            pass
        start_ambient()

if __name__ == '__main__':
    main()
