#!/usr/bin/python

####################################################################
# See:
# http://musicmachinery.com/2009/03/02/in-search-of-the-click-track/
####################################################################

import echonest.audio as audio

usage = """
Usage: 
    python click.py <inputFilename>

Example:
    python click.py EverythingIsOnTheOne.mp3
"""

def main(inputFile):
    audiofile = audio.LocalAudioFile(inputFile)
    beats = audiofile.analysis.beats
    avgList = []
    time = 0;
    output = []
    sum = 0
    for beat in beats:
        time += beat.duration
        avg = runningAverage(avgList, beat.duration)
        sum += avg
        output.append((time, avg))
    base = sum / len(output)
    for d in output:
        print d[0], d[1] - base

def runningAverage(list, dur):
   max = 16
   list.append(dur)
   if len(list) > max:
        list.pop(0)
   return sum(list) / len(list)

if __name__ == '__main__':
    import sys
    try:
        inputFilename = sys.argv[-1]
    except:
        print usage
        sys.exit(-1)
    main(inputFilename)

