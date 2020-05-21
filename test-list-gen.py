import os
from pathlib import Path
import json

dir = Path('test-data/bsaber/')

for d in (d for d in dir.glob('*') if d.is_dir()):
    info = json.loads(open(d.joinpath('info.dat')).read())
    bpm = info['_beatsPerMinute']
#    print('("{}" . {})'.format(d.joinpath(info['_songFilename']), bpm))
    print('("{}" . {})'.format(d.joinpath('song.wav'), bpm))

