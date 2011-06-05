#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Ściąga komiksy internetowe do podanego katalogu."""

import os
import re
import sys
import urllib
import urllib2


def download_webcomic(url, img_regex, download_dir, filename, def_ext='png'):

    def get_img_url(url, img_regex):
        request = urllib2.Request(url)
        request.add_header('User-Agent', 'Mozilla/5.0')
        for line in urllib2.urlopen(request).readlines():
            if re.match(img_regex, line):
                img_url = re.match(img_regex, line).group(1)
                return img_url

    print filename + '...',; sys.stdout.flush()
    img_url = get_img_url(url, img_regex)
    if img_url is None or len(img_url) < 4:
        print u'nie znaleziono komiksu na stronie ' + url
    else:
        if not img_url.startswith('http'):
            url = re.sub(r'(http://.*?)/.*', '\\1', url)
            if img_url.startswith('/'):
                img_url = img_url[1:]
            img_url = url + '/' + img_url
        ext = img_url[-3:].lower() if img_url[-4] == '.' else def_ext
        if download_dir.endswith('/'):
            download_dir = download_dir[:-1]
        download_file = download_dir + '/' + filename + '.' + ext
        urllib.urlretrieve(img_url, download_file)
        print u'OK'


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print >> sys.stderr, u'Użycie: comics.py KATALOG'
        sys.exit(2)
    dl_dir = sys.argv[1]
    if not os.path.isdir(dl_dir):
        print >> sys.stderr, dl_dir + u' nie jest katalogiem'
        sys.exit(1)

    download_webcomic('http://www.dilbert.com/fast/',
            r'<img src="(.*?)" />', dl_dir, 'dilbert')
    download_webcomic('http://xkcd.com/',
            r'.*embedding\): (.*?)</h3>$', dl_dir, 'xkcd')
    download_webcomic('http://notinventedhe.re/',
            r'.*<img alt="Not Invented Here.*? src="(.*?)"', dl_dir, 'nih')
    download_webcomic('http://sinfest.net/',
            r'.*<img src="(.*?comikaze.*?)"', dl_dir, 'sinfest')
    download_webcomic('http://freefall.purrsia.com/',
            r'.*<img src="(.*?)"', dl_dir, 'freefall')
    download_webcomic('http://questionablecontent.net/',
            r'<img id="strip" src="(.*?)">$', dl_dir, 'qc')
    download_webcomic('http://phdcomics.com/comics.php',
            r'.*<img src=(.*?comics/archive/phd.*?) ', dl_dir, 'phdcomics')
    download_webcomic('http://www.penny-arcade.com/comic/',
            r'\s+<img src="(.*?)"', dl_dir, 'penny_arcade')
    download_webcomic('http://explosm.net/comics/',
            r'.*<img.*? src="(.*?net/db/files/Comics.*?)"', dl_dir, 'cyanide')
    download_webcomic('http://www.smbc-comics.com/',
            r'\s+<img src=\'(.*?)\'><br>$', dl_dir, 'smbc')
    download_webcomic('http://www.smbc-comics.com/',
            r'<img src=\'(.*?after.*?)\'>$', dl_dir, 'smbc_bonus')
    download_webcomic('http://abstrusegoose.com/',
            r'.*?<img.*? src="(.*?strips.*?)"', dl_dir, 'abstrusegoose')
    download_webcomic('http://qwantz.com/index.php',
            r'.*<img src="(.*?)" class="comic"', dl_dir, 'dinosaur')
    download_webcomic('http://userfriendly.org/',
            r'.*<IMG ALT="Latest Strip".*? SRC="(.*?)"', dl_dir, 'uf')
    download_webcomic('http://cowbirdsinlove.com/',
            r'<img src="(.*?)".*id="comic"', dl_dir, 'cowbirds')
    download_webcomic('http://nodwick.humor.gamespy.com/ffn/',
            r'.*<img src="(.*?strips.*?)"', dl_dir, 'ffn')
    download_webcomic('http://www.chainsawsuit.com',
            r'.*<div id="comic">\s*<img src="(.*?)"', dl_dir, 'chainsawsuit')

    download_gocomics_webcomic = lambda name: \
            download_webcomic('http://gocomics.com/' + name,
                    r'\s+<link rel="image_src" href="(.*?)"\s?/>$',
                    dl_dir, name, 'gif')
    download_gocomics_webcomic('garfield')
    download_gocomics_webcomic('monty')
    download_gocomics_webcomic('pearlsbeforeswine')
    download_gocomics_webcomic('scarygary')
    download_gocomics_webcomic('meaningoflila')
    download_gocomics_webcomic('onaclaireday')
    download_gocomics_webcomic('daddyshome')
    download_gocomics_webcomic('cafeconleche')
    download_gocomics_webcomic('calvinandhobbes')

