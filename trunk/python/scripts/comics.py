#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Ściąga komiksy internetowe do podanego katalogu."""

import re
import os
import sys
import urllib


def download_webcomic(url, img_regex, download_dir, filename):

    def get_img_url(url, img_regex):
        for line in urllib.urlopen(url).readlines():
            if re.match(img_regex, line):
                pic_url = re.match(img_regex, line).group(1)
                return pic_url
        raise Exception('nie znaleziono komiksu na stronie ' + url)

    print u'pobieranie ' + filename + '...',; sys.stdout.flush()
    img_url = get_img_url(url, img_regex)
    if not img_url.startswith('http'):
        url = re.sub(r'(http://.*?)/.*', '\\1', url)
        if img_url.startswith('/'):
            img_url = img_url[1:]
        img_url = url + '/' + img_url
    ext = img_url[-3:] if img_url[-4] == '.' else 'png'
    if download_dir.endswith('/'):
        download_dir = download_dir[:-1]
    download_file = download_dir + '/' + filename + '.' + ext
    urllib.urlretrieve(img_url, download_file)
    print 'gotowe'


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
            r'.*embedding\): (.*?)</h3>', dl_dir, 'xkcd')
    download_webcomic('http://notinventedhe.re/',
            r'.*<img alt="Not Invented Here.* src="(.*?)".*', dl_dir, 'nih')
    download_webcomic('http://sinfest.net/',
            r'.*<img src="(.*?comikaze.*?)".*', dl_dir, 'sinfest')
    download_webcomic('http://freefall.purrsia.com/',
            r'.*<img src="(.*?)".*', dl_dir, 'freefall')
    download_webcomic('http://questionablecontent.net/',
            r'<img id="strip" src="(.*?)">', dl_dir, 'qc')
    download_webcomic('http://phdcomics.com/comics.php',
            r'.*<img src=(.*?comics/archive/phd.*?) .*', dl_dir, 'phdcomics')

