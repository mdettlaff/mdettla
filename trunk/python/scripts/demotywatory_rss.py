#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Tworzy kanał RSS strony demotywatory.pl zawierający obrazki.

Domyślnie kanał RSS demotywatorów nie ma obrazków i trzeba kliknąć, żeby
przejść na osobną stronę i je zobaczyć. Ten skrypt rozwiązuje ten problem.
Do działania programu wymagane jest oczywiście połączenie z Internetem.
Wynikowy kanał RSS jest wypisywany na standardowe wyjście.
"""


import datetime
import re
import urllib


def get_demotywatory_rss_channel_with_pics():

    def get_pic_url(demot_url):
        for line in urllib.urlopen(demot_url).readlines():
            if 'class="demot" alt' in line:
                pic_url = re.search(
                        r'^\s+<img src="(.*)" class="demot"', line).group(1)
                return pic_url

    rss = ''
    for line in urllib.urlopen('http://demotywatory.pl/rss.xml').readlines():
        if not '<description></description>' in line:
            rss += line
        if 'isPermaLink' in line:
            demot_url = re.search(r'"true">(.*)\?utm_source', line).group(1)
            pic_url = get_pic_url(demot_url)
            rss += '\t\t<description>&lt;img src=&quot;' + \
                    pic_url + '&quot;&gt;</description>\n'
    return rss


if __name__ == '__main__':
    print get_demotywatory_rss_channel_with_pics()
    print '\n<!-- created: ' + str(datetime.datetime.now()) + ' -->'

