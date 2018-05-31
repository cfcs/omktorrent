omktorrent [![Build Status](https://travis-ci.org/cfcs/omktorrent.svg?branch=master)](https://travis-ci.org/cfcs/omktorrent)
====================================

Torrent creation tool, some parts are available as a library, but that is probably not very useful at the moment.

It does a single pass over the filesystem, and streams the output, meaning it should
scale to arbitrary-sized torrents without linearly requiring more memory the
more files you have (like other torrent creation tools do).

File-aligned padding is supported, and the default. `--unaligned` is not properly implemented yet.

RFCs:
- [BEP-0003: BitTorrent v1](http://bittorrent.org/beps/bep_0003.html)
- [BEP-0013: `announce-list`](http://bittorrent.org/beps/bep_0012.html)
- [BEP-0027: Private torrents](http://bittorrent.org/beps/bep_0027.html)
- [BEP-0047: Padding files and extended file attributes](http://bittorrent.org/beps/bep_0047.html)
