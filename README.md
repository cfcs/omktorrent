omktorrent [![Build Status](https://travis-ci.org/cfcs/omktorrent.svg?branch=master)](https://travis-ci.org/cfcs/omktorrent)
====================================

Torrent creation tool, some parts are available as a library, but that is probably not very useful at the moment.

It does a single pass over the filesystem, and streams the output, meaning it should
scale to arbitrary-sized torrents without linearly requiring more memory the
more files you have (like other torrent creation tools do).

File-aligned padding is supported, and the default. `--unaligned` is not properly implemented yet.

RFCs:
- BEP-0003: BitTorrent v1
- TODO
