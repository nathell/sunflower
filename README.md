Sunflower
=========

What is Sunflower?
------------------

Sunflower extracts the text proper out of similarly-formatted HTML
files.  When you download an offline mirror of a Web site, the HTML
files typically consist mostly of noise, such as advertisements,
header, footer, a bunch of links.  Sunflower is able to extract
most of this noise automatically, with only minimal human intervention.

Sunflower (and its predecessor) has been used to extract many press
texts for the [National Corpus of Polish].

How it works
------------

Just launch Sunflower and point it at the directory containing your
collection.  Sunflower will pick one file at random, extract all the strings
from it and ask you to pick the strings that are contained in the
text proper.  You don't have to select all the strings -- typically
two or three will do.

Once you are satisfied with the extracted document, click Next.
Sunflower will then process another file from your collection, using
the extraction rules it has just learned, and show you the result for
review.  Upon confirmation, the entire collection will be backed up
(in case something goes wrong), and then modified in place.

Building it
-----------

Sunflower is written in Clojure, using Leiningen. Just say

    lein uberjar

to produce a standalone jar.

Name and author
---------------

The program is so named because it hulls the essence out of HTML
files, similarly to removing hull and extracting the kernel out of a
sunflower seed.

Written by [Daniel Janus], 2010; available under the terms of MIT
License. See the file LICENSE for details.

 [Daniel Janus]: http://danieljanus.pl
 [National Corpus of Polish]: http://nkjp.pl/index.php?page=0&lang=1
