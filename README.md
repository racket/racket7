This is a work-in-progress implementation of Racket with

 * The macro expander and module system implemented in Racket; see

     racket/src/expander/README.txt

   If you just `make` in the top-level directory, you get that.

 * A reimplementation of Racket to run on top of Chez Scheme; see

     racket/src/cs/README.txt

   for more information, including information on building and running
   Racket on Chez Scheme.

----------------------------------------

This is the source code for the core of Racket.  See
"INSTALL.txt" for full information on building Racket.

To build the full Racket distribution from this repository, run `make`
in the top-level directory. To build the Minimal Racket, run `make
base`.

The rest of the Racket distribution source code is in other
repositories under [the Racket GitHub
organization](https://github.com/racket).

Contribute to Racket by submitting a pull request, joining the
[development mailing list](https://lists.racket-lang.org), or visiting
the IRC channel.

License
-------

Racket
Copyright (c) 2010-2018 PLT Design Inc.

Racket is distributed under the GNU Lesser General Public License
(LGPL).  This implies that you may link Racket into proprietary
applications, provided you follow the rules stated in the LGPL.  You can
also modify Racket; if you distribute a modified version, you must
distribute it under the terms of the LGPL, which in particular states
that you must release the source code for the modified software.  

See racket/src/COPYING_LESSER.txt for more information.
