The repository contains the commit history for

 * Racket's switch from a C-implemented expander and module system to
   a Racket-implemented expander and module system; and

 * the initial implementation of Racket to run on Chez Scheme.

The changes have been merged (as a single commit) to the main Racket
repository, and this repository is no longer modified.

----------------------------------------

This is the source code for the core of Racket. See "INSTALL.txt" for
full information on building Racket.

To build the full Racket distribution from this repository, run `make`
in the top-level directory. To build minimal Racket, run `make base`.

The rest of the Racket distribution source code is in other
repositories, mostly under [the Racket GitHub
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
