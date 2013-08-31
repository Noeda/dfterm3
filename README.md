Dfterm3
=======

This is a remote Dwarf Fortress playing software. It quite unfinished but it
somewhat works. You can play and chat while enjoying Dwarves killing
themselves.

Installation instructions
-------------------------

These instructions are for Linux. Not everything works properly in Linux; for
example, launching Dwarf Fortresses has problems.

First, get yourself the Glasgow Haskell Compiler. Its name is usually "ghc" in
Linux distributions. Make sure you get GHC version 7.6.3. Many distributions
have a GHC from the 7.4.x series and that one is no good. The latest Haskell
Platform (2013.2.0.0) uses the correct GHC
and you might want to grab its binaries if you can't get the required software
from your Linux distribution.

Next, get yourself the Dfterm3 source code:

    $ git clone https://github.com/Noeda/dfterm3.git
    $ cd dfterm3

Now, install all the dependencies:

    $ cabal install --only-dependencies

Installing the dependencies might take a moment and may require some
development libraries to be installed. Next, configure and build dfterm3.

    $ cabal configure
    $ cabal build

If everything worked, you will end up with a dfterm3 binary in
`./dist/build/dfterm3/dfterm3`. You can copy this binary to somewhere reachable
from `PATH` or whatever you want. Whatever you do, this binary should be run so
that the working directory contains `web-interface` directory. For example, if
you want to run it directly from the source directory, you could just do:

    $ ln -s ./dist/build/dfterm3/dfterm3 ./
    $ ./dfterm3 --admin-password
    $ ./dfterm3

The `--admin-password` command should be run first because there is no
administrator password yet. You need the administrator panel to configure your
Dwarf Fortresses. For the command I presented above, the administrator panel
can be accessed from <http://127.0.0.1:8081/admin/>.

Now, to configure Dwarf Fortresses, you need Dfhack that has a Dfterm3 plugin
included. I have put the source code at <https://github.com/Noeda/dfhack.git>.
Refer to Dfhack on how to compile and install this. The important thing is that
you end up with a `dfhack` script in the root of the Dwarf Fortress
installation.

Start up a Dwarf Fortress normally and then, in the dfhack command line, write
the following command:

    [DFHack] # start-dfterm3

This should make the Dwarf Fortress visible to Dfterm3. Go to the administrator
panel (see earlier) and you should see the Dwarf Fortress there (it takes at
most 20 seconds for it to appear after you write `start-dfterm3`). You can
write in the name of the game that you want others to see and click
"Register". The game should now be visible to anyone who points their browser
at your computer at the port you gave in `--websocket-http`. For example, if
your IP-address is "1.2.3.4", then the address would be
<http://1.2.3.4:8080/> (remember the last slash '/').


File hierarchy
--------------

    src/              Haskell source code
    main/             Haskell source code for `main` function.
    web-interface/    HTML, JavaScript and other client-side files needed for
                      the web interface.


