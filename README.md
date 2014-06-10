Dfterm3
=======

This is a remote Dwarf Fortress playing software. It quite unfinished but it
somewhat works. You can play and chat while enjoying Dwarves killing
themselves.

The main discussion forum is a thread in the Bay12 forums:

<http://www.bay12forums.com/smf/index.php?topic=129995.90>.

Overview
--------

Dwarf Fortress is a single player game that's installed on your computer. This
software takes Dwarf Fortress and puts its output on the web browser. Now Dwarf
Fortress is still running on your computer but you can access and play it over
the web.

As a software, Dfterm3 as a has two parts. One part is the Dfterm3 server and
the other is a Dfhack Dfterm3 plugin\[1\] (which currently works only on
modified Dfhack\[2\]). The Dfterm3 server acts as a HTTP and WebSocket server
that together implement a web interface for playing Dwarf Fortress. The Dfhack
Dfterm3 plugin runs with Dfhack in the Dwarf Fortress process and talks with
the Dfterm3 server to manage screen data and input from the web interface to
Dwarf Fortress.

[1] <https://github.com/Noeda/dfterm3-plugin>

[2] <https://github.com/Noeda/dfhack>

Installation instructions for Linux and Unixy environments
----------------------------------------------------------

(Note that Linux version has problems with very high CPU usage).

As for prerequisites, install GHC, version 7.8.2. It may work on 7.6.3 but I
have not tested this with the current incarnation of the code. You also need
cabal-install but it probably comes with the GHC. Also install node and have
npm in path.

Get the code:

    $ git clone https://github.com/Noeda/dfterm3.git
    $ cd dfterm3

Now, install all the Haskell dependencies:

    $ cabal install --only-dependencies

Installing the dependencies might take a moment and may require some
development libraries to be installed. Next, configure and build dfterm3.

    $ cabal configure
    $ cabal build

If everything worked, you will end up with a dfterm3 binary in
`./dist/build/dfterm3/dfterm3`.

    $ ln -s ./dist/build/dfterm3/dfterm3 ./
    $ ./dfterm3 --admin-password
    $ ./dfterm3

The `--admin-password` command should be run first because there is no
administrator password yet. You need the administrator panel to configure your
Dwarf Fortresses. For the command I presented above, the administrator panel
can be accessed from <http://127.0.0.1:8081/admin/>.

How to make Dwarf Fortresses available in Dfterm3
-------------------------------------------------

If you have the binary Dfterm3 DFHack plugin, install that to Dwarf
Fortress that already has DFHack installed (the plugin usually goes under
hack/plugins directory in Dwarf Fortress).

Compiling the plugin from source is a bit non-straightforward at the moment but
the plugin code is at <https://github.com/Noeda/dfterm3-plugin>.

Assuming that you have successfully installed the plugin, the following command
should be available in the DFHack console:

    [DFHack] # start-dfterm3

This should make the Dwarf Fortress visible to Dfterm3. Go to the administrator
panel (see earlier) and you should see the Dwarf Fortress there (it takes at
most 20 seconds for it to appear after you write `start-dfterm3`). You can
write in the name of the game that you want others to see and click
"Register". The game should now be visible to anyone who points their browser
at your computer at the port you gave in `--websocket-http`. For example, if
your IP-address is "1.2.3.4", then the address would be
<http://1.2.3.4:8080/> (remember the last slash '/').

Make sure that, in Dwarf Fortress configuration, you have

  * Disabled TrueType fonts

  * PRINT_MODE is not TEXT (this mode makes Dfterm3 even less well on Linux)

If you have tilesets turned on, it may affect output on the web screen,
however, tilesets are not supported in the web interface.

Hacking with the web code
-------------------------

The default client-side web code is implemented in the web/ directory. You can
compile (that is, bundle and optimize the JavaScript code) by going there and
typing 'make':

    $ cd web

    # Install dependencies if you need to
    $ npm install browserify
    $ npm install uglify-js
    $ npm install underscore

    $ make

You need node.js installed to do this. The compiled bundle.js ends up in
web/out/ directory.

File hierarchy
--------------

    src/              Haskell source code
    main/             Haskell source code for `main` function.
    web/              HTML, JavaScript and other client-side files needed for
                      the web interface.
    ico/              Dfterm3 icons.

