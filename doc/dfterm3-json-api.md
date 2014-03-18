Dfterm3 JSON/WebSocket API
================

This document describes the JSON API in Dfterm3 as it is used in a WebSocket
connection.

The version of this document is 0.1.0. The version semantics are as follows:

    MAJOR.MINOR.PATCH

If MAJOR is increased, then the API has changed in a backwards incompatible
manner.

If MINOR is increased, then the API has added something to the API but the
changes are backwards compatible.

if PATCH is increased, then the API is the same but bug fixes may have been
applied to this document.

It is undefined how dfterm3 will respond if client does not quite obey this
document.

Connection mechanics
====================

At the moment, only connections encapsulated in a WebSocket are considered. The
procedure of communication is as follows.

All the JSON sent are put in WebSocket text packets and that JSON should be a
single object. All the messages defined in this document are single objects.

Handshaking
-----------

After client connects to the server, the server will send a handshake to the
client.

    server -> client
    {
        "message":"handshake"
       ,"server":"dfterm3"
       ,"major":MAJOR
       ,"minor":MINOR
       ,"patch":PATCH
    }

MAJOR, MINOR and PATCH describe the protocol version the server is using, as
integers. In this version of protocol, the burden of adapting to different
versions is on client.

Client has to send a login message before any further communication can happen.

    client -> server
    {
        "message":"login"
       ,"identity":IDENTITY
       ,"password":PASSWORD       optional
    }

PASSWORD is a JSON string, if given.

IDENTITY shall take the form of one of the following objects.

    {
        "user":USERNAME
    }

    or

    {
        "guest":true
    }

These two forms are for users with accounts and users without accounts.

The server responds to a login message with the following message.

    server -> client
    {
        "message":"login_acknowledgement"
       ,"status":STATUS
       ,"notice":NOTICE
    }

Where STATUS is either the boolean true or false, for successful and
unsuccessful logins respectively. NOTICE is a string meant for humans to read
and should be told to the user logging in. This is used to tell why a login
might have been unsuccessful. In case of successful login, it can contain any
server notice that should be seen at login such as a MOTD.

Only after a successful login has been completed, other kinds of messages can
be exchanged.

It is possible to perform login again during the connection but only to upgrade
from a guest to a user. If you are already logged in as a user, you are not
allowed to login to another user. You must disconnect and make another
connection to login as another user.

Game sessions
-------------

Client requests a list of active games by sending this message:

    client -> server
    {
        "message":"query_all_games"
    }

Server responds with this message:

    server -> client
    {
        "message":"all_games"
       ,"games":ARRAY
    }

ARRAY is an array of games. Each item in the array has the following form:

    {
        "game_name":GAME-NAME
        "instance_name":INSTANCE-NAME
        "key":KEY
        "made-available":MADE-AVAILABLE        optional
        "handler":HANDLER                      optional
    }

GAME-NAME is the general name of what game this is. As in, this is "Dwarf
Fortress" for all instances that have Dwarf Fortress running in them.

INSTANCE-NAME is the name of this specific game. If GAME-NAME is "Dwarf
Fortress" then INSTANCE-NAME could be "0.31.11 Running ABC Modpack" or
something like that.

KEY is a string that uniquely identifies this game.

MADE-AVAILABLE is a boolean and can only be seen by administrators. If it is
true, it means the game is registered to the server and can (usually) be seen
by non-admins.

HANDLER is also only seen by administrators and only if the game has been made
available. (that is, MADE-AVAILABLE and HANDLER will never appear in the same
message).

The client can then join a game by sending this message:

    client -> server
    {
        "message":"join_game"
       ,"key":KEY
    }

Server acknowledges the join with this message.

    server -> client
    {
        "message":"join_acknowledgement"
        "status":STATUS
        "notice":NOTICE
        "instance_key":KEY         optional
    }

STATUS is a boolean true if joining was successful. NOTICE contains a string
explaining why joining failed, if it failed. It can also be empty.

KEY is only present if joining succeeded. It contains a unique key that can be
later used to refer to this game instance. It is _not_, in general, the same
key as in game listing; in current implementation it is very likely to be
different. The KEY is always a string.

Available games can change between the time server hands a list of games and
client asks to join a game. The server shall keep state in such a way that when
client asks to join a game, the KEY refers in the same set of keys as was
reported by the server in last listing of games. In case the game is no longer
valid, then trying to join the game will properly report an error.

If the client has logged in as an administrator, they can also do:

    client -> server
    {
        "message":"make_available"
        "handler":HANDLER
        "instance_key":KEY
    }

If "make_available" message is used, the game is made available for playing.
HANDLER is the name of the script to use for dealing with matters such as who
can watch or play the game. They can be added by users of the servers so the
list of valid names cannot all be listed here. However, the following names are
guaranteed to be available:

    "private"             Only administrator(s) can watch and play the game.
                          The game won't even appear in game listings.

    "watch-only"          This game can only be watched. No one
                          (except administators) can play. There are no
                          restrictions on who can watch though. Anyone can use
                          the game chat.

    "silent-watch-only"   Same as "watch-only" but chatting is restricted to
                          administrators only.

Server acknowledges the availability request with this message:

    server -> client
    {
        "message":"make_available_acknowledgement"
        "status":STATUS
        "notice":NOTICE
    }

STATUS is a boolean true if the request succeeded. NOTICE may be a
human-readable message telling why the request failed or it may be empty. It
will always be present.

Game display
------------

Once a game has been joined, the game may send "changesets" that happened in
the game. These changesets can be used to reconstruct a view of the game.

The changeset format is tied to the type of the game. At the moment we only
deal with Dwarf Fortress and that is why this part of the protocol is Dwarf
Fortress specific.

The format of a changeset is as follows:

    server -> client
    {
        "message":"game_changeset"
        "instance_key":KEY
        "changeset":CHANGESET
    }

KEY is the instance key.

CHANGESET is an object of following form:

    {
        "last_player":PLAYER             optional
       ,"width":WIDTH
       ,"height":HEIGHT
       ,"cursor":CURSOR                  optional
       ,"changes_zlib":CHANGES_ZLIB
    }

PLAYER is a string that tells the name of the last player who touched the input
of this game. It may not be present.

WIDTH and HEIGHT are integers that tell the size of the Dwarf Fortress screen
at the moment, in number of characters. They may change during a game.

CURSOR is an object:

    {
        "x":X
       ,"y":Y
    }

Where X and Y tell where on the screen the cursor is right now. If there is no
CURSOR, then it means cursor is not visible.

CHANGES_ZLIB is complex. It is base91[1] encoded binary. The binary has been
compressed with zlib. After uncompression and decoding, the binary has the
following format:

    X Y C U ...

Where X and Y are 16-bit unsigned integers in big endian that tell which
location on the screen has changed. C is 8-bit unsigned integer with the
following internal format.

        BBBBFFFF
        ^      ^
        |      +---- Least significant bit
        |
      Most significant bit

BBBB and FFFF are 4-bit values telling the background and foreground
colors of the location.

U is a 32-bit unsigned integer, in big endian, that tells the unicode code
point in this location. At the moment, the protocol supports having only one
unicode code point per location[2].

All in all, bit by bit, this is the format of one change in one location.

    XXXXXXXX XXXXXXXX YYYYYYYY YYYYYYYY BBBBFFFF UUUUUUUU UUUUUUUU
    UUUUUUUU UUUUUUUU

That's 9 bytes per location.

These changes are packed tighly together in the binary.

When client first joins a game, the first changeset will give changes for every
location on the game display.

[1] http://base91.sourceforge.net/
[2] Internally Dfterm3 has support for arbitrary number of code points per
    location but this is not exposed to the protocol.

Chatting
--------

At the moment, chatting rooms are tied to game instances. That is, one chat
room per one running game, no more, no less.

Client can send a message to a game with this message:

    client -> server
    {
        "message":"chat_to_game"
       ,"instance_key":KEY
       ,"content":CONTENT
    }

KEY is the same as the instance key returned in a join acknowledgement from
server.

Server reports chat events with this message:

    server -> client
    {
        "message":"chat_to_game"
       ,"instance_key":KEY
       ,"speaker":SPEAKER
       ,"content":CONTENT
    }

KEY is the instance key (see above). SPEAKER is a string that tells who was
speaking. It is usually the username but you should not rely on this as future
versions may have anything they want here. CONTENT is a string that contains
the chat message.

The chat messages sent by client to the server are "echoed back" to the client.
This means that client gets a server event for chat messages the client
themselves sent.

User registration
-----------------

Not everyone wants to stay as a guest.

If client is currently logged in as a guest, they are allowed to do a
registration.

    client -> server
    {
        "message":"register"
        "username":USERNAME
        "password":PASSWORD       optional
    }

This message registers a new user and logs in the client as that user. The
client must have already logged in as a guest. PASSWORD is optional but if set,
it will set the first password of the user. However, the server may reject
passwordless registrations.

Server acknowledgement looks like this:

    server -> client
    {
        "message":"register_acknowledgement"
       ,"status":STATUS
       ,"notice":NOTICE
    }

STATUS is a boolean true if registering succeeded and false if it did not.
NOTICE is a human-readable string that tells why registration might have failed
but it can be empty.

If registration failed, the client remains logged in as a guest.

There are no guarantees that registered users are persistent. It is possible
that immediately upon disconnection, the user is removed from the system.


