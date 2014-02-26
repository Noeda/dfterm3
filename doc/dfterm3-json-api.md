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
    }

GAME-NAME is the general name of what game this is. As in, this is "Dwarf
Fortress" for all instances that have Dwarf Fortress running in them.

INSTANCE-NAME is the name of this specific game. If GAME-NAME is "Dwarf
Fortress" then INSTANCE-NAME could be "0.31.11 Running ABC Modpack" or
something like that.

KEY is a string that uniquely identifies this game.

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
    }

STATUS is a boolean true if joining was successful. NOTICE contains a string
explaining why joining failed, if it failed. It can also be empty.

Available games can change between the time server hands a list of games and
client asks to join a game. The server shall keep state in such a way that when
client asks to join a game, the KEY refers in the same set of keys as was
reported by the server in last listing of games. In case the game is no longer
valid, then trying to join the game will properly report an error.


